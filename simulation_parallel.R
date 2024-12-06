library(data.table)
library(terra)
library(ggplot2)
library(tictoc)
library(parallel)
library(SharedObject)
setwd("~/GIT/bs.diet/Script")
rm(list=ls())
#
resources<-readRDS("../Data/resources_raw.100.10.rda")
#resources<-share(resources)
resource_conf<-readRDS("../Data/resources_conf.100.10.rda")
land_size<-nrow(resources[[1]])
direction<-c(-2, -1, 0, 1, 2)
blank_path<-resources[[1]]
values(blank_path)<-0

number_species<-10

individual_list <- share(list())

for (i in c(1:number_species)){
  init_loc<-round(runif(2, 1, land_size))
  init_loc<-data.table(x=init_loc[1], y=init_loc[2])
  init_loc<-data.table(x=50, y=50)
  
  individual<-list(id=i, 
                   sp_id=as.character(i),
                   init_hp=1000, 
                   hp=1000,
                   hp_lost=10,
                   efficiency=c(0.4, 0.3, 0, 0, 0.1),
                   max_age=6000,
                   age=0,
                   reproduction_threshold=5000,
                   reproduction_probability=0.1,
                   reproduction_cost=3000,
                   init_loc=init_loc,
                   loc=init_loc,
                   hp_gain=0,
                   move=F,
                   alive=T)
  individual_list[[i]]<-share(individual)
}

xy2index<-function(loc, land_size){
  index<-land_size * (loc$y-1)+loc$x
  index[between(index, 1, land_size^2)]
}

index2xy<-function(index, land_size){
  y <- (index - 1) %/% land_size + 1
  x <- (index - 1) %% land_size + 1
  
  data.table(x=x, y=y)
}
getNeighbors<-function(loc, direction, land_size){
  neighbors<-data.table(expand.grid(x=direction+loc$x, 
                                    y=direction+loc$y))
  neighbors<-neighbors[between(x, 1, land_size) &
                         between(y, 1, land_size)]
  neighbors<-neighbors[x!=loc$x & y!=loc$y]
  neighbors
}
setResources<-function(item){
  values(resources)[item$index,]<<-item$v
  return(NULL)
}
calc_lost_gain<-function(i, land_size, resources,
                         resource_conf){
  loc_str<-names(cell_list)[i]
  loc_str<-strsplit(loc_str, "_")[[1]]
  loc<-data.table(x=as.numeric(loc_str[1]), y=as.numeric(loc_str[2]))
  index<-(loc$y-1)*land_size + loc$x
  v<-values(resources)[index,]
  v_afford<-v-resource_conf$min_energy
  v_afford[v_afford<0]<-0
  for (j in c(1:length(v_afford))){
    if (v_afford[j]>0){
      taken_efficiency<-rep(0, length(cell_list[[i]]$individuals))
      for (k in c(1:length(cell_list[[i]]$individuals))){
        individual<-individual_list[[cell_list[[i]]$individuals[k]]]
        taken_efficiency[k]<-individual$efficiency[j]
        #individual_list[[cell_list[[i]]$individuals[k]]]<-individual
      }
      taken_efficiency<-sum(taken_efficiency)
      base<-ifelse(taken_efficiency<1, 1, taken_efficiency)
      #Calculate the energy gain for each individual, the total lost of a given resource in this cell.
      for (k in c(1:length(cell_list[[i]]$individuals))){
        individual<-individual_list[[cell_list[[i]]$individuals[k]]]
        hp_gain<-v_afford[j] * individual$efficiency[j]/base
        individual$hp_gain<-individual$hp_gain+hp_gain
        v[j]<-v[j]-hp_gain
        if (cell_list[[i]]$individuals[k]==1){
          print(sprintf("gained.%f", individual$hp_gain))
        }
        individual_list[[cell_list[[i]]$individuals[k]]]<-individual
      }
    }
  }
  print(sprintf("gained.%f, check", individual_list[[1]]$hp_gain))
  #values(resources)[index,]<-v
  return(list("index"=index, "v"=v)) 
}

loc_test<-data.table(x=c(0, 2, 44, 1, 100, 100), y=c(1, 1, 43, 100, 100,1))
index2xy(xy2index(loc_test,land_size),land_size)

getNeighbors(loc_test[1], direction, land_size)
getNeighbors(loc_test[2], direction, land_size)
getNeighbors(loc_test[3], direction, land_size)
getNeighbors(loc_test[4], direction, land_size)
getNeighbors(loc_test[5], direction, land_size)

log<-list()
resource_snapshot<-list()
max_steps<-1e4
pb <- txtProgressBar(min=1, max=max_steps, initial=1, style=3)

for (steps in c(1:max_steps)){
  #get the next location for all the individuals
  #tic("get the next location for all the individuals")
  for (i in c(1:length(individual_list))){
    individual<-individual_list[[i]]
    
    if (individual$alive & individual$move){
      neighbors<-getNeighbors(individual$loc, direction, land_size)
      
      #try to move to a fresh cell which is never been visited by the given individual.
      path_index_raw<-xy2index(neighbors, land_size)
      path_index<-path_index_raw
      path_index<-path_index[!values(blank_path)[path_index]==1]
      
      
      #if all the neighbors are visited, pick a random neighbor cell.
      if (length(path_index)==0){
        path_index<-path_index_raw
      }
      next_index<-path_index[sample(length(path_index), 1)]
      
      next_loc<-index2xy(next_index, land_size)
      #force to move to a new cell. But if the new cell is at the out
      #of the map, we need to generate a new cell.
      
      individual$loc<-next_loc
      index<-(individual$loc$y-1)*land_size + individual$loc$x
      #values(individual$path)[index]<-1
    }
    individual_list[[i]]<-individual
  }
  #toc()
  #calculating the energy distribution for all the individuals in the same cell
  #tic("calculating the energy distribution for all the individuals in the same cell")
  cell_list<-share(list())
  for (i in c(1:length(individual_list))){
    individual<-individual_list[[i]]
    if (individual$alive){
      loc_id<-sprintf("%d_%d", individual$loc$x, individual$loc$y)
      if (loc_id %in% names(cell_list)){
        cell_list[[loc_id]]$individuals<-c(cell_list[[loc_id]]$individuals, i)
      }else{
        cell_list[[loc_id]]$individuals<-c(i)
      }
    }
  }
  if (length(cell_list)==0){
    print("ALL GONE!")
    break()
  }
  #toc()
  #Iterate through the cells which have individual(s), calculate the energy cost, lost and gain
  tic("Iterate through the cells which have individual(s), calculate the energy cost, lost and gain")
  print(individual_list[[1]]$hp)
  xxx<-mclapply(1:length(cell_list), function(i) 
    calc_lost_gain(i, land_size, resources, resource_conf), mc.cores = 10)
  print(individual_list[[1]]$hp_gain)
  result <- lapply(xxx, setResources)
  resource_snapshot[[length(resource_snapshot)+1]]<-resources
  toc()
  
  #set the new hp for each individual
  #tic("set the new hp for each individual")
  for (i in c(1:length(individual_list))){
    individual<-individual_list[[i]]
    if (individual$alive){
      individual$hp<-individual$hp-individual$hp_lost+individual$hp_gain
      individual$age<-individual$age+1
      individual$move<-individual$hp_gain<individual$hp_lost
      
      individual_log<-data.table(id=individual$id, 
                                 sp_id=individual$sp_id, step=steps,
                                 x=individual$loc$x, y=individual$loc$y,
                                 hp=individual$hp, move=individual$move,
                                 hp_gain=individual$hp_gain,
                                 hp_lost=individual$hp_lost
      )
      log[[length(log)+1]]<-individual_log
      
      
      individual$alive<-(individual$hp>0)&(individual$age<=individual$max_age)
      
      #reproduce
      if (individual$hp>individual$reproduction_threshold & 
          individual$hp_gain>individual$hp_lost &
          individual$alive){
        rnd_number<-runif(1)
        if (rnd_number<=individual$reproduction_probability){
          individual$hp<-individual$hp-individual$reproduction_cost
          number_species<-number_species+1
          new_individual<-individual
          new_individual$init_loc<-individual$loc
          new_individual$id<-number_species
          new_individual$sp_id<-sprintf("%s.%d", individual$sp_id, individual$id)
          new_individual$hp<-individual$init_hp
          new_individual$age<-0
          new_individual$hp_gain<-0
          new_individual$move<-F
          new_individual$alive<-T
          individual_list[[number_species]]<-new_individual
        }
      }
      individual$hp_gain<-0
      individual_list[[i]]<-individual
    }
  }
  #toc()
  
  #resource reproduct
  #tic("resource reproduct")
  all_v<-values(resources)
  v_reproduct<-all_v*resource_conf$r * ((resource_conf$K-all_v)/resource_conf$K)
  values(resources)<-values(resources)+ v_reproduct
  setTxtProgressBar(pb, steps)
  #toc()
}
log<-rbindlist(log)
log
table(log$move)
log_path<-unique(log[, c("id", "x", "y")])
ggplot(log_path)+geom_tile(aes(x=x, y=y, fill=factor(id)))+
  theme(legend.position = "none")
plot(resources[[c(1, 2, 5)]])
ggplot(log)+geom_line(aes(x=step, y=hp, color=factor(id)))+
  theme(legend.position = "none")
