library(data.table)
library(terra)
library(ggplot2)
setwd("~/GIT/bs.diet/Script")
rm(list=ls())

#
resources<-readRDS("../Data/resources_raw.100.10.rda")
resource_conf<-readRDS("../Data/resources_conf.100.10.rda")
land_size<-nrow(resources[[1]])
direction<-c(-1, 0, 1)
blank_path<-resources[[1]]
values(blank_path)<-0


individual_list<-list()
for (i in c(1:10)){
  init_loc<-round(runif(2, 1, land_size))
  init_loc<-data.table(x=init_loc[1], y=init_loc[2])
  init_loc<-data.table(x=50, y=50)
  
  individual<-list(id=i, 
                   sp_id=i,
                   init_hp=1000, 
                   hp=1000,
                   full_hp=10000,
                   hp_lost=10,
                   efficiency=c(0.4, 0.3, 0, 0, 0.1),
                   max_age=10000,
                   age=0,
                   reproduction_threshold=5000,
                   reproduction_probability=0.1,
                   reproduction_cost=3000,
                   init_loc=init_loc,
                   loc=init_loc,
                   path=blank_path,
                   hp_gain=0,
                   move=F)
  individual_list[[i]]<-individual
}



#loc<-data.table(x=1, y=1)

log<-list()
resource_snapshot<-list()
max_steps<-1e4
pb <- txtProgressBar(min=1, max=max_steps, initial=1, style=3)

for (steps in c(1:max_steps)){
  #get the next location for all the individuals
  for (i in c(1:length(individual_list))){
    individual<-individual_list[[i]]
    
    if (individual$move){
      while(T){
        neighbors<-data.table(expand.grid(x=c(-1, 0, 1)+individual$loc$x, 
                                          y=c(-1, 0, 1)+individual$loc$y))
        
        #try to move to a fresh cell which is never been visited by the given individual.
        path_index<-(neighbors$y-1)*land_size + neighbors$x
        path_index<-path_index[!values(individual$path)[path_index]==1]
        path_index<-path_index[!is.na(path_index)]
        
        #if all the neighbors are visited, pick a random neighbor cell.
        if (length(path_index)==0){
          path_index<-(neighbors$y-1)*land_size + neighbors$x
          path_index<-path_index[!is.na(path_index)]
        }
        next_index<-path_index[sample(length(path_index), 1)]
        next_y<-floor(next_index/land_size)+1
        next_x<-next_index - (next_y-1) * land_size
        next_loc<-data.table(x=next_x, y=next_y)
        #force to move to a new cell. But if the new cell is at the out
        #of the map, we need to generate a new cell.
        if (next_loc$x<1 | next_loc$x>land_size |
            next_loc$y<1 | next_loc$y>land_size | 
            (next_loc$x==loc$x & next_loc$y==loc$y)){
          next()
        }
        individual$loc<-next_loc
        index<-(individual$loc$y-1)*land_size + individual$loc$x
        values(individual$path)[index]<-1
        break()
      }
    }
    individual_list[[i]]<-individual
  }
  #calculating the energy distribution for all the individuals in the same cell
  cell_list<-list()
  for (i in c(1:length(individual_list))){
    loc_id<-sprintf("%d_%d", individual_list[[i]]$loc$x, individual_list[[i]]$loc$y)
    if (loc_id %in% names(cell_list)){
      cell_list[[loc_id]]$individuals<-c(cell_list[[loc_id]]$individuals, i)
    }else{
      cell_list[[loc_id]]$individuals<-c(i)
    }
  }
  #Iterate through the cells which have individual(s), calculate the energy cost, lost and gain
  for (i in c(1:length(cell_list))){
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
          individual_list[[cell_list[[i]]$individuals[k]]]<-individual
        }
        taken_efficiency<-sum(taken_efficiency)
        base<-ifelse(taken_efficiency<1, 1, taken_efficiency)
        #Calculate the energy gain for each individual, the total lost of a given resource in this cell.
        for (k in c(1:length(cell_list[[i]]$individuals))){
          individual<-individual_list[[cell_list[[i]]$individuals[k]]]
          hp_gain<-v_afford[j] * individual$efficiency[j]/base
          individual$hp_gain<-individual$hp_gain+hp_gain
          v[j]<-v[j]-hp_gain
          individual_list[[cell_list[[i]]$individuals[k]]]<-individual
        }
      }
    }
    values(resources)[index,]<-v
    
  }
  resource_snapshot[[length(resource_snapshot)+1]]<-resources
  
  #set the new hp for each individual
  for (i in c(1:length(individual_list))){
    individual<-individual_list[[i]]
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
    
    individual$hp_gain<-0
    individual_list[[i]]<-individual
  }
  #resource reproduct
  all_v<-values(resources)
  v_reproduct<-all_v*resource_conf$r * ((resource_conf$K-all_v)/resource_conf$K)
  values(resources)<-values(resources)+ v_reproduct
  setTxtProgressBar(pb, steps)
}
log<-rbindlist(log)
log
table(log$move)
log_path<-unique(log[, c("id", "x", "y")])
ggplot(log_path)+geom_tile(aes(x=x, y=y, fill=factor(id)))
plot(resources[[c(1, 2, 5)]])
ggplot(log)+geom_line(aes(x=step, y=hp, color=factor(id)))
