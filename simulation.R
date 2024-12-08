library(data.table)
library(terra)
library(ggplot2)
library(tictoc)
rm(list=ls())
#setwd("/media/huijieqiao/SSD_Fast_11/bs.diet/bs.diet")
setwd("~/GIT/bs.diet/Script")
args = commandArgs(trailingOnly=TRUE)
res.conf<-args[1]
sp.conf<-args[2]



source("env.R")
source("functions.R")

#
resources<-readRDS(sprintf("../Data/%s.rda", res.conf))
resource_conf<-readRDS(sprintf("../Data/%s.conf.rda", res.conf))
land_size<-nrow(resources[[1]])
#direction<-c(-2, -1, 0, 1, 2)


individual_list<-readRDS(sprintf("../Data/%s.rda", sp.conf))

blank_path<-resources[[1]]
values(blank_path)<-0
number_species<-c()
for (i in c(1:length(individual_list))){
  individual<-individual_list[[i]]
  individual$path<-blank_path
  number_species<-c(number_species, individual$id)
  individual_list[[i]]<-individual
}

number_species<-max(number_species)
log<-list()
resource_snapshot<-list()
max_steps<-1e4
pb <- txtProgressBar(min=1, max=max_steps, initial=1, style=3)
steps<-1
for (steps in c(3556:max_steps)){
  #get the next location for all the individuals
  #tic("get the next location for all the individuals")
  for (i in c(1:length(individual_list))){
    individual<-individual_list[[i]]
    
    if (individual$alive & individual$move){
      neighbors<-getNeighbors(individual$loc, 
                              individual$direction, 
                              land_size)
      
      #try to move to a fresh cell which is never been visited by the given individual.
      path_index_raw<-xy2index(neighbors, land_size)
      path_index<-path_index_raw
      path_index<-path_index[!values(individual$path)[path_index]==1]
      
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
      values(individual$path)[index]<-1
    }
    individual_list[[i]]<-individual
  }
  #toc()
  #calculating the energy distribution for all the individuals in the same cell
  #tic("calculating the energy distribution for all the individuals in the same cell")
  cell_list<-list()
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
  #tic("Iterate through the cells which have individual(s), calculate the energy cost, lost and gain")
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
          #individual_list[[cell_list[[i]]$individuals[k]]]<-individual
        }
        taken_efficiency<-sum(taken_efficiency)
        base<-ifelse(taken_efficiency<v_afford[j], 
                     v_afford[j], taken_efficiency)
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
  
  #toc()
  
  #set the new hp for each individual
  #tic("set the new hp for each individual")
  for (i in c(1:length(individual_list))){
    individual<-individual_list[[i]]
    if (individual$alive){
      individual$hp<-individual$hp-individual$hp_lost+individual$hp_gain
      individual$age<-individual$age+1
      individual$move<-individual$hp_gain<individual$hp_lost
      individual$alive<-(individual$hp>0)&(individual$age<=individual$max_age)
      individual_log<-data.table(id=individual$id, 
                                 sp_id=individual$sp_id, step=steps,
                                 x=individual$loc$x, y=individual$loc$y,
                                 hp=individual$hp, move=individual$move,
                                 hp_gain=individual$hp_gain,
                                 hp_lost=individual$hp_lost,
                                 label=individual$label,
                                 alive=individual$alive
      )
      log[[length(log)+1]]<-individual_log
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
  resource_snapshot[[steps]]<-serialize(resources, NULL)
  setTxtProgressBar(pb, steps)
  #toc()
}
logdf<-rbindlist(log)
target<-sprintf("%s_%s", res.conf, sp.conf)
saveRDS(logdf, sprintf("../Results/log.%s.rda", target))
saveRDS(resource_snapshot, sprintf("../Results/resource_snapshot.%s.rda", target))

table(logdf$move)
log_path<-unique(logdf[, c("id", "x", "y", "label", "alive")])
ggplot(log_path)+geom_tile(aes(x=x, y=y, fill=label))+
  facet_grid(label~alive)
plot(resources[[c(1, 2)]])
p1<-ggplot(logdf)+geom_line(aes(x=step, y=hp, group=id, color=label))

log_N<-logdf[, .(N=length(unique(id))), by=list(step, label)]

p2<-ggplot(log_N)+geom_line(aes(x=step, y=N, color=label))
ddd<-list()
for (i in c(1:length(resource_snapshot))){
  rrr<-unserialize(resource_snapshot[[i]])
  
  for (j in c(1:nlyr(rrr))){
    item<-data.frame(step=i, resource=j,
                     v=sum(values(rrr[[j]])))
    ddd[[length(ddd)+1]]<-item
  }
}
ddd<-rbindlist(ddd)
saveRDS(ddd, sprintf("../Results/resource_summary.%s.rda", target))
p3<-ggplot(ddd)+geom_line(aes(x=step, y=v, color=factor(resource)))+
  scale_y_log10()

p<-ggpubr::ggarrange	(plotlist = list(p1, p2, p3), nrow =3)
ggsave(p, filename=sprintf("../Figures/fig.%s.png", target), width=10, height=15)
if (F){
  NNN<-logdf[, .(N=.N, label=length(unique(label))), by=list(x, y, step)]
  
  NNN[label==3]
  
  
  xx<-16
  yy<-17
  stepsss<-13
  log[step==stepsss & x==xx & y==yy]
  values(resource_snapshot[[stepsss-1]])[xy2index(data.table(x=xx, y=yy), land_size),]
  values(resource_snapshot[[stepsss]])[xy2index(data.table(x=xx, y=yy), land_size),]
  
  (40.64046 -10)/4
  
  
}