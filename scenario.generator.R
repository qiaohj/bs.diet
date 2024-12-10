library(googlesheets4)
library(googledrive)
library(glue)
library(dplyr)
library(data.table)
setwd("~/GIT/bs.diet/Script")
rm(list=ls())

config_link<-'https://docs.google.com/spreadsheets/d/1GsrbPiJvf2VvMOykcKvj6ZzB0uR06gOosY9EqTleAUM/edit?usp=sharing'
resource.template<-data.table(read_sheet(config_link,
                                         sheet="resource.template"))

resource.group<-data.table(read_sheet(config_link,
                                      sheet="resource.group"))

for (j in c(1:nrow(resource.group))){
  resource.group.item<-resource.group[j]
  target<-sprintf("../Data/Resources/%d.raster.rda", resource.group.item$resource.group.id)
  if (file.exists(target)){
    next()
  }
  resource.template.ids<-
    as.numeric(trimws(strsplit(resource.group.item$resource.template.id, ",")[[1]]))
  resource_conf<-list()
  for (i in c(1:length(resource.template.ids))){
    item_template<-resource.template[resource.template.id==resource.template.ids[i]]
    nrow_col<-item_template$land_size/item_template$resolution
    if (nrow_col!=round(nrow_col)){
      stop("land_size should be divisible by resolution")
    }
    if (item_template$n_cell_with_resource>item_template$land_size^2){
      stop("n_cell_with_resource should be smaller than resolution^2.")
    }
    conf_item<-data.table(land_size=item_template$land_size, 
                          resolution=item_template$resolution,
                          n_cell_with_resource=item_template$n_cell_with_resource,
                          r=item_template$r,
                          K=item_template$K,
                          init_energy=item_template$init_energy,
                          min_energy=item_template$min_energy)
    resource_conf[[length(resource_conf)+1]]<-conf_item
  }
  resource_conf<-rbindlist(resource_conf)
  if (length(unique(resource_conf$land_size))!=1){
    stop("all the landsize should be the same")
  }
  resource_conf$id<-c(1:nrow(resource_conf))
  resources<-list()
  for (i in c(1:nrow(resource_conf))){
    resource_conf_item<-resource_conf[i]
    nrow_col<-resource_conf_item$land_size/resource_conf_item$resolution
    land<-rast(vals=c(1:nrow_col^2),
               nrows=nrow_col, 
               ncols=nrow_col,
               xmin=0, xmax=resource_conf_item$land_size,
               ymin=0, ymax=resource_conf_item$land_size,
               resolution=resource_conf_item$resolution)
    index<-sample(nrow_col^2, resource_conf_item$n_cell)
    values(land)<-0
    values(land)[index]<-resource_conf_item$init_energy
    if (resource_conf_item$resolution!=1){
      land_raw<-disagg(land, fact=resource_conf_item$resolution)
    }else{
      land_raw<-land
    }
    resources[[i]]<-land_raw
  }
  #plot(resources[[1]])
  #plot(resources[[2]])
  resources_raw<-rast(resources)
  names(resources_raw)<-sprintf("resource.%d", resource_conf$id)
  #plot(resources_raw[[2]])
  
  saveRDS(resources_raw, sprintf("../Data/Resources/%d.raster.rda", resource.group.item$resource.group.id))
  saveRDS(resource_conf, sprintf("../Data/Resources/%d.conf.rda", resource.group.item$resource.group.id))
}

species<-data.table(read_sheet(config_link,
                               sheet="species"))
species$species.id<-as.character(species$species.id)
individual.pool<-data.table(read_sheet(config_link,
                                       sheet="individual.pool"))

for (i in c(1:nrow(individual.pool))){
  individual.pool.item<-individual.pool[i]
  target<-sprintf("../Data/IndividualPools/%d.rda", individual.pool.item$individual.pool.id)
  if (file.exists(target)){
    next
  }
  resource.group.conf<-readRDS(sprintf("../Data/Resources/%d.conf.rda", individual.pool.item$resource.group.id))
  resources<-readRDS(sprintf("../Data/Resources/%d.raster.rda", individual.pool.item$resource.group.id))
  
  blank_path<-resources[[1]]
  values(blank_path)<-0
  blank_path<-serialize(blank_path, NULL)
  
  species.ids<-strsplit(individual.pool.item$species.id, ",")[[1]]
  counts<-as.numeric(trimws(strsplit(individual.pool.item$count, ",")[[1]]))
  
  if (length(species.ids)!=length(counts)){
    stop("the length of counts doesn't equal to length of species.ids")
  }
  individual_list<-list()
  for (j in c(1:length(species.ids))){
    species.item<-species[species.id==species.ids[j]]
    if (nrow(species.item)!=1){
      stop(sprintf("Error species.id: %s. Not found or duplicated species.id", species.ids[j]))
    }
    efficiency<-as.numeric(trimws(strsplit(species.item$efficiency, ",")[[1]]))
    if (length(efficiency)!=nrow(resource.group.conf)){
      stop("efficiency should equal to the number of resources")
    }
    direction<-as.numeric(trimws(strsplit(species.item$direction, ",")[[1]]))
    for (k in c(1:counts[j])){
      species.item$hp<-species.item$init_hp
      species.item$age<-species.item$init_age
      
      if (species.item$init_loc=="RND"){
        loc<-data.table(x=round(runif(1, 1, resource.group.conf[1]$land_size)),
                        y=round(runif(1, 1, resource.group.conf[1]$land_size)))
      }else{
        loc<-as.numeric(trimws(strsplit(species.item$init_loc, ",")[[1]]))
        loc<-data.table(x=loc[1], y=loc[2])
      }
      species.item$hp_gain<-0
      str_formula <- species.item$death_fun
      formula_obj <- as.formula(str_formula)
      rhs <- as.expression(formula_obj[[3]])
      
      individual.item<-list(id=0, 
           sp_id=species.item$species.id,
           init_hp=species.item$init_hp, 
           hp=species.item$hp,
           hp_lost_move=species.item$hp_lost_move,
           hp_lost_reside=species.item$hp_lost_reside,
           death_fun=rhs,
           efficiency=efficiency,
           max_age=species.item$max_age,
           age=species.item$age,
           reproduction_threshold=species.item$reproduct_th,
           reproduction_probability=species.item$reproduct_prob,
           reproduction_cost=species.item$reproduct_cost,
           init_loc=species.item$init_loc,
           loc=loc,
           hp_gain=0,
           move=F,
           alive=T,
           path=blank_path,
           direction=direction)
      individual.item$id<-length(individual_list)+1
      individual_list[[individual.item$id]]<-individual.item
    }
    
  }
  saveRDS(individual_list, target)
}


