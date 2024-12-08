library(data.table)
library(terra)
library(ggplot2)
library(tictoc)

setwd("/media/huijieqiao/SSD_Fast_11/bs.diet/bs.diet")
res1<-c(15, 10)
res2<-c(50, 0)
res3<-c(0, 20)

resources<-readRDS("../Data/resources_raw.full.rda")
resource_conf<-readRDS("../Data/resources_raw.full.conf.rda")
land_size<-nrow(resources[[1]])
#direction<-c(-2, -1, 0, 1, 2)
direction<-c(-1, 0, 1)
blank_path<-resources[[1]]
values(blank_path)<-0
init_loc<-data.table(x=land_size/2, y=land_size/2)

template<-list(id=0, 
               sp_id="",
               init_hp=1000, 
               hp=1000,
               hp_lost=10,
               efficiency=c(10, 10),
               max_age=6000,
               age=0,
               reproduction_threshold=5000,
               reproduction_probability=0.1,
               reproduction_cost=3000,
               init_loc=init_loc,
               loc=init_loc,
               hp_gain=0,
               move=F,
               alive=T,
               path=blank_path,
               direction=direction,
               label="")

individual_list<-list()


for (i in c(1:10)){
  individual<-template
  individual$id<-i
  individual$sp_id<-sprintf("generalized.%d", i)
  individual$efficiency<-res1
  individual$label<-"generalized"
  individual_list[[i]]<-individual
}

for (i in c(11:20)){
  individual<-template
  individual$id<-i
  individual$sp_id<-sprintf("specialized1.%d", i-10)
  individual$efficiency<-res2
  individual$label<-"specialized1"
  individual_list[[i]]<-individual
}


for (i in c(21:30)){
  individual<-template
  individual$id<-i
  individual$sp_id<-sprintf("specialized2.%d", i-20)
  individual$efficiency<-res3
  individual$label<-"specialized2"
  individual_list[[i]]<-individual
}

saveRDS(individual_list, sprintf("../Data/sp.%d.%d.%d.%d.%d.%d.rda", 
                                 res1[1], res1[2], res2[1], res2[2], res3[1], res3[2]))