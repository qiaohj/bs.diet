library(data.table)
library(terra)
library(ggplot2)
setwd("~/GIT/bs.diet/Script")
land_size<-100
resouce_n<-5
resolutions<-c(1, 5, 10, 20, 50)
resource_conf<-data.table(id=c(1:resouce_n), 
                          res=resolutions[round(runif(resouce_n, 1, resouce_n))],
                          energy_density=round(runif(resouce_n, 1, 100)),
                          r=round(runif(resouce_n, 0, 0.1)),
                          K=round(runif(resouce_n, 10, 100)))

resource_conf<-data.table(id=c(1:resouce_n), 
                          res=rep(10, resouce_n),
                          n_cell=rep(10, resouce_n),
                          energy_density=rep(1, resouce_n),
                          r=rep(0.1, resouce_n),
                          K=rep(100, resouce_n),
                          init_energy=rep(100, resouce_n),
                          min_energy=10)
resources<-list()
for (i in c(1:nrow(resource_conf))){
  item<-resource_conf[i]
  nrow_col<-land_size/item$res
  land<-rast(vals=c(1:nrow_col^2),
             nrows=nrow_col, 
             ncols=nrow_col,
             xmin=0, xmax=land_size,
             ymin=0, ymax=land_size,
             resolution=item$res)
  index<-sample(nrow_col^2, item$n_cell)
  values(land)<-0
  values(land)[index]<-item$init_energy
  land_raw<-disagg(land, fact=item$res)
  resources[[i]]<-land_raw
}
resources_raw<-rast(resources)
names(resources_raw)<-sprintf("resource.%d", c(1:nlyr(resources_raw)))
plot(resources_raw[[2]])

saveRDS(resources_raw, "../Data/resources_raw.100.10.rda")
saveRDS(resource_conf, "../Data/resources_conf.100.10.rda")
if (F){
  item<-data.table(id=0, v=100)
  v<-list()
  for (i in c(1:100)){
    v[[i]]<-item
    item<-data.table(id=i, v=item$v+0.1 * item$v * ((1000-item$v)/1000))
  }
  v<-rbindlist(v)
  v
  ggplot(v)+geom_line(aes(x=id, y=v))
}

