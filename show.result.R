library(data.table)
library(ggplot2)
setwd("~/GIT/bs.diet/Script")
result<-1
r<-readRDS(sprintf("../Data/Simulations/%d.rda", result))
r


ggplot(r)+geom_line(aes(x=age, y=death_rate, color=sp_id))

ggplot(r[sp_id==3])+geom_line(aes(x=age, y=death_rate, color=factor(id)), linetype=2)+
  geom_line(aes(x=age, y=random, color=factor(id)), linetype=1)
