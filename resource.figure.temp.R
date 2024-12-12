library(ggplot2)
resources_raw_df<-as.data.frame(resources_raw, xy=T)
l<-list()
for (i in c(3:ncol(resources_raw_df))){
  res.label<-colnames(resources_raw_df)[i]
  item<-resources_raw_df[, c("x", "y", res.label)]
  colnames(item)[3]<-"v"
  item$res<-res.label
  l[[length(l)+1]]<-item
}

l<-rbindlist(l)
ggplot(l)+geom_tile(aes(x=x, y=y, fill=factor(v)))+
  coord_equal()+
  facet_wrap(~res)

resources_raw_df$check<-resources_raw_df$res.3+resources_raw_df$res.6*2

ggplot(resources_raw_df)+geom_tile(
                   aes(x=x, y=y, fill=factor(check)))


ggplot()+geom_tile(data=l[res=="res.3"],
                   aes(x=x, y=y, fill=factor(v)))+
  geom_tile(data=l[res=="res.6"],
            aes(x=x, y=y, fill=factor(v+2)),
            alpha=0.3)
