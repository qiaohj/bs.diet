library(ggplot2)
library(data.table)
library(gganimate)
library(stringr)

setwd("~/GIT/bs.diet/Script")
scenaria<-"ind.pool.2"
logs<-readRDS(sprintf("../Data/Simulations/%s.rda", scenaria))
resource<-readRDS(sprintf("../Data/Simulations/resource_snapshot.%s.rda", scenaria))
land<-unserialize(resource[[1]])
land<-land[[1]]
values(land)<-1
land_df<-as.data.frame(land, xy=T)
grid_size<-nrow(land)

cell<-expand.grid(x = 0:grid_size, y = 0:grid_size)
max_hp<-max(logs$hp)
target<-sprintf("../Animations/%s", scenaria)
if (!dir.exists(target)){
  dir.create(target)
  dir.create(sprintf("%s/slides", target))
}
for (i in c(1:max(logs$step))){
  print(i)
  log_item<-logs[step==i]
  log_past<-logs[step<=i]
  p <- ggplot(log_item, aes(x = x, y = y)) +
    geom_tile(data = cell,aes(x = x, y = y), fill = NA, color = "gray90") +
    geom_path(data=log_past, aes(x=x, y=y, color=sp_id, group=id), linewidth = 0.3, alpha=0.5)+
    geom_point(aes(fill=sp_id, color=sp_id), size = 1, shape = 21) +
    geom_segment(aes(x = x - hp/max_hp, xend = x + hp/max_hp, y = y + 0.6, 
                     yend = y + 0.6), linewidth = 0.5) +
    scale_size_identity() +
    coord_fixed() +
    theme_minimal() +
    labs(title = i, x = "X Coordinate", y = "Y Coordinate")+
    theme(legend.position = "none")
  ggsave(p, filename=sprintf("%s/slides/%s.png", target, str_pad(i, 6, pad = "0")), 
         width=10, height=10,
         bg="white")
}
slides_path<-sprintf('%s/%s/slides/*.png', getwd(), target)
movie_path<-sprintf("%s/%s/simulation.mp4", getwd(), target)
unlink(movie_path)
command<-sprintf("ffmpeg -framerate 5 -pattern_type glob -i '%s' -c:v libx264 -pix_fmt yuv420p %s",
                 slides_path, movie_path)
system(command)
