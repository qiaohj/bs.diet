
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
  neighbors<-neighbors[!(x==loc$x & y==loc$y)]
  neighbors
}

if (F){
  
  loc_test<-data.table(x=c(0, 2, 44, 1, 100, 100), y=c(1, 1, 43, 100, 100,1))
  index2xy(xy2index(loc_test,land_size),land_size)
  
  getNeighbors(loc_test[1], direction, land_size)
  getNeighbors(loc_test[2], direction, land_size)
  getNeighbors(loc_test[3], direction, land_size)
  getNeighbors(loc_test[4], direction, land_size)
  getNeighbors(loc_test[5], direction, land_size)
}