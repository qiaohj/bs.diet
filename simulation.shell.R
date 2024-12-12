library(googlesheets4)
library(googledrive)
library(glue)
library(dplyr)
library(data.table)
setwd("~/GIT/bs.diet/Script")
rm(list=ls())
source("functions.R")
source("simulation.R")
config_link<-'https://docs.google.com/spreadsheets/d/1GsrbPiJvf2VvMOykcKvj6ZzB0uR06gOosY9EqTleAUM/edit?usp=sharing'
scenario<-read_sheet(config_link, sheet="scenario")
individual.pool<-data.table(read_sheet(config_link,
                                      sheet="individual.pool"))

for (i in c(1:nrow(scenario))){
  item<-scenario[i,]
  
  if (item$status %in% c("RUNNING", "DONE")){
    next()
  }
  
  scenario[i,]$status<-"RUNNING"
  write_sheet(scenario, ss=config_link, sheet="scenario")
  #load resources
  individual.pool.item<-individual.pool[individual.pool.id==item$individual.pool.id]
  rrr<-simulation(individual.pool.item$resource.group.id,
                  individual.pool.item$individual.pool.id,
                  scenario[i,]$max_steps)
  
  drive_upload(rrr$path,
               overwrite=T)
  
  
  drive_share(rrr$filename,
              role = "reader", type = "anyone")
  
  scenario[i,]$link<-googledrive::drive_link(rrr$filename)
  
  scenario[i,]$status<-"DONE"
  
}

#Upload to Google Doc
scenario<-scenario%>%mutate(
  link = glue(
    '=HYPERLINK("{link}", "SEE")'
  )
)
scenario$link<-gs4_formula(scenario$link)
write_sheet(scenario, ss=config_link, sheet="scenario")
