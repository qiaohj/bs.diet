library(googlesheets4)
library(googledrive)
library(glue)
library(dplyr)

rappdirs::user_cache_dir("gargle")

drive_deauth()
drive_auth(
  email = "siqi.huang.2015@gmail.com"
)
scenario_link<-'https://docs.google.com/spreadsheets/d/1GsrbPiJvf2VvMOykcKvj6ZzB0uR06gOosY9EqTleAUM/edit?usp=sharing'

x <- read_sheet('https://docs.google.com/spreadsheets/d/1GsrbPiJvf2VvMOykcKvj6ZzB0uR06gOosY9EqTleAUM/edit?usp=sharing',
                sheet="scenario", col_types = "iic??")

drive_upload("/Users/huijieqiao/GIT/bs.diet/Figures/fig.resources_raw.full_sp.10.10.20.0.0.20.png",
             overwrite=T)


drive_share("fig.resources_raw.full_sp.10.10.20.0.0.20.png",
            role = "reader", type = "anyone")
glue(
  '=HYPERLINK("{link}", "{id}")', "http://xxx.com", "xxx")
  
x$link<-googledrive::drive_link("fig.resources_raw.full_sp.10.10.20.0.0.20.png")

x<-x%>%mutate(
  link_url = glue(
    '=HYPERLINK("{link}", "SEE")'
  )
)
x$link_f<-gs4_formula(x$link_url)
class(x$link)<-"hyperlink"
write_sheet(x, ss=scenario_link, sheet="scenario")
