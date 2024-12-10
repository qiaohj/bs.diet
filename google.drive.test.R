
if (F){
  rappdirs::user_cache_dir("gargle")
  
  drive_deauth()
  drive_auth(
    email = "siqi.huang.2015@gmail.com"
  )
  
  
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
  write_sheet(x, ss=config_link, sheet="scenario")
  
  str_formula <- "y ~ 4.81 * 10^-9* (x - 1440.59)^2"
  formula_obj <- as.formula(str_formula)
  x <- 3
  x2 <- 4
  calc_env <- list(x = x)
  
  # 将公式右侧转化为可计算表达式
  rhs <- as.expression(formula_obj[[3]])
  
  # 计算结果
  y_value <- eval(rhs, envir = calc_env)
  
  # 查看结果
  print(y_value)
  curve<-list()
  for (x in c(0:6000)){
    calc_env<-list(x=x)
    item<-data.table(x=x, y=eval(rhs, envir = calc_env))
    curve[[x+1]]<-item
  }
  curve<-rbindlist(curve)
  ggplot(curve)+geom_line(aes(x=x, y=y))
}