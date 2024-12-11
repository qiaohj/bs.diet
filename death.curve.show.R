str_formula <- "y ~ 4.81 * 10^-9* (x - 1440.59)^2"
formula_obj <- as.formula(str_formula)
rhs <- as.expression(formula_obj[[3]])
max_age<-6000
curve<-list()
for (x in c(0:max_age)){
  calc_env<-list(x=x)
  item<-data.table(x=x, y=eval(rhs, envir = calc_env))
  curve[[x+1]]<-item
}
curve<-rbindlist(curve)
ggplot(curve)+geom_line(aes(x=x, y=y))