source("~/Documents/Gelman Research/Replication/Subsequent Research/R code/plot_fixed_and_rand_effects.R")
library(lme4)
library(rstan)

generate_objects_and_plot_to_compare <- function(formula, data_matrix, model_name, family = "gaussian")
{
  stan_file_name <- paste(model_name, "stan", sep = ".")
  stan_tst_obj <- stan_lmer(formula, family = family, data = data_matrix, file_name = stan_file_name)
  tst_stan_fit <- list(stan(stan_file_name, model_name = model_name, data = stan_tst_obj$data))
  
  tst_lmer_fit <- list(lmer(formula, data_matrix))
  
  pointstyle_list <- c(1, 2)
  colstyle_list <- c("blue", "red")
  
  plot_fixed_effects(tst_lmer_fit, tst_stan_fit, color_list = colstyle_list, point_list = pointstyle_list)
  plot_rand_effects(tst_lmer_fit, tst_stan_fit, color_list = colstyle_list, point_list = pointstyle_list)
}

sleepstudy_folder <- "~/Documents/Gelman Research/stanRegression rewrite/Sleepstudy ex"
dir.create(sleepstudy_folder)
setwd(sleepstudy_folder)
generate_objects_and_plot_to_compare(Reaction ~ Days + (Days | Subject), sleepstudy, "sleepstudy")

cake_folder <- "~/Documents/Gelman Research/stanRegression rewrite/Cake ex"
dir.create(cake_folder)
setwd(cake_folder)
generate_objects_and_plot_to_compare(angle ~ recipe * temperature + (1|recipe:replicate), cake, "cake")

herd_folder <- "~/Documents/Gelman Research/stanRegression rewrite/Herd ex"
dir.create(herd_folder)
setwd(herd_folder)
generate_objects_and_plot_to_compare(cbind(incidence, size - incidence) ~ period + (1 | herd), cbpp, "herd", "binomial")