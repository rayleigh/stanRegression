#This is a script that informally tests the stan_regression by printing out what the routines will return.
#Test the sections needed.

#--Set up test design matrix--
#Set up factor columns
tst_design_matrix <- as.data.frame(expand.grid(1:3, 1:5, 1:2))

#Set up non-factor columns
num_rows <- dim(tst_design_matrix)[1]
for (i in 1:3)
{
  tst_design_matrix <- cbind(tst_design_matrix, rnorm(num_rows))
}

colnames(tst_design_matrix) <- c("d", "e", "f", "a", "b", "c")

#Set up response columns
y <- rowSums(tst_design_matrix[, c("a", "b", "c")])
y_2 <- y + 1
tst_design_matrix <- cbind(tst_design_matrix, y, y_2)

#--Test adding parsed term to sections--
source('~/Documents/Gelman Research/stanRegression rewrite/rl_stan_lmer.R')

#Test fixed effects
tst_fe_parsed_term <- parse_fixed_eff_term("a", tst_design_matrix)
add_fixed_eff_term_to_data_section(tst_fe_parsed_term, "")
add_fixed_eff_term_to_param_section(tst_fe_parsed_term, "")
add_fixed_eff_term_to_model_section(tst_fe_parsed_term, "")
add_fixed_eff_term_to_ll_stmt(tst_fe_parsed_term, "")

#Test random effects
tst_re_varying_parsed_term <- parse_random_eff_term("a | d", tst_design_matrix)
add_rand_eff_term_to_data_section(tst_re_varying_parsed_term, "")
add_varying_term_rand_eff_term_to_param_section(tst_re_varying_parsed_term, "")
add_varying_term_rand_eff_term_to_trans_param_section(tst_re_varying_parsed_term, "")
add_varying_term_rand_eff_term_to_model_section(tst_re_varying_parsed_term, "")
add_varying_term_rand_eff_term_to_ll_stmt(tst_re_varying_parsed_term, "")

tst_re_varying_w_intercept_parsed_term <- parse_random_eff_term("1 + a | d", tst_design_matrix)
add_rand_eff_term_to_data_section(tst_re_varying_w_intercept_parsed_term, "")
add_varying_term_with_intercept_rand_eff_term_to_param_section(tst_re_varying_w_intercept_parsed_term, "")
add_varying_term_with_intercept_rand_eff_term_to_trans_param_section(tst_re_varying_w_intercept_parsed_term, "")
add_varying_term_with_intercept_rand_eff_term_to_model_section(tst_re_varying_w_intercept_parsed_term, "")
add_varying_term_with_intercept_rand_eff_term_to_ll_stmt(tst_re_varying_w_intercept_parsed_term, "")

#Test intercept
add_intercept_to_param_section("")
add_intercept_to_model_section("")
add_intercept_to_ll_stmt(" bob")

#Test response term
tst_one_resp_parsed_term <- parse_resp_term("y", tst_design_matrix)
tst_two_resp_parsed_term <- parse_resp_term(cbind("y", "y_2"), tst_design_matrix)

#Normal
add_resp_term_to_data_section_for_gaussian(tst_one_resp_parsed_term, "")
add_resp_term_to_model_section_for_gaussian(tst_one_resp_parsed_term, "")
add_ll_stmt_to_model_section_for_gaussian("bob", tst_one_resp_parsed_term$"size"[[1]], "")

#Binomial
add_resp_term_to_data_section_for_binomial(tst_two_resp_parsed_term, "")
add_resp_term_to_model_section_for_binomial(tst_two_resp_parsed_term, "")
add_ll_stmt_to_model_section_for_gaussian("bob", tst_one_resp_parsed_term$"size"[[1]], "")

#--Test on real data--
library(lme4)

stan_lmer(Reaction ~ Days + (Days | Subject), sleepstudy, file_name = "sleepstudy.stan")

stan_lmer(angle ~ recipe * temperature + (1|recipe:replicate), cake, file_name = "cake.stan")

stan_lmer(cbind(incidence, size - incidence) ~ period + (1 | herd), family = "binomial", data = cbpp, file_name = "herd.stan")
