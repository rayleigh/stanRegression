add_fixed_eff_term_to_model_section <- function(parsed_term, model_section)
{
  return(paste(model_section, create_prior_line(parsed_term$"param_terms"[[1]], "normal(0, 10)"), sep = ""))
}

add_varying_term_rand_eff_term_to_model_section <- function(parsed_term, model_section)
{
  model_section <- paste(model_section, create_prior_line(parsed_term$"param_terms"[[1]], "normal(0,1)"))
  model_section <- paste(model_section, create_prior_line(parsed_term$"param_terms"[[2]], "normal(0,1)"))
  
  return(model_section)
}

add_varying_term_with_intercept_rand_eff_term_to_model_section <- function(parsed_term, model_section)
{
  model_section <- paste(model_section, create_prior_line(parsed_term$"param_terms"[[1]], "normal(0,1)"))
  
  term_mod_for_matrix <- paste("to_vector(", parsed_term$"param_terms"[[2]], ")", sep = "")
  model_section <- paste(model_section, create_prior_line(term_mod_for_matrix, "normal(0,1)"))

  model_section <- paste(model_section, create_prior_line(parsed_term$"param_terms"[[3]], "lkj_corr_cholesky(2)"))
  
  return(model_section)
}

add_intercept_to_model_section <- function(model_section, default_prior)
{
  return(paste(model_section, create_prior_line("Intercept", "normal(0, 10)"), sep = ""))
}

add_ll_stmt_to_model_section_for_gaussian <- function(ll_stmt, resp_term_size, model_section)
{
  model_section <- paste(model_section, create_prior_line("sigma", "cauchy(0,2)"), sep = "")
  return(add_ll_stmt_to_section(ll_stmt, resp_term_size, "mu", model_section))
}

add_ll_stmt_to_model_section_for_binomial <- function(ll_stmt, resp_term_size, model_section)
{
  return(add_ll_stmt_to_section(ll_stmt, resp_term_size, "p", model_section))
}

add_resp_term_to_model_section_for_gaussian <- function(resp_term, model_section)
{
  return(paste(model_section, create_prior_line(resp_term$data_terms[[1]], "normal(mu, sigma)"), sep = ""))
}

add_resp_term_to_model_section_for_binomial <- function(resp_term, model_section)
{
  model_distr <- paste("binomial_logit(", resp_term$"data_terms"[[resp_term$"special_vectors_index"$"sample_size"]], ", p)", sep = "")
  
  return(paste(model_section, create_prior_line(resp_term$data_terms[[1]], model_distr), sep = ""))  
}

create_prior_line <- function(var_name, prior)
{
  line <- paste(prior, ";", sep = "")
  line <- paste(var_name, line, sep = " ~ ")
  return(line)
}
