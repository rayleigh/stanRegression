add_parsed_term_to_model_selection <- function(parsed_term, model_section, default_prior)
{
  if (parsed_term$"random_eff")
  {
    model_section <- add_rand_eff_term_to_model_section(parsed_term, param_section)
  }
  else
  {    
    model_section <- add_fixed_eff_term_to_model_section(parsed_term, param_section)
  }
  return(model_section)
}

add_fixed_eff_term_to_model_section <- function(parsed_term, model_section, default_prior)
{
  line <- paste(parsed_term$"param_terms"[[1]], default_prior, sep = " ~ ")
  
  return(paste(model_section, line, ";\n", sep = ""))
}

add_intercept_to_model_section <- function(model_section, default_prior)
{
  line <- paste("Intercept ~ ", default_prior, ";\n", sep = "")
  return(paste(model_section, line, sep = ""))
}

add_ll_stmt_to_model_section_for_gaussian <- function(ll_stmt, resp_term_size, model_section)
{
  model_section <- paste(model_section, "sigma ~ cauchy(0,2);\n", sep = "")
  return(add_ll_stmt_to_section(ll_stmt, resp_term_size, "mu", model_section))
}

add_ll_stmt_to_model_section_for_binomial <- function(ll_stmt, resp_term_size, model_section)
{
  return(add_ll_stmt_to_section(ll_stmt, resp_term_size, "mu", model_section))
}

add_resp_term_to_model_section_for_gaussian <- function(resp_term, model_section)
{
  model_section <- paste(model_section, resp_term$data_terms[[1]], " ~ normal(mu, sigma);\n", sep = "")

  return(model_section)  
}

add_resp_term_to_model_section_for_binomial <- function(resp_term, model_section)
{
  model_section <- paste(model_section, resp_term$data_terms[[1]], " ~ binomial_logit(", resp_term$data_terms[[resp_term$num_terms]],", p);\n", sep = "")
  
  return(model_section)  
}

