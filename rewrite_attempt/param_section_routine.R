add_fixed_term_to_param_section <- function(parsed_term, param_section)
{
  line <- paste("real", parsed_term$"param_terms"[[1]], sep = " ")
  
  return(paste(param_section, line, ";\n", sep = ""))
}

add_rand_eff_term_to_param_section <- function(parsed_term, param_section)
{
  line <- paste("real b", parsed_term$"stan_name_template", sep = "_")
  
  return(paste(param_section, line, ";\n", sep = ""))
}

add_intercept_to_param_section <- function(param_section)
{
  return(paste(param_section, "real Intercept;\n", sep = ""))
}

add_sigma_to_param_section <- function(param_section)
{
  return(paste(param_section, "real<lower=0> sigma;\n"), sep = "")
}
