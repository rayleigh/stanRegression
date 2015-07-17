add_fixed_eff_term_to_param_section <- function(parsed_term, param_section)
{
  return(paste(param_section, create_param_constant_line(parsed_term$"param_terms"[[1]])))
}

add_varying_term_rand_eff_term_to_param_section <- function(parsed_term, param_section)
{
  param_section <- paste(param_section, create_param_constant_line(parsed_term$"param_terms"[[1]], "<lower=0>"), sep = "")
  param_section <- paste(param_section, create_param_vector_line(parsed_term$"param_terms"[[2]], parsed_term$"data_terms"[[2]]), sep = "")
  return(param_section)
}

add_varying_term_with_intercept_rand_eff_term_to_param_section <- function(parsed_term, param_section)
{
  param_section <- paste(param_section, create_param_vector_line(parsed_term$"param_terms"[[1]],"2", "<lower=0>"), sep = "")
  param_section <- paste(param_section, create_param_matrix_line(parsed_term$"param_terms"[[2]], "2", parsed_term$"data_terms"[[2]]), sep = "")
  param_section <- paste(param_section, create_param_vector_line(parsed_term$"param_terms"[[2]], "2", var_type = "cholesky_factor_corr"), sep = "")
  return(param_section)
}

add_intercept_to_param_section <- function(param_section)
{
  return(paste(param_section, create_param_constant_line("Intercept"), sep = ""))
}

add_sigma_to_param_section <- function(param_section)
{
  return(paste(param_section, create_param_constant_line("sigma", "lower=0")))
}

create_param_constant_line <- function(var_name, var_constr = "", var_type = "real")
{
  return(paste(var_type, var_constr, " ", var_name, ";"), sep = "")
}

create_param_vector_line <- function(var_name, array_size, var_constr = "", var_type = "vector")
{
  return(paste(var_type, var_constr, "[", array_size,"]", var_name, ";"), sep = "")
}

create_param_matrix_line <- function(var_name, num_row, num_col, var_constr = "", var_type = "matrix")
{
  return(paste(var_type, var_constr, "[", num_row, ",", num_col , "] ", var_name, ";"), sep = "")
}