add_fixed_term_to_param_section <- function(parsed_term, param_section)
{
  return(paste(param_section, create_param_constant_line(parsed_term$"param_terms"[[1]])))
}

add_rand_eff_term_to_param_section <- function(parsed_term, param_section)
{
  param_section <- paste(param_section, create_param)
  return(paste(param_section, line, ";", sep = ""))
}

add_intercept_to_param_section <- function(param_section)
{
  return(paste(param_section, create_param_constant_line("Intercept"), sep = ""))
}

add_sigma_to_param_section <- function(param_section)
{
  return(paste(param_section, create_param_constant_line("sigma", "lower=0")))
}

create_param_constant_line <- function(var_name, var_constr = "")
{
  return(paste("real", var_constr, " ", var_name, ";"), sep = "")
}

create_param_vector_line <- function(var_name, var_constr = "", array_size)
{
  return(paste("vector", var_constr, "[", array_size,"]", var_name, ";"), sep = "")
}

create_param_matrix_line <- function(var_name, var_constr = "", num_row, num_col)
{
  return(paste("matrix", var_constr, "[", num_row, ",", num_col , "] ", var_name, ";"), sep = "")
}