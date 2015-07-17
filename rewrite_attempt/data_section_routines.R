add_fixed_eff_term_to_data_section <- function(parsed_fixed_term, data_section)
{
  return(paste(data_section, create_var_data_line_for_array(parsed_fixed_term$"data_terms"[[1]], parsed_fixed_term$"size"[[1]], parsed_fixed_term$"stan_type"[[1]]), sep = ""))
}

add_rand_eff_term_to_data_section <- function(parsed_term, data_section)
{
  data_section <- paste(data_section, create_array_data_line(parsed_term$"data_terms"[[1]], "int", parsed_term$"size"[[1]]), sep = "")
  return(paste(data_section, create_constant_data_line(parsed_term$"data_terms"[[2]], "int", "<lower = 1>"), sep = ""))
}

add_resp_term_to_data_section_for_gaussian <- function(parsed_resp_term, data_section)
{
  resp_var_name <- parsed_resp_term$"data_terms"[[1]]
  resp_var_size <- parsed_resp_term$"size"[[1]]
  
  data_section <- paste(data_section, create_var_data_line_for_constant(resp_var_size, "int", "lower=1"), sep = "")
  data_section <- paste(data_section, create_array_data_line(resp_var_name, resp_var_size, "real"), sep = "")

  return(data_section)
}

add_resp_term_to_data_section_for_binomial <- function(parsed_resp_term, data_var_list)
{
  resp_var_name <- parsed_resp_term$"data_terms"[[1]]
  resp_var_sample_size_name <- parsed_resp_term$"data_terms"[[parsed_resp_term$"special_vectors_index"$"sample_size"]]
  resp_var_size <- parsed_resp_term$"size"[[1]]
  
  data_section <- paste(data_section, create_var_data_line_for_constant(resp_var_size, "int", "lower=1"), sep = "")
  data_section <- paste(data_section, create_array_data_line(resp_var_name, "int", resp_var_size), sep = "")
  data_section <- paste(data_section, create_array_data_line(resp_var_sample_size_name, "int", resp_var_size), sep = "")
  
  return(data_section)
}

create_matrix_data_line <- function(var_name, var_type, var_row, var_col)
{
  var_line <- paste(var_name, "[", var_row, ", ", var_col, "];", sep = "")
  var_line <- paste(var_type, var_line, sep = " ")
  
  return(var_line)
}

create_array_data_line <- function(var_name, var_type, var_size)
{
  var_line <- paste(var_name, "[", var_size, "];", sep = "")
  var_line <- paste(var_type, var_line, sep = " ")

  return(var_line)
}

create_constant_data_line <- function(var_name, var_type, var_constr = "")
{
  
  var_line <- paste(var_type, var_constr, sep = "")
  var_line <- paste(var_line, var_name, sep = " ")
  var_line <- paste(var_line, ";", sep = "")
  
  return(var_line)
}