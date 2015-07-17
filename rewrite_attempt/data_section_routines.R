get_data_var_from_fixed_eff_parsed_fixed_term <- function(parsed_fixed_term, data_section, data_var_list)
{
  return(paste(data_section, create_var_data_line_for_array(parsed_fixed_term$"data_terms"[[1]], parsed_fixed_term$"size"[[1]], parsed_fixed_term$"stan_type"[[1]]), ";", sep = ""))
}

get_data_var_from_rand_eff_parsed_term <- function(parsed_term, data_section)
{
  data_section <- paste(data_section, create_data_line_for_array(parsed_term$"data_terms"[[1]], "int", parsed_term$"size"[[1]]), ";", sep = "")
  return(paste(data_section, create_data_line_for_constant(parsed_term$"data_terms"[[2]], "int", "<lower = 1>"), ";", sep = ""))
}

get_data_var_from_resp_term_for_gaussian <- function(parsed_resp_term, data_section)
{
  resp_var_name <- parsed_resp_term$"data_terms"[[1]]
  resp_var_size <- parsed_resp_term$"size"[[1]]
  
  data_section <- paste(data_section, create_var_data_line_for_constant(resp_var_size, "int", "lower=1"), ";", sep = "")
  data_section <- paste(data_section, create_data_line_for_array(resp_var_name, resp_var_size, "real"), ";", sep = "")

  return(data_section)
}

get_data_var_from_resp_term_for_binomial <- function(parsed_resp_term, data_var_list)
{
  resp_var_name <- parsed_resp_term$"data_terms"[[1]]
  resp_var_sample_size <- parsed_resp_term$"data_terms"[[(length(parsed_resp_term$"data_terms") - 1)]]
  resp_var_size <- parsed_resp_term$"size"[[1]]
  
  data_section <- paste(data_section, create_var_data_line_for_constant(resp_var_size, "int", "lower=1"), ";", sep = "")
  data_section <- paste(data_section, create_data_line_for_array(resp_var_name, "int", resp_var_size), ";", sep = "")
  data_section <- paste(data_section, create_data_line_for_array(resp_var_sample_size, "int", resp_var_size), ";", sep = "")
  
  return(data_section)
}

add_parsed_term_to_data_section <- function(parsed_term, data_section, added_data_var_list)
{
  data_var_list <- parsed_term$"data_terms"
  data_var_size_index = 1
  
  for (i in length(data_var_list))
  {
    data_var <- data_var_list[[i]]
    if (!exists(data_var, where = added_data_var_list))
    {
      var_type <- parsed_term$"data_var_type"[[i]]
      if (var_type == "constant")
      {
        data_section <- paste(data_section, create_data_line_for_constant(data_var, parsed_term$"stan_data_type"[[i]], parsed_term$"data_constraint"[[i]]), ";", sep = "")

      } else if (var_type = "array") {

        data_section <- paste(data_section, create_data_line_for_array(data_var, parsed_term$"stan_data_type"[[i]], parsed_term$"size"[[data_var_size_index]]), ";", sep = "")
        data_var_size_index = data_var_size_index + 1                    
        
      } else if (var_type = "matrix") {
        
        data_section <- paste(data_section, create_data_line_for_array(data_var, parsed_term$"stan_data_type"[[i]], parsed_term$"size"[[data_var_size_index]], parsed_term$"size"[[data_var_size_index + 1]]), ";", sep = "")
        data_var_size_index = data_var_size_index + 2
      }
    }
    data_var_list[[length(data_var_list) + 1]] <- data_var
  }
  
  return("data_section" = data_section, "added_vars" = added_data_var_list)
}

create_data_line_for_matrix <- function(var_name, var_type, var_row, var_col)
{
  var_line <- paste(var_name, "[", var_row, ", ", var_col, "];", sep = "")
  var_line <- paste(var_type, var_line, sep = " ")
  
  return(var_line)
}

create_data_line_for_array <- function(var_name, var_type, var_size)
{
  var_line <- paste(var_name, "[", var_size, "];", sep = "")
  var_line <- paste(var_type, var_line, sep = " ")

  return(var_line)
}

create_data_line_for_constant <- function(var_name, var_type, var_constr = "")
{
  
  var_line <- paste(var_type, var_constr, sep = "")
  var_line <- paste(var_line, var_name, sep = " ")
  var_line <- paste(var_line, ";", sep = "")
  
  return(var_line)
}