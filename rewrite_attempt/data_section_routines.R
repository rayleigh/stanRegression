#data section related routines
build_data_section_from_var_list <- function(var_list)
{
  
  var_names <- names(var_list)
  
  for (i in 1:length(var_list))
  {
    var_info = var_list[[i]]
    var_name = var_names[i]
    
    if (exists('constraint', where = var_info))
    {
      temp_line <- paste(var_info$"type_decl", var_info$"constraint", sep = "")
    }
    else
    {
      temp_line <- var_info$"type_decl"
    }
    
    temp_line <- paste(temp_line, var_name, sep = " ")
    temp_line <- paste(temp_line, ";\n", sep = "")
    
    line <- paste(line, temp_line, sep = "")
  }
  
  return(line)
}

get_data_var_from_fixed_eff_parsed_fixed_term <- function(parsed_fixed_term, data_section, data_var_list)
{
  fixed_term_name <- parsed_fixed_term$"data_terms"[[1]]
  temp_var_line = ""
  
  if (!exists(fixed_term_name, where = data_var_list))
  {
    temp_var_line <- create_var_data_line_for_array(fixed_term_name, parsed_fixed_term$"size"[[1]], parsed_fixed_term$"stan_type"[[1]])
  }
  
  return(paste(data_section, temp_var_line, ";", sep = ""))
}

get_data_var_from_rand_eff_parsed_term <- function(parsed_term, data_var_list)
{
  if (!exists(var)) {}
}

get_data_var_from_resp_term_for_gaussian <- function(parsed_resp_term, data_var_list)
{
  resp_var_name <- parsed_resp_term$"data_terms"[[1]]
  resp_var_size <- parsed_resp_term$"size"[[1]]
  
  data_var_list$"size" <- create_var_data_line_for_constant(resp_var_size, "int", "lower=1")

  data_var_list$resp_var_name <- create_data_line_for_array(resp_var_size, resp_var_size, "real")

  return(data_var_list)
}

get_data_var_from_resp_term_for_binomial <- function(parsed_resp_term, data_var_list)
{
  data_var_list$"size" <- create_var_data_line_for_constant(parsed_resp_term$"size"[[1]], "int", "lower=1")
    
  for (i in 1:length(parsed_resp_term$"data_terms"))
  {
    resp_var_name <- parsed_resp_term$"data_terms"[[i]]
    data_var_list$resp_var_name <- create_data_line_for_array(resp_var_name, parsed_resp_term$"size"[[i]], "int")
  }

  return(data_var_list)
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
  var_line <- paste(var_name, "[", var_row, ", ", var_col, "]", sep = "")
  var_line <- paste(var_type, var_line, sep = " ")
  
  return(var_line)
}

create_data_line_for_array <- function(var_name, var_type, var_size)
{
  var_line <- paste(var_name, "[", var_size, "]", sep = "")
  var_line <- paste(var_type, var_line, sep = " ")

  return(var_line)
}

create_data_line_for_constant <- function(var_name, var_type, var_constr = "")
{
  
  var_line <- paste(var_type, "<", var_constr, ">", sep = "")
  var_line <- paste(var_line, var_name, sep = " ")
  
  return(var_line)
}