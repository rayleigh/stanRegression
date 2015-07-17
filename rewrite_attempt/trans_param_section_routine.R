add_varying_term_rand_eff_term_to_trans_param_section <- function(parsed_term, trans_param_section)
{
  trans_param_section <- paste(trans_param_section, create_param_vector_line(parsed_term$"trans_param_terms"[[1]], parsed_term$"data_terms"[[2]]), sep = "")
  
  trans_param_section <- paste(trans_param_section, create_trans_param_multiplication_assignment_line(parsed_term$"trans_param_terms"[[1]], parsed_term$"param_terms"[[1]], parsed_term$"param_terms"[[2]]), sep = "")

  return(trans_param_section)
}

add_varying_term_with_intercept_rand_eff_term_to_trans_param_section <- function(parsed_term, trans_param_section)
{
  trans_param_section <- paste(trans_param_section, create_param_matrix_line(parsed_term$"trans_param_terms"[[1]], parsed_term$"data_terms"[[2]], 2), sep = "")
  
  cholesky_opt_term1 <- paste("diag_pre_multiply(", parsed_term$"param_terms"[[1]], ", ", parsed_term$"param_terms"[[3]], ")", sep = "")
  
  trans_param_section <- paste(trans_param_section, create_trans_param_multiplication_assignment_line(parsed_term$"trans_param_terms"[[1]], cholesky_opt_term1, parsed_term$"param_terms"[[2]]), sep = "")
  
  return(trans_param_section)
}

create_trans_param_multiplication_assignment_line <- function(assign_var, multiply_var1, multiply_var2)
{
  return(paste(assign_var, " <- ", multiply_var1, " * ", multiply_var2, ";", sep = ""))
}
