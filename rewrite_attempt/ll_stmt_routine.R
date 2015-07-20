add_fixed_eff_term_to_ll_stmt <- function(parsed_term, ll_stmt)
{
  model_segment <- paste(parsed_term$"data_terms"[[1]], "[i]")
  model_segment <- paste(parsed_term$"param_terms"[[1]], "*", model_segment, sep = " ")
  return(paste(ll_stmt, model_segment, sep= " + "))
}

add_varying_term_rand_eff_term_to_ll_stmt <- function(parsed_term, ll_stmt)
{
  model_segment <- paste(parsed_term$"trans_param_terms"[[1]], "[", parsed_term$"data_terms"[[1]], "[i]]", sep = "")
  
  if (parsed_term$"component_terms"[[1]] != "Intercept")
  {
    model_segment <- paste(model_segment, parsed_term$"component_terms"[[1]], sep = " * ")
  }

  return(paste(ll_stmt, model_segment, sep = " + "))
}

add_varying_term_with_intercept_rand_eff_term_to_ll_stmt <- function(parsed_term, ll_stmt)
{
  model_segment <- paste(parsed_term$"trans_param_terms"[[1]], "[", parsed_term$"data_terms"[[1]], "[i], ", sep = "")
  
  model_segment_intercept <- paste(model_segment,  "1]", sep = "")
  
  model_segment_var <- paste(model_segment, "2]", sep = "")
  model_segment_var <- paste(model_segment_var, parsed_term$"component_terms"[[1]], sep = " * ")
  model_segment_var <- paste(model_segment_var, "[i]", sep = "")
  
  return(paste(ll_stmt, model_segment_intercept, model_segment_var, sep = " + "))
}

add_intercept_to_ll_stmt <- function(ll_stmt)
{
  return(paste("Intercept", ll_stmt, sep = ""))
}

add_ll_stmt_to_section_for_gaussian <- function(ll_stmt, resp_term_size, section)
{
  return(add_ll_stmt_to_section(ll_stmt, resp_term_size, "mu", section))
}

add_ll_stmt_to_section_for_binomial <- function(ll_stmt, resp_term_size, section)
{
  return(add_ll_stmt_to_section(ll_stmt, resp_term_size, "p", section))
}

add_ll_stmt_to_section <- function(ll_stmt, resp_term_size, ll_prob_term, section)
{
  section <- paste("vector[", resp_term_size, "] ", ll_prob_term, ";", section, sep = "") 
  section <- paste(section, "for (i in 1:", resp_term_size, ") {  ", sep = "")
  section <- paste(section, ll_prob_term, "[i] <- ", ll_stmt, ";}", sep = "")
  
  return(section)  
}

