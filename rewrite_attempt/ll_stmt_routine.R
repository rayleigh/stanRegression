add_fixed_eff_term_to_ll_stmt <- function(parsed_fixed_term, ll_stmt)
{
  model_segment <- paste(parsed_fixed_term$"data_terms"[[1]], "[i]", sep = "")
  model_segment <- paste(parsed_fixed_term$"param_terms"[[1]], model_segment, sep = " * ")
  ll_stmt <- paste(ll_stmt, model_segment, sep= " + ")

  return(ll_stmt)
}

add_varying_term_rand_eff_term_to_ll_stmt <- function(parsed_term, ll_stmt)
{
  model_segment <- paste(parsed_term$"trans_param_terms"[[1]], "[", parsed_term$"data_terms"[[1]], "[i]]", sep = "")
  varying_term <- parsed_term$"varying_terms"[[1]]
  
  if (varying_term != "Intercept")
  {
    model_segment_data_var <- paste(varying_term, "[i]", sep = "")
    model_segment <- paste(model_segment, model_segment_data_var, sep = " * ")
  }

  return(paste(ll_stmt, model_segment, sep = " + "))
}

add_varying_term_with_intercept_rand_eff_term_to_ll_stmt <- function(parsed_term, ll_stmt)
{
  model_segment <- paste(parsed_term$"trans_param_terms"[[1]], "[", parsed_term$"data_terms"[[1]], "[i], ", sep = "")
  
  for (i in 1:parsed_term$"num_varying_terms")
  {
    model_segment_var <- paste(model_segment, i, "]", sep = "")
    varying_term <- parsed_term$"varying_terms"[[i]]
    
    if (varying_term != "Intercept")
    {
      model_segment_var <- paste(model_segment_var, varying_term, sep = " * ")
      model_segment_var <- paste(model_segment_var, "[i]", sep = "")
    }

    ll_stmt <- paste(ll_stmt, model_segment_var, sep = " + ")
  }
  
  return(ll_stmt)
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

create_term_multiplication_item <- function(term1, term2)
{
  return(paste(term1, term2, sep = " * "))
}
