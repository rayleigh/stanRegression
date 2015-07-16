add_parsed_term_to_ll_stmt <- function(parsed_term, ll_stmt)
{
  if (parsed_term$"random_eff")
  {
    ll_stmt <- add_rand_eff_term_to_ll_stmt(parsed_term, ll_stmt)
  }
  else
  {    
    ll_stmt <- add_fixed_eff_term_to_ll_stmt(parsed_term, ll_stmt)
  }
  return(ll_stmt)
}

add_fixed_eff_term_to_ll_stmt <- function(parsed_term, ll_stmt)
{
  model_segment <- paste(parsed_term$"data_terms"[[1]], "[i]")
  model_segment <- paste(parsed_term$"param_terms"[[1]], "*", model_segment, sep = " ")
  return(paste(ll_stmt, model_segment, " + ", sep=""))
}

add_intercept_to_ll_stmt <- function(ll_stmt)
{
  return(paste("Intercept + ", ll_stmt, sep = ""))
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
  section <- paste("vector[", resp_term_size, "] ", ll_prob_term, ";\n", section, sep = "") 
  section <- paste(section, "for (i in 1:", resp_term_size, ") {\n", sep = "")
  section <- paste(section, ll_prob_term, "[i] <-", ll_stmt, ";\n}\n", sep = "")
  
  return(section)  
}

