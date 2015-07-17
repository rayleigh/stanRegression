create_generated_section_for_gaussian <- function(ll_stmt, resp_term)
{
  gen_section <- 'real dev;\ndev <- 0;\n'
  gen_section <- add_ll_stmt_to_section_for_gaussian(ll_stmt, resp_term$"size"[[1]], generated_section)
  gen_section <- paste("dev <- dev + (-2) * normal_log(", resp_term$"data_terms"[[1]], ", mu, sigma);\n", sep = "")
  return(gen_section)
}

create_generated_section_for_binomial <- function(ll_stmt, resp_term)
{
  gen_section <- 'real dev;\ndev <- 0;\n'
  gen_section <- add_ll_stmt_to_section_for_binomial(ll_stmt, resp_term$"size"[[1]], generated_section)
  temp_params_section <- paste(resp_term$"data_terms"[[1]], resp_term$"data_terms"[[resp_term$"special_vectors_index"$"sample_size"]], "p", sep = ", ")
  gen_section <- paste("dev <- dev + (-2) * binomial_logit_log(", temp_params_section, ");\n", sep = "")
  return(gen_section)
}