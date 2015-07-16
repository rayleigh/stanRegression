stan_lmer <- function(user_formula, data, family = "gaussian",
                      default_prior = "dnorm(0,1)", loc_scale_transform = TRUE,
                      file_name = NULL, run_stan = FALSE, ...) 
{
  formula_info <- terms(user_formula, simpify = T)
  predictor_terms <- attr(formula_info, "term.labels")
  resp_term <- attr(formula_info, "variables")[1]
  add_intercept <- (attr(formula_info, "intercept") == 1)
  
  stan_code_sections <- create_stan_code_sections(predictor_terms, resp_term, data, add_intercept, family, data_matrix)

  write_stan_code_to_file(stan_code_sections, file_name)
}

create_stan_code_sections <- function(predictor_terms, resp_term, predictor_and_resp_matrix, add_intercept, family, data_matrix)
{
  
  code_section_list <- parse_and_add_predictor_terms_to_sections(predictor_terms, list(), data_matrix)

  if (add_intercept)
  {
    code_section_list <- add_intercept_to_sections(code_section_list)
  }
  
  code_section_list <- add_resp_term_and_ll_to_sections(resp_term, family, code_section_list)
    
  code_section_list$data_section <- build_data_section_from_var_list(code_section_list$data_var_list)

  return(list("data" = code_section_list$data_section, "param" = code_section_list$param_section, 
                "trans_param" = code_section_list$trans_param_section, "model" = code_section_list$model_section,
                  "generated" = code_section_list$generated_section))
  
}

parse_and_add_predictor_terms_to_sections <- function(predictor_terms, code_section_list, data_matrix)
{
  for (i in 1:length(predictor_terms))
  {
    parsed_term <- parse_predictor_term(predictor_terms[i], data_matrix)
    if (parsed_term$"random_eff")
    {
      add_parsed_random_eff_to_sections(parsed_term, code_section_list)
    }
    else
    {
      add_parsed_fixed_eff_to_sections(parsed_term, code_section_list)
    }
  }
  return(code_section_list)
}

add_parsed_random_eff_to_sections <- function(parsed_term, code_section_list)
{
  code_section_list$data_var_list <- get_data_var_from_parsed_term(parsed_term, code_section_list$data_var_list)
  code_section_list$param_section <- add_parsed_term_to_param_section(parsed_term,code_section_list$param_section)
  code_section_list$trans_param_section <- add_parsed_term_to_trans_param_section(parsed_term, code_section_list$trans_param_section)
  code_section_list$model_section <- add_parsed_term_to_model_selection(parsed_term, code_section_list$model_section)    
  code_section_list$ll_stmt <- add_parsed_term_to_ll_stmt(parsed_term, code_section_list$ll_stmt)
}

add_parsed_fixed_eff_to_sections <- function(parsed_term, code_section_list)
{
  code_section_list$data_var_list <- get_data_var_from_fixed_eff_parsed_term(parsed_term, code_section_list$data_var_list)
  code_section_list$param_section <- add_fixed_term_to_param_section(parsed_term,code_section_list$param_section)
  code_section_list$model_section <- add_fixed_eff_term_to_model_section(parsed_term, code_section_list$model_section)    
  code_section_list$ll_stmt <- add_fixed_eff_term_to_ll_stmt(parsed_term, code_section_list$ll_stmt)
}

add_intercept_to_sections <- function(code_section_list)
{
  code_section_list$param_section <- add_intercept_to_param_section(param_section)
  code_section_list$model_section <- add_intercept_to_model_section(model_section)
  code_section_list$ll_stmt <- add_intercept_to_ll_stmt(ll_stmt)
 
  return(code_section_list) 
}

add_resp_term_and_ll_to_sections <- function(resp_term, family, code_section_list)
{
  parsed_resp_term <- parse_resp_term(resp_term)
  code_section_list <- add_ll_to_sections(parsed_resp_term, family, code_section_list)
  code_section_list$data_var_list <- add_resp_vars_to_data_var_list(stan_resp_var_list, data_var_list) 
  code_section_list$model_section <- add_resp_vars_to_model_section(stan_resp_var_list, model_section)

  return(code_section_list)
}

add_ll_to_sections <- function(resp_term, family, code_section_list)
{
  code_section_list$model_section <- add_ll_stmt_to_model_section(ll_stmt, model_section, resp_term)
  code_section_list$generated_section <- create_generated_section(ll_stmt, family)
  
  return(code_section_list)  
}