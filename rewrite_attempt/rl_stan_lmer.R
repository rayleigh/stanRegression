source('~/Documents/Gelman Research/Replication/data_section_routines.R')
source('~/Documents/Gelman Research/Replication/parse_terms.R')
source('~/Documents/Gelman Research/Replication/param_section_routine.R')
source('~/Documents/Gelman Research/Replication/trans_param_section_routine.R')
source('~/Documents/Gelman Research/Replication/model_section_routine.R')
source('~/Documents/Gelman Research/Replication/ll_stmt_routine.R')
source('~/Documents/Gelman Research/Replication/generated_section_routine.R')

stan_lmer <- function(user_formula, data, family = "gaussian",
                      default_prior = "dnorm(0,1)", loc_scale_transform = TRUE,
                      file_name = NULL, run_stan = FALSE, ...) 
{
  formula_info <- terms(user_formula, simpify = T)
  predictor_terms <- attr(formula_info, "term.labels")
  resp_term <- deparse(attr(formula_info, "variables")[[2]])
  add_intercept <- (attr(formula_info, "intercept") == 1)
  
  stan_info <- create_stan_code_sections_and_data_list(predictor_terms, resp_term, add_intercept, family, data)
  
  stan_code_sections <- stan_info[-length(stan_info)]
  
  stan_code <- write_stan_code_to_file(stan_code_sections, file_name)
  
  return(list("stan_code" = stan_code, "data" = stan_info$"stan_data_list"))
}

create_stan_code_sections_and_data_list <- function(predictor_terms, resp_term, add_intercept, family, data_matrix)
{
  
  predictor_info <- parse_and_add_predictor_terms_to_sections_and_data_list(predictor_terms, data_matrix, list())
  code_section_list <- predictor_info$"code"
  stan_data_list <- predictor_info$"stan_data_list"
  
  if (add_intercept)
  {
    code_section_list <- add_intercept_to_sections(code_section_list)
  }
  else
  {
    code_section_list <- gsub("^ \\+ ", "", code_section_list$ll_stmt, perl = T)
  }

  resp_info <- eval(parse(text = paste("add_resp_term_and_ll_to_sections_for_", family, "(resp_term, data_matrix, code_section_list)", sep = "")))
  
  code_section_list <- resp_info$"code"
  stan_data_list <- c(stan_data_list, resp_info$"stan_data_list")
  
  return(list("data" = code_section_list$data_section, "parameters" = code_section_list$param_section, 
                "transformed parameters" = code_section_list$trans_param_section, "model" = code_section_list$model_section,
                  "generated quantities" = code_section_list$generated_section, "stan_data_list" = stan_data_list))
  
}

#Also builds the data needed to run Stan later
parse_and_add_predictor_terms_to_sections_and_data_list <- function(predictor_terms, data_matrix, code_section_list)
{
  stan_data_list <- list()
  stan_formula <- "~ -1"
  
  for (i in 1:length(predictor_terms))
  {
    predictor_term <- predictor_terms[[i]]
    if (grepl(" | ", predictor_term, fixed = T))
    {
      new_info <- parse_and_add_random_eff_to_sections_and_data_list(predictor_term, data_matrix, code_section_list)
      code_section_list <- new_info$code
      stan_data_list <- c(stan_data_list, new_info$"stan_data_list")
    }
    else
    {
      new_info <- parse_and_add_fixed_eff_to_sections_and_data_list(predictor_term, data_matrix, code_section_list)
      code_section_list <- new_info$code
      stan_data_list <- c(stan_data_list, new_info$"stan_data_list")
    }
  }
  
  return(list("code" = code_section_list, "stan_data_list" = stan_data_list))
}

parse_and_add_random_eff_to_sections_and_data_list <- function(predictor_term, data_matrix, code_section_list)
{
  parsed_term_list <- parse_random_eff_term(predictor_term, data_matrix)
  stan_data_list <- list()
  
  for (i in 1:length(parsed_term_list))
  {
    parsed_term <- parsed_term_list[[i]]
    
    code_section_list$data_section <- add_rand_eff_term_to_data_section(parsed_term, code_section_list$data_section)
    code_section_list$param_section <- eval(parse(text = paste("add_", parsed_term$"ran_eff_type", "_rand_eff_term_to_param_section(parsed_term,code_section_list$param_section)", sep = "")))
    code_section_list$trans_param_section <- eval(parse(text = paste("add_", parsed_term$"ran_eff_type", "_rand_eff_term_to_trans_param_section(parsed_term,code_section_list$trans_param_section)", sep = "")))
    code_section_list$model_section <- eval(parse(text = paste("add_", parsed_term$"ran_eff_type", "_rand_eff_term_to_model_section(parsed_term,code_section_list$model_section)", sep = "")))
    code_section_list$ll_stmt <- eval(parse(text = paste("add_", parsed_term$"ran_eff_type", "_rand_eff_term_to_ll_stmt(parsed_term,code_section_list$ll_stmt)", sep = "")))

    stan_data_list <- c(stan_data_list, parsed_term$"stan_data_list")
  }
  return(list("code" = code_section_list, "stan_data_list" = stan_data_list))
}

parse_and_add_fixed_eff_to_sections_and_data_list <- function(predictor_term, data_matrix, code_section_list)
{
  parsed_term <- parse_fixed_eff_term(predictor_term, data_matrix)
  
  code_section_list$data_section <- add_fixed_eff_term_to_data_section(parsed_term, code_section_list$data_section)
  code_section_list$param_section <- add_fixed_eff_term_to_param_section(parsed_term,code_section_list$param_section)
  code_section_list$model_section <- add_fixed_eff_term_to_model_section(parsed_term, code_section_list$model_section)    
  code_section_list$ll_stmt <- add_fixed_eff_term_to_ll_stmt(parsed_term, code_section_list$ll_stmt)
  
  return(list("code" = code_section_list, "stan_data_list" = parsed_term$"stan_data_list"))
}

add_intercept_to_sections <- function(code_section_list)
{
  code_section_list$"param_section" <- add_intercept_to_param_section(code_section_list$"param_section")
  code_section_list$"model_section" <- add_intercept_to_model_section(code_section_list$"model_section")
  code_section_list$"ll_stmt" <- add_intercept_to_ll_stmt(code_section_list$"ll_stmt")
 
  return(code_section_list) 
}

add_resp_term_and_ll_to_sections_for_gaussian <- function(resp_term, data_matrix, code_section_list)
{
  parsed_resp_term <- parse_resp_term(resp_term, data_matrix)

  code_section_list$"data_section" <- add_resp_term_to_data_section_for_gaussian(parsed_resp_term, code_section_list$"data_section")  
  
  code_section_list$"param_section" <- add_sigma_to_param_section(code_section_list$"param_section")
  
  code_section_list$"model_section" <- add_ll_stmt_to_model_section_for_gaussian(code_section_list$"ll_stmt", parsed_resp_term$"size"[[1]], code_section_list$"model_section")
  code_section_list$"model_section" <- add_resp_term_to_model_section_for_gaussian(parsed_resp_term, code_section_list$"model_section")
  
  code_section_list$"generated_section" <- create_generated_section_for_gaussian(code_section_list$"ll_stmt", parsed_resp_term)
  
  return(list("code" = code_section_list, "stan_data_list" = parsed_resp_term$"stan_data_list"))
}

add_resp_term_and_ll_to_sections_for_binomial <- function(resp_term, data_matrix, code_section_list)
{
  parsed_resp_term <- parse_resp_term(resp_term, data_matrix)
  
  code_section_list$"data_section" <- add_resp_term_to_data_section_for_binomial(parsed_resp_term, code_section_list$"data_section")  
  
  code_section_list$"model_section" <- add_ll_stmt_to_model_section_for_binomial(code_section_list$"ll_stmt", parsed_resp_term$"size"[[1]], code_section_list$"model_section")
  code_section_list$"model_section" <- add_resp_term_to_model_section_for_binomial(parsed_resp_term, code_section_list$"model_section")
  
  code_section_list$"generated_section" <- create_generated_section_for_binomial(code_section_list$"ll_stmt", parsed_resp_term)
  
  return(list("code" = code_section_list, "stan_data_list" = parsed_resp_term$"stan_data_list"))
}

write_stan_code_to_file <- function(stan_code_sections, file_name)
{
  indent = "  "
  semicolon_indenting = paste(";\n", indent, sep = "")
  open_bracket_indenting = paste("{\n", indent, sep = "")
  close_bracket_indenting = paste("}\n", indent, sep = "")
  
  section_names_list <- names(stan_code_sections)
  code_string = ""
  
  for (i in 1:length(stan_code_sections))
  {
    code_for_section <- stan_code_sections[[i]]
    section_name <- section_names_list[i]
   
    code_for_section <- gsub(";", semicolon_indenting, code_for_section, fixed = T)   
    code_for_section <- gsub("{", open_bracket_indenting, code_for_section, fixed = T)
    code_for_section <- gsub("}", close_bracket_indenting, code_for_section, fixed = T)
    code_for_section <- gsub(paste(indent, "$", sep = ""), "", code_for_section, perl = T)
    
    code_string <- paste(code_string, section_name, "{\n", indent, code_for_section, "}\n", sep = "")
  }
  
  writeLines(code_string, file_name)
  
  return(code_string)
}

