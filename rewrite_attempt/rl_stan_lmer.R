source('~/Documents/Gelman Research/stanRegression rewrite/data_section_routines.R')
source('~/Documents/Gelman Research/stanRegression rewrite/parse_terms.R')
source('~/Documents/Gelman Research/stanRegression rewrite/param_section_routine.R')
source('~/Documents/Gelman Research/stanRegression rewrite/trans_param_section_routine.R')
source('~/Documents/Gelman Research/stanRegression rewrite/model_section_routine.R')
source('~/Documents/Gelman Research/stanRegression rewrite/ll_stmt_routine.R')
source('~/Documents/Gelman Research/stanRegression rewrite/generated_section_routine.R')

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
  
  organized_predictor_term_info <- gather_and_organize_predictor_terms(predictor_terms, data_matrix)
  predictor_info <- parse_and_add_predictor_terms_to_sections_and_data_list(organized_predictor_term_info, data_matrix, list())
  code_section_list <- predictor_info$"code"
  stan_data_list <- predictor_info$"stan_data_list"
  
  if (add_intercept)
  {
    code_section_list <- add_intercept_to_sections(code_section_list)
  }
  else
  {
    #Remove the starting + sign
    code_section_list <- gsub("^ \\+ ", "", code_section_list$ll_stmt, perl = T)
  }

  resp_info <- eval(parse(text = paste("add_resp_term_and_ll_to_sections_for_", family, "(resp_term, data_matrix, code_section_list)", sep = "")))
  
  code_section_list <- resp_info$"code"
  stan_data_list <- c(stan_data_list, resp_info$"stan_data_list")
  
  return(list("data" = code_section_list$data_section, "parameters" = code_section_list$param_section, 
                "transformed parameters" = code_section_list$trans_param_section, "model" = code_section_list$model_section,
                  "generated quantities" = code_section_list$generated_section, "stan_data_list" = stan_data_list))
  
}

gather_and_organize_predictor_terms <- function(predictor_terms_list, data_matrix)
{
  ran_eff_terms_list <- list()
  fixed_eff_formula <- "~ 1"
  
  for (i in 1:length(predictor_terms_list))
  {
    predictor_term <- predictor_terms_list[[i]]
    if (grepl(" | ", predictor_term, fixed = T))
    {
      ran_eff_terms_list <- c(ran_eff_terms_list, expand_slash(predictor_term))
    }
    else
    {
      fixed_eff_formula <- paste(fixed_eff_formula, predictor_term, sep = " + ")
    }
  }
  
  fixed_eff_data_matrix <- as.data.frame(model.matrix(formula(fixed_eff_formula), data_matrix))
  
  return(list("ran_eff_list" = ran_eff_terms_list, "fixed_eff_list" = as.list(fixed_eff_data_matrix)))
}

#Also builds the data needed to run Stan later
parse_and_add_predictor_terms_to_sections_and_data_list <- function(organized_predictor_term_info, data_matrix, code_section_list)
{
  stan_data_list <- list()
  
  #Parse random effects
  ran_eff_term_list <- organized_predictor_term_info$"ran_eff_list"
  num_ran_eff_terms <- length(ran_eff_term_list)
  if (num_ran_eff_terms > 0)
  {
    for (i in 1:num_ran_eff_terms)
    {
      ran_eff_term <- ran_eff_term_list[[i]]
      new_info <- parse_and_add_random_eff_to_sections_and_data_list(ran_eff_term, data_matrix, code_section_list)
      code_section_list <- new_info$code
      stan_data_list <- c(stan_data_list, new_info$"stan_data_list")
    }
  }
  
  #Parse fixed effects
  fixed_eff_list <- organized_predictor_term_info$"fixed_eff_list"
  fixed_eff_term_list <- names(fixed_eff_list)
  num_fixed_eff_terms <- length(fixed_eff_term_list)
  if (num_fixed_eff_terms > 1)
  {
    for (i in 2:length(fixed_eff_term_list))
    {
      fixed_eff_term <- fixed_eff_term_list[i]
      fixed_eff_term_data <- fixed_eff_list[[i]]
      new_info <- parse_and_add_fixed_eff_to_sections_and_data_list(fixed_eff_term, fixed_eff_term_data, code_section_list)
      code_section_list <- new_info$code
      stan_data_list <- c(stan_data_list, new_info$"stan_data_list")
    }
  }
  
  return(list("code" = code_section_list, "stan_data_list" = stan_data_list))
}

parse_and_add_random_eff_to_sections_and_data_list <- function(predictor_term, data_matrix, code_section_list)
{
  parsed_term <- parse_random_eff_term(predictor_term, data_matrix)

  code_section_list$data_section <- add_rand_eff_term_to_data_section(parsed_term, code_section_list$data_section)
  code_section_list$param_section <- eval(parse(text = paste("add_", parsed_term$"ran_eff_type", "_rand_eff_term_to_param_section(parsed_term,code_section_list$param_section)", sep = "")))
  code_section_list$trans_param_section <- eval(parse(text = paste("add_", parsed_term$"ran_eff_type", "_rand_eff_term_to_trans_param_section(parsed_term,code_section_list$trans_param_section)", sep = "")))
  code_section_list$model_section <- eval(parse(text = paste("add_", parsed_term$"ran_eff_type", "_rand_eff_term_to_model_section(parsed_term,code_section_list$model_section)", sep = "")))
  code_section_list$ll_stmt <- eval(parse(text = paste("add_", parsed_term$"ran_eff_type", "_rand_eff_term_to_ll_stmt(parsed_term,code_section_list$ll_stmt)", sep = "")))

  return(list("code" = code_section_list, "stan_data_list" = parsed_term$"stan_data_list"))
}

parse_and_add_fixed_eff_to_sections_and_data_list <- function(predictor_term, data_col, code_section_list)
{
  parsed_term <- parse_fixed_eff_term(predictor_term, data_col)
  
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

