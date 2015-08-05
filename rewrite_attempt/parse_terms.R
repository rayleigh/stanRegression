parse_fixed_eff_term <- function(predictor_term, data_col)
{
  parsed_term <- list()
  
  term_template_name <- clean_term_name(predictor_term)
    
  #Data section
  parsed_term$"data_terms"[[1]] <- term_template_name
  parsed_term$"stan_data_type"[[1]] <- determine_stan_type(typeof(data_col))
  parsed_term$"size"[[1]] <- "N"
    
  #Param section
  parsed_term$"param_terms"[[1]] <- attach_prefix(term_template_name, "beta")
    
  parsed_term$"stan_data_list" <- list()
  parsed_term$"stan_data_list"[[term_template_name]] <- data_col  

  return(parsed_term)    
}

parse_random_eff_term <- function(group_term, varying_terms_str, data_matrix, covered_fixed_eff_terms_list)
{
  parsed_term <- list()
  
  parsed_term <- fill_info_for_random_eff_varying_terms(parsed_term, varying_terms_str, data_matrix, covered_fixed_eff_terms_list)
  
  if (parsed_term$"num_varying_terms" == 1)
  {
    parsed_term <- fill_info_for_random_eff_group_term(group_term, "vector", parsed_term, data_matrix)
    parsed_term$"ran_eff_type" <- "varying_term"  
  } 
  else 
  {
    parsed_term <- fill_info_for_random_eff_group_term(group_term, "matrix", parsed_term, data_matrix)
    parsed_term$"ran_eff_type" <- "varying_term_with_intercept"
  } 
    
  return(parsed_term)
}

fill_info_for_random_eff_varying_terms <- function(parsed_term, varying_terms_str, data_matrix, covered_fixed_eff_terms_list)
{
  #Varying term section info
  varying_terms_info <- get_varying_terms_names_and_data(varying_terms_str, data_matrix, covered_fixed_eff_terms_list)
  varying_terms_list <- varying_terms_info$"terms_list"
  parsed_term$"varying_terms" <- varying_terms_list
  parsed_term$"num_varying_terms" <- length(varying_terms_list)
  
  missing_fixed_eff_info <- varying_terms_info$"missing_fixed_eff_info"

  #Data section
  parsed_term$"data_terms" <- names(missing_fixed_eff_info$"missing_data")
  parsed_term$"stan_data_type" <- missing_fixed_eff_info$"missing_type"
  parsed_term$"size" <- missing_fixed_eff_info$"missing_size"
  
  #Stan data information
  parsed_term$"stan_data_list" <- missing_fixed_eff_info$"missing_data"
  
  return(parsed_term)
}

fill_info_for_random_eff_group_term <- function(group_term, term_type, parsed_term, data_matrix)
{
  group_term_template <- clean_term_name(group_term)
  varying_term_template <- paste(parsed_term$varying_terms, collapse = "_")
  
  group_term_size <- paste("N", group_term_template, sep = "_")
  term_name_template <- paste(varying_term_template, "by", group_term_template, sep = "_")
  term_name_template <- attach_prefix(term_name_template, term_type)
  
  #Data section info
  parsed_term$"data_terms" <- c(list(group_term_template, group_term_size), parsed_term$"data_terms")
  parsed_term$"size" <- c(list("N", ""), parsed_term$"size")
  parsed_term$"stan_data_type" <- c(list("", ""), parsed_term$"stan_data_type")
  
  #Parameter section info
  parsed_term$"param_terms" <- list(paste("sigma", term_name_template, sep = "_"), paste(term_name_template, "std", sep = "_"))
  if (term_type == "matrix")
  {
    parsed_term$"param_terms"[[(length(parsed_term$"param_terms") + 1)]] <- paste("L_Rho", term_name_template, sep = "_")
  }
  
  #Transform parameter section info
  parsed_term$"trans_param_terms" <- list(term_name_template)
  
  #Stan data list info for group term
  parsed_term$"stan_data_list"[[group_term_template]] <- coerce_index(with(data_matrix, eval(parse(text = group_term))))
  parsed_term$"stan_data_list"[[group_term_size]] <- length(unique(parsed_term$"stan_data_list"[[group_term_template]]))

  return(parsed_term)
}

parse_resp_term <- function(resp_term, data_matrix)
{
  temp_formula <- formula(paste(resp_term, "~ 1"))
  parsed_resp_term <- list()
  size_term <- "N"
  
  resp_data_matrix <- model.frame(temp_formula, data_matrix)[[1]]
  if (is.null(dim(resp_data_matrix)))
  {
    resp_data_matrix <- model.frame(temp_formula, data_matrix)[1] 
  }
  resp_data_matrix <- as.data.frame(resp_data_matrix)
  resp_var_names <- colnames(resp_data_matrix)
  sample_size_name <- paste(resp_var_names[1], "sample_num", sep = "_")
  resp_data_matrix[[sample_size_name]] <- rowSums(resp_data_matrix)
  resp_var_names <- c(resp_var_names, sample_size_name)
  num_resp_vars <- length(resp_var_names)
  
  #Data section  
  parsed_resp_term$"data_terms" <- lapply(1:num_resp_vars, 
                                          function(i) {var_name <- clean_term_name(resp_var_names[i]);
                                          colnames(resp_data_matrix)[i] <- var_name;
                                          return(var_name);})
  parsed_resp_term$"data_terms" <- c(parsed_resp_term$"data_terms", list(size_term))
  parsed_resp_term$"size" <- lapply(1:num_resp_vars, function(i) return("N"))
  
  parsed_resp_term$"special_vectors_index" <- list("sample_size" = (length(parsed_resp_term$"data_terms") - 1)) 
  
  #Stan data info
  stan_data_list <- as.list(resp_data_matrix)
  stan_data_list[[size_term]] <- length(stan_data_list[[1]])
  parsed_resp_term$"stan_data_list" <- stan_data_list
  
  return(parsed_resp_term)
}

get_varying_terms_names_and_data <- function(terms_formula_str, data_matrix, fixed_eff_list)
{
  varying_terms_list <- list()
  if (grepl("Intercept( \\+ )*", terms_formula_str, perl = T))
  {
    varying_terms_list[[1]] <- "Intercept"
    terms_formula_str <- gsub("Intercept( \\+ )*", "", terms_formula_str, perl = T)
  }

  if (terms_formula_str != "")
  {
    terms_formula <- formula(paste("~ 1", terms_formula_str, sep = " + "))
    terms_formula_matrix <- as.data.frame(model.matrix(terms_formula, data_matrix))
    
    varying_terms_list <- c(varying_terms_list, colnames(terms_formula_matrix)[-1])
  }

  missing_fixed_terms_list <- varying_terms_list[!varying_terms_list %in% fixed_eff_list]
  varying_terms_list <- lapply(varying_terms_list, clean_term_name)
    
  return(list("terms_list" = varying_terms_list, 
              "missing_fixed_eff_info" = get_missing_fixed_eff_info(missing_fixed_terms_list, terms_formula_matrix)))
}

get_missing_fixed_eff_info <- function(missing_fixed_eff_terms_list, data_matrix)
{
  num_missing_fixed_terms <- length(missing_fixed_eff_terms_list)
  missing_fixed_terms_data <- list()
  missing_fixed_terms_type <- list()
  missing_fixed_terms_size <- list()
  if (num_missing_fixed_terms > 0)
  {
    for (i in 1:num_missing_fixed_terms)
    {
      missing_fixed_term <- clean_term_name(missing_fixed_eff_terms_list[[i]])
      missing_fixed_terms_data[[missing_fixed_term]] <- data_matrix[, missing_fixed_term] 
      missing_fixed_terms_type <- determine_stan_type(typeof(missing_fixed_terms_data[[missing_fixed_term]]))
      missing_fixed_terms_size <- "N"
    }
  }
  return(list("missing_data" = missing_fixed_terms_data, "missing_type" = missing_fixed_terms_type, "missing_size" = missing_fixed_terms_size))
}

coerce_index <- function( x ) 
{
  as.integer(as.factor(as.character(x)))
}

clean_term_name <- function(term_name) {
  term_name <- gsub( "." , "_" , term_name , fixed=TRUE )
  term_name <- gsub( "^" , "_" , term_name , fixed=TRUE )
  term_name <- gsub( ":" , "_X_" , term_name , fixed=TRUE )
  term_name <- gsub( "|" , "_by_" , term_name , fixed=TRUE )
  term_name <- gsub( "(" , "" , term_name , fixed=TRUE )
  term_name <- gsub( ")" , "" , term_name , fixed=TRUE )
  term_name <- gsub( " " , "" , term_name , fixed=TRUE )
  term_name <- gsub( "\"", "", term_name, fixed = TRUE)
  return(term_name)
}

attach_prefix <- function(term_name, term_type)
{
  prefix = switch(term_type, beta = "b", vector = "v", matrix = "m")
  return(paste(prefix, term_name, sep = "_"))
}

determine_stan_type <- function(term_type)
{
  if (term_type == "integer")
  {
    return("int")
  }
  else
  {
    return("real")
  }
}

