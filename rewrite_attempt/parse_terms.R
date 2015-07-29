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

parse_random_eff_term <- function(group_term, varying_term_list, data_matrix)
{
  parsed_term <- list()
  
  varying_term_list <- unique(varying_term_list)
  varying_term_list <- lapply(varying_term_list, clean_term_name)
  parsed_term$"varying_terms" <- varying_term_list
  parsed_term$"num_varying_terms" <- length(varying_term_list)

  if (parsed_term$"num_varying_terms" == 1)
  {
    parsed_term <- fill_info_for_random_eff_term(group_term, "vector", parsed_term, data_matrix)
    parsed_term$"ran_eff_type" <- "varying_term"  
  } 
  else 
  {
    parsed_term <- fill_info_for_random_eff_term(group_term, "matrix", parsed_term, data_matrix)
    parsed_term$"ran_eff_type" <- "varying_term_with_intercept"
  } 
    
  return(parsed_term)
}

fill_info_for_random_eff_term <- function(group_term, term_type, parsed_term, data_matrix)
{
  group_term_template <- clean_term_name(group_term)
  varying_term_template <- paste(parsed_term$varying_terms, collapse = "_")
  
  group_term_size <- paste("N", group_term_template, sep = "_")
  term_name_template <- paste(varying_term_template, "by", group_term_template, sep = "_")
  term_name_template <- attach_prefix(term_name_template, term_type)
  
  #Data section info
  parsed_term$"data_terms" <- list(group_term_template, group_term_size)
  parsed_term$"size" <- list("N", "")

  #Parameter section info
  parsed_term$"param_terms" <- list(paste("sigma", term_name_template, sep = "_"), paste(term_name_template, "std", sep = "_"))
  if (term_type == "matrix")
  {
    parsed_term$"param_terms"[[(length(parsed_term$"param_terms") + 1)]] <- paste("L_Rho", term_name_template, sep = "_")
  }
  
  #Transform parameter section info
  parsed_term$"trans_param_terms" <- list(term_name_template)
  
  #Stan data list info
  parsed_term$"stan_data_list" <- list()
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

