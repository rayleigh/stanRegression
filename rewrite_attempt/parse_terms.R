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

expand_slash <- function(original_term)
{
  var_terms <- get_terms_on_right_side(original_term, sep_symbol = " + ")
  varying_terms <- get_terms_on_right_side(var_terms, sep_symbol = " | ")
  
  if(regexec(pattern = '/', fixed = TRUE, text = varying_terms) == -1) 
  {
    return(list(original_term))
  } 
  else 
  {
    var_list <- strsplit(varying_terms,split = '/',fixed=TRUE)[[1]]
    var_length <- length(var_list)
    if(var_length > 2) stop('Only supports maximum of 1 "/"')
    var_term <- var_list[1]
    interact_ranef_term <- paste(varying_var, var_list[2], sep=':')
    return(list(var_term, interact_ranef_term))
  }
}

parse_random_eff_term <- function(predictor_term, data_matrix)
{
  parsed_term <- list()
    
  split_terms_list <- get_terms_on_both_sides(predictor_term, " + ")
  var_terms <- split_terms_list[["right_term"]]
  intercept_term <- split_terms_list[["left_term"]]

  if (intercept_term == var_terms)
  {
    var_terms <- gsub( "1 |" , "Intercept |" , var_terms , fixed=TRUE )
    parsed_term <- fill_info_for_random_eff_term(var_terms, predictor_term, "vector", parsed_term, data_matrix)
    parsed_term$"ran_eff_type" <- "varying_term"  
    
  } else if (intercept_term == "1") {
  
    parsed_term <- fill_info_for_random_eff_term(var_terms, predictor_term, "matrix", parsed_term, data_matrix)
    parsed_term$"ran_eff_type" <- "varying_term_with_intercept"
      
  } else {
  
    stop("Invalid expression")  
    
  }
    
  return(parsed_term)
}

fill_info_for_random_eff_term <- function(var_terms, original_term, term_type, parsed_term, data_matrix)
{
  split_var_terms_list <- get_terms_on_both_sides(var_terms, " | ")
  varying_term <- split_var_terms_list[["right_term"]]
  term_to_vary <- clean_term_name(split_var_terms_list[["left_term"]])
  
  varying_template_name <- clean_term_name(varying_term)
  varying_term_size <- paste("N", varying_template_name, sep = "_")
  var_terms_template_name <- attach_prefix(clean_term_name(var_terms), term_type)
  
  #Data section info
  parsed_term$"data_terms" <- list(varying_template_name, varying_term_size)
  parsed_term$"size" <- list("N", "")

  #Parameter section info
  parsed_term$"param_terms" <- list(paste("sigma", var_terms_template_name, sep = "_"), paste(var_terms_template_name, "std", sep = "_"))
  if (term_type == "matrix")
  {
    parsed_term$"param_terms"[[(length(parsed_term$"param_terms") + 1)]] <- paste("L_Rho", var_terms_template_name, sep = "_")
  }
  
  #Transform parameter section info
  parsed_term$"trans_param_terms" <- list(var_terms_template_name)
  
  #Stan data list info
  parsed_term$"stan_data_list" <- list()
  parsed_term$"stan_data_list"[[varying_template_name]] <- coerce_index(with(data_matrix, eval(parse(text = varying_term))))
  parsed_term$"stan_data_list"[[varying_term_size]] <- length(unique(parsed_term$"stan_data_list"[[varying_template_name]]))

  parsed_term$"component_terms" <- list(term_to_vary, varying_template_name)
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

get_terms_on_both_sides <- function(term_str, sep_symbol)
{
  terms <- strsplit(term_str, split = sep_symbol,fixed=TRUE)[[1]]
  return(list("left_term" = terms[1], "right_term" = terms[length(terms)]))
}

get_terms_on_right_side <- function(term_str, sep_symbol)
{
  terms <- strsplit(term_str, split = sep_symbol,fixed=TRUE)[[1]]
  return(terms[length(terms)])
}

get_terms_on_left_side <- function(term_str, sep_symbol)
{
  terms <- strsplit(term_str,split = sep_symbol,fixed=TRUE)[[1]]
  return(terms[1])
}
