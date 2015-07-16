parse_predictor_term <- function(predictor_term, data_matrix)
{
  parsed_term <- list()
  parsed_term$"term_name" <- predictor_term
  parsed_term$"random_eff" <- grepl("|", predictor_term)
  if (parsed_term$"random_eff")
  {
    parsed_term <- parse_random_eff_term(predictor_term, data_matrix, parsed_term)
  }
  else
  {
    parsed_term <- parse_fixed_eff_term(predictor_term, data_matrix, parsed_term)
  }
}

parse_fixed_eff_term <- function(predictor_term, data_matrix)
{
  parsed_term <- list()
  
  term_template_name <- clean_term_name(predictor_term)

  parsed_term$"stan_data_type" <- list(determine_stan_type(typeof(data_matrix$predictor_term)))
  parsed_term$"data_terms" <- list(term_template_name)
  parsed_term$"param_terms" <- list(attach_prefix(term_template_name, "beta"))
  
  parsed_term$"data_var_type" <- list("array")
  parsed_term$"size" <- list("N")
  
  
  parsed_term$"stan_data_info" = predictor_term

  return(parsed_term)    
}

parse_random_eff_term <- function(predictor_term, data_matrix)
{
  predictor_term_list <- expand_slash(predictor_term)
  
  return(lapply(predictor_term_list, function(predictor_term) parse_a_rand_eff_term(predictor_term, data_matrix)))
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
    return(list(original_term, paste("1", interact_ranef_term, sep = " | ")))
  }
}

parse_a_rand_eff_term <- function(original_term, data_matrix)
{
  parsed_term <- list()
    
  split_terms_list <- get_terms_on_both_sides(original_term, " + ")
  var_terms <- split_terms_list[["right_term"]]
  intercept_term <- split_terms_list[["left_term"]]

  if (intercept_term == var_terms)
  {

    var_terms <- gsub( "1 |" , "Intercept |" , var_terms , fixed=TRUE )
    parsed_term <- fill_info_for_random_eff_term(var_terms, original_term, "vector", parsed_term, data_matrix)
    parsed_term$"ran_eff_type" <- "varying_term"  
    
  } else if (intercept_term == "1") {
  
    parsed_term <- fill_info_for_random_eff_term(var_terms, original_term, "matrix", parsed_term, data_matrix)
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
  term_to_vary <- split_var_terms_list[["left_term"]]
  
  varying_template_name <- clean_term_name(varying_term)
  varying_term_size <- paste("N", varying_template_name, sep = "_")
  var_terms_template_name <- attach_prefix(clean_term_name(var_terms), term_type)
  
  #Data section info
  parsed_term$"data_terms" <- list(varying_template_name, varying_term_size)
  parsed_term$"data_var_type" <- list("array", "constant")
  parsed_term$"stan_data_type" <- list("int", "int")
  parsed_term$"size" <- list("N", "")
  parsed_term$"data_constraint" <- list("", "lower = 1")
  
  #Parameter section info
  parsed_term$"param_terms" <- list(paste("sigma", var_terms_template_name, sep = "_"), paste(var_terms_template_name, "std", sep = "_"))
  if (term_type == "matrix")
  {
    parsed_term$"param_terms"[[(length(parsed_term$"param_terms") + 1)]] <- paste("L_Rho", var_terms_template_name, sep = "_")
  }
  
  #Transform parameter section info
  parsed_term$"trans_param_terms" <- list(var_terms_template_name)
  
  #Stan data list info
  parsed_term$"stan_data_info" <- list()
  parsed_term$"stan_data_info"[[varying_template_name]] <- coerce_index(with(data_matrix, eval(parse(text = varying_term))))
  parsed_term$"stan_data_info"[[varying_term_size]] <- length(unique(parsed_term$"stan_data_info"[[varying_template_name]]))

  return(parsed_term)
}

parse_resp_term <- function(resp_term, data_matrix, family)
{
  temp_formula <- formula(paste(resp_term, "~ 1"))
  parsed_resp_term <- list()
  size_term <- "N"
  
  resp_data_matrix <- as.data.frame(model.frame(temp_formula, data_matrix))
  resp_var_names <- colnames(resp_data_matrix)
  num_resp_vars <- length(resp_var_names)
  
  #Data section  
  parsed_resp_term$"data_terms" <- lapply(1:num_resp_vars, 
                                          function(i) {var_name <- clean_term_name(resp_var_names[i]);
                                          colnames(resp_data_matrix)[i] <- var_name;
                                          return(var_name);})
  
  #Stan data info
  stan_data_list <- as.list(resp_data_matrix)
  stan_data_list[[size_term]] <- length(stan_data_list[[1]])
  parsed_resp_term$"stan_data_info" <- stan_data_list
  
  #Fill in rest of information based on the family
  parsed_resp_term <- eval(parse(text = paste("add_resp_param_info_for_", family, "(parsed_resp_term)", sep = "")))
  
  parsed_resp_term$"data_terms" <- c(parsed_resp_term$"data_terms", list(size_term))
  parsed_term$"data_var_type" <- c(list(rep("array", num_resp_vars)), list("constant"))
  parsed_resp_term$"size" <- list(rep(size_term, num_resp_vars))
  parsed_term$"data_constraint" <- list(rep("", num_resp_vars), "lower=1")
  
  return(parsed_resp_term)
}

add_resp_term_info_for_gaussian <- function(parsed_term)
{
  #Data section
  num_data_vars <- length(parsed_resp_term$"data_terms")
  parsed_term$"stan_data_type" <- list(rep("real", num_data_vars - 1), "int")

  #Param section
  parsed_term$"param_terms" <- list("sigma")  
}

add_resp_term_info_for_binomial <- function(parsed_term)
{
  #Data section
  sample_size
  num_data_vars <- length(parsed_resp_term$"data_terms")
  parsed_term$"stan_data_type" <- list(rep("real", num_data_vars - 1), "int")
  
  sample_size <- rowSums(resp_data_matrix)
  resp_data_matrix <- cbind(resp_data_matrix, sample_size)
  
}



coerce_index <- function( x ) 
{
  as.integer(as.factor(as.character(x)))
}

clean_term_name <- function(term_name) {
  term_name <- gsub( "." , "_" , term_name , fixed=TRUE )
  term_name <- gsub( ":" , "_X_" , term_name , fixed=TRUE )
  term_name <- gsub( "|" , "_by_" , term_name , fixed=TRUE )
  term_name <- gsub( "(" , "" , term_name , fixed=TRUE )
  term_name <- gsub( ")" , "" , term_name , fixed=TRUE )
  term_name <- gsub( " " , "" , term_name , fixed=TRUE )
  return(term_name)
}

attach_prefix <- function(term_name, term_type)
{
  prefix = switch(term_type, beta = "b", vector = "v", matrix = "m")
  return(paste(prefix, term_name, sep = "_"))
}

determine_stan_type <- function(term_type)
{
  if (termp_type == "integer")
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
