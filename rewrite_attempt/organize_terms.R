organize_random_effect_terms <- function(predictor_term, organized_ran_eff_list)
{
  if (grepl(" || ", predictor_term, fixed = T))
  {
    predictor_term <- expand_double_bar(predictor_term)
  }
  
  split_terms_list <- get_terms_on_both_sides(predictor_term, " | ")
  group_term <- split_terms_list[["right_term"]]
  group_terms_list <- expand_slash(group_term)
  
  varying_terms_formula_str <- split_terms_list[["left_term"]]
  varying_terms_formula_str <- gsub(" - ", " + -", varying_terms_formula_str, fixed = T)
  
  for (i in 1:length(group_terms_list))
  {
    grp_term <- group_terms_list[[i]]
    
    if (exists(grp_term, where = organized_ran_eff_list))
    {
      varying_terms_formula_str <- paste(organized_ran_eff_list[[grp_term]], varying_terms_formula_str, sep = " + ")
    }
    
    varying_terms_formula_str <- add_intercept_term_to_formula_str(varying_terms_formula_str)
    varying_terms_formula_str <- remove_intercept_terms(varying_terms_formula_str)
    organized_ran_eff_list[[grp_term]] <- varying_terms_formula_str
  }
  
  return(organized_ran_eff_list)
}

expand_double_bar <- function(double_bar_term)
{
  split_double_bar_term <- strsplit(double_bar_term, " || ", fixed = T)[[1]]
  
  rejoined_split_double_bar_term <- paste(split_double_bar_term, collapse = " | ")
  
  return(paste(0, rejoined_split_double_bar_term, sep = " + "))
}

expand_slash <- function(slash_term)
{
  split_slash_term <- strsplit(slash_term, "/",  fixed = T)[[1]]
  expanded_terms_list <- list()
  
  for (i in 1:length(split_slash_term))
  {
    expanded_terms_list[[i]] = paste(split_slash_term[1:i], collapse = ":")
  }
  
  return(expanded_terms_list)
}

remove_intercept_terms <- function(terms_formula_str)
{
  no_intercept_terms_formula_str <- terms_formula_str
  no_intercept_terms_formula_str <- gsub("-1 + ", "", no_intercept_terms_formula_str, fixed = T)
  no_intercept_terms_formula_str <- gsub(" + -1", "", no_intercept_terms_formula_str, fixed = T)
  no_intercept_terms_formula_str <- gsub("0 + ", "", no_intercept_terms_formula_str, fixed = T)
  no_intercept_terms_formula_str <- gsub(" + 0", "", no_intercept_terms_formula_str, fixed = T)
  no_intercept_terms_formula_str <- gsub("1 + ", "", no_intercept_terms_formula_str, fixed = T)
  no_intercept_terms_formula_str <- gsub(" + 1", "", no_intercept_terms_formula_str, fixed = T)
  return(no_intercept_terms_formula_str)
}

add_intercept_term_to_formula_str <- function(terms_formula_str)
{
  no_intercept = (grepl("( \\+ )*-1( \\+ )*", terms_formula_str, perl = T) | grepl("( \\+ )*0( \\+ )*", terms_formula_str, perl = T))
  add_intercept = grepl("( \\+ )*1( \\+ )", terms_formula_str, perl = T)
  
  has_intercept = grepl("Intercept", terms_formula_str, fixed = T)
  
  if (!no_intercept & !has_intercept)
  {
    terms_formula_str <- paste("Intercept", terms_formula_str, sep = " + ")
  }
  
  return(terms_formula_str)
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
