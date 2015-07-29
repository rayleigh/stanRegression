organize_random_effect_terms <- function(predictor_term, organized_ran_eff_list)
{
  if (grepl(" || ", predictor_term, fixed = T))
  {
    predictor_term <- expand_double_bar(predictor_term)
  }
  
  split_terms_list <- get_terms_on_both_sides(predictor_term, " | ")
  group_term <- split_terms_list[["right_term"]]
  group_terms_list <- expand_slash(group_term)
  
  varying_term <- split_terms_list[["left_term"]]
  varying_terms_list <- strsplit(varying_term, " + ", fixed = T)[[1]]
  expanded_varying_terms_list <- as.list(unlist(lapply(varying_terms_list, expand_slash)))
  expanded_varying_terms_list <- add_intercept_to_list(expanded_varying_terms_list)
  expanded_varying_terms_list <- remove_numeric_intercept_terms_from_list(expanded_varying_terms_list)
  
  for (i in 1:length(group_terms_list))
  {
    grp_term <- group_terms_list[[i]]
    if (exists(grp_term, where = organized_ran_eff_list))
    {
      organized_ran_eff_list[[grp_term]] <- c(organized_ran_eff_list[[grp_term]], expanded_varying_terms_list)
    }
    else
    {
      organized_ran_eff_list[[grp_term]] <- expanded_varying_terms_list
    }
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

add_intercept_term_to_list <- function(terms_list)
{
  add_intercept = !("-1" %in% terms_list | "0" %in% terms_list)
  intercept_in_list = "Intercept" %in% terms_list
  
  if (add_intercept && !intercept_in_list)
  {
    terms_list <- c(list("Intercept"), terms_list)
  }
  
  return(terms_list)
}

remove_numeric_intercept_terms_from_list <- function(terms_list)
{
  numeric_intercept_term_list <- c("-1", "0", "1")
  
  for (i in 1:length(numeric_intercept_term_list))
  {
    numeric_intercept_term <- numeric_intercept_term_list[i]
    if (numeric_intercept_term %in% terms_list)
    {
      terms_list <- terms_list[terms_list != numeric_intercept_term]
    }
  }

  return(terms_list)
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
