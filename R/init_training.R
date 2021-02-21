#' Most basic steps to start/initialize the training of machine learning models

#' Read the data and clean columns names
#'
#' @param path Path of the data file
#'
#' @return A tibble
#' 
#' @importFrom vroom vroom
#' @importFrom janitor clean_names
read_and_clean <- function(path) {
  df = vroom::vroom(path) %>%
    janitor::clean_names()
  return(df)
}


#' Split data into traind and test sets
#'
#' @param data_raw The raw dataframe (e.g. a tibble)
#' @param seed Integer value for the random state
#' @param prop Proportion of data for training
#'
#' @return List containing train and test sets
#' 
#' @importFrom rsample initial_split training testing
split_data <- function(data_raw, seed, prop) {
  # reproducibility
  set.seed(seed)
  
  # split into training and test sets
  data_split = initial_split(data_raw, prop = prop)
  df_train = training(data_split)
  df_test = testing(data_split)
  
  # return list of both train and test sets
  return(list(df_train, df_test))
}


#' Initialize a recipe (preprocessing pipeline)
#'
#' @param train_data The training data to use
#'
#' @return A recipe
#' 
#' @importFrom recipes recipe
init_recipe <- function(train_data) {
  rec = recipe(sale_price  ~ ., data = train_data)
  return(rec)
}


#' Change variable types and roles
#' 
#' @description Affects the variables `id` (role: ID) and `ms_sub_class` (type: factor).
#'
#' @param recipe A recipe
#'
#' @return A recipe
#' 
#' @importFrom recipes update_role step_mutate
step_role_and_type <- function(recipe) {
  rec = recipe %>%
    update_role(id, new_role = "ID") %>%
    step_mutate(ms_sub_class = factor(ms_sub_class))
  
  return(rec)
}



