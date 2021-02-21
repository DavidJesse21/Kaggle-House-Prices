library(targets)

# custom functions / pipeline steps
source("R/init_training.R")
source("R/rec_impute.R")


# Set target-specific options such as packages.
tar_option_set(
  packages = c(
    "vroom",
    "rsample",
    "recipes",
    "qs"
  ),
  format = "qs"
)

# End this file with a list of target objects.
list(
  # raw data file
  tar_target(raw_data_file, "data/train.csv", format = "file"),
  # read data
  tar_target(data, read_and_clean(raw_data_file)),
  # get train and test sets
  tar_target(data_split, split_data(data, seed = 123, prop = 0.7)),
  # initial recipe (preprocessing pipeline)
  tar_target(rec_init, init_recipe(data_split[[1]])),
  # first steps: update role and data types
  tar_target(rec_update, step_role_and_type(rec_init)),
  # impute missing values
  tar_target(rec_impute, step_na_everything(rec_update))
)
