#' Various step functions for dealing with missing values
#'
#' @param rec A recipe object from {recipes}
#'
#' @return A recipe object from {recipes}
#' 
#' @import recipes


#' `pool_qc`
#' creatre new feature `has_pool`, drop `pool_qc`
step_na_pool <- function(rec) {
  rec = rec %>%
    step_mutate(has_pool = ifelse(is.na(pool_qc), 0L, 1L)) %>%
    step_rm(pool_qc)
  
  return(rec)
}

#' `misc_feature`
#' create new feature `has_shed`, drop `misc_feature`
step_na_shed <- function(rec) {
  rec = rec %>%
    step_unknown(misc_feature, new_level = "none") %>%
    step_mutate(has_shed = ifelse(misc_feature == "Shed", 1L, 0L)) %>%
    step_rm(misc_feature)
  
  return(rec)
}


#' `alley`
step_na_alley <- function(rec) {
  rec = rec %>%
    step_unknown(alley, new_level = "no_alley")
  
  return(rec)
}


#' `fence`
step_na_fence <- function(rec) {
  rec = rec %>%
    step_unknown(fence, new_level = "no_fence")
  
  return(rec)
}


#' `fireplace_qu`
#' Impute `fireplace_qu` with mode and create interactions/counts of fireplaces of specific quality
step_na_fire <- function(rec) {
  rec = rec %>%
    step_modeimpute(fireplace_qu) %>%
    step_interact(terms = ~ fireplace_qu:fireplaces) %>%
    step_rm(fireplace_qu)
  
  return(rec)
}


#' `lot_frontage`
step_na_lotfront <- function(rec) {
  rec = rec %>%
    step_mutate(lot_frontage = ifelse(is.na(lot_frontage), 0, lot_frontage))
  
  return(rec)
}


#' `garage_` features
#' NAs are present in those cases where there is no garage
#' Impute `garage_yr_blt` with oldest year
step_na_garage <- function(rec) {
  rec = rec %>%
    # if house has no garage assign old date to it
    # arbitrarily since `step_lowerimpute` does not seem to work
    step_mutate(
      garage_yr_blt = ifelse(is.na(garage_yr_blt), 1920, garage_yr_blt)
      ) %>%
    step_mutate(garage_age = 2020 - garage_yr_blt) %>%
    step_rm(garage_yr_blt) %>%
    # other garage related features imputed with unknown
    step_unknown(starts_with("garage") & is.character,
                 new_level = "no_garage")
  
  return(rec)
}


#' `bsmt_` features
step_na_bsmt <- function(rec) {
  rec = rec %>%
    step_unknown(starts_with("bsmt") & is.character,
                 new_level = "no_bsmt")
  
  return(rec)
}


#' `mas_vnr` features
step_na_masvnr <- function(rec) {
  rec = rec %>%
    step_unknown(mas_vnr_type, new_level = "no_masvnr") %>%
    step_mutate(mas_vnr_area = ifelse(is.na(mas_vnr_area), 0, mas_vnr_area))
  
  return(rec)
}


#' remaining nominal features
step_na_miscchr <- function(rec) {
  rec = rec %>%
    step_modeimpute(all_nominal(), -all_outcomes())
  
  return(rec)
}

#' remaining numeric features
step_na_miscnum  <-  function(rec) {
  rec = rec %>%
    step_medianimpute(all_numeric(), -all_outcomes())
  
  return(rec)
}


#' combine all steps
step_na_everything <- function(rec) {
  rec = rec %>%
    step_na_pool() %>%
    step_na_shed() %>%
    step_na_alley() %>%
    step_na_fence() %>%
    step_na_fire() %>%
    step_na_lotfront() %>%
    step_na_garage() %>%
    step_na_bsmt() %>%
    step_na_masvnr() %>%
    step_na_miscchr() %>%
    step_na_miscnum()
  
  return(rec)
}

