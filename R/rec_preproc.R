#' Data preprocessing steps
#'
#' @param rec A recipe object from the {recipes} package
#'
#' @return A recipe object
#' 
#' @import recipes


#' combine `bathroom` features
step_bathrooms <- function(rec) {
  rec = rec %>%
    step_mutate(
      bathrooms = bsmt_full_bath + full_bath + 0.5 * (bsmt_half_bath + half_bath)
    ) %>%
    step_rm(bsmt_full_bath, full_bath, bsmt_half_bath, half_bath)
  
  return(rec)
}


#' calculate ratios/shares of  `bsmt_` area features
step_bsmt_ratio <- function(rec) {
  rec = rec %>%
    step_mutate_at(
      bsmt_fin_sf1, bsmt_fin_sf2, bsmt_unf_sf,
      fn = ~ ./total_bsmt_sf
    )
  
  return(rec)
}


#' calculate ratios/shares of  `sf` living area features
step_sf_ratio <- function(rec) {
  rec = rec %>%
    step_mutate_at(
      ends_with("flr_sf"), low_qual_fin_sf,
      fn =   ~ ./gr_liv_area
    )
  
  return(rec)
}


#' combine `proch` and `wooddeck` area features
step_comb_porch  <- function(rec) {
  rec = rec %>%
    step_mutate(
      porch_sf = open_porch_sf + enclosed_porch + x3ssn_porch + screen_porch + wood_deck_sf
    ) %>%
    step_rm(open_porch_sf, enclosed_porch, x3ssn_porch, screen_porch, wood_deck_sf)
  
  return(rec)
}


#' apply boxcox-transformation to main area features
step_boxcox_area <- function(rec) {
  rec = rec %>%
    step_BoxCox(
      lot_area, mas_vnr_area, gr_liv_area, garage_area,
      total_bsmt_sf, porch_sf, lot_frontage
    )
  
  return(rec)
}


#' create interactions between numeric and nominal `bsmt` features
step_bsmt_interact <- function(rec) {
  rec = rec %>%
    step_interact(terms = ~ bsmt_fin_sf1:bsmt_fin_type1) %>%
    step_interact(terms = ~ bsmt_fin_sf2:bsmt_fin_type2) %>%
    step_interact(terms = ~ bsmt_qual:total_bsmt_sf)
  
  return(rec)
}


#' create interactions between numeric and nominal `garage` features
step_garage_interact <- function(rec) {
  rec = rec %>%
    step_interact(terms = ~ garage_type:garage_area) %>%
    step_interact(terms = ~ garage_type:garage_cars)
  
  return(rec)
}


#' create interactions between main area features and `neighborhood`
step_neigh_interact <- function(rec) {
  rec = rec %>%
    step_other(neighborhood, threshold = 0.015) %>%
    step_interact(terms = ~ neighborhood:gr_liv_area) %>%
    step_interact(terms = ~ neighborhood:lot_area) %>%
    step_interact(terms = ~ neighborhood:mas_vnr_area) %>%
    step_interact(terms = ~ neighborhood:garage_area) %>%
    step_interact(terms = ~ neighborhood:garage_cars) %>%
    step_interact(terms = ~ neighborhood:porch_sf)
  
  return(rec)
}


#' calculate age of house
step_age_house <- function(rec) {
  rec = rec %>%
    step_mutate(
      age_house = 2020 -  year_built
    )
  
  return(rec)
}


#' dummy for remodelling
step_has_remod <- function(rec) {
  rec = rec %>%
    step_mutate(
      has_remod = ifelse(year_built == year_remod_add, 0L, 1L)
    ) %>%
    step_rm(year_built, year_remod_add)
  
  return(rec)
}


#' turn sale date features into factors
step_factor_saledate <- function(rec) {
  rec = rec %>%
    step_mutate_at(
      yr_sold, mo_sold,
      fn =  ~factor(.)
    )
  
  return(rec)
}


#' encode all nominal variables
step_enc_nominals <- function(rec) {
  rec = rec %>%
    # drop this column because of no variance
    step_rm(utilities) %>%
    step_other(all_nominal()) %>%
    step_dummy(all_nominal())
  
  return(rec)
}


#' filter/remove features
step_filter <- function(rec) {
  rec = rec %>%
    step_zv(all_predictors()) %>%
    step_nzv(all_predictors(), freq_cut = 995/5) %>%
    step_corr(all_predictors(), threshold = 0.99)
  
  return(rec)
}


#' combine all steps
step_preprocess <- function(rec) {
  rec = rec %>%
    step_bathrooms() %>%
    step_bsmt_ratio() %>%
    step_sf_ratio() %>%
    step_comb_porch() %>%
    step_boxcox_area() %>%
    step_bsmt_interact() %>%
    step_garage_interact() %>%
    step_neigh_interact() %>%
    step_age_house() %>%
    step_has_remod() %>%
    step_factor_saledate() %>%
    step_enc_nominals() %>%
    step_filter()
  
  return(rec)
}
