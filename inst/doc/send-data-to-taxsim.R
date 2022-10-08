## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(usincometaxes)

## -----------------------------------------------------------------------------
data(taxpayer_finances)

taxsim_dataset <- create_dataset_for_taxsim(taxpayer_finances)

knitr::kable(head(taxsim_dataset))

## ----eval = FALSE-------------------------------------------------------------
#  taxsim_filename <- 'taxsim_dataset.csv'
#  
#  vroom::vroom_write(taxsim_dataset, taxsim_filename)

