################################################################################
##### Expression function
################################################################################

#Observed predicted function   -------------------------------------------------
obs_pred_expressions <- function(authors) {

  if(authors == "Serbin et al. (2019)") {

    x_name <- expression(paste("Predicted LMA (g m"^-2, ")"), sep = "")

  }


  return(x_name)

}

#Units function   --------------------------------------------------------------
units_expressions <- function(authors) {

  if(authors == "Serbin et al. (2019)") {
    units <- expression(paste("LMA (g m"^-2, ")"), sep = "")
  }

  return(units)

}
