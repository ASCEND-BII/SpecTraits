################################################################################
# SpecTraits - Build PLSR models from command line
################################################################################

suppressPackageStartupMessages({
  library(optparse)
  library(data.table)
  library(pls)
})

################################################################################
# Source of helpers ------------------------------------------------------------
################################################################################

# Functions for build panel
source("R/build/trait_selector_input.R")
source("R/build/build_import_plot.R")
source("R/build/split_input.R")
source("R/build/run_split_action.R")
source("R/build/split_action_plot.R")
source("R/build/press_input.R")
source("R/build/run_press_action.R")
source("R/build/pls_permutation_press_aux.R")
source("R/build/press_action_plot.R")
source("R/build/find_optimal_ncomp_aux.R")
source("R/build/final_optimal_input.R")
source("R/build/run_plsr_action.R")
source("R/build/pls_permutation_coef_aux.R")
source("R/build/vip_aux.R")
source("R/build/pls_summary_aux.R")
source("R/build/confidence_interval_aux.R")
source("R/build/coefficients_plot.R")
source("R/build/build_plsr_predict.R")
source("R/build/plsr_predict_aux.R")
source("R/build/model_performance_aux.R")
source("R/build/performance_plot.R")
source("R/build/build_export.R")

# --- 1. Core functions --------------------------------------------------------

import_spectra <- function(path) {
  message("Reading spectra from: ", path)
  data.table::fread(path)
}

import_traits <- function(path) {
  message("Reading traits from: ", path)
  data.table::fread(path)
}

create_split_vector <- function(traits_dt, trait_name, method = "random",
                                ratio = 0.7, group = NULL) {
  # Replace with your actual split logic (Kennard-Stone, etc.)
  n <- nrow(traits_dt)
  set.seed(123)
  idx <- sample(n)
  train_n <- floor(ratio * n)
  split_vec <- rep("Testing", n)
  split_vec[idx[1:train_n]] <- "Training"
  split_vec
}

compute_press <- function(spectra_dt, traits_dt, trait_name,
                          split_vec, method = "loo",
                          maxcomp = 20, prop = 0, iterations = 1) {
  # Replace with your actual PRESS computation
  data.frame(
    ncomp = 1:maxcomp,
    PRESS = runif(maxcomp)
  )
}

run_final_plsr <- function(spectra_dt, traits_dt, trait_name,
                           split_vec, method = "pls",
                           ncomp = 10, prop = 0, iterations = 1) {
  y <- traits_dt[[trait_name]]
  X <- as.matrix(spectra_dt[, -1, with = FALSE])   # First col -> ID

  fit <- pls::plsr(y ~ X, ncomp = ncomp, validation = "none")

  list(
    model = fit,
    coefficients = coef(fit, ncomp = ncomp)
  )
}

compute_predictions <- function(final_plsr, spectra_dt, traits_dt,
                                trait_name, split_vec) {
  X <- as.matrix(spectra_dt[, -1, with = FALSE])
  preds <- drop(predict(final_plsr$model, newdata = data.frame(X)))

  data.table::data.table(
    ID        = spectra_dt[[1]],
    Observed  = traits_dt[[trait_name]],
    Predicted = preds,
    Dataset   = split_vec
  )
}

compute_performance <- function(pred_dt) {
  pred_dt[, .(
    RMSE = sqrt(mean((Observed - Predicted)^2, na.rm = TRUE)),
    R2   = cor(Observed, Predicted, use = "complete.obs")^2
  ), by = Dataset]
}

# --- 2. High-level pipeline function -----------------------------------------

run_spectra_pipeline <- function(spectra_file, traits_file, trait_name,
                                 split_method = "random", split_ratio = 0.7,
                                 press_method = "loo", maxcomp = 20,
                                 final_method = "pls", final_ncomp = 10,
                                 out_prefix = "SpecTraits_build") {

  spectra_dt <- import_spectra(spectra_file)
  traits_dt  <- import_traits(traits_file)

  if (!trait_name %in% names(traits_dt)) {
    stop("Trait '", trait_name, "' not found in traits file.")
  }

  # Step 2: split
  split_vec <- create_split_vector(traits_dt, trait_name,
                                   method = split_method,
                                   ratio  = split_ratio)

  # Step 3: PRESS / optimal components
  press_df <- compute_press(spectra_dt, traits_dt, trait_name,
                            split_vec, method = press_method,
                            maxcomp = maxcomp)

  # Here you can define your own rule to select optimal ncomp.
  # For now, use the argument final_ncomp.
  message("Using ncomp = ", final_ncomp, " for final model.")

  # Step 4: final PLSR
  final_plsr <- run_final_plsr(spectra_dt, traits_dt, trait_name,
                               split_vec, method = final_method,
                               ncomp = final_ncomp)

  # Step 5: predictions & performance
  pred_dt <- compute_predictions(final_plsr, spectra_dt, traits_dt,
                                 trait_name, split_vec)
  perf_dt <- compute_performance(pred_dt)

  # Export
  coef_file <- paste0(out_prefix, "_coefficients.rds")
  model_file <- paste0(out_prefix, "_model.rds")
  pred_file  <- paste0(out_prefix, "_predictions.csv")
  perf_file  <- paste0(out_prefix, "_performance.csv")
  press_file <- paste0(out_prefix, "_press.csv")

  saveRDS(final_plsr$coefficients, file = coef_file)
  saveRDS(final_plsr$model,        file = model_file)
  data.table::fwrite(pred_dt,  pred_file)
  data.table::fwrite(perf_dt,  perf_file)
  data.table::fwrite(press_df, press_file)

  message("Saved coefficients to: ", coef_file)
  message("Saved model to:        ", model_file)
  message("Saved predictions to:  ", pred_file)
  message("Saved performance to:  ", perf_file)
  message("Saved PRESS to:        ", press_file)
}

# --- 3. Command-line interface -----------------------------------------------

option_list <- list(
  make_option(c("-s", "--spectra"), type = "character", help = "Spectra .csv file", metavar = "FILE"),
  make_option(c("-t", "--traits"),  type = "character", help = "Traits .csv file",  metavar = "FILE"),
  make_option(c("-r", "--trait"),   type = "character", help = "Trait name to model", metavar = "TRAIT"),
  make_option(c("--split_method"),  type = "character", default = "random",
              help = "Data split method [default %default]"),
  make_option(c("--split_ratio"),   type = "double", default = 0.7,
              help = "Training ratio [default %default]"),
  make_option(c("--press_method"),  type = "character", default = "loo",
              help = "PRESS method [default %default]"),
  make_option(c("--maxcomp"),       type = "integer", default = 20,
              help = "Max components for PRESS [default %default]"),
  make_option(c("--final_method"),  type = "character", default = "pls",
              help = "Final model method [default %default]"),
  make_option(c("--final_ncomp"),   type = "integer", default = 10,
              help = "Final number of components [default %default]"),
  make_option(c("-o", "--out_prefix"), type = "character", default = "SpecTraits_build",
              help = "Output prefix [default %default]")
)

opt <- parse_args(OptionParser(option_list = option_list))

if (is.null(opt$spectra) || is.null(opt$traits) || is.null(opt$trait)) {
  stop("You must provide --spectra, --traits and --trait")
}

run_spectra_pipeline(
  spectra_file = opt$spectra,
  traits_file  = opt$traits,
  trait_name   = opt$trait,
  split_method = opt$split_method,
  split_ratio  = opt$split_ratio,
  press_method = opt$press_method,
  maxcomp      = opt$maxcomp,
  final_method = opt$final_method,
  final_ncomp  = opt$final_ncomp,
  out_prefix   = opt$out_prefix
)
