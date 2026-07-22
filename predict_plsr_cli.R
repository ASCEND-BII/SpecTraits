#!/usr/bin/env Rscript
# SpecTraits — Predict CLI
# Run from the SpecTraits project root: Rscript predict_plsr_cli.R [options]

suppressPackageStartupMessages({
  library(optparse)
  library(data.table)
})

source("R/backend/predict_backend.R")

option_list <- list(
  make_option(c("-s", "--spectra"),  type = "character", default = NULL,
              help = "Spectra CSV file (required). First col = ID, rest = wavelengths."),
  make_option(c("-m", "--method"),   type = "character", default = "pls",
              help = "Prediction method: pls | rtm [default: %default]"),
  make_option(c("-o", "--output"),   type = "character", default = NULL,
              help = "Output CSV path [default: predicted_<method>_<date>.csv]"),

  # ── PLS ────────────────────────────────────────────────────────────────────
  make_option("--coefficients",      type = "character", default = NULL,
              help = "[pls] Coefficients CSV produced by build_plsr_cli.R (required for pls)"),

  # ── RTM ────────────────────────────────────────────────────────────────────
  make_option("--rtm_model",         type = "character", default = "prospect_d",
              help = "[rtm] RTM model: prospect_d | prospect_5b [default: %default]")
)

parser <- OptionParser(
  option_list = option_list,
  usage       = "Rscript predict_plsr_cli.R -s spectra.csv -m pls --coefficients coeff.csv [options]",
  description = paste0(
    "\nSpecTraits Predict CLI\n",
    "Predict leaf traits from spectral data using PLSR coefficients or RTM inversion.\n",
    "PLS  requires spectra + a coefficients CSV from build_plsr_cli.R.\n",
    "RTM  requires spectra with columns 400–2400 nm at 1 nm spacing.\n",
    "Run from the SpecTraits project root directory."
  )
)

opt <- parse_args(parser)

# ── Validate ─────────────────────────────────────────────────────────────────
if (is.null(opt$spectra)) { print_help(parser); stop("--spectra is required.", call. = FALSE) }
if (!file.exists(opt$spectra)) stop("Spectra file not found: ", opt$spectra, call. = FALSE)
if (!opt$method %in% c("pls", "rtm"))
  stop("--method must be: pls | rtm", call. = FALSE)

if (opt$method == "pls") {
  if (is.null(opt$coefficients))
    stop("--coefficients is required when --method pls", call. = FALSE)
  if (!file.exists(opt$coefficients))
    stop("Coefficients file not found: ", opt$coefficients, call. = FALSE)
}

if (opt$method == "rtm" && !opt$rtm_model %in% c("prospect_d", "prospect_5b"))
  stop("--rtm_model must be: prospect_d | prospect_5b", call. = FALSE)

# ── Load spectra ──────────────────────────────────────────────────────────────
cat(sprintf("[SpecTraits] Reading spectra: %s\n", opt$spectra))
spectra_dt <- fread(opt$spectra)
cat(sprintf("[SpecTraits] Input: %d samples x %d bands\n", nrow(spectra_dt), ncol(spectra_dt) - 1))

# ── Predict ───────────────────────────────────────────────────────────────────
if (opt$method == "pls") {
  cat(sprintf("[SpecTraits] Loading coefficients: %s\n", opt$coefficients))
  coeff_dt <- fread(opt$coefficients)
  cat("[SpecTraits] Applying PLSR coefficients...\n")
  result <- plsr_traits_predict(spectra_frame = spectra_dt, coefficients = coeff_dt)

} else if (opt$method == "rtm") {
  if (!requireNamespace("ccrtm", quietly = TRUE))
    stop("Package 'ccrtm' is required for RTM. Install with:\n",
         "  remotes::install_github('MarcoDVisser/ccrtm')", call. = FALSE)
  cat(sprintf("[SpecTraits] Running RTM inversion (model = %s)...\n", opt$rtm_model))
  result <- rtm_traits_predict(spectra_frame = spectra_dt, rtm_model = opt$rtm_model)
}

# ── Save ──────────────────────────────────────────────────────────────────────
out_path <- if (!is.null(opt$output)) opt$output else
  sprintf("predicted_%s_%s.csv", opt$method, Sys.Date())

fwrite(result, out_path)
cat(sprintf("[SpecTraits] Saved %d predictions to: %s\n", nrow(result), out_path))
