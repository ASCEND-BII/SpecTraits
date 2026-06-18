#!/usr/bin/env Rscript
# SpecTraits — Preprocessing CLI
# Run from the SpecTraits project root: Rscript preprocess_cli.R [options]

suppressPackageStartupMessages({
  library(optparse)
  library(data.table)
})

source("R/backend/preprocess_backend.R")

option_list <- list(
  make_option(c("-i", "--input"),  type = "character", default = NULL,
              help = "Input spectra CSV file (required)"),
  make_option(c("-o", "--output"), type = "character", default = NULL,
              help = "Output CSV path [default: <method>_spectra_<date>.csv]"),
  make_option(c("-m", "--method"), type = "character", default = "resample",
              help = "Preprocessing method: resample | smoothing | transformation [default: %default]"),

  # --- resample ---
  make_option("--wl_start",         type = "double",  default = 400.0,
              help = "[resample] Start wavelength nm [default: %default]"),
  make_option("--wl_end",           type = "double",  default = 2400.0,
              help = "[resample] End wavelength nm [default: %default]"),
  make_option("--wl_step",          type = "double",  default = 10.0,
              help = "[resample] Wavelength step nm [default: %default]"),

  # --- smoothing ---
  make_option("--window",           type = "integer", default = 11L,
              help = "[smoothing] Savitzky-Golay window (odd, >= 3) [default: %default]"),
  make_option("--deriv_order",      type = "integer", default = 0L,
              help = "[smoothing] Derivative order (0 = no derivative) [default: %default]"),

  # --- transformation ---
  make_option("--transform_type",   type = "character", default = "norm",
              help = "[transformation] Type: norm | wavelet | derivative [default: %default]"),
  make_option("--scales",           type = "character", default = "1,2,3",
              help = "[transformation/wavelet] CWT scales, comma-separated [default: %default]"),
  make_option("--variance",         type = "double",  default = 1.0,
              help = "[transformation/wavelet] CWT variance [default: %default]"),
  make_option("--deriv_window",     type = "integer", default = 11L,
              help = "[transformation/derivative] SG window (odd, >= 3) [default: %default]"),
  make_option("--deriv_scale_order",type = "integer", default = 1L,
              help = "[transformation/derivative] SG derivative order [default: %default]")
)

parser <- OptionParser(
  option_list  = option_list,
  usage        = "Rscript preprocess_cli.R -i spectra.csv -m resample [options]",
  description  = paste0(
    "\nSpecTraits Preprocessing CLI\n",
    "Apply FWHM resampling, Savitzky-Golay smoothing, or spectral transformation.\n",
    "Input CSV: first column = sample ID, remaining columns = wavelengths.\n",
    "Run from the SpecTraits project root directory."
  )
)

opt <- parse_args(parser)

# ── Validate ────────────────────────────────────────────────────────────────
if (is.null(opt$input)) {
  print_help(parser)
  stop("--input is required.", call. = FALSE)
}
if (!file.exists(opt$input))
  stop("Input file not found: ", opt$input, call. = FALSE)
if (!opt$method %in% c("resample", "smoothing", "transformation"))
  stop("--method must be one of: resample, smoothing, transformation", call. = FALSE)

# ── Read data ───────────────────────────────────────────────────────────────
cat(sprintf("[SpecTraits] Reading: %s\n", opt$input))
spectra_dt <- fread(opt$input)
if (ncol(spectra_dt) < 2)
  stop("Input must have an ID column plus at least one wavelength column.", call. = FALSE)
cat(sprintf("[SpecTraits] Input: %d samples x %d bands\n", nrow(spectra_dt), ncol(spectra_dt) - 1))

# ── Apply method ─────────────────────────────────────────────────────────────
cat(sprintf("[SpecTraits] Applying: %s\n", opt$method))

if (opt$method == "resample") {
  result       <- spectra_resample(spectra_dt,
                                   wl_start = opt$wl_start,
                                   wl_end   = opt$wl_end,
                                   wl_step  = opt$wl_step)
  default_name <- sprintf("resampled_spectra_%s.csv", Sys.Date())

} else if (opt$method == "smoothing") {
  result       <- spectra_smooth(spectra_dt,
                                  window      = opt$window,
                                  deriv_order = opt$deriv_order)
  default_name <- sprintf("smoothed_spectra_%s.csv", Sys.Date())

} else if (opt$method == "transformation") {
  scales_vec   <- as.numeric(strsplit(opt$scales, ",")[[1]])
  result       <- spectra_transform(spectra_dt,
                                     transform_type    = opt$transform_type,
                                     scales            = scales_vec,
                                     variance          = opt$variance,
                                     deriv_window      = opt$deriv_window,
                                     deriv_scale_order = opt$deriv_scale_order)
  default_name <- sprintf("%s_spectra_%s.csv", opt$transform_type, Sys.Date())
}

# ── Save ─────────────────────────────────────────────────────────────────────
out_path <- if (!is.null(opt$output)) opt$output else default_name
fwrite(result, out_path)
cat(sprintf("[SpecTraits] Saved %d samples x %d bands to: %s\n",
            nrow(result), ncol(result) - 1, out_path))
