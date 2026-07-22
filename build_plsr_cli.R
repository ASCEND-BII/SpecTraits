#!/usr/bin/env Rscript
# SpecTraits — Build PLSR CLI (full export, matches Shiny ZIP output)
# Run from the SpecTraits project root: Rscript build_plsr_cli.R [options]

suppressPackageStartupMessages({
  library(optparse)
  library(data.table)
  library(pls)
  library(ggplot2)
  library(reshape2)
  library(rlang)
})

# magrittr pipe used inside plot helpers
suppressPackageStartupMessages(library(magrittr))

source("R/build/vip_aux.R")
source("R/build/confidence_interval_aux.R")
source("R/build/find_optimal_ncomp_aux.R")
source("R/build/pls_summary_aux.R")
source("R/build/pls_permutation_press_aux.R")
source("R/build/pls_permutation_coef_aux.R")
source("R/build/plsr_predict_aux.R")
source("R/build/model_performance_aux.R")
source("R/build/build_import_plot.R")
source("R/build/split_action_plot.R")
source("R/build/press_action_plot.R")
source("R/build/coefficients_plot.R")
source("R/build/performance_plot.R")
source("R/backend/build_backend.R")

# ── Options ───────────────────────────────────────────────────────────────────
option_list <- list(
  make_option(c("-s", "--spectra"),     type = "character", default = NULL,
              help = "Spectra CSV (required). First col = ID, rest = wavelengths."),
  make_option(c("-t", "--traits"),      type = "character", default = NULL,
              help = "Traits CSV (required). First col = ID, other cols = traits."),
  make_option("--trait",                type = "character", default = NULL,
              help = "Trait column name to model (required)"),
  make_option(c("-o", "--output"),      type = "character", default = ".",
              help = "Output directory [default: current directory]"),

  # ── Split ──────────────────────────────────────────────────────────────────
  make_option("--split_method",         type = "character", default = "random",
              help = "Split: none | random | stratified | group [default: %default]"),
  make_option("--split_ratio",          type = "double",  default = 0.7,
              help = "Training proportion [default: %default]"),
  make_option("--split_group",          type = "character", default = NULL,
              help = "[group] Column name for group-based partitioning"),

  # ── PRESS ──────────────────────────────────────────────────────────────────
  make_option("--press_method",         type = "character", default = "cv",
              help = "PRESS method: loo | cv | permutation [default: %default]"),
  make_option("--maxcomp",              type = "integer",  default = 30L,
              help = "Maximum components to evaluate [default: %default]"),
  make_option("--prop",                 type = "double",  default = 0.8,
              help = "[permutation] Proportion per iteration [default: %default]"),
  make_option("--iterations",           type = "integer",  default = 100L,
              help = "[permutation] Number of iterations [default: %default]"),

  # ── Final model ────────────────────────────────────────────────────────────
  make_option("--ncomp",                type = "integer",  default = NULL,
              help = "Components for final model [default: auto from PRESS]"),
  make_option("--plsr_method",          type = "character", default = NULL,
              help = "Final model method [default: same as --press_method]"),

  # ── Report ─────────────────────────────────────────────────────────────────
  make_option("--report_format",        type = "character", default = NULL,
              help = "Generate report: pdf | html [default: no report]"),
  make_option("--report_author",        type = "character", default = "SpecTraits User",
              help = "Author name for report [default: %default]"),

  # ── Reproducibility ────────────────────────────────────────────────────────
  make_option("--seed",                 type = "integer",  default = 42L,
              help = "Random seed [default: %default]")
)

parser <- OptionParser(
  option_list = option_list,
  usage       = "Rscript build_plsr_cli.R -s spectra.csv -t traits.csv --trait LMA [options]",
  description = paste0(
    "\nSpecTraits Build PLSR CLI\n",
    "Full pipeline with all outputs matching the Shiny ZIP export:\n",
    "  - Figures: spectra, histogram, data split, RMSEP, coefficients, VIP,\n",
    "             training/testing scatter + histogram + residuals\n",
    "  - Tables: coefficients, VIP, RMSEP, observed-predicted, performance\n",
    "  - Report: optional PDF or HTML\n",
    "Run from the SpecTraits project root directory."
  )
)

opt <- parse_args(parser)

# ── Validate ─────────────────────────────────────────────────────────────────
if (is.null(opt$spectra)) { print_help(parser); stop("--spectra is required.", call. = FALSE) }
if (is.null(opt$traits))  { print_help(parser); stop("--traits is required.",  call. = FALSE) }
if (is.null(opt$trait))   { print_help(parser); stop("--trait is required.",   call. = FALSE) }

if (!file.exists(opt$spectra)) stop("Spectra file not found: ", opt$spectra, call. = FALSE)
if (!file.exists(opt$traits))  stop("Traits file not found: ",  opt$traits,  call. = FALSE)

valid_split  <- c("none", "random", "stratified", "group")
valid_method <- c("loo", "cv", "permutation")
if (!opt$split_method %in% valid_split)
  stop("--split_method must be: ", paste(valid_split,  collapse = " | "), call. = FALSE)
if (!opt$press_method %in% valid_method)
  stop("--press_method must be: ", paste(valid_method, collapse = " | "), call. = FALSE)
if (!is.null(opt$report_format) && !opt$report_format %in% c("pdf", "html"))
  stop("--report_format must be: pdf | html", call. = FALSE)

if (!dir.exists(opt$output)) {
  dir.create(opt$output, recursive = TRUE)
  cat(sprintf("[SpecTraits] Created output directory: %s\n", opt$output))
}

# ── Helper: save a ggplot as JPEG ────────────────────────────────────────────
save_fig <- function(plot_obj, path, w = 150, h = 90) {
  jpeg(path, width = w, height = h, units = "mm", res = 300)
  print(plot_obj)
  dev.off()
  cat(sprintf("[SpecTraits]   %s\n", basename(path)))
  invisible(path)
}

# ── Step 1 : Load and align data ─────────────────────────────────────────────
cat(sprintf("[SpecTraits] Loading spectra : %s\n", opt$spectra))
spectra_dt <- fread(opt$spectra)
cat(sprintf("[SpecTraits] Loading traits  : %s\n", opt$traits))
traits_dt  <- fread(opt$traits)

if (!opt$trait %in% names(traits_dt))
  stop(sprintf("Trait '%s' not found. Columns: %s",
               opt$trait, paste(names(traits_dt), collapse = ", ")), call. = FALSE)

traits_dt  <- traits_dt[!is.na(traits_dt[[opt$trait]]), ]
common_ids <- intersect(spectra_dt[[1]], traits_dt[[1]])
spectra_dt <- spectra_dt[spectra_dt[[1]] %in% common_ids, ]
traits_dt  <- traits_dt[traits_dt[[1]]  %in% common_ids, ]

cat(sprintf("[SpecTraits] Trait: %s | Samples: %d | Seed: %d\n",
            opt$trait, nrow(spectra_dt), opt$seed))

# ── Step 2 : Split ───────────────────────────────────────────────────────────
cat(sprintf("[SpecTraits] Step 2: Split (method = %s, ratio = %.2f)\n",
            opt$split_method, opt$split_ratio))

split_vector <- split_data(traits_dt  = traits_dt,
                           trait_name = opt$trait,
                           method     = opt$split_method,
                           ratio      = opt$split_ratio,
                           group      = opt$split_group,
                           seed       = opt$seed)

cat(sprintf("[SpecTraits]   Training: %d | Testing: %d\n",
            length(split_vector), nrow(traits_dt) - length(split_vector)))

# ── Step 3 : PRESS ───────────────────────────────────────────────────────────
cat(sprintf("[SpecTraits] Step 3: PRESS (method = %s, maxcomp = %d)\n",
            opt$press_method, opt$maxcomp))

press_result <- press_plsr_eval(spectra_dt   = spectra_dt,
                                 traits_dt    = traits_dt,
                                 trait_name   = opt$trait,
                                 split_vector = split_vector,
                                 method       = opt$press_method,
                                 maxcomp      = opt$maxcomp,
                                 prop         = opt$prop,
                                 iterations   = opt$iterations,
                                 seed         = opt$seed)

optimal_ncomp <- press_result$optimal
cat(sprintf("[SpecTraits]   Optimal components: %d\n", optimal_ncomp))

# ── Step 4 : Build final model ────────────────────────────────────────────────
ncomp_final  <- if (!is.null(opt$ncomp))       opt$ncomp       else optimal_ncomp
method_final <- if (!is.null(opt$plsr_method)) opt$plsr_method else opt$press_method

cat(sprintf("[SpecTraits] Step 4: Build PLSR (method = %s, ncomp = %d)\n",
            method_final, ncomp_final))

model_results <- build_plsr_model(spectra_dt   = spectra_dt,
                                   traits_dt    = traits_dt,
                                   trait_name   = opt$trait,
                                   split_vector = split_vector,
                                   method       = method_final,
                                   ncomp        = ncomp_final,
                                   prop         = opt$prop,
                                   iterations   = opt$iterations,
                                   seed         = opt$seed)

# ── Step 5 : Compute observed-predicted for all samples ───────────────────────
predicted <- plsr_predict(spectra_frame = spectra_dt,
                          coefficients  = model_results$coefficients)

results_predict <- cbind(
  predicted[, 1],
  Dataset = "Training",
  traits_dt[, .SD, .SDcols = opt$trait],
  predicted[, -1]
)
results_predict[!split_vector, Dataset := "Testing"]
setnames(results_predict, 3, "observed")

results_train <- results_predict[Dataset == "Training", ]
results_test  <- results_predict[Dataset == "Testing",  ]

make_summary <- function(result) {
  values         <- result[, -c(1:3)]
  mean_predicted <- rowMeans(values, na.rm = TRUE)
  sd_predicted   <- apply(values, 1, sd, na.rm = TRUE)
  cbind(result[, c(1:3)], mean_predicted, sd_predicted)
}

train_summary <- make_summary(results_train)
test_summary  <- make_summary(results_test)

group_arg <- if (is.null(opt$split_group)) "none" else opt$split_group

# ── Step 6 : Save all outputs ─────────────────────────────────────────────────
p <- opt$output
tr <- opt$trait
cat("[SpecTraits] Saving outputs:\n")

# ── Figures ────────────────────────────────────────────────────────────────────
fig_spectra       <- save_fig(spectra_summary_figure(spectra_dt),
                               file.path(p, sprintf("%s_spectra.jpeg", tr)))

fig_hist          <- save_fig(trait_summary_figure(traits_dt, tr),
                               file.path(p, sprintf("%s_histogram-distribution.jpeg", tr)))

fig_spectra_split <- save_fig(spectra_split_summary_figure(spectra_dt, split_vector),
                               file.path(p, sprintf("%s_spectra-split.jpeg", tr)))

fig_trait_split   <- save_fig(trait_split_summary_figure(traits_dt, split_vector, tr, group_arg),
                               file.path(p, sprintf("%s_trait-distribution-split.jpeg", tr)))

fig_press         <- save_fig(press_figure(press_result$rmsep, press_result$optimal, press_result$legend),
                               file.path(p, sprintf("%s_RMSEP-components.jpeg", tr)))

fig_coeff         <- save_fig(coef_figure(model_results, method_final),
                               file.path(p, sprintf("%s_coefficients.jpeg", tr)))

fig_vip_path      <- save_fig(vip_figure(model_results, method_final),
                               file.path(p, sprintf("%s_vip.jpeg", tr)))

fig_train_scatter <- save_fig(scatter_performance_plot(train_summary, tr, method_final),
                               file.path(p, sprintf("%s_training_observed-predicted.jpeg", tr)))

fig_train_hist    <- save_fig(histogram_performance_plot(train_summary, tr),
                               file.path(p, sprintf("%s_training_histogram.jpeg", tr)))

fig_train_resid   <- save_fig(residuals_performance_plot(train_summary, tr),
                               file.path(p, sprintf("%s_training_residuals.jpeg", tr)))

fig_test_scatter  <- save_fig(scatter_performance_plot(test_summary, tr, method_final),
                               file.path(p, sprintf("%s_testing_observed-predicted.jpeg", tr)))

fig_test_hist     <- save_fig(histogram_performance_plot(test_summary, tr),
                               file.path(p, sprintf("%s_testing_histogram.jpeg", tr)))

fig_test_resid    <- save_fig(residuals_performance_plot(test_summary, tr),
                               file.path(p, sprintf("%s_testing_residuals.jpeg", tr)))

# ── CSVs ──────────────────────────────────────────────────────────────────────
perf_train <- metrics_performance_frame(results_train, method_final)
perf_test  <- metrics_performance_frame(results_test,  method_final)

fwrite(model_results$coefficients,        file.path(p, sprintf("%s_coefficients.csv",                  tr))); cat(sprintf("[SpecTraits]   %s_coefficients.csv\n",                  tr))
fwrite(model_results$vip,                 file.path(p, sprintf("%s_vip.csv",                           tr))); cat(sprintf("[SpecTraits]   %s_vip.csv\n",                           tr))
fwrite(press_result$rmsep,                file.path(p, sprintf("%s_RMSEP.csv",                         tr))); cat(sprintf("[SpecTraits]   %s_RMSEP.csv\n",                         tr))
fwrite(results_predict,                   file.path(p, sprintf("%s_observed-predicted.csv",             tr))); cat(sprintf("[SpecTraits]   %s_observed-predicted.csv\n",             tr))
fwrite(as.data.table(perf_train),         file.path(p, sprintf("%s_training_performance.csv",           tr))); cat(sprintf("[SpecTraits]   %s_training_performance.csv\n",           tr))
fwrite(as.data.table(perf_test),          file.path(p, sprintf("%s_testing_performance.csv",            tr))); cat(sprintf("[SpecTraits]   %s_testing_performance.csv\n",            tr))

# ── Report (optional) ──────────────────────────────────────────────────────────
if (!is.null(opt$report_format)) {

  cat(sprintf("[SpecTraits] Generating %s report...\n", toupper(opt$report_format)))

  template_path <- normalizePath("R/build/plsr_report_template.qmd", mustWork = FALSE)

  if (!file.exists(template_path)) {
    cat("[SpecTraits] Warning: report template not found at R/build/plsr_report_template.qmd\n")
  } else {

    temp_qmd <- file.path(normalizePath(p), "report.qmd")
    file.copy(template_path, temp_qmd, overwrite = TRUE)

    report_params <- list(
      author          = opt$report_author,
      date            = format(Sys.Date(), "%B %d, %Y"),
      seed            = opt$seed,
      trait_selector  = tr,
      split_method    = list(split = opt$split_method, ratio = opt$split_ratio, group = group_arg),
      press_method    = list(method = opt$press_method, maxcomp = opt$maxcomp,
                             permutation = opt$prop, iterations = opt$iterations),
      press_frame     = press_result,
      final_method    = list(method = method_final, ncomp = ncomp_final,
                             permutation = opt$prop, iterations = opt$iterations),
      final_PLSR      = model_results,
      results_predict = results_predict,
      perf_train      = perf_train,
      perf_test       = perf_test,
      fig_spectra           = fig_spectra,
      fig_trait_hist        = fig_hist,
      fig_split_spectra     = fig_spectra_split,
      fig_split_trait       = fig_trait_split,
      fig_press             = fig_press,
      fig_coefficients      = fig_coeff,
      fig_vip               = fig_vip_path,
      fig_train_scatter     = fig_train_scatter,
      fig_train_histogram   = fig_train_hist,
      fig_train_residuals   = fig_train_resid,
      fig_test_scatter      = fig_test_scatter,
      fig_test_histogram    = fig_test_hist,
      fig_test_residuals    = fig_test_resid
    )

    output_filename <- sprintf("%s_report.%s", tr, opt$report_format)
    render_success  <- FALSE

    if (requireNamespace("quarto", quietly = TRUE)) {
      quarto_ok <- tryCatch({ !is.null(quarto::quarto_version()) }, error = function(e) FALSE)
      if (quarto_ok) {
        tryCatch({
          quarto::quarto_render(
            input          = temp_qmd,
            output_format  = opt$report_format,
            execute_params = report_params,
            output_file    = output_filename
          )
          render_success <- TRUE
        }, error = function(e) cat("[SpecTraits] Quarto render failed:", e$message, "\n"))
      }
    }

    if (!render_success && requireNamespace("rmarkdown", quietly = TRUE)) {
      output_filename <- sprintf("%s_report.html", tr)
      tryCatch({
        rmarkdown::render(
          input         = temp_qmd,
          output_format = rmarkdown::html_document(toc = TRUE, self_contained = TRUE),
          output_file   = output_filename,
          output_dir    = normalizePath(p),
          params        = report_params,
          quiet         = TRUE,
          envir         = new.env()
        )
        render_success <- TRUE
      }, error = function(e) cat("[SpecTraits] rmarkdown render failed:", e$message, "\n"))
    }

    if (file.exists(temp_qmd)) file.remove(temp_qmd)

    if (render_success)
      cat(sprintf("[SpecTraits]   %s\n", output_filename))
    else
      cat("[SpecTraits] Warning: report generation failed.\n")
  }
}

cat(sprintf("[SpecTraits] Done. Seed: %d | Components: %d | Method: %s\n",
            opt$seed, ncomp_final, method_final))
