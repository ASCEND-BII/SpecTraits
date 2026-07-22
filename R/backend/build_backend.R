################################################################################
# Build backend — pure R functions shared by Shiny modules and CLI
# Requires: data.table, pls
# Assumes in scope: find_optimal_ncomp, pls_summary,
#                   pls_permutation_press, pls_permutation_coef
################################################################################

split_data <- function(traits_dt, trait_name, method = "random", ratio = 0.7,
                       group = NULL, seed = 42) {
  set.seed(seed)
  traits_dt <- data.table::as.data.table(traits_dt)
  n <- nrow(traits_dt)

  if (method == "none") {
    return(seq_len(n))

  } else if (method == "random") {
    return(sample(n, floor(n * ratio)))

  } else if (method == "stratified") {
    breaks  <- hist(traits_dt[[trait_name]], plot = FALSE)$breaks
    dt      <- data.table::copy(traits_dt)
    dt[, bin := cut(get(trait_name), breaks = breaks,
                    include.lowest = TRUE, right = FALSE)]
    dt[, row := .I]
    sampled <- dt[, .SD[sample(.N, floor(.N * ratio))], by = bin]
    return(sampled$row)

  } else if (method == "group") {
    if (is.null(group)) stop("'group' column must be provided for group split.")
    plt <- traits_dt[, .SD, .SDcols = group]
    data.table::setnames(plt, 1, "group")
    idx <- caret::createDataPartition(plt$group, p = ratio, list = FALSE, times = 1)
    return(as.integer(idx))

  } else {
    stop("Unknown split method: '", method, "'. Choose from: none, random, stratified, group.")
  }
}

press_plsr_eval <- function(spectra_dt, traits_dt, trait_name, split_vector,
                             method = "cv", maxcomp = 30, prop = 0.8,
                             iterations = 100, seed = 42) {
  set.seed(seed)
  spectra_dt <- data.table::as.data.table(spectra_dt)
  traits_dt  <- data.table::as.data.table(traits_dt)

  variables      <- c("ID", trait_name)
  frame_to_model <- merge(traits_dt[, .SD, .SDcols = variables], spectra_dt, by = "ID")
  frame_to_model <- frame_to_model[, -"ID"]
  data.table::setnames(frame_to_model, 1, "trait")
  frame_training <- frame_to_model[split_vector, ]

  if (method == "loo") {
    plsr_model <- pls::plsr(trait ~ ., scale = FALSE, center = TRUE, ncomp = maxcomp,
                            validation = "LOO", trace = FALSE, method = "oscorespls",
                            data = frame_training)
    opt <- find_optimal_ncomp(model = plsr_model, traits = frame_training$trait, method = "loo")

  } else if (method == "cv") {
    plsr_model <- pls::plsr(trait ~ ., scale = FALSE, center = TRUE, ncomp = maxcomp,
                            validation = "CV", trace = FALSE, method = "oscorespls",
                            data = frame_training)
    opt <- find_optimal_ncomp(model = plsr_model, traits = frame_training$trait, method = "cv")

  } else if (method == "permutation") {
    press_results <- pls_permutation_press(formula = trait ~ ., maxcomp = maxcomp,
                                           iterations = iterations, prop = prop,
                                           data = frame_training)
    opt <- find_optimal_ncomp(model = press_results, traits = frame_training$trait,
                              method = "permutation")

  } else {
    stop("Unknown PRESS method: '", method, "'. Choose from: loo, cv, permutation.")
  }

  opt
}

build_plsr_model <- function(spectra_dt, traits_dt, trait_name, split_vector,
                              method = "cv", ncomp = 10, prop = 0.8,
                              iterations = 100, seed = 42) {
  set.seed(seed)
  spectra_dt <- data.table::as.data.table(spectra_dt)
  traits_dt  <- data.table::as.data.table(traits_dt)

  variables      <- c("ID", trait_name)
  frame_to_model <- merge(traits_dt[, .SD, .SDcols = variables], spectra_dt, by = "ID")
  frame_to_model <- frame_to_model[, -"ID"]
  data.table::setnames(frame_to_model, 1, "trait")
  frame_training <- frame_to_model[split_vector, ]

  if (method == "loo") {
    plsr_model <- pls::plsr(trait ~ ., scale = FALSE, center = TRUE, ncomp = ncomp,
                            validation = "LOO", trace = FALSE, jackknife = TRUE,
                            method = "oscorespls", data = frame_training)
    results <- pls_summary(model = plsr_model, ncomp = ncomp, data = frame_training)

  } else if (method == "cv") {
    plsr_model <- pls::plsr(trait ~ ., scale = FALSE, center = TRUE, ncomp = ncomp,
                            validation = "CV", trace = FALSE, jackknife = TRUE,
                            method = "oscorespls", data = frame_training)
    results <- pls_summary(model = plsr_model, ncomp = ncomp, data = frame_training)

  } else if (method == "permutation") {
    results <- pls_permutation_coef(formula = trait ~ ., maxcomp = ncomp,
                                    iterations = iterations, prop = prop,
                                    data = frame_training)

  } else {
    stop("Unknown PLSR method: '", method, "'. Choose from: loo, cv, permutation.")
  }

  results
}
