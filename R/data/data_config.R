################################################################################
# Data Configuration for Different Datasets
################################################################################

get_data_config <- function(filename) {

  # Define configurations for known datasets
  configs <- list(

    # Cavender-Bares Lab dataset
    "Cavender-Bares_LAB_1nm.csv" = list(
      filter_columns = c(
        "projectLocation",
        "sampleID",
        "sensor",
        "condition",
        "family",
        "species"
      ),
      traits = c(
        "thickness_dry",
        "thickness_fresh",
        "SLA",
        "LMA",
        "WC",
        "LDMC",
        "EWT",
        "solubles",
        "hemicellulose",
        "cellulose",
        "lignin",
        "Na",
        "Mg",
        "P",
        "K",
        "Ca",
        "Mn",
        "Fe",
        "Zn",
        "Mo"
      ),
      spectral_pattern = "^[0-9]+$",  # Pure numbers (450, 451, 452...)
      display_name = "Cavender-Bares Lab 1nm Spectra",
      citation = "Cavender-Bares Lab 1nm Spectra"
    ),

    # Global Spectra-Trait Initiative dataset
    "Global_Spectra-Trait_Initiative.csv" = list(
      filter_columns = c(
        "Site_name",
        "Dataset_name",
        "Species",
        "Sun_Shade",
        "Phenological_stage",
        "Plant_type",
        "Photosynthetic_pathway",
        "Soil"
      ),
      traits = c(
        "LMA",
        "Narea",
        "Nmass",
        "Parea",
        "Pmass",
        "LWC",
        "Vcmax25",
        "Jmax25",
        "TPU25",
        "Rday25"
      ),
      spectral_pattern = "^Wave_\\.[0-9]+$",  # Wave_.350, Wave_.351...
      display_name = "Global Spectra-Trait Initiative",
      citation = "Lamour J ; Serbin S ; Rogers A ; Ely K ; Acebron K T ; Ainsworth E ; Albert L P ; Alonzo M ; Anderson J ; Atkin O K ; Barbier N ; Barnes M L ; Bernacchi C J ; Besson N ; Burnett A C ; Caplan J S ; Chave J ; Cheesman A W ; Clocher I ; Coast O ; Coste S ; Croft H ; Cui B ; Dauvissat C ; Davidson K J ; Doughty C ; Evans J ; Feret J ; Filella I ; Fortunel C ; Fu P ; Furbank R ; Garcia M ; Gimenez B O ; Guan K ; Guo Z ; Heckman D ; Heuret P ; Marney I ; Kothari S ; Kumagai E ; Kyaw T Y ; Liu L ; Liu L ; Liu S ; Llusià J ; Magney T ; Maréchaux I ; Martin A R ; Meacham-Hensold K ; Montes C M ; Ogaya R ; Ojo J ; Oliveira R ; Paquette A ; Peñuelas J ; Placido A D ; Posada J M ; Qian X ; Renninger H J ; Rodriguez-Caton M ; Rojas-González A ; Schlüter U ; Sellan G ; Siegert C M ; Silva Pérez V ; Song G ; Southwick C D ; Souza D C ; Stahl C ; Su Y ; Sujeeun L ; Ting T ; Vasquez V ; Vijayakumar V ; Vilas-Boas M ; Wang D R ; Wang S ; Wang H ; Wang J ; Wang X ; Weber A P ; Wong C Y ; Wu J ; Wu F ; Wu S ; Yan Z ; Yang D ; Zhao Y (2025): The Global Spectra-Trait Initiative: A database of paired leaf spectroscopy and functional traits associated with leaf photosynthetic capacity. Next-Generation Ecosystem Experiments (NGEE) Tropics, ESS-DIVE repository. Dataset. doi:10.15485/2530733 accessed via https://data.ess-dive.lbl.gov/datasets/doi:10.15485/2530733 on 2025-12-11"
    )
  )

  # Return config if found, otherwise return NULL for auto-detection
  if (filename %in% names(configs)) {
    return(configs[[filename]])
  } else {
    return(NULL)
  }
}

# Auto-detect metadata, trait, and spectral columns
auto_detect_columns <- function(data) {

  all_cols <- names(data)

  # Common spectral column patterns
  spectral_patterns <- c(
    "^[0-9]+$",              # Pure numbers (450, 451, ...)
    "^[0-9]+\\.[0-9]+$",     # Decimals (450.5, 451.0, ...)
    "^Wave_?[0-9]+$",        # Wave350, Wave_350
    "^Wave_?\\.[0-9]+$",     # Wave_.350
    "^wl[0-9]+$",            # wl350, wl351
    "^nm[0-9]+$"             # nm350, nm351
  )

  # Identify spectral columns
  spectral_cols <- all_cols[
    sapply(all_cols, function(col) {
      any(sapply(spectral_patterns, function(pattern) {
        grepl(pattern, col, ignore.case = FALSE)
      }))
    })
  ]

  # Common filter column patterns (for UI filtering)
  filter_patterns <- c(
    "site", "Site", "location", "Location",
    "ID", "id", "sensor", "Sensor",
    "condition", "Condition", "family", "Family",
    "species", "Species", "genus", "Genus",
    "type", "Type", "stage", "Stage", "pathway", "Pathway",
    "shade", "Shade", "soil", "Soil", "dataset", "Dataset"
  )

  # Identify filter columns
  filter_cols <- all_cols[
    !all_cols %in% spectral_cols &
      sapply(all_cols, function(col) {
        any(sapply(filter_patterns, function(pattern) {
          grepl(pattern, col, ignore.case = FALSE)
        }))
      })
  ]

  # Common trait column patterns
  trait_patterns <- c(
    "thickness", "SLA", "LMA", "WC", "LDMC", "EWT",
    "cellulose", "lignin", "solubles", "hemicellulose",
    "^Na$", "^Mg$", "^P$", "^K$", "^Ca$", "^Mn$", "^Fe$", "^Zn$", "^Mo$",
    "Narea", "Nmass", "Parea", "Pmass",
    "Vcmax", "Jmax", "TPU", "Rday"
  )

  # Identify trait columns (numeric, not spectral)
  trait_cols <- all_cols[
    !all_cols %in% spectral_cols &
      sapply(data, is.numeric) &
      sapply(all_cols, function(col) {
        any(sapply(trait_patterns, function(pattern) {
          grepl(pattern, col, ignore.case = FALSE)
        }))
      })
  ]

  return(list(
    filter_columns = filter_cols,
    traits = trait_cols,
    spectral = spectral_cols,
    spectral_pattern = NULL
  ))
}

# Extract spectral columns based on config
get_spectral_columns <- function(data, config) {
  all_cols <- names(data)

  if (!is.null(config$spectral_pattern)) {
    # Use pattern from config
    spectral_cols <- all_cols[grepl(config$spectral_pattern, all_cols)]
  } else if (!is.null(config$spectral)) {
    # Use pre-identified columns
    spectral_cols <- config$spectral
  } else {
    # No spectral columns defined
    spectral_cols <- character(0)
  }

  return(spectral_cols)
}
