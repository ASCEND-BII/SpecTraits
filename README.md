# SpecTraits

<div align="center">

**A Shiny Application for Prediction and Modeling of Leaf Traits from Reflectance**

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![GitHub](https://img.shields.io/badge/GitHub-ASCEND--BII%2FSpecTraits-brightgreen)](https://github.com/ASCEND-BII/SpecTraits)

</div>

---

## Summary

**SpecTraits** is an interactive web application designed to facilitate the prediction and modeling of leaf functional traits using hyperspectral reflectance data. The application provides a user-friendly interface for ecologists, remote sensing scientists, and researchers to:

- **Predict** leaf traits from spectral data using pre-built PLSR (Partial Least Squares Regression) models or radiative transfer models (RTM)
- **Build** custom PLSR models with cross-validation and optimization frameworks
- **Preprocess** leaf spectra according to user needs
- **Data** export of spectra and trait data from different sources

SpecTraits bridges the gap between spectroscopic analysis and practical trait estimation, making leaf trait prediction accessible to anyone without extensive programming knowledge.

---

## Repository Description

This repository contains the complete source code for the SpecTraits Shiny application, organized into modular components:

### Project Structure

```
SpecTraits/
├── app.R                       # Main application file
├── R/                          # Application modules and functions
│   ├── predict_panel.R         # Trait prediction module
│   ├── build_panel.R           # Model building module
│   ├── preprocessing_panel.R   # Spectral resampling module
│   ├── data_panel.R            # Data management module
│   ├── predict/                # Prediction-related functions
│   ├── build/                  # Model building functions
│   ├── preprocessing/          # Preprocessing functions
│   └── data/                   # Data configuration
├── inst/                       # Installed files
│   └── extdata/                # Example datasets
├── LICENSE                     # GPL-3 License
└── README.md                   # This file
```

### Key Features

#### 1. **Predict Module**
- Import spectral reflectance data (.csv format)
- Apply pre-trained PLSR coefficients for trait prediction
- Use radiative transfer models (RTM) for trait inversion
- Validate predictions against observed data
- Visualize predicted vs. observed relationships
- Export predictions and validation metrics

#### 2. **Build Module**
- Upload paired spectral and trait data
- Automatic filtering of missing values and ID matching
- Multiple data splitting strategies (random, stratified, group-based)
- Optimal component selection via:
  - Leave-One-Out Cross-Validation (LOO-CV)
  - k-Fold Cross-Validation
  - Permutation-based approaches
- Visualization of model coefficients and Variable Importance in Projection (VIP)
- Comprehensive performance assessment (R², RMSE, MAE, bias, RPD, RPIQ)
- Export complete model results with automated report generation

#### 3. **Preprocess Module**
- Spectral resampling to match different sensor specifications
- Support for multiple resampling methods
- Export resampled spectra

#### 4. **Data Module**
- Access to example datasets
- Data format guidelines
- Template downloads

---

## Requirements

### System Requirements

- **R version**: ≥ 4.0.0
- **Operating System**: Windows, macOS, or Linux
- **Quarto** (optional, for report generation): [Install Quarto](https://quarto.org/docs/get-started/)

### R Packages

SpecTraits automatically installs required packages on first run. Core dependencies include:

```r
# User Interface
shiny, shinycssloaders, bslib, DT

# Data Manipulation
data.table, dplyr, magrittr, reshape2

# Modeling and Statistics
pls, caret, rlang

# Visualization
ggplot2

# Spectral Analysis
spectrolab

# Utilities
here, zip, knitr, rmarkdown, quarto, kableExtra

# Radiative Transfer Models
ccrtm (from GitHub: MarcoDVisser/ccrtm)
```

### Input Data Format

**Spectral Data (.csv):**
- First column: `ID` (unique sample identifiers)
- Subsequent columns: Wavelengths (in nm) as column headers
- Rows: Individual samples
- Values: Reflectance (0-1 or 0-100%)

**Trait Data (.csv):**
- First column: `ID` (matching spectral data IDs)
- Subsequent columns: Trait names
- Rows: Individual samples
- Values: Trait measurements

**Example:**

Spectral file:
```
ID,400,401,402,...,2400
sample_001,0.05,0.06,0.06,...,0.45
sample_002,0.04,0.05,0.05,...,0.42
```

Trait file:
```
ID,LMA,Nitrogen,Chlorophyll
sample_001,85.3,2.1,45.2
sample_002,92.1,1.8,38.7
```

---

## Installation and Usage

### Installation

1. **Install R** from [CRAN](https://cran.r-project.org/)

2. **Clone this repository:**
   ```bash
   git clone https://github.com/ASCEND-BII/SpecTraits.git
   cd SpecTraits
   ```

3. **(Optional) Install Quarto** for report generation:
   - Download from [quarto.org](https://quarto.org/docs/get-started/)

### Running the Application

1. **Open R or RStudio** and set the working directory:
   ```r
   setwd("path/to/SpecTraits")
   ```

2. **Launch the application:**
   ```r
   shiny::runApp("app.R")
   ```

3. The application will automatically install required packages on first run.

4. Your web browser will open with the SpecTraits interface.

---

## Citation

If you use SpecTraits in your research, please cite:

**Guzmán Q., J. Antonio** (2025). *SpecTraits: A Shiny Application for Prediction of Leaf Traits from Spectral Models*. GitHub: ASCEND-BII/SpecTraits. https://github.com/ASCEND-BII/SpecTraits

### BibTeX Entry

```bibtex
@software{guzman2025spectraits,
  author = {Guzmán, J.A. et al.},
  title = {SpecTraits: A Shiny Application for Prediction of Leaf Traits from Spectral Models},
  year = {2025},
  publisher = {GitHub},
  journal = {GitHub repository},
  howpublished = {\url{https://github.com/ASCEND-BII/SpecTraits}},
  url = {https://github.com/ASCEND-BII/SpecTraits}
}
```

---

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

### Reporting Issues

If you encounter bugs or have feature requests, please open an issue on the [GitHub Issues page](https://github.com/ASCEND-BII/SpecTraits/issues).

---

## License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details.

---

## Contact

**Author:** J. Antonio Guzmán Q.
**Email:** aguzman@fas.harvard.edu
**Institution:** ASCEND-BII

---

## Acknowledgments

SpecTraits is developed as part of the ASCEND-BII initiative. We thank all contributors and users for their feedback and support.

---

<div align="center">

**Version 0.1** | **Last Updated:** 2026-03-11

</div>
