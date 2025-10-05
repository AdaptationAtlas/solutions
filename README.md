 # Adaptation Atlas – Solutions Repository

  This repository hosts the analytical workflow, scripts, and outputs for the **Solutions Explorer** component of the Africa Agriculture Adaptation Atlas (https://adaptationatlas.cgiar.org).  
  It provides the reproducible codebase behind the *Analogues and Solution Mapping* module, which identifies where climate and management conditions align across regions, supporting evidence-based adaptation planning.

  ---

  ## 🌍 Purpose

  The **Solutions** repository operationalizes a central concept in the Adaptation Atlas:  
  > “Where have similar challenges been successfully addressed, and which solutions might work here?”

  It implements algorithms to map **agroecological analogues** between locations—comparing climate, yield, and management characteristics to identify transferable adaptation options.

  ---

  ## 📁 Repository Structure

  solutions/
  ├── analogues_admin_aez.qmd      # Quarto notebook driving the analysis and visualization
  ├── spotlight2.R                 # Supporting analysis script for highlighting regions
  ├── R/                           # Core analytical functions
  │   ├── 1_Prepare_Datasets.R
  │   ├── 2_Create_Analogues.R
  │   ├── 3_Combine_Analogues_Limits.R
  │   ├── 4.1_Classify_Thresholds_Limits.R
  │   ├── 5_admin_extract.R
  │   ├── ERAAnalyze.R, PrepareERA.R, OutCalc.R
  │   └── archive/ (older script versions)
  ├── data/
  │   ├── ERA_Derived.rda, Practices.csv, Outcomes.csv
  │   ├── aez/ (AEZ and climate rasters)
  │   ├── boundaries/atlas-region_admin0_harmonized.gpkg
  │   └── results_mean_class_crop5_ex/ (example outputs)
  └── analogues_admin_aez.html     # Rendered interactive notebook

  ---

  ## 🔍 Key Functions

  | Stage | Script | Description |
  |-------|---------|-------------|
  | **1. Data Preparation** | 1_Prepare_Datasets.R, PrepareERA.R | Loads ERA and AEZ datasets, harmonizes geospatial layers. |
  | **2. Analogues Creation** | 2_Create_Analogues.R, 2.1_Analogues_Functions.R | Computes similarity metrics between AEZ–ERA combinations. |
  | **3. Aggregation & Thresholds** | 3_Combine_Analogues_Limits.R, 4.1_Classify_Thresholds_Limits.R | Combines candidate analogues, applies confidence limits. |
  | **4. Visualization & Export** | analogues_admin_aez.qmd, 5_admin_extract.R | Produces Quarto HTML outputs and regional solution summaries. |

  ---

  ## 📊 Output

  - **Interactive Quarto notebook:**  
    analogues_admin_aez.html — displays analogue matches across Africa using ERA and AEZ data.

  - **Derived datasets:**  
    Processed tables and maps for potential inclusion in Atlas visualizations.

  ---

  ## 🧠 Background

  This work supports the *Climate Context* and *Discover Solutions* sections of the Adaptation Atlas by:
  - Identifying where conditions today resemble projected futures elsewhere.  
  - Linking observed management practices (from ERA dataset) to similar climatic zones.  
  - Providing the empirical foundation for “solution transferability” across agroecological zones.

  ---

  ## 🧩 Dependencies

  Built with R ≥ 4.2, using core packages:  
  terra, data.table, sf, dplyr, arrow, progressr, and quarto.  

  All dependencies can be installed via:
  ```r
  source("R/PrepareERA.R")
