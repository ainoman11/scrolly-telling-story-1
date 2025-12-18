## Learning Gradient Shiny Visualizations

This repository contains interactive R Shiny applications for exploring **learning gradients** and **wealth-related gradients** in foundational skills using harmonized survey data. The focus is on two production apps and their supporting data; any experimental or legacy materials in other folders are intentionally out of scope.

### Project Structure

- **`learning_gradient.Rproj`**: RStudio project file for convenient opening of the repository.
- **`data/`**
  - `learning_gradient_raw_with_loess.csv`: Input for the per-country learning gradient app (Visual 1).
  - `learning_gradient_bands_raw.csv`: Input for the wealth-gradient app (Visual 2).
  - `learning_gradient_bands_with_loess.csv`: Additional processed data (not required by the two main apps but kept for reference).
- **`visual1_per_country_shiny.R`**
  - Shiny app: **Interactive per‑country learning gradients** (grades 1–8).
  - Uses LOESS-smoothed proficiency values (`proficiency_loess_span1`) and multiple filters.
- **`visual2_wealth_shiny.R`**
  - Shiny app: **Wealth gradients across education stages** (grade bands and wealth quintiles).
  - Focuses on differences between wealth groups and regions, with robust filtering and data‑quality controls.

> Note: Any materials in a `backup/` folder are **not part of the primary, documented workflow** and can be ignored for standard use.

---

### Data Inputs and Expected Columns

#### 1. Visual 1 – Per-Country Learning Gradients

**File**: `data/learning_gradient_raw_with_loess.csv`

**Key expected columns** (non-exhaustive, but required for the app to run):

- **`iso3`**: Country ISO3 code.
- **`grade_numeric`**: Numeric grade indicator (app restricts to grades 1–8).
- **`proficiency_loess_span1`**: LOESS-smoothed proficiency rate.
- **`n`**: Observation count used for data-quality filtering.
- **Optional but supported filter columns** (if present):
  - `category`
  - `subject`
  - `income_level`
  - `Region`
  - `wealth_quintile`
  - `is_africa`

The app validates that:

- `grade_numeric` exists and is numeric.
- Required columns (`iso3`, `grade_numeric`, `proficiency_loess_span1`, `n`) are present.
- Rows with missing critical values are removed.

#### 2. Visual 2 – Wealth Gradients in Foundational Skills

**File**: `data/learning_gradient_bands_raw.csv`

**Key expected columns** (non-exhaustive, but required for the app to run):

- **`iso3`**: Country ISO3 code.
- **`grade_band`**: Grade-band label (e.g. `"Primary (1-3)"`, `"Upper Primary (4-6)"`, `"Lower Secondary (7-9)"`).
- **`subject`**: Subject domain (e.g. reading).
- **`category`**: Wealth group category (e.g. `"Poorest"`, `"Second"`, `"Middle"`, `"Fourth"`, `"Richest"`, `"All"`).
- **`proficiency_rate`**: Proficiency rate used in the charts.
- **`n`**: Observation count.
- **Optional but supported filter columns**:
  - `income_level`
  - `Region`
  - `year`

The app:

- Restricts to a fixed set of valid grade bands.
- Expects wealth categories `c("All", "Poorest", "Second", "Middle", "Fourth", "Richest")` for filtering and coloring.
- Optionally uses `year` to keep the latest year per country (if present).

---

### Software Requirements

- **R** (version 4.x recommended).
- Recommended environment: **RStudio** (using the `learning_gradient.Rproj` file).

#### R Packages

Both apps use (at minimum):

- `shiny`
- `plotly`
- `dplyr`
- `readr`

Depending on your R setup or data preparation workflow, you may also use common tidyverse components (e.g. `tidyr`, `purrr`, `ggplot2`), but the apps as written primarily rely on the packages above.

Install required packages in R:

```r
install.packages(c("shiny", "plotly", "dplyr", "readr"))
```

---

### How to Run the Apps

#### 1. Open the Project

1. Open **RStudio**.
2. Use **File → Open Project…** and select `learning_gradient.Rproj`, or set the working directory to the project root:

```r
setwd("path/to/learning_gradient")
```

Make sure the `data/` folder (with the CSV files described above) is located directly under this root.

#### 2. Run Visual 1 – Per-Country Learning Gradients

- **File**: `visual1_per_country_shiny.R`

From R or RStudio:

```r
shiny::runApp("visual1_per_country_shiny.R")
```

or (if your working directory is the project root and you open the script in RStudio) click **Run App**.

**Main features:**

- Interactive line chart of **LOESS-smoothed proficiency** by grade (1–8).
- Filters for:
  - Category and subject (e.g. reading) when available in the data.
  - Income level and region (multi-select).
  - Wealth quintile with a `<Show All>` option.
  - Africa/non-Africa grouping with a `<Show All>` option.
  - **Minimum observations (`n`)** slider to exclude low‑sample country–grade combinations.
  - **Missing grades tolerance** slider to control how many grades can be missing before excluding a country.
- A dedicated panel listing **countries with missing grades**, plus:
  - Textual **data summary**.
  - **Chart information** panel describing what is being displayed.

Use the **“Reset All Filters”** button to quickly return to the default configuration.

#### 3. Run Visual 2 – Wealth Gradients in Foundational Skills

- **File**: `visual2_wealth_shiny.R`

From R or RStudio:

```r
shiny::runApp("visual2_wealth_shiny.R")
```

or open the script in RStudio and click **Run App**.

**Main features:**

- Interactive visualization of **wealth gradients** across:
  - Grade bands (`Primary (1–3)`, `Upper Primary (4–6)`, `Lower Secondary (7–9)`).
  - Wealth groups (`All`, `Poorest`, `Second`, `Middle`, `Fourth`, `Richest`).
- Configurable **aggregation method** (mean/median) for summarizing proficiency.
- Filters for:
  - Subject (e.g. reading).
  - Selected wealth groups (one or many).
  - Income level (multi-select).
  - Region (multi-select).
  - Year (with `<Show All>` option when multiple years are present).
- **Minimum observations (`n`)** slider and **missing combinations tolerance** slider:
  - Ensures data quality by requiring sufficient observations.
  - Tracks expected 15 combinations per country (3 grade bands × 5 quintiles excluding `"All"`).
- Side panel listing **countries with missing combinations**, plus:
  - Textual **data summary**.
  - **Chart information** panel describing the visualization.

As with Visual 1, a **“Reset All Filters”** button restores the default settings.

---

### Typical Workflow

1. **Prepare or update data** in the `data/` folder, ensuring that the required columns and formats are respected.
2. **Start Visual 1** (`visual1_per_country_shiny.R`) to inspect overall learning gradients by grade and subject, applying:
   - Minimum observations and missing-grade tolerances.
   - Country grouping, income level, region, and wealth filters as needed.
3. **Start Visual 2** (`visual2_wealth_shiny.R`) to dig into wealth-related gradients across grade bands:
   - Compare wealth groups within and across regions/income groups.
   - Adjust aggregation method and data-quality thresholds.
4. Use the **data summary** and **missing countries/combination panels** in both apps to:
   - Diagnose data coverage issues.
   - Document which countries or combinations are excluded due to quality filters.

---

### Reproducibility and Customization

- If you adapt the apps to new datasets:
  - Preserve the key column names (e.g. `iso3`, `grade_numeric`, `grade_band`, `proficiency_loess_span1`, `proficiency_rate`, `n`) or update the app code accordingly.
  - Keep filter columns (`subject`, `income_level`, `Region`, `wealth_quintile`, `category`, `year`, `is_africa`) consistent with the current logic.
- For deployment (e.g. Shiny Server, ShinyApps.io), use these files as your entry-point apps and deploy from the project root with the `data/` folder included.

For any future development, prefer extending or modifying the two main app scripts and the `data/` inputs rather than relying on auxiliary or legacy materials in other folders.


