# Interactive Line Chart Shiny App for Learning Gradient Data
# This Shiny app provides full interactivity with dropdowns and filters
# Uses raw data with LOESS-smoothed proficiency values (proficiency_loess_span1)
# Allows filtering by observation count (n) for data quality control

library(shiny)
library(plotly)
library(dplyr)
library(readr)

# ============================================================================
# DATA LOADING
# ============================================================================

data_file <- "data/learning_gradient_raw_with_loess.csv"

# Load data
data <- read_csv(data_file, show_col_types = FALSE)

# ============================================================================
# DATA PREPARATION
# ============================================================================

# Check that grade_numeric exists
if (!"grade_numeric" %in% colnames(data)) {
  stop("ERROR: grade_numeric column not found in the data.")
}

# Ensure grade_numeric is numeric and restrict to grades 1–8
data <- data %>%
  mutate(grade_numeric = as.numeric(grade_numeric)) %>%
  filter(grade_numeric >= 1, grade_numeric <= 8)

# Validate required columns exist
required_cols <- c("iso3", "grade_numeric", "proficiency_loess_span1", "n")
missing_cols <- required_cols[!required_cols %in% colnames(data)]

if (length(missing_cols) > 0) {
  stop("ERROR: Required columns missing from data: ", paste(missing_cols, collapse = ", "))
}

# Remove rows with missing critical data
data <- data %>%
  filter(!is.na(grade_numeric) & !is.na(proficiency_loess_span1))

# Get unique values for filters - dynamically based on available columns
filter_columns <- list()

# Category: no "Show All", just actual values (including "All")
if ("category" %in% colnames(data)) {
  filter_columns$category <- sort(unique(data$category))
}

# Subject: for dropdown (no "Show All")
if ("subject" %in% colnames(data)) {
  filter_columns$subject <- sort(unique(data$subject))
}

# Income level: for multi-select (all values, no "Show All")
if ("income_level" %in% colnames(data)) {
  filter_columns$income_level <- sort(unique(data$income_level))
}

# Region: for multi-select (all values, no "Show All")
if ("Region" %in% colnames(data)) {
  filter_columns$Region <- sort(unique(data$Region))
}

# Wealth quintile: with "Show All" option
if ("wealth_quintile" %in% colnames(data)) {
  filter_columns$wealth_quintile <- c("<Show All>", sort(unique(data$wealth_quintile)))
}

# Is Africa: with "Show All" option
if ("is_africa" %in% colnames(data)) {
  filter_columns$is_africa <- c("<Show All>", sort(unique(data$is_africa)))
}

# ============================================================================
# UI DEFINITION
# ============================================================================

ui <- fluidPage(
  titlePanel("Learning Gradient Interactive Line Chart - LOESS Smoothed"),

  tags$head(
    tags$style(HTML("
      .chart-container {
        width: 100% !important;
        height: auto !important;
        min-height: 400px;
      }
      .plotly.html-widget {
        width: 100% !important;
        height: auto !important;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("Data Information"),
      helpText("This dataset shows LOESS-smoothed proficiency rates. Filtering by observation count (n) ensures data quality."),
      
      hr(),
      
      h4("Filters"),
      
      # Category: dropdown, default to "All"
      if ("category" %in% names(filter_columns)) {
        selectInput("filter_category",
                    "Category:",
                    choices = filter_columns$category,
                    selected = "All")
      },
      
      # Subject: dropdown, default to "reading"
      if ("subject" %in% names(filter_columns)) {
        selectInput("filter_subject",
                    "Subject:",
                    choices = filter_columns$subject,
                    selected = "reading")
      },
      
      # Income Level: multi-select dropdown, all selected by default
      if ("income_level" %in% names(filter_columns)) {
        selectInput("filter_income",
                    "Income Level:",
                    choices = filter_columns$income_level,
                    selected = filter_columns$income_level,
                    multiple = TRUE)
      },

      # Region: multi-select dropdown, all selected by default
      if ("Region" %in% names(filter_columns)) {
        selectInput("filter_region",
                    "Region:",
                    choices = filter_columns$Region,
                    selected = filter_columns$Region,
                    multiple = TRUE)
      },
      
      # Wealth Quintile: dropdown with "Show All"
      if ("wealth_quintile" %in% names(filter_columns)) {
        selectInput("filter_wealth",
                    "Wealth Quintile:",
                    choices = filter_columns$wealth_quintile,
                    selected = "<Show All>")
      },
      
      # Is Africa: dropdown with "Show All"
      if ("is_africa" %in% names(filter_columns)) {
        selectInput("filter_africa",
                    "Is Africa:",
                    choices = filter_columns$is_africa,
                    selected = "<Show All>")
      },
      
      # Minimum observations filter (slider)
      sliderInput("min_observations",
                  "Minimum Observations (n):",
                  min = 0,
                  max = 100,
                  value = 50,
                  step = 5),

      # Missing grades tolerance filter (slider)
      sliderInput("missing_grades_tolerance",
                  "Missing Grades Tolerance:",
                  min = 1,
                  max = 8,
                  value = 8,
                  step = 1),

      helpText("Minimum observations: Excludes country-grade combinations with fewer observations than this threshold."),
      helpText("Missing grades tolerance: Maximum number of missing grades allowed per country (1 = complete data with all 8 grades, 8 = all countries included)."),
      
      hr(),
      
      # Reset filters button
      actionButton("reset_filters", "Reset All Filters", 
                   class = "btn-warning")
    ),
    
    mainPanel(
      width = 9,
      
      fluidRow(
        column(
          width = 8,
          # Plot output - full width, resizable
          div(class = "chart-container",
              plotlyOutput("line_chart", height = "900px")
          )
        ),
        column(
          width = 4,
          # Missing countries list
          wellPanel(
            h4("Countries with Missing Grades"),
            htmlOutput("missing_countries_list"),
            style = "height: 900px; overflow-y: auto;"
          )
        )
      ),
      
      # Data summary panel
      wellPanel(
        h4("Data Summary"),
        verbatimTextOutput("data_summary")
      ),
      
      # Information panel below data summary
      wellPanel(
        h4("Chart Information"),
        htmlOutput("chart_info")
      )
    )
  )
)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Function to create hover text with all available information
create_hover_text <- function(row_data, y_value) {
  text_parts <- c(paste0("<b>Country: ", row_data$iso3, "</b>"))
  
  # Add all available columns to hover text
  if ("subject" %in% names(row_data)) 
    text_parts <- c(text_parts, paste0("Subject: ", row_data$subject))
  if ("category" %in% names(row_data)) 
    text_parts <- c(text_parts, paste0("Category: ", row_data$category))
  if ("income_level" %in% names(row_data)) 
    text_parts <- c(text_parts, paste0("Income Level: ", row_data$income_level))
  if ("Region" %in% names(row_data)) 
    text_parts <- c(text_parts, paste0("Region: ", row_data$Region))
  if ("is_africa" %in% names(row_data)) 
    text_parts <- c(text_parts, paste0("Is Africa: ", row_data$is_africa))
  if ("wealth_quintile" %in% names(row_data)) 
    text_parts <- c(text_parts, paste0("Wealth Quintile: ", row_data$wealth_quintile))
  
  text_parts <- c(text_parts, paste0("Grade: ", row_data$grade_numeric))
  text_parts <- c(text_parts, paste0("Proficiency LOESS: ", round(y_value, 2), "%"))
  
  if ("n" %in% names(row_data)) 
    text_parts <- c(text_parts, paste0("N: ", row_data$n))
  if ("n_readskill" %in% names(row_data)) 
    text_parts <- c(text_parts, paste0("N Read Skill: ", row_data$n_readskill))
  if ("n_numbskill" %in% names(row_data)) 
    text_parts <- c(text_parts, paste0("N Numb Skill: ", row_data$n_numbskill))
  
  return(paste0(text_parts, collapse = "<br>"))
}

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {
  
  # Reactive data based on filters
  filtered_data <- reactive({
    df <- data
    
    # Keep only the latest year for each country to avoid mixing years
    if ("year" %in% colnames(df)) {
      df <- df %>%
        group_by(iso3) %>%
        filter(year == max(year, na.rm = TRUE)) %>%
        ungroup()
    }
    
    # Category filter: always applies (no "Show All" option)
    if (!is.null(input$filter_category)) {
      df <- df %>% filter(category == input$filter_category)
    }
    
    # Subject filter: always applies (no "Show All" option)
    if (!is.null(input$filter_subject)) {
      df <- df %>% filter(subject == input$filter_subject)
    }
    
    # Income level filter: multi-select (filter if any are selected)
    if (!is.null(input$filter_income) && length(input$filter_income) > 0) {
      df <- df %>% filter(income_level %in% input$filter_income)
    }
    
    # Region filter: multi-select (filter if any are selected)
    if (!is.null(input$filter_region) && length(input$filter_region) > 0) {
      df <- df %>% filter(Region %in% input$filter_region)
    }
    
    # Wealth quintile filter: only apply if not "Show All"
    if (!is.null(input$filter_wealth) && input$filter_wealth != "<Show All>") {
      df <- df %>% filter(wealth_quintile == input$filter_wealth)
    }
    
    # Is Africa filter: only apply if not "Show All"
    if (!is.null(input$filter_africa) && input$filter_africa != "<Show All>") {
      df <- df %>% filter(is_africa == input$filter_africa)
    }
    
    # Filter out rows with missing proficiency values
    df <- df %>% filter(!is.na(proficiency_loess_span1))
    
    # Remove rows with missing grade
    df <- df %>% filter(!is.na(grade_numeric))
    
    # Track countries before minimum observations filter
    countries_before <- unique(df$iso3)
    rows_before_n_filter <- nrow(df)
    
    # Track countries with their grade count before filter
    country_grades_before <- df %>%
      group_by(iso3) %>%
      summarise(grades_before = n_distinct(grade_numeric), .groups = "drop")
    
    # Apply minimum observations filter
    if (!is.null(input$min_observations) && input$min_observations > 0) {
      if ("n" %in% colnames(df)) {
        df <- df %>% filter(n > input$min_observations)
      }
    }
    
    # Track after minimum observations filter (BEFORE tolerance filter)
    countries_after_min_obs <- unique(df$iso3)
    rows_after_n_filter <- nrow(df)
    
    # Track countries with their grade count after minimum observations filter (BEFORE tolerance)
    country_grades_after_min_obs <- df %>%
      group_by(iso3) %>%
      summarise(grades_after_min_obs = n_distinct(grade_numeric), .groups = "drop")

    # Find countries with missing grades (had data before, lost some or all grades after min_obs filter)
    # This includes both partially and completely excluded countries
    # Calculate this BEFORE the tolerance filter to track all affected countries
    countries_with_missing_grades_before_tolerance <- country_grades_before %>%
      left_join(country_grades_after_min_obs, by = "iso3") %>%
      mutate(grades_after_min_obs = ifelse(is.na(grades_after_min_obs), 0, grades_after_min_obs)) %>%
      filter(grades_after_min_obs < grades_before)  # Includes both partial and complete exclusions

    # Apply missing grades tolerance filter
    countries_dropped_by_tolerance <- c()
    if (!is.null(input$missing_grades_tolerance)) {
      # Calculate expected grades (1-8 = 8 grades total)
      expected_grades <- 8
      max_missing_allowed <- input$missing_grades_tolerance - 1
      min_required_grades <- expected_grades - max_missing_allowed

      # Determine which countries meet the tolerance requirement
      # Check all countries that had data before min_obs filter
      # For countries still in df, use their actual grade count
      # For countries not in df (lost all grades), they have 0 grades
      all_countries_to_check <- unique(c(
        countries_after_min_obs,
        countries_with_missing_grades_before_tolerance$iso3
      ))
      
      # Calculate actual grades for each country
      country_actual_grades <- country_grades_after_min_obs %>%
        right_join(
          data.frame(iso3 = all_countries_to_check),
          by = "iso3"
        ) %>%
        mutate(
          actual_grades = ifelse(is.na(grades_after_min_obs), 0, grades_after_min_obs)
        )
      
      # Countries with sufficient grades
      countries_with_sufficient_grades <- country_actual_grades %>%
        filter(actual_grades >= min_required_grades) %>%
        pull(iso3)

      # Track which countries were dropped by the tolerance filter
      countries_dropped_by_tolerance <- all_countries_to_check[
        !all_countries_to_check %in% countries_with_sufficient_grades
      ]

      df <- df %>% filter(iso3 %in% countries_with_sufficient_grades)
    }

    # Update after missing grades filter
    countries_after <- unique(df$iso3)
    rows_after_n_filter <- nrow(df)

    # Recalculate grade counts after missing grades filter (for final data)
    country_grades_after <- df %>%
      group_by(iso3) %>%
      summarise(grades_after = n_distinct(grade_numeric), .groups = "drop")
    
    # Add dropped flag to missing grades detail
    countries_with_missing_grades <- countries_with_missing_grades_before_tolerance %>%
      mutate(
        dropped = iso3 %in% countries_dropped_by_tolerance,
        missing = grades_before - grades_after_min_obs
      ) %>%
      arrange(desc(missing), iso3)  # Order by missing grades descending
    
    countries_missing_categories <- nrow(countries_with_missing_grades)
    countries_dropped_count <- length(countries_dropped_by_tolerance)
    
    excluded_combinations <- rows_before_n_filter - rows_after_n_filter
    
    # Calculate medians for benchmarks using data BEFORE category filter but AFTER other filters
    # Start fresh from data and apply same filters as above (year, subject, income, region, africa, min_obs)
    df_median_base <- data

    # Apply same year filter
    if ("year" %in% colnames(df_median_base)) {
      df_median_base <- df_median_base %>%
        group_by(iso3) %>%
        filter(year == max(year, na.rm = TRUE)) %>%
        ungroup()
    }

    # Apply subject filter (same as main data)
    if (!is.null(input$filter_subject)) {
      df_median_base <- df_median_base %>% filter(subject == input$filter_subject)
    }

    # Apply income level filter
    if (!is.null(input$filter_income) && length(input$filter_income) > 0) {
      df_median_base <- df_median_base %>% filter(income_level %in% input$filter_income)
    }

    # Apply region filter
    if (!is.null(input$filter_region) && length(input$filter_region) > 0) {
      df_median_base <- df_median_base %>% filter(Region %in% input$filter_region)
    }

    # Apply is_africa filter
    if (!is.null(input$filter_africa) && input$filter_africa != "<Show All>") {
      df_median_base <- df_median_base %>% filter(is_africa == input$filter_africa)
    }

    # Apply minimum observations filter
    if (!is.null(input$min_observations) && input$min_observations > 0) {
      if ("n" %in% colnames(df_median_base)) {
        df_median_base <- df_median_base %>% filter(n > input$min_observations)
      }
    }

    # NOW filter for category == "All" for median calculation (all countries have region data)
    df_median_base <- df_median_base %>%
      filter(
        category == "All",
        !is.na(proficiency_loess_span1),
        !is.na(grade_numeric)
      ) %>%
      mutate(
        region_group = ifelse(is_africa,
                             "MICS6 Africa countries",
                             "MICS6 Rest of the world countries")
      )

    medians_summary <- df_median_base %>%
      filter(grade_numeric >= 1, grade_numeric <= 8) %>%
      group_by(region_group) %>%
      summarise(
        n_countries = n_distinct(iso3),
        grade_range = paste(min(grade_numeric, na.rm = TRUE), "-", max(grade_numeric, na.rm = TRUE)),
        .groups = "drop"
      )

    # Store excluded counts as attributes
    attr(df, "excluded_combinations") <- excluded_combinations
    attr(df, "countries_dropped") <- countries_dropped_count
    attr(df, "countries_missing_categories") <- countries_missing_categories
    attr(df, "missing_grades_detail") <- countries_with_missing_grades
    attr(df, "medians_summary") <- medians_summary

    return(df)
  })
  
  # Create the line chart
  output$line_chart <- renderPlotly({

    plot_data <- filtered_data()

    # Validate data
    req(nrow(plot_data) > 0)

    # Calculate medians for regional benchmarks using ORIGINAL data with same filters
    # but forcing category == "All" for median calculation
    df_median_base <- data

    # Apply same year filter
    if ("year" %in% colnames(df_median_base)) {
      df_median_base <- df_median_base %>%
        group_by(iso3) %>%
        filter(year == max(year, na.rm = TRUE)) %>%
        ungroup()
    }

    # Apply subject filter (same as main data)
    if (!is.null(input$filter_subject)) {
      df_median_base <- df_median_base %>% filter(subject == input$filter_subject)
    }

    # Apply income level filter
    if (!is.null(input$filter_income) && length(input$filter_income) > 0) {
      df_median_base <- df_median_base %>% filter(income_level %in% input$filter_income)
    }

    # Apply region filter
    if (!is.null(input$filter_region) && length(input$filter_region) > 0) {
      df_median_base <- df_median_base %>% filter(Region %in% input$filter_region)
    }

    # Apply is_africa filter
    if (!is.null(input$filter_africa) && input$filter_africa != "<Show All>") {
      df_median_base <- df_median_base %>% filter(is_africa == input$filter_africa)
    }

    # Apply minimum observations filter
    if (!is.null(input$min_observations) && input$min_observations > 0) {
      if ("n" %in% colnames(df_median_base)) {
        df_median_base <- df_median_base %>% filter(n > input$min_observations)
      }
    }

    # NOW filter for median calculation: category == "All" (all countries have region data)
    df_median_base <- df_median_base %>%
      filter(
        category == "All",
        !is.na(Region),
        !is.na(proficiency_loess_span1),
        !is.na(grade_numeric)
      ) %>%
      mutate(
        region_group = ifelse(is_africa,
                             "MICS6 Africa countries",
                             "MICS6 Rest of the world countries")
      )

    # Calculate medians by region and grade
    medians <- df_median_base %>%
      group_by(region_group, grade_numeric) %>%
      summarise(
        median_prof = median(proficiency_loess_span1, na.rm = TRUE),
        n_countries = n_distinct(iso3),
        .groups = "drop"
      ) %>%
      filter(grade_numeric >= 1, grade_numeric <= 8)

    # Create plot
    fig <- plot_ly()
    
    # Get unique countries
    unique_countries <- unique(plot_data$iso3)

    # Add a trace for each country with reduced opacity
    for (i in seq_along(unique_countries)) {
      country <- unique_countries[i]
      country_data <- plot_data %>%
        filter(iso3 == country) %>%
        arrange(grade_numeric)

      # Get Y values
      y_values <- country_data$proficiency_loess_span1

      # Create hover text for each point
      hover_texts <- sapply(1:nrow(country_data), function(j) {
        create_hover_text(country_data[j, ], y_values[j])
      })

      fig <- fig %>%
        add_trace(
          data = country_data,
          x = ~grade_numeric,
          y = y_values,
          type = 'scatter',
          mode = 'lines+markers',
          name = country,
          text = hover_texts,
          hoverinfo = 'text',
          line = list(width = 2.5, shape = 'spline', color = 'rgba(150, 150, 150, 0.6)'),
          marker = list(size = 4, opacity = 0.6),
          opacity = 0.6,
          showlegend = TRUE,
          legendrank = 10 + i  # Higher rank so countries appear after medians
        )
    }

    # Add median lines with labels FIRST (so they appear at top of legend)
    if (nrow(medians) > 0) {
      # Africa median line
      africa_medians <- medians %>% filter(region_group == "MICS6 Africa countries")
      if (nrow(africa_medians) > 0) {
        fig <- fig %>%
          add_trace(
            data = africa_medians,
            x = ~grade_numeric,
            y = ~median_prof,
            type = 'scatter',
            mode = 'lines+markers+text',
            name = "<b>Africa</b>",
            line = list(width = 4, color = '#0066CC', shape = 'spline'),
            marker = list(size = 8, color = '#0066CC'),
            text = paste0(round(africa_medians$median_prof, 1), "%"),
            textposition = "bottom right",
            textfont = list(size = 10, color = '#0066CC', family = 'Arial, sans-serif'),
            hovertemplate = paste0(
              "<b>Africa</b><br>",
              "Grade: %{x}<br>",
              "Median Proficiency: %{y:.2f}%<br>",
              "<extra></extra>"
            ),
            showlegend = TRUE,
            legendrank = 1
          )
      }

      # Non-Africa median line
      non_africa_medians <- medians %>% filter(region_group == "MICS6 Rest of the world countries")
      if (nrow(non_africa_medians) > 0) {
        fig <- fig %>%
          add_trace(
            data = non_africa_medians,
            x = ~grade_numeric,
            y = ~median_prof,
            type = 'scatter',
            mode = 'lines+markers+text',
            name = "<b>Non-Africa</b>",
            line = list(width = 4, color = '#000000', shape = 'spline'),
            marker = list(size = 8, color = '#000000'),
            text = paste0(round(non_africa_medians$median_prof, 1), "%"),
            textposition = "bottom right",
            textfont = list(size = 10, color = '#000000', family = 'Arial, sans-serif'),
            hovertemplate = paste0(
              "<b>Non-Africa</b><br>",
              "Grade: %{x}<br>",
              "Median Proficiency: %{y:.2f}%<br>",
              "<extra></extra>"
            ),
            showlegend = TRUE,
            legendrank = 2
          )
      }
    }
    
    # Update layout
    fig <- fig %>%
      layout(
        title = list(
          text = "<b>Learning Gradient Analysis - LOESS Smoothed</b><br><sub>Proficiency by Grade</sub>",
          x = 0.5,
          xanchor = "center"
        ),
        xaxis = list(
          title = "<b>Grade (1-8)</b>",
          range = c(0.5, 8.5),
          dtick = 1,
          gridcolor = "#E5E5E5"
        ),
        yaxis = list(
          title = "<b>Proficiency LOESS (%)</b>",
          range = c(0, 100),
          gridcolor = "#E5E5E5"
        ),
        hovermode = 'closest',
        legend = list(
          title = list(text = "<b>Countries</b>"),
          orientation = "v",
          x = 1.02,
          y = 1,
          bgcolor = "rgba(255, 255, 255, 0.8)",
          bordercolor = "#CCCCCC",
          borderwidth = 1
        ),
        margin = list(r = 250, t = 80, l = 80, b = 80),
        plot_bgcolor = "#F8F8F8",
        paper_bgcolor = "white",
        autosize = FALSE,
        width = 900,
        height = 900
      ) %>%
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        staticPlot = FALSE,
        responsive = FALSE
      )
    
    return(fig)
  })
  
  # Data summary output
  output$data_summary <- renderText({
    df <- filtered_data()
    excluded_combinations <- attr(df, "excluded_combinations")
    countries_dropped <- attr(df, "countries_dropped")
    countries_missing_cats <- attr(df, "countries_missing_categories")
    medians_summary <- attr(df, "medians_summary")

    # Get year information
    year_info <- ""
    if ("year" %in% colnames(df)) {
      years_used <- sort(unique(df$year))
      if (length(years_used) == 1) {
        year_info <- paste0("- Year: ", years_used, "\n")
      } else {
        year_info <- paste0("- Years: ", paste(years_used, collapse = ", "),
                           " (latest per country)\n")
      }
    }

      # Median information
    median_info <- ""
    if (!is.null(medians_summary) && nrow(medians_summary) > 0) {
      median_lines <- apply(medians_summary, 1, function(row) {
        paste0("  • ", row["region_group"], ": ", row["n_countries"],
               " countries (grades ", row["grade_range"], ")")
      })
      median_info <- paste0("\nRegional Benchmarks (category='All'):\n",
                           paste(median_lines, collapse = "\n"), "\n")
    }

    # Calculate missing grades info
    max_missing <- input$missing_grades_tolerance - 1
    min_required <- 8 - max_missing

    summary_text <- paste0(
      "Filtered Data:\n",
      "- Total observations: ", nrow(df), "\n",
      "- Excluded combinations (n ≤ ", input$min_observations, "): ", excluded_combinations, "\n",
      "- Countries with missing grades: ", countries_missing_cats, "\n",
      "- Countries dropped: ", countries_dropped, "\n",
      "- Missing grades tolerance: ", input$missing_grades_tolerance, " (countries need ≥ ", min_required, " grades)\n",
      "- Unique countries: ", length(unique(df$iso3)), "\n",
      "- Grade range: ", min(df$grade_numeric, na.rm = TRUE), " - ",
                         max(df$grade_numeric, na.rm = TRUE), "\n",
      year_info,
      median_info
    )

    return(summary_text)
  })
  
  # Chart information output
  output$chart_info <- renderUI({
    HTML(
      "<p><strong>How to use this chart:</strong></p>
      <ul>
        <li><strong>Smoothed Data:</strong> This dataset shows raw proficiency rates alongside LOESS-smoothed values for clearer trend visualization</li>
        <li><strong>Regional Benchmarks:</strong> Two bold lines show median proficiency across countries by grade and region:
          <ul>
            <li><strong>Africa</strong> (blue): Median proficiency across African countries by grade</li>
            <li><strong>Non-Africa</strong> (black): Median proficiency across non-African countries by grade</li>
            <li>Grade markers show median proficiency for that specific grade</li>
          </ul>
        </li>
        <li><strong>Filters:</strong> Use the sidebar filters to focus on specific subsets of data:
          <ul>
            <li><em>Category</em>: Defaults to 'All' (aggregated across all student categories)</li>
            <li><em>Subject</em>: Defaults to 'reading' (switch to 'numeracy' to see math proficiency)</li>
            <li><em>Income Level</em>: Multi-select dropdown - select one or more income levels to compare</li>
            <li><em>Region</em>: Multi-select dropdown - select one or more regions to compare</li>
            <li><em>Minimum Observations</em>: Exclude country-grade combinations with small sample sizes (default: 50)</li>
            <li><em>Missing Grades Tolerance</em>: Maximum missing grades allowed per country (1 = complete data with all 8 grades, 8 = all countries included, default: 8)</li>
          </ul>
        </li>
        <li><strong>Hover:</strong> Hover over data points to see comprehensive information including grade and proficiency values</li>
        <li><strong>Legend:</strong> Click country names to show/hide specific lines. Double-click to isolate a single country.</li>
        <li><strong>Zoom:</strong> Click and drag to zoom into specific areas; double-click the chart to reset zoom</li>
        <li><strong>Export:</strong> Use the toolbar (top-right) to download the chart as PNG</li>
      </ul>
      <p><strong>Data Quality:</strong></p>
      <ul>
        <li>Only the latest year of data is used for each country (no year mixing)</li>
        <li>Minimum observations filter ensures reliable estimates by excluding country-grade combinations with too few data points</li>
        <li>Data summary shows excluded combinations and countries with missing grades</li>
        <li>The 'n' column indicates sample size for each data point</li>
      </ul>
      <p><strong>Data Structure:</strong></p>
      <ul>
        <li>Individual country lines (grey, opacity 0.6) represent each country's learning gradient</li>
        <li>Bold lines show median proficiency across countries for each grade, grouped by Africa vs. non-Africa</li>
        <li>X-axis shows grades from 1 to 8 (discrete grade levels)</li>
        <li>Y-axis shows LOESS smoothed proficiency values as percentages (0-100%)</li>
        <li>LOESS smoothing (span=1) applied to raw proficiency rates for each country-category-subject combination</li>
        <li>All filters work in real-time</li>
        <li>Category 'All' is an actual data category (includes all student categories combined)</li>
      </ul>
      <p><strong>Tips:</strong></p>
      <ul>
        <li>Default view shows: category='All', subject='reading', all income levels and regions</li>
        <li>Use Income Level and Region checkboxes to compare specific groups side-by-side</li>
        <li>Switch between 'reading' and 'numeracy' to compare literacy vs mathematics proficiency</li>
        <li>Adjust minimum observations to balance data quality vs coverage - higher values = more reliable but fewer countries</li>
        <li>Use missing grades tolerance to control data completeness - lower values = more complete datasets, higher values = more countries included</li>
        <li>Spline interpolation provides smooth visual lines between data points</li>
        <li>The median lines provide regional benchmarks for comparison</li>
        <li>Click 'Reset All Filters' to return to default view</li>
      </ul>"
    )
  })
  
  # Missing countries list output
  output$missing_countries_list <- renderUI({
    df <- filtered_data()
    missing_detail <- attr(df, "missing_grades_detail")
    
    if (is.null(missing_detail) || nrow(missing_detail) == 0) {
      return(HTML("<p style='color: green;'><strong>No countries with missing grades!</strong></p>
                   <p>All countries have complete data across all grades.</p>"))
    }
    
    # Calculate percentage missing
    missing_with_context <- missing_detail %>%
      mutate(
        pct_missing = round((missing / grades_before) * 100, 1)
      )
    
    # Count dropped countries
    dropped_count <- sum(missing_with_context$dropped, na.rm = TRUE)
    affected_count <- nrow(missing_with_context)
    
    # Generate HTML table rows with light red background for dropped countries
    table_rows <- lapply(1:nrow(missing_with_context), function(i) {
      row <- missing_with_context[i, ]
      bg_color <- if (row$dropped) "background-color: #ffcccc;" else ""
      paste0(
        "<tr style='", bg_color, "'>",
        "<td style='padding: 5px; border-bottom: 1px solid #ddd;'><strong>", row$iso3, "</strong></td>",
        "<td style='padding: 5px; border-bottom: 1px solid #ddd; text-align: center;'><strong>", row$missing, "/", row$grades_before, "</strong></td>",
        "<td style='padding: 5px; border-bottom: 1px solid #ddd; text-align: center;'><strong>", row$pct_missing, "%</strong></td>",
        "</tr>"
      )
    })
    
    HTML(paste0(
      "<p style='color: #d9534f; margin-bottom: 10px;'><strong>", affected_count,
      " countries affected | ", dropped_count, " dropped</strong></p>",
      "<table style='width: 100%; font-size: 0.9em; border-collapse: collapse;'>",
      "<thead>",
      "<tr style='background-color: #f5f5f5;'>",
      "<th style='padding: 5px; text-align: left; border-bottom: 2px solid #ddd;'>Country</th>",
      "<th style='padding: 5px; text-align: center; border-bottom: 2px solid #ddd;'>Missing</th>",
      "<th style='padding: 5px; text-align: center; border-bottom: 2px solid #ddd;'>%</th>",
      "</tr>",
      "</thead>",
      "<tbody>",
      paste(table_rows, collapse = ""),
      "</tbody>",
      "</table>",
      "<p style='font-size: 0.85em; color: #666; margin-top: 10px;'><em>Light red = dropped by tolerance filter</em></p>"
    ))
  })
  
  # Reset filters button
  observeEvent(input$reset_filters, {
    # Reset category to "All"
    if (!is.null(input$filter_category)) {
      updateSelectInput(session, "filter_category", selected = "All")
    }
    
    # Reset subject to "reading"
    if (!is.null(input$filter_subject)) {
      updateSelectInput(session, "filter_subject", selected = "reading")
    }
    
    # Reset minimum observations to 50
    updateSliderInput(session, "min_observations", value = 50)

    # Reset missing grades tolerance to 8
    updateSliderInput(session, "missing_grades_tolerance", value = 8)

    # Reset income level to all selected
    if (!is.null(input$filter_income)) {
      updateSelectInput(session, "filter_income", selected = filter_columns$income_level)
    }

    # Reset region to all selected
    if (!is.null(input$filter_region)) {
      updateSelectInput(session, "filter_region", selected = filter_columns$Region)
    }
    
    # Reset wealth quintile to "Show All"
    if (!is.null(input$filter_wealth)) {
      updateSelectInput(session, "filter_wealth", selected = "<Show All>")
    }
    
    # Reset is_africa to "Show All"
    if (!is.null(input$filter_africa)) {
      updateSelectInput(session, "filter_africa", selected = "<Show All>")
    }
  })
}

# ============================================================================
# RUN THE APP
# ============================================================================

shinyApp(ui = ui, server = server)



