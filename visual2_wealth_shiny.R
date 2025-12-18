# Visual 2 Shiny App: Wealth Gradients in Foundational Skills
# Interactive visualization with faceting by Africa/Non-Africa/All

library(shiny)
library(plotly)
library(dplyr)
library(readr)

# ============================================================================
# DATA LOADING
# ============================================================================

data_file <- "data/learning_gradient_bands_raw.csv"
data <- read_csv(data_file, show_col_types = FALSE)

# Valid grade bands
valid_grade_bands <- c("Primary (1-3)", "Upper Primary (4-6)", "Lower Secondary (7-9)")

# Wealth categories
wealth_categories <- c("All", "Poorest", "Second", "Middle", "Fourth", "Richest")

# Wealth colors
wealth_colors <- list(
  "All" = "#2E86DE", "Poorest" = "#8B0000", "Second" = "#FF8C00",
  "Middle" = "#808080", "Fourth" = "#90EE90", "Richest" = "#006400"
)

# Prepare filter options
filter_columns <- list()

if ("subject" %in% colnames(data)) {
  filter_columns$subject <- sort(unique(data$subject))
}
if ("income_level" %in% colnames(data)) {
  filter_columns$income_level <- sort(unique(data$income_level))
}
if ("Region" %in% colnames(data)) {
  filter_columns$Region <- sort(unique(data$Region))
}
if ("year" %in% colnames(data)) {
  filter_columns$year <- c("<Show All>", sort(unique(data$year)))
}

# Wealth categories for filtering
filter_columns$wealth_categories <- wealth_categories

# ============================================================================
# UI DEFINITION
# ============================================================================

ui <- fluidPage(
  titlePanel("Visual 2: Wealth Gradients - Foundational Skills Across Education Stages"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("Settings"),
      
      # Subject selection
      selectInput("subject",
                  "Subject:",
                  choices = filter_columns$subject,
                  selected = "reading"),
      
      # Aggregation method
      selectInput("agg_method",
                  "Aggregation Method:",
                  choices = c("Mean" = "mean", "Median" = "median"),
                  selected = "mean"),
      
      hr(),
      
      h4("Filters"),
      
      # Wealth groups selection
      checkboxGroupInput("filter_wealth",
                        "Wealth Groups:",
                        choices = filter_columns$wealth_categories,
                        selected = filter_columns$wealth_categories),
      
      # Income Level: multi-select
      if ("income_level" %in% names(filter_columns)) {
        checkboxGroupInput("filter_income",
                          "Income Level:",
                          choices = filter_columns$income_level,
                          selected = filter_columns$income_level)
      },
      
      # Region: multi-select
      if ("Region" %in% names(filter_columns)) {
        checkboxGroupInput("filter_region",
                          "Region:",
                          choices = filter_columns$Region,
                          selected = filter_columns$Region)
      },
      
      # Year filter
      if ("year" %in% names(filter_columns)) {
        selectInput("filter_year",
                    "Year:",
                    choices = filter_columns$year,
                    selected = "<Show All>")
      },
      
      # Minimum observations filter
      numericInput("min_observations",
                   "Minimum Observations (n):",
                   value = 50,
                   min = 0,
                   step = 10),
      
      helpText("Excludes country-grade combinations with fewer observations than this threshold."),
      
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
          # Plot output - wider for three facets
          plotlyOutput("main_chart", height = "400px", width = "100%")
        ),
        column(
          width = 4,
          # Missing countries list
          wellPanel(
            h4("Countries with Missing Grade Bands"),
            htmlOutput("missing_countries_list"),
            style = "height: 400px; overflow-y: auto;"
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
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    df <- data %>%
      filter(grade_band %in% valid_grade_bands) %>%
      filter(subject == input$subject) %>%
      filter(!is.na(proficiency_rate))
    
    # Keep only the latest year for each country to avoid mixing years
    if ("year" %in% colnames(df)) {
      df <- df %>%
        group_by(iso3) %>%
        filter(year == max(year, na.rm = TRUE)) %>%
        ungroup()
    }
    
    # Apply wealth groups filter
    if (!is.null(input$filter_wealth) && length(input$filter_wealth) > 0) {
      df <- df %>% filter(category %in% input$filter_wealth)
    } else {
      df <- df %>% filter(category %in% wealth_categories)
    }
    
    # Apply income level filter
    if (!is.null(input$filter_income) && length(input$filter_income) > 0) {
      df <- df %>% filter(income_level %in% input$filter_income)
    }
    
    # Apply region filter
    if (!is.null(input$filter_region) && length(input$filter_region) > 0) {
      df <- df %>% filter(Region %in% input$filter_region)
    }
    
    # Apply year filter
    if (!is.null(input$filter_year) && input$filter_year != "<Show All>") {
      df <- df %>% filter(year == as.numeric(input$filter_year))
    }
    
    # Count before minimum observations filter (AFTER all other filters)
    rows_before_n_filter <- nrow(df)
    countries_before <- unique(df$iso3)
    
    # Track country-wealth combinations with their grade bands before minimum observations filter
    # This now only considers the filtered wealth groups
    # We group by BOTH iso3 and category (wealth group) because each wealth group is a separate line
    country_wealth_grades_before <- df %>%
      group_by(iso3, category) %>%
      summarise(grades_before = n_distinct(grade_band), .groups = "drop")
    
    # Apply minimum observations filter
    if (!is.null(input$min_observations) && input$min_observations > 0) {
      df <- df %>% filter(n > input$min_observations)
    }
    
    # Count after minimum observations filter
    rows_after_n_filter <- nrow(df)
    excluded_combinations <- rows_before_n_filter - rows_after_n_filter
    countries_after <- unique(df$iso3)
    
    # Track country-wealth combinations with their grade bands after minimum observations filter
    country_wealth_grades_after <- df %>%
      group_by(iso3, category) %>%
      summarise(grades_after = n_distinct(grade_band), .groups = "drop")
    
    # Find country-wealth combinations with missing grade bands
    # (had data before, lost some or all grades after)
    # Only within the filtered wealth groups
    country_wealth_missing_grades <- country_wealth_grades_before %>%
      left_join(country_wealth_grades_after, by = c("iso3", "category")) %>%
      mutate(grades_after = ifelse(is.na(grades_after), 0, grades_after)) %>%
      filter(grades_after < grades_before)
    
    # Count unique COUNTRIES (not country-wealth combinations) with missing grade bands
    # This is clearer: how many countries have at least one wealth group with missing grade bands
    countries_missing_categories <- n_distinct(country_wealth_missing_grades$iso3)
    
    # Create ordered factors
    df <- df %>%
      mutate(
        grade_band_ordered = factor(grade_band, levels = valid_grade_bands),
        category_ordered = factor(category, levels = wealth_categories)
      )
    
    # Store excluded counts as attributes
    attr(df, "excluded_combinations") <- excluded_combinations
    attr(df, "countries_missing_categories") <- countries_missing_categories
    attr(df, "missing_grades_detail") <- country_wealth_missing_grades
    
    return(df)
  })
  
  # Create visualization with facets
  output$main_chart <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    # Create three datasets: All, Africa, Non-Africa
    datasets <- list(
      All = df,
      Africa = df %>% filter(is_africa == TRUE),
      `Non-Africa` = df %>% filter(is_africa == FALSE)
    )
    
    # Aggregate data for each facet
    aggregated_data <- lapply(names(datasets), function(facet_name) {
      facet_df <- datasets[[facet_name]]
      
      if (nrow(facet_df) == 0) return(NULL)
      
      agg_df <- facet_df %>%
        group_by(category_ordered, grade_band_ordered) %>%
        summarise(
          proficiency_agg = if(input$agg_method == "mean") {
            mean(proficiency_rate, na.rm = TRUE)
          } else {
            median(proficiency_rate, na.rm = TRUE)
          },
          n_systems = n_distinct(iso3),
          total_children = sum(n, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(facet = facet_name)
      
      return(agg_df)
    })
    
    # Combine all facets
    all_data <- bind_rows(aggregated_data)
    
    if (nrow(all_data) == 0) {
      return(plot_ly() %>% layout(title = "No data available for selected filters"))
    }
    
    # Create subplot with three facets
    fig <- plot_ly()
    
    # Add traces for each facet
    for (facet_name in c("All", "Africa", "Non-Africa")) {
      facet_data <- all_data %>% filter(facet == facet_name)
      
      if (nrow(facet_data) == 0) next
      
      # Get unique wealth categories in this facet
      unique_wealth <- unique(facet_data$category_ordered)
      
      for (wealth in unique_wealth) {
        w_data <- facet_data %>%
          filter(category_ordered == wealth) %>%
          arrange(grade_band_ordered)
        
        if (nrow(w_data) == 0) next
        
        line_width <- ifelse(wealth == "All", 3.5, 2)
        
        hover_text <- paste0(
          "<b>", facet_name, " - ", wealth, "</b><br>",
          "Grade: ", w_data$grade_band_ordered, "<br>",
          "Proficiency: ", round(w_data$proficiency_agg, 1), "%<br>",
          "N systems: ", w_data$n_systems, "<br>",
          "Total children: ", format(w_data$total_children, big.mark = ",")
        )
        
        # Determine if this trace should show in legend (only show once per wealth group)
        show_legend <- (facet_name == "All")
        legend_name <- as.character(wealth)
        
        fig <- fig %>%
          add_trace(
            data = w_data,
            x = ~grade_band_ordered,
            y = ~proficiency_agg,
            type = 'scatter',
            mode = 'lines+markers',
            name = legend_name,
            legendgroup = legend_name,
            showlegend = show_legend,
            text = hover_text,
            hoverinfo = 'text',
            line = list(width = line_width, color = wealth_colors[[as.character(wealth)]]),
            marker = list(size = 6, color = wealth_colors[[as.character(wealth)]]),
            xaxis = if(facet_name == "All") "x" else if(facet_name == "Africa") "x2" else "x3",
            yaxis = if(facet_name == "All") "y" else if(facet_name == "Africa") "y2" else "y3"
          )
      }
    }
    
    # Layout with three subplots in one row
    agg_label <- tools::toTitleCase(input$agg_method)
    
    fig <- fig %>%
      layout(
        title = list(
          text = paste0("<b>Wealth Gradients</b><br><sub>",
                       tools::toTitleCase(input$subject), " - ",
                       agg_label, " across systems</sub>"),
          x = 0.5, xanchor = "center"
        ),
        # All systems facet
        xaxis = list(
          domain = c(0, 0.3),
          title = list(text = "<b>All</b>", standoff = 10),
          type = "category",
          categoryorder = "array",
          categoryarray = valid_grade_bands,
          tickfont = list(size = 9)
        ),
        yaxis = list(
          title = "<b>% with foundational skills</b>",
          range = c(0, 100),
          gridcolor = "#E5E5E5"
        ),
        # Africa facet
        xaxis2 = list(
          domain = c(0.35, 0.65),
          title = list(text = "<b>Africa</b>", standoff = 10),
          type = "category",
          categoryorder = "array",
          categoryarray = valid_grade_bands,
          tickfont = list(size = 9)
        ),
        yaxis2 = list(
          range = c(0, 100),
          showticklabels = FALSE,
          gridcolor = "#E5E5E5"
        ),
        # Non-Africa facet
        xaxis3 = list(
          domain = c(0.7, 1),
          title = list(text = "<b>Non-Africa</b>", standoff = 10),
          type = "category",
          categoryorder = "array",
          categoryarray = valid_grade_bands,
          tickfont = list(size = 9)
        ),
        yaxis3 = list(
          range = c(0, 100),
          showticklabels = FALSE,
          gridcolor = "#E5E5E5"
        ),
        hovermode = 'closest',
        legend = list(
          title = list(text = "<b>Wealth Group</b>"),
          orientation = "v",
          x = 1.02,
          y = 1,
          bgcolor = "rgba(255, 255, 255, 0.9)",
          bordercolor = "#CCCCCC",
          borderwidth = 1
        ),
        margin = list(r = 180, t = 100, l = 80, b = 80),
        plot_bgcolor = "#F8F8F8",
        paper_bgcolor = "white",
        showlegend = TRUE
      )
    
    return(fig)
  })
  
  # Data summary
  output$data_summary <- renderText({
    df <- filtered_data()
    excluded_count <- attr(df, "excluded_combinations")
    countries_missing_cats <- attr(df, "countries_missing_categories")
    
    all_systems <- length(unique(df$iso3))
    africa_systems <- length(unique(df$iso3[df$is_africa == TRUE]))
    non_africa_systems <- length(unique(df$iso3[df$is_africa == FALSE]))
    
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
    
    summary_text <- paste0(
      "Filtered Data:\n",
      "- Total country-grade combinations: ", nrow(df), "\n",
      "- Excluded combinations (n ≤ ", input$min_observations, "): ", excluded_count, "\n",
      "- Countries with missing grade bands: ", countries_missing_cats, "\n",
      "- All systems: ", all_systems, "\n",
      "- Africa systems: ", africa_systems, "\n",
      "- Non-Africa systems: ", non_africa_systems, "\n",
      "- Wealth groups: ", length(unique(df$category)), "\n",
      year_info
    )
    
    return(summary_text)
  })
  
  # Chart information
  output$chart_info <- renderUI({
    HTML(
      "<p><strong>Visual 2: Wealth Gradients</strong></p>
      <ul>
        <li><strong>Purpose:</strong> Show inequality in foundational skills by household wealth</li>
        <li><strong>Facets:</strong> Three panels showing All systems, Africa only, and Non-Africa only</li>
        <li><strong>Blue thick line:</strong> All children (aggregated)</li>
        <li><strong>Red to green gradient:</strong> Poorest → Richest quintiles</li>
      </ul>
      <p><strong>Interpretation:</strong></p>
      <ul>
        <li>Only the latest year of data is used for each country (no year mixing)</li>
        <li>Aggregation method (mean/median) across systems can be selected in filters</li>
        <li>Mean (default) gives arithmetic average; each country contributes equally</li>
        <li>Median is more robust to outliers</li>
        <li>Larger gaps between lines indicate greater inequality</li>
        <li>Compare patterns across All/Africa/Non-Africa to see regional differences</li>
      </ul>
      <p><strong>How to use filters:</strong></p>
      <ul>
        <li><strong>Subject:</strong> Switch between reading and numeracy</li>
        <li><strong>Aggregation Method:</strong> Choose mean (default) or median</li>
        <li><strong>Minimum Observations:</strong> Exclude country-grade combinations with small sample sizes (default: 50)</li>
        <li><strong>Wealth Groups:</strong> Select which wealth quintiles to display</li>
        <li><strong>Income Level & Region:</strong> Multi-select to focus on specific contexts</li>
        <li><strong>Reset All Filters:</strong> Return to default view</li>
      </ul>
      <p><em>Lines show unweighted aggregates across education systems. Each country contributes equally.</em></p>"
    )
  })
  
  # Missing countries list output
  output$missing_countries_list <- renderUI({
    df <- filtered_data()
    missing_detail <- attr(df, "missing_grades_detail")
    
    if (is.null(missing_detail) || nrow(missing_detail) == 0) {
      return(HTML("<p style='color: green;'><strong>No countries with missing grade bands!</strong></p>
                   <p>All country-wealth combinations have complete data across all grade bands.</p>"))
    }
    
    # Group by country and aggregate the affected wealth groups
    missing_by_country <- missing_detail %>%
      group_by(iso3) %>%
      summarise(
        wealth_groups = paste(category, collapse = ", "),
        grades_before = sum(grades_before),
        grades_after = sum(grades_after),
        .groups = "drop"
      ) %>%
      mutate(missing = grades_before - grades_after) %>%
      arrange(iso3)
    
    # Generate HTML table rows
    table_rows <- lapply(1:nrow(missing_by_country), function(i) {
      row <- missing_by_country[i, ]
      paste0(
        "<tr>",
        "<td style='padding: 5px; border-bottom: 1px solid #ddd;'><strong>", row$iso3, "</strong></td>",
        "<td style='padding: 5px; border-bottom: 1px solid #ddd; font-size: 0.85em;'>", row$wealth_groups, "</td>",
        "<td style='padding: 5px; border-bottom: 1px solid #ddd; text-align: center;'>", row$missing, "/", row$grades_before, "</td>",
        "</tr>"
      )
    })
    
    HTML(paste0(
      "<p style='color: #d9534f; margin-bottom: 10px;'><strong>", nrow(missing_by_country), 
      " countries affected</strong></p>",
      "<table style='width: 100%; font-size: 0.9em; border-collapse: collapse;'>",
      "<thead>",
      "<tr style='background-color: #f5f5f5;'>",
      "<th style='padding: 5px; text-align: left; border-bottom: 2px solid #ddd;'>Country</th>",
      "<th style='padding: 5px; text-align: left; border-bottom: 2px solid #ddd;'>Wealth Groups</th>",
      "<th style='padding: 5px; text-align: center; border-bottom: 2px solid #ddd;'>Missing</th>",
      "</tr>",
      "</thead>",
      "<tbody>",
      paste(table_rows, collapse = ""),
      "</tbody>",
      "</table>"
    ))
  })
  
  # Reset filters button
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "subject", selected = "reading")
    updateSelectInput(session, "agg_method", selected = "mean")
    updateNumericInput(session, "min_observations", value = 50)
    
    updateCheckboxGroupInput(session, "filter_wealth", selected = filter_columns$wealth_categories)
    
    if (!is.null(input$filter_income)) {
      updateCheckboxGroupInput(session, "filter_income", selected = filter_columns$income_level)
    }
    if (!is.null(input$filter_region)) {
      updateCheckboxGroupInput(session, "filter_region", selected = filter_columns$Region)
    }
    if (!is.null(input$filter_year)) {
      updateSelectInput(session, "filter_year", selected = "<Show All>")
    }
  })
}

# ============================================================================
# RUN THE APP
# ============================================================================

shinyApp(ui = ui, server = server)

