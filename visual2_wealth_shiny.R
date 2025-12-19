# Visual 2 Shiny App: Wealth Gradients in Foundational Skills
# Interactive visualization with faceting by Africa/Non-Africa/All

library(shiny)
library(plotly)
library(dplyr)
library(readr)
library(rlang)
library(htmlwidgets)


# ============================================================================
# DATA LOADING
# ============================================================================

data_file <- "data/learning_gradient_bands_with_loess.csv"
countries_file <- "data/unicef_countries.csv"
data <- read_csv(data_file, show_col_types = FALSE)

# Load country names lookup (iso3 -> country)
countries_lookup <- read_csv(countries_file, show_col_types = FALSE) %>%
  select(iso3, country)

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

  tags$head(
    tags$style(HTML("
      .unicef-accent {
        border-left: 4px solid #00AEEF;
        padding-left: 8px;
      }
      .wealth-chart-container {
        width: 100% !important;
        height: 700px;        /* fixed container height so inner widget can use 100% */
        min-height: 400px;
      }
      .plotly.html-widget,
      .plotly {
        width: 100% !important;
        height: 100% !important; /* fill the container height */
      }
      @media (max-width: 991px) {
        .lg-column {
          margin-bottom: 20px;
        }
      }
    "))
  ),

  fluidRow(
    # LEFT COLUMN: Filters (3/12)
    column(
      width = 3,
      class = "lg-column",
      wellPanel(
        h4("Settings", class = "unicef-accent"),
        
        # Subject selection
        selectInput("subject",
                    "Subject:",
                    choices = c("Foundational Reading" = "reading",
                                "Foundational Numeracy" = "numeracy"),
                    selected = "reading"),
        
        # Metric type (mean/median with or without LOESS smoothing)
        selectInput("metric_type",
                    "Metric:",
                    choices = c(
                      "Median" = "median",
                      "Median with LOESS" = "median_loess",
                      "Mean" = "mean",
                      "Mean with LOESS" = "mean_loess"
                    ),
                    selected = "mean_loess"),
        
        hr(),
        
        h4("Filters"),
        
        # Wealth groups selection
        selectInput("filter_wealth",
                    "Wealth Groups:",
                    choices = filter_columns$wealth_categories,
                    selected = filter_columns$wealth_categories,
                    multiple = TRUE),

        # Income Level: multi-select dropdown
        if ("income_level" %in% names(filter_columns)) {
          selectInput("filter_income",
                      "Income Level:",
                      choices = filter_columns$income_level,
                      selected = filter_columns$income_level,
                      multiple = TRUE)
        },

        # Region: multi-select dropdown
        if ("Region" %in% names(filter_columns)) {
          selectInput("filter_region",
                      "Region:",
                      choices = filter_columns$Region,
                      selected = filter_columns$Region,
                      multiple = TRUE)
        },
        
        # Year filter
        if ("year" %in% names(filter_columns)) {
          selectInput("filter_year",
                      "Year:",
                      choices = filter_columns$year,
                      selected = "<Show All>")
        },
        
        # Minimum observations filter (slider)
        sliderInput("min_observations",
                    "Minimum Observations (n):",
                    min = 0,
                    max = 100,
                    value = 50,
                    step = 5),

        # Missing combinations tolerance filter (slider)
        sliderInput("missing_combinations_tolerance",
                    "Missing Combinations Tolerance:",
                    min = 1,
                    max = 15,
                    value = 15,
                    step = 1),

        helpText("Minimum observations: Excludes individual combinations with fewer observations than this threshold."),
        helpText("Missing combinations tolerance: Maximum missing combinations allowed per country (1 = complete data with all 15 combinations, 15 = all countries included). Each country should have 15 combinations: 3 grade bands × 5 wealth quintiles (Poorest-Richest, excluding 'All')."),
        
        hr(),
        
        # Reset filters button
        actionButton("reset_filters", "Reset All Filters", 
                     class = "btn-warning")
      )
    ),

    # MIDDLE COLUMN: Chart + Summary + Info (6/12)
    column(
      width = 6,
      class = "lg-column",
      div(
        class = "wealth-chart-container",
        plotlyOutput("main_chart", height = "100%", width = "100%")
      ),
      br(),
      wellPanel(
        h4("Data Summary"),
        verbatimTextOutput("data_summary")
      )
    ),

    # RIGHT COLUMN: Missing combinations table (3/12)
    column(
      width = 3,
      class = "lg-column",
      wellPanel(
        h4("Countries with Missing Combinations"),
        htmlOutput("missing_countries_list"),
        style = "max-height: 800px; overflow-y: auto;"
      )
    )
  )
)

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {
  
  # Reactive settings for metric selection (column + aggregation + labels)
  metric_settings <- reactive({
    type <- input$metric_type
    if (is.null(type)) type <- "mean_loess"
    
    switch(
      type,
      "median" = list(
        col = "proficiency_rate",
        agg = "median",
        label = "Median"
      ),
      "median_loess" = list(
        col = "proficiency_loess_span1",
        agg = "median",
        label = "Median"
      ),
      "mean" = list(
        col = "proficiency_rate",
        agg = "mean",
        label = "Mean"
      ),
      "mean_loess" = list(
        col = "proficiency_loess_span1",
        agg = "mean",
        label = "Mean"
      )
    )
  })
  
  # Reactive filtered data
  filtered_data <- reactive({
    metric <- metric_settings()
    metric_col <- metric$col
    
    df <- data %>%
      filter(grade_band %in% valid_grade_bands) %>%
      filter(subject == input$subject) %>%
      filter(!is.na(.data[[metric_col]]))
    
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

    # Track combinations (grade band × wealth quintile) before minimum observations filter
    # Expected: 3 grade bands × 5 wealth quintiles = 15 combinations per country
    # Exclude "All" from expected combinations as it's an aggregate, not a quintile
    country_combinations_before <- df %>%
      filter(category != "All") %>%
      group_by(iso3) %>%
      summarise(
        combinations_before = n(),
        unique_bands = n_distinct(grade_band),
        unique_wealth = n_distinct(category),
        .groups = "drop"
      )

    # Apply minimum observations filter
    if (!is.null(input$min_observations) && input$min_observations > 0) {
      df <- df %>% filter(n > input$min_observations)
    }

    # Track after minimum observations filter
    rows_after_n_filter <- nrow(df)
    excluded_combinations <- rows_before_n_filter - rows_after_n_filter

    # Count combinations per country after minimum observations filter (excluding "All")
    country_combinations_after <- df %>%
      filter(category != "All") %>%
      group_by(iso3) %>%
      summarise(
        combinations_after = n(),
        .groups = "drop"
      )

    # Calculate missing combinations per country
    country_missing_combinations <- country_combinations_before %>%
      left_join(country_combinations_after, by = "iso3") %>%
      mutate(
        combinations_after = ifelse(is.na(combinations_after), 0, combinations_after),
        missing_combinations = combinations_before - combinations_after
      ) %>%
      filter(missing_combinations > 0)

    # Create detailed missing combinations info for the table BEFORE tolerance filter
    # This shows all countries with any missing combinations, regardless of tolerance
    # Note: Only counting wealth quintiles (not "All"), so max is 15 combinations
    missing_combinations_detail <- country_missing_combinations %>%
      mutate(
        present_combinations = combinations_after,
        missing_count = missing_combinations,
        missing_percentage = round((missing_combinations / 15) * 100, 1),  # Out of 15 expected
        expected_total = 15
      ) %>%
      left_join(countries_lookup, by = "iso3") %>%
      mutate(
        country_name = ifelse(is.na(country) | country == "", iso3, country)
      ) %>%
      arrange(desc(missing_count), iso3)

    # Apply missing combinations tolerance filter
    countries_dropped <- c()
    if (!is.null(input$missing_combinations_tolerance)) {
      max_missing_allowed <- input$missing_combinations_tolerance - 1

      # Find countries with acceptable data completeness
      countries_with_sufficient_data <- country_combinations_after %>%
        left_join(
          country_combinations_before %>% select(iso3, combinations_before),
          by = "iso3"
        ) %>%
        mutate(
          missing = combinations_before - combinations_after
        ) %>%
        filter(missing <= max_missing_allowed) %>%
        pull(iso3)

      # Track which countries were dropped
      countries_dropped <- setdiff(unique(countries_before), countries_with_sufficient_data)

      df <- df %>% filter(iso3 %in% countries_with_sufficient_data)
    }

    # Update final counts
    countries_after <- unique(df$iso3)
    countries_missing_combinations_count <- nrow(missing_combinations_detail)
    countries_dropped_count <- length(countries_dropped)

    # Add dropped flag to missing combinations detail
    missing_combinations_detail <- missing_combinations_detail %>%
      mutate(dropped = iso3 %in% countries_dropped)
    
    # Create ordered factors
    df <- df %>%
      mutate(
        grade_band_ordered = factor(grade_band, levels = valid_grade_bands),
        category_ordered = factor(category, levels = wealth_categories)
      )
    
    # Store excluded counts as attributes
    attr(df, "excluded_combinations") <- excluded_combinations
    attr(df, "countries_missing_combinations") <- countries_missing_combinations_count
    attr(df, "countries_dropped") <- countries_dropped_count
    attr(df, "missing_combinations_detail") <- missing_combinations_detail
    
    return(df)
  })
  
  # Create visualization with facets
  output$main_chart <- renderPlotly({
    df <- filtered_data()
    metric <- metric_settings()
    metric_col <- metric$col
    use_mean <- identical(metric$agg, "mean")
    metric_sym <- rlang::sym(metric_col)
    subject_word <- ifelse(is.null(input$subject) || input$subject == "reading",
                           "Reading", "Numeracy")
    loess_suffix <- if (grepl("loess", input$metric_type)) " (LOESS Smoothed)" else ""
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
          proficiency_agg = if (use_mean) {
            mean(!!metric_sym, na.rm = TRUE)
          } else {
            median(!!metric_sym, na.rm = TRUE)
          },
          n_countries = n_distinct(iso3),
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
    
    # Store "All" category data for annotations
    all_category_annotations <- list()
    
    # Add traces for each facet (only Africa and Non-Africa, excluding All)
    for (facet_name in c("Africa", "Non-Africa")) {
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
          "N countries: ", w_data$n_countries, "<br>",
          "N: ", format(w_data$total_children, big.mark = ",")
        )
        
        # Determine if this trace should show in legend (only show once per wealth group)
        show_legend <- (facet_name == "Africa")
        legend_name <- as.character(wealth)
        
        # Store "All" category data for annotations
        if (wealth == "All") {
          for (i in 1:nrow(w_data)) {
            all_category_annotations[[length(all_category_annotations) + 1]] <- list(
              x = as.character(w_data$grade_band_ordered[i]),
              y = w_data$proficiency_agg[i],
              text = paste0("<b>", round(w_data$proficiency_agg[i], 1), "%</b>"),
              xref = if(facet_name == "Africa") "x" else "x2",
              yref = if(facet_name == "Africa") "y" else "y2",
              showarrow = FALSE,
              xanchor = "left",
              yanchor = "top",
              xshift = 8,
              yshift = -8,
              font = list(
                size = 12,
                color = "#000000",
                family = "Arial Black, Arial, sans-serif"
              )
            )
          }
        }
        
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
            hovertext = hover_text,
            hoverinfo = 'text',
            line = list(width = line_width, color = wealth_colors[[as.character(wealth)]]),
            marker = list(size = 6, color = wealth_colors[[as.character(wealth)]]),
            xaxis = if(facet_name == "Africa") "x" else "x2",
            yaxis = if(facet_name == "Africa") "y" else "y2"
          )
      }
    }
    
    # Layout with two subplots in one row
    metric_label <- metric$label
    y_axis_title <- paste0("<b>", subject_word, " - % with foundational skills</b>")
    title_text <- paste0(
      "<sub><b>Foundational ", subject_word, "</b><br>",
      metric_label, " proficiency by wealth and grade band",
      loess_suffix,
      "</sub>"
    )
    
    fig <- fig %>%
      layout(
        title = list(
          text = title_text,
          x = 0.5, xanchor = "center"
        ),
        # Africa facet
        xaxis = list(
          domain = c(0, 0.44),
          title = list(text = "<b>Africa</b>", standoff = 10),
          type = "category",
          categoryorder = "array",
          categoryarray = valid_grade_bands,
          tickfont = list(size = 10),
          automargin = TRUE
        ),
        yaxis = list(
          title = y_axis_title,
          range = c(0, 100),
          gridcolor = "#E5E5E5"
        ),
        # Non-Africa facet
        xaxis2 = list(
          domain = c(0.52, 0.9),  # leave space on the right for legend
          title = list(text = "<b>Non-Africa</b>", standoff = 10),
          type = "category",
          categoryorder = "array",
          categoryarray = valid_grade_bands,
          tickfont = list(size = 10),
          automargin = TRUE
        ),
        yaxis2 = list(
          range = c(0, 100),
          showticklabels = FALSE,
          gridcolor = "#E5E5E5"
        ),
        hovermode = 'closest',
        legend = list(
          title = list(text = "<b>Wealth Group</b>"),
          orientation = "v",
          x = 1,
          y = 1,
          xanchor = "right",
          bgcolor = "rgba(255, 255, 255, 0.9)",
          bordercolor = "#CCCCCC",
          borderwidth = 1
        ),
        annotations = all_category_annotations,
        margin = list(r = 0, t = 100, l = 80, b = 80),
        plot_bgcolor = "#F8F8F8",
        paper_bgcolor = "white",
        showlegend = TRUE,
        autosize = TRUE
      ) %>%
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        staticPlot = FALSE,
        responsive = TRUE
      )

    return(fig)
  })
  
  # Data summary
  output$data_summary <- renderText({
    df <- filtered_data()
    excluded_count <- attr(df, "excluded_combinations")
    countries_missing_combos <- attr(df, "countries_missing_combinations")
    countries_dropped <- attr(df, "countries_dropped")

    all_countries <- length(unique(df$iso3))
    africa_countries <- length(unique(df$iso3[df$is_africa == TRUE]))
    non_africa_countries <- length(unique(df$iso3[df$is_africa == FALSE]))

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

    # Calculate missing combinations info
    max_missing_allowed <- input$missing_combinations_tolerance - 1
    min_required_combos <- 15 - max_missing_allowed

    summary_text <- paste0(
      "Filtered Data:\n",
      "- Total combinations: ", nrow(df), "\n",
      "- Excluded combinations (n ≤ ", input$min_observations, "): ", excluded_count, "\n",
      "- Countries with missing combinations: ", countries_missing_combos, "\n",
      "- Countries dropped (tolerance filter): ", countries_dropped, "\n",
      "- Missing combinations tolerance: ", input$missing_combinations_tolerance, " (countries need ≥ ", min_required_combos, "/15 combinations)\n",
      "- All countries: ", all_countries, "\n",
      "- Africa countries: ", africa_countries, "\n",
      "- Non-Africa countries: ", non_africa_countries, "\n",
      "- Wealth groups: ", length(unique(df$category)), "\n",
      year_info
    )

    return(summary_text)
  })
  
  # Missing combinations list output
  output$missing_countries_list <- renderUI({
    df <- filtered_data()
    missing_detail <- attr(df, "missing_combinations_detail")

    if (is.null(missing_detail) || nrow(missing_detail) == 0) {
      return(HTML("<p style='color: green;'><strong>All countries have complete data!</strong></p>
                   <p>All countries have all 15 combinations (3 grade bands × 5 wealth quintiles).</p>"))
    }

    # Count dropped countries
    dropped_count <- sum(missing_detail$dropped, na.rm = TRUE)
    affected_count <- nrow(missing_detail)

    # Generate HTML table rows with light red background for dropped countries
    table_rows <- lapply(1:nrow(missing_detail), function(i) {
      row <- missing_detail[i, ]
      bg_color <- if (row$dropped) "background-color: #ffcccc;" else ""
      paste0(
        "<tr style='", bg_color, "'>",
        "<td style='padding: 5px; border-bottom: 1px solid #ddd; vertical-align: middle; white-space: normal; word-wrap: break-word;'>", row$country_name, "</td>",
        "<td style='padding: 5px; border-bottom: 1px solid #ddd; text-align: left; vertical-align: middle; width: 55px;'><strong>", row$missing_count, "/15</strong></td>",
        "<td style='padding: 5px; border-bottom: 1px solid #ddd; text-align: center; vertical-align: middle; width: 45px;'><strong>", row$missing_percentage, "%</strong></td>",
        "</tr>"
      )
    })

    HTML(paste0(
      "<p style='color: #d9534f; margin-bottom: 10px;'><strong>", affected_count,
      " countries affected | ", dropped_count, " dropped</strong></p>",
      "<table style='width: 100%; font-size: 0.9em; border-collapse: collapse;'>",
      "<thead>",
      "<tr style='background-color: #f5f5f5;'>",
      "<th style='padding: 5px; text-align: left; border-bottom: 2px solid #ddd; vertical-align: middle; border-right: 1px solid #bbb;'>Country</th>",
      "<th style='padding: 5px; text-align: center; border-bottom: 2px solid #ddd; vertical-align: middle; width: 55px; border-right: 1px solid #bbb;'>Missing</th>",
      "<th style='padding: 5px; text-align: center; border-bottom: 2px solid #ddd; vertical-align: middle; width: 45px;'>%</th>",
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
    updateSelectInput(session, "subject", selected = "reading")
    updateSelectInput(session, "metric_type", selected = "mean_loess")
    updateSliderInput(session, "min_observations", value = 50)
    updateSliderInput(session, "missing_combinations_tolerance", value = 15)

    updateSelectInput(session, "filter_wealth", selected = filter_columns$wealth_categories)

    if (!is.null(input$filter_income)) {
      updateSelectInput(session, "filter_income", selected = filter_columns$income_level)
    }
    if (!is.null(input$filter_region)) {
      updateSelectInput(session, "filter_region", selected = filter_columns$Region)
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



