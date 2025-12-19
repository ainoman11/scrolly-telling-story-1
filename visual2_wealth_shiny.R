# Visual 2 Shiny App: Wealth Gradients in Foundational Skills
# Interactive visualization with faceting by Africa/Non-Africa/All

library(shiny)
library(plotly)
library(dplyr)
library(readr)
library(rlang)
library(htmlwidgets)
library(shinyWidgets)


# ============================================================================
# DATA LOADING
# ============================================================================

data_file <- "data/learning_gradient_bands_with_loess.csv"
countries_file <- "data/unicef_countries.csv"
data <- read_csv(data_file, show_col_types = FALSE)

# Load country names lookup (iso3 -> country)
# Make unique by iso3 to avoid duplicates (same country can appear in multiple region groupings)
countries_lookup <- read_csv(countries_file, show_col_types = FALSE) %>%
  select(iso3, country) %>%
  distinct(iso3, .keep_all = TRUE)

# Valid grade bands
valid_grade_bands <- c("Primary (1-3)", "Upper Primary (4-6)", "Lower Secondary (7-9)")

# Wealth categories
wealth_categories <- c("All", "Poorest", "Second", "Middle", "Fourth", "Richest")

# Wealth quintiles only (for combination counting, excludes "All")
wealth_quintiles_only <- c("Poorest", "Second", "Middle", "Fourth", "Richest")

# Wealth colors - UNICEF palette
wealth_colors <- list(
  "All" = "#00AEEF",  # UNICEF primary blue
  "Poorest" = "#E2231A",  # UNICEF red
  "Second" = "#F7941E",  # UNICEF orange
  "Middle" = "#808080",  # Neutral gray
  "Fourth" = "#1AB394",  # UNICEF teal/green
  "Richest" = "#006B5F"  # Darker teal/green
)

# Prepare filter options
filter_columns <- list()

if ("subject" %in% colnames(data)) {
  filter_columns$subject <- sort(unique(data$subject))
}
if ("income_level" %in% colnames(data)) {
  filter_columns$income_level <- sort(unique(data$income_level))
}

# Wealth categories for filtering
filter_columns$wealth_categories <- wealth_categories

# ============================================================================
# UI DEFINITION
# ============================================================================

ui <- fluidPage(
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("

    :root{
      --unicef-blue:#00AEEF;
      --unicef-blue-dark:#0077A3;
      --ink:#1F2A37;
      --muted:#6B7280;
      --bg:#F6F8FB;
      --card:#FFFFFF;
      --line:#E5E7EB;
      --shadow: 0 10px 30px rgba(16,24,40,0.08);
      --shadow2: 0 6px 16px rgba(16,24,40,0.08);
      --radius: 16px;
    }

    body{
      background: var(--bg);
      color: var(--ink);
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Inter, Arial, sans-serif;
    }

    .container-fluid { padding-top: 14px; }

    .app-header{
      background: linear-gradient(90deg, rgba(0,174,239,0.12), rgba(0,174,239,0.00));
      border: 1px solid rgba(0,174,239,0.18);
      border-radius: var(--radius);
      padding: 16px 18px;
      margin-bottom: 14px;
      box-shadow: var(--shadow2);
    }
    .app-title{
      font-size: 18px;
      font-weight: 750;
      margin: 0;
      letter-spacing: 0.2px;
    }
    .app-subtitle{
      margin: 6px 0 0 0;
      color: var(--muted);
      font-size: 13px;
      line-height: 1.35;
    }
    .accent-bar{
      border-left: 5px solid var(--unicef-blue);
      padding-left: 12px;
    }

    .lg-column { margin-bottom: 16px; }

    .panel-card{
      background: var(--card);
      border: 1px solid var(--line);
      border-radius: var(--radius);
      box-shadow: var(--shadow2);
      padding: 14px 14px 10px 14px;
    }

    .sidebar-card{
      position: sticky;
      top: 12px;
    }
    @media (max-width: 991px){
      .sidebar-card{ position: static; }
    }

    .section-title{
      font-size: 14px;
      font-weight: 750;
      margin: 0 0 10px 0;
    }

    .help-text{
      color: var(--muted);
      font-size: 12px;
      line-height: 1.35;
      margin-top: 6px;
    }

    .control-label{
      font-size: 12px;
      color: var(--muted);
      font-weight: 700;
      letter-spacing: 0.2px;
    }
    .form-control{
      border-radius: 12px !important;
      border: 1px solid var(--line) !important;
      box-shadow: none !important;
    }
    .form-control:focus{
      border-color: rgba(0,174,239,0.65) !important;
      box-shadow: 0 0 0 4px rgba(0,174,239,0.15) !important;
    }

    .bootstrap-select > .dropdown-toggle{
      border-radius: 12px !important;
      border: 1px solid var(--line) !important;
    }
    .bootstrap-select .dropdown-menu{
      border-radius: 12px;
      overflow: hidden;
    }
    .bootstrap-select .dropdown-menu li a{
      padding: 8px 12px;
    }

    .irs--shiny .irs-bar { background: var(--unicef-blue) !important; border-top: 1px solid var(--unicef-blue) !important; border-bottom: 1px solid var(--unicef-blue) !important; }
    .irs--shiny .irs-single { background: var(--unicef-blue) !important; }
    .irs--shiny .irs-handle { border: 2px solid var(--unicef-blue) !important; }

    .btn-warning{
      background: var(--unicef-blue) !important;
      border: 1px solid rgba(0,174,239,0.9) !important;
      color: #fff !important;
      font-weight: 750;
      border-radius: 12px !important;
      padding: 10px 12px;
    }
    .btn-warning:hover{
      background: var(--unicef-blue-dark) !important;
      border-color: var(--unicef-blue-dark) !important;
    }

    .chart-card{
      padding: 0;
      overflow: hidden;
    }
    .wealth-chart-container{
      width: 100% !important;
      height: 720px;
      min-height: 420px;
      border-radius: var(--radius);
    }
    .plotly.html-widget,
    .plotly{
      width:100% !important;
      height:100% !important;
    }

    .summary-two-col pre{
      background: #FBFCFE !important;
      border: 1px solid var(--line) !important;
      border-radius: 14px !important;
      padding: 12px !important;
      color: var(--ink) !important;
      font-size: 12.5px !important;
      line-height: 1.45 !important;
      column-count: 2;
      column-gap: 20px;
      white-space: pre-wrap;
    }
    @media (max-width: 991px){
      .summary-two-col pre{ column-count: 1; }
    }

    .scroll-card{
      max-height: 820px;
      overflow-y: auto;
      padding-right: 6px;
    }
    .scroll-card::-webkit-scrollbar{ width: 10px; }
    .scroll-card::-webkit-scrollbar-thumb{ background: rgba(31,42,55,0.15); border-radius: 999px; }
    .scroll-card::-webkit-scrollbar-track{ background: rgba(31,42,55,0.04); border-radius: 999px; }

    hr{ border-top: 1px solid var(--line); }

    .well { background: transparent; border: none; box-shadow: none; padding: 0; }

  "))
  ),

  div(
    class = "app-header",
    div(
      class = "accent-bar",
      h2("Visual 2: Wealth Gradients - Foundational Skills Across Education Stages", class = "app-title"),
      p("Faceted view of wealth gradients (Africa vs Non-Africa) with quality controls. Styling only: same logic, same behavior.",
        class = "app-subtitle")
    )
  ),

  fluidRow(
    # LEFT COLUMN: Filters (3/12)
    column(
      width = 3,
      class = "lg-column",
      div(
        class = "panel-card sidebar-card",
        h4("Settings", class = "section-title accent-bar"),
        
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
        
        h4("Filters", class = "section-title"),

        # Income Level: multi-select dropdown
        if ("income_level" %in% names(filter_columns)) {
          pickerInput(
            inputId = "filter_income",
            label   = "Income Level:",
            choices = filter_columns$income_level,
            selected = NULL,
            multiple = TRUE,
            options = pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE,
              noneSelectedText = "All income levels"
            )
          )
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

        div(class="help-text", "Minimum observations: Excludes individual combinations with fewer observations than this threshold."),
        div(class="help-text", "Missing combinations tolerance: Maximum missing combinations allowed per country (1 = complete data with all 15 combinations, 15 = all countries included). Each country should have 15 combinations: 3 grade bands × 5 wealth quintiles (Poorest-Richest, excluding 'All')."),
        
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
        class = "panel-card chart-card",
        div(
          class = "wealth-chart-container",
          plotlyOutput("main_chart", height = "100%", width = "100%")
        )
      ),
      br(),
      div(
        class = "panel-card",
        h4("Data Summary", class = "section-title"),
        div(class = "summary-two-col", verbatimTextOutput("data_summary"))
      )
    ),

    # RIGHT COLUMN: Missing combinations table (3/12)
    column(
      width = 3,
      class = "lg-column",
      div(
        class = "panel-card",
        h4("Countries with Missing Combinations", class = "section-title"),
        div(class = "scroll-card", htmlOutput("missing_countries_list"))
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
    
    # Apply income level filter
    if (!is.null(input$filter_income) && length(input$filter_income) > 0) {
      df <- df %>% filter(income_level %in% input$filter_income)
    }
    
    # Count before minimum observations filter (AFTER all other filters)
    rows_before_n_filter <- nrow(df)
    countries_before <- unique(df$iso3)

    # Track combinations (grade band × wealth quintile) before minimum observations filter
    # Expected: 3 grade bands × 5 wealth quintiles = 15 combinations per country
    # Only count the 5 wealth quintiles (Poorest, Second, Middle, Fourth, Richest), exclude "All"
    country_combinations_before <- df %>%
      filter(category %in% wealth_quintiles_only) %>%
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

    # Count combinations per country after minimum observations filter
    # Only count the 5 wealth quintiles (Poorest, Second, Middle, Fourth, Richest), exclude "All"
    country_combinations_after <- df %>%
      filter(category %in% wealth_quintiles_only) %>%
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
        
        # Keep "All" line prominent, make individual quintile lines less pronounced
        line_width <- ifelse(wealth == "All", 3.5, 2.0)
        line_opacity <- ifelse(wealth == "All", 1.0, 0.4)
        marker_size <- ifelse(wealth == "All", 6, 4)
        marker_opacity <- ifelse(wealth == "All", 1.0, 0.4)
        
        # Convert color to rgba for opacity control
        color_hex <- wealth_colors[[as.character(wealth)]]
        color_rgb <- col2rgb(color_hex)
        color_rgba <- paste0("rgba(", color_rgb[1], ",", color_rgb[2], ",", color_rgb[3], ",", line_opacity, ")")
        marker_color_rgba <- paste0("rgba(", color_rgb[1], ",", color_rgb[2], ",", color_rgb[3], ",", marker_opacity, ")")
        
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
              xshift = 4,
              yshift = -4,
              font = list(
                size = 12,
                color = "#000000",
                family = "Arial, Arial, sans-serif"
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
            line = list(width = line_width, color = color_rgba),
            marker = list(size = marker_size, color = marker_color_rgba),
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
          domain = c(0, 0.48),  # 48% width - can use more space now
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
          domain = c(0.52, 1.0),  # 48% width (equal to Africa), with 4% gap between facets
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
          orientation = "h",
          x = 0.5,  # centered horizontally
          y = -0.15,  # below the chart
          xanchor = "center",
          yanchor = "top",
          bgcolor = "rgba(255, 255, 255, 0.9)",
          bordercolor = "#CCCCCC",
          borderwidth = 1
        ),
        annotations = all_category_annotations,
        margin = list(r = 40, t = 100, l = 80, b = 120),  # increased bottom margin for legend
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


    # Calculate missing combinations info
    max_missing_allowed <- input$missing_combinations_tolerance - 1
    min_required_combos <- 15 - max_missing_allowed

    summary_text <- paste0(
      "Filtered Data:\n",
      "- Total combinations: ", nrow(df), "\n",
      "- Excluded combinations (n ≤ ", input$min_observations, "): ", excluded_count, "\n",
      "- Countries with missing combinations: ", countries_missing_combos, "\n",
      "- Countries dropped (tolerance filter): ", countries_dropped, "\n",
      "- Missing combinations tolerance: ", input$missing_combinations_tolerance, "\n(countries need ≥ ", min_required_combos, "/15 combinations)\n",
      "- All countries: ", all_countries, "\n",
      "- Africa countries: ", africa_countries, "\n",
      "- Non-Africa countries: ", non_africa_countries
    )

    return(summary_text)
  })
  
  # Missing combinations list output
  output$missing_countries_list <- renderUI({
    df <- filtered_data()
    missing_detail <- attr(df, "missing_combinations_detail")

    if (is.null(missing_detail) || nrow(missing_detail) == 0) {
      return(HTML("<p style='color: #0B7A3E; font-weight:700; margin: 6px 0 6px 0;'>All countries have complete data.</p>
     <p style='color:#6B7280; margin:0;'>All countries have all 15 combinations (3 grade bands × 5 wealth quintiles).</p>"))
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

    if (!is.null(input$filter_income)) {
      updatePickerInput(session, "filter_income", selected = NULL)
    }
  })
}

# ============================================================================
# RUN THE APP
# ============================================================================

shinyApp(ui = ui, server = server)



