# ==========================================
# GEM Analytics For Arab Region: The Startup-Survival Gap
# Version 2.7 - Final Integrated Edition
# ==========================================

# ==========================================
# 1. Libraries
# ==========================================
library(shiny)
library(tidyverse)
library(bslib)
library(plotly)
library(viridis)
library(janitor)
library(bsicons)
library(scales)
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")
library(DT)

# ==========================================
# 2. Data Loading & Cleaning
# ==========================================
raw_data <- read.csv("data.csv") |> clean_names()
names(raw_data) <- gsub("_$", "", names(raw_data))

clean_data <- raw_data |>
  filter(!economy %in% c("Tunisia", "Sudan", "Kuwait", "")) |>
  mutate(
    year = as.numeric(year),
    entrepreneurial_intentions    = as.numeric(entrepreneurial_intentions),
    tea                           = as.numeric(tea),
    established_business_ownership = as.numeric(established_business_ownership),
    financing_for_entrepreneurs   = as.numeric(financing_for_entrepreneurs),
    governmental_support_and_policies = as.numeric(governmental_support_and_policies),
    fear_of_failure_rate          = as.numeric(fear_of_failure_rate),
    perceived_capabilities        = as.numeric(perceived_capabilities),
    survival_gap                  = tea - established_business_ownership
  ) |>
  drop_na(tea, established_business_ownership)

all_economies  <- sort(unique(clean_data$economy))
default_sel    <- intersect(c("Oman", "Saudi Arabia", "Qatar", "UAE", "Egypt", "Jordan", "Morocco"), all_economies)
pal7 <- c("#E63946","#2A9D8F","#E9C46A","#457B9D","#F4A261","#8338EC","#06D6A0")

# ==========================================
# 3. Custom Theme
# ==========================================
custom_theme <- bs_theme(
  version = 5, bootswatch = "flatly", primary = "#1B2A4A", secondary = "#E63946"
) |>
  bs_add_rules("
    body { background: #F0F4F8 !important; }
    .sidebar { background: #1B2A4A !important; color: #FFFFFF !important; padding: 1rem; }
    .sidebar label, .sidebar .control-label, .sidebar .checkbox label, .sidebar .form-check-label { 
        color: #FFFFFF !important; 
        font-weight: 500 !important;
    }
    .sidebar .selectize-control.multi .selectize-input > div { 
        background: #E63946 !important; color: #FFFFFF !important; border-radius: 4px; 
    }
    .sidebar input[type='checkbox'] { accent-color: #E63946; }
    .card { border: none !important; border-radius: 12px !important; box-shadow: 0 2px 12px rgba(27,42,74,0.08) !important; }
    .card-header { font-weight: 600; font-size: 0.88rem; text-transform: uppercase; }
    .insight-box { background: linear-gradient(135deg,#F7F9FC,#EDF1F7); border-left: 4px solid #E63946; padding: 1rem; border-radius: 0 10px 10px 0; }
    .live-dot { display:inline-block; width:8px; height:8px; background:#2A9D8F; border-radius:50%; animation: pulse 2s infinite; margin-right:6px; }
    @keyframes pulse { 0%,100%{opacity:1} 50%{opacity:0.4} }
  ")

# ==========================================
# 4. UI
# ==========================================
ui <- page_navbar(
  # CHANGE 1: Navbar title — simpler and more meaningful
  title = div(style="display:flex;align-items:center;gap:10px;",
              tags$span(style="font-size:1.35rem;font-weight:700;color:#FFF;","GEM Analytics For Arab Region"),
              tags$span(style="font-size:0.72rem; background:#E63946; color:#FFF; padding:2px 8px; border-radius:20px; font-weight:600;","LIVE")),
  theme = custom_theme,
  fillable = TRUE,
  sidebar = sidebar(
    width = 260,
    selectizeInput("country", "Select Countries:", choices = all_economies, selected = default_sel, multiple = TRUE),
    sliderInput("year", "Year Range:", min = min(clean_data$year), max = max(clean_data$year), value = range(clean_data$year), step = 1, sep = ""),
    hr(),
    checkboxInput("show_trend", "Show trend lines", value = TRUE),
    actionButton("show_methodology", "ℹ Methodology", class = "btn-secondary btn-sm w-100")
  ),
  
  nav_panel(title = tagList(bs_icon("graph-up"), " Overview"),
            layout_columns(
              col_widths = c(3, 3, 3, 3),
              value_box("Avg Intentions", textOutput("avg_intent"), showcase = bs_icon("lightbulb"), theme = "primary"),
              value_box("Avg New Startups (TEA)", textOutput("avg_tea"), showcase = bs_icon("rocket"), theme = "info"),
              value_box("Avg Established Businesses", textOutput("avg_surv"), showcase = bs_icon("building-check"), theme = "success"),
              value_box("Avg Startup–Survival Gap", textOutput("avg_gap"), showcase = bs_icon("exclamation-triangle"), theme = "danger")
            ),
            card(card_header(tagList(tags$span(class = "live-dot"), "Key Insight"), class = "bg-primary text-white"),
                 uiOutput("dynamic_insight"), max_height = "140px"),
            layout_columns(
              col_widths = c(8, 4),
              # CHANGE 2: Chart header
              card(full_screen = TRUE, card_header("1. Startup vs Survival Over Time", class="bg-primary text-white"), plotlyOutput("ribbonPlot", height = "380px")),
              card(full_screen = TRUE, card_header("2. From Intention to Survival", class="bg-info text-white"), plotlyOutput("funnelPlot", height = "380px"))
            )
  ),
  
  # CHANGE 1: Nav panel label
  nav_panel(title = tagList(bs_icon("diagram-3-fill"), " Key Factors"),
            layout_columns(
              col_widths = c(6, 6),
              # CHANGE 2: Chart headers
              card(full_screen = TRUE, card_header("3. Finance Drives Business Survival", class="bg-success text-white"), plotlyOutput("bubblePlot", height = "380px")),
              card(full_screen = TRUE, card_header("4. Fear vs Confidence in Business", class="bg-danger text-white"), plotlyOutput("scatterPlot", height = "380px"))
            ),
            layout_columns(
              col_widths = c(12),
              card(full_screen = TRUE, card_header("5. Change in Business Survival", class="bg-primary text-white"), plotlyOutput("slopePlot", height = "360px"))
            )
  ),
  
  # CHANGE 1: Nav panel label
  nav_panel(title = tagList(bs_icon("bar-chart-line-fill"), " Trends & Distributions"),
            # CHANGE 2: Chart headers
            card(full_screen = TRUE, card_header("6. Survival Gap Across Countries", class="bg-primary text-white"), plotlyOutput("heatmapPlot", height = "340px")),
            layout_columns(
              col_widths = c(6, 6),
              card(full_screen = TRUE, card_header("7. Variation in Survival Gap", class="bg-info text-white"), plotlyOutput("boxPlot", height = "340px")),
              card(full_screen = TRUE, card_header("8. Business Stages Comparison", style="background:#8338EC;color:white;"), plotlyOutput("stackedBarPlot", height = "340px"))
            )
  ),
  
  # CHANGE 1: Nav panel label
  nav_panel(title = tagList(bs_icon("table"), " Raw Data"), card(DTOutput("data_table")))
)

# ==========================================
# 5. Server
# ==========================================
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$country)
    clean_data |> filter(economy %in% input$country, year >= input$year[1], year <= input$year[2])
  })
  
  # --- METHODOLOGY MODAL ---
  observeEvent(input$show_methodology, {
    showModal(modalDialog(
      title = "Data Methodology & Perception Principles",
      size = "l", easyClose = TRUE, footer = modalButton("Close"),
      HTML("
        <h6>Data Source</h6>
        <ul>
          <li><b>Source:</b> Global Entrepreneurship Monitor (GEM) database</li>
          <li><b>Derived metrics:</b> <code>survival_gap = TEA − EBO</code>.</li>
        </ul>
        <h6>Perception Principles Applied</h6>
        <table class='table table-sm table-bordered'>
        <tr><th>Chart</th><th>Principle</th><th>Justification</th></tr>
        <tr><td>Area / Ribbon</td><td>Gestalt Enclosure</td><td>Filled area between TEA and EBO makes gap pre-attentive</td></tr>
        <tr><td>Bubble Chart</td><td>Bertin — Size variable</td><td>Gov support encoded as bubble area; position encodes finance vs survival</td></tr>
        <tr><td>Scatter Quadrant</td><td>Cleveland & McGill</td><td>Position along common scale for most accurate comparison</td></tr>
        <tr><td>Slope Graph</td><td>Tufte (1983)</td><td>Two-point slope minimizes chart-junk; change magnitude encoded by line steepness</td></tr>
        <tr><td>Heatmap</td><td>Mackinlay — Color hue+value</td><td>Ordered color ramp encodes continuous survival gap across 2D matrix</td></tr>
        <tr><td>Box Plot</td><td>Tukey (EDA)</td><td>Five-number summary reveals outliers and distributional asymmetry</td></tr>
        </table>
      ")
    ))
  })
  
  # --- DYNAMIC INSIGHT ---
  output$dynamic_insight <- renderUI({
    df <- filtered_data()
    req(nrow(df) > 0)
    avg_gap <- round(mean(df$survival_gap, na.rm=T), 1)
    gap_sum <- df |> group_by(economy) |> summarise(gap = mean(survival_gap, na.rm=T), .groups="drop") |> arrange(desc(gap))
    surv_sum <- df |> group_by(economy) |> summarise(surv = mean(established_business_ownership, na.rm=T), .groups="drop") |> arrange(desc(surv))
    fin_cor <- round(cor(df$financing_for_entrepreneurs, df$established_business_ownership, use="complete.obs"), 2)
    worst <- if(nrow(gap_sum) > 0) gap_sum$economy[1] else "N/A"
    best <- if(nrow(surv_sum)> 0) surv_sum$economy[1] else "N/A"
    
    tags$div(class = "insight-box",
             HTML(paste0(
               "<b>", input$year[1], "–", input$year[2], " | ", length(input$country), " countries</b><br>",
               "Avg startup–survival gap: <span style='color:#E63946;font-weight:700;'>", avg_gap, "%</span>. ",
               "Highest dropout risk: <b style='color:#E63946;'>", worst, "</b>. ",
               "Best survival rate: <b style='color:#2A9D8F;'>", best, "</b>. ",
               "Financing–survival correlation: <b>r = ", fin_cor, "</b>."
             ))
    )
  })
  
  output$avg_intent <- renderText({ paste0(round(mean(filtered_data()$entrepreneurial_intentions, na.rm=T), 1), "%") })
  output$avg_tea    <- renderText({ paste0(round(mean(filtered_data()$tea, na.rm=T), 1), "%") })
  output$avg_surv   <- renderText({ paste0(round(mean(filtered_data()$established_business_ownership, na.rm=T), 1), "%") })
  output$avg_gap    <- renderText({ paste0(round(mean(filtered_data()$survival_gap, na.rm=T), 1), "%") })
  
  # -------------------------------------------------------
  # CHART 1: Startup vs Survival Over Time
  # Law of Closure: lines BREAK at missing data gaps (no false connections).
  # A dashed grey bridge spans the gap explicitly, open circles mark gap edges.
  # -------------------------------------------------------
  output$ribbonPlot <- renderPlotly({
    df <- filtered_data()
    
    # Build a complete year grid per economy
    all_years <- seq(min(df$year), max(df$year))
    full_grid <- expand.grid(
      economy = unique(df$economy),
      year    = all_years,
      stringsAsFactors = FALSE
    )
    
    df_full <- full_grid |>
      left_join(df |> select(economy, year, tea, established_business_ownership),
                by = c("economy", "year")) |>
      arrange(economy, year) |>
      group_by(economy) |>
      mutate(
        missing    = is.na(tea) | is.na(established_business_ownership),
        # assign a contiguous segment ID per economy — increments each time missing flips
        segment_id = cumsum(!missing & (lag(missing, default = TRUE) | row_number() == 1)) * (!missing),
        gap_marker = !missing & (lag(missing, default = FALSE) | lead(missing, default = FALSE))
      ) |>
      ungroup() |>
      # rows with segment_id == 0 are missing rows — keep NA so geom_line breaks
      mutate(segment_id = if_else(missing, NA_real_, as.numeric(segment_id)))
    
    # Data that actually exists (no NAs) — used for solid/dashed lines
    df_present <- df_full |> filter(!missing)
    
    # Points at the edges of every gap (open circles to signal the break)
    gap_points <- df_full |> filter(gap_marker)
    
    # Build dashed grey bridge segments across each gap:
    # For each economy, find consecutive pairs of present rows that have a missing row between them
    bridge_segs <- df_full |>
      arrange(economy, year) |>
      group_by(economy) |>
      mutate(
        prev_present_tea  = if_else(!missing, tea,  NA_real_),
        prev_present_ebo  = if_else(!missing, established_business_ownership, NA_real_),
        prev_year         = if_else(!missing, year, NA_real_)
      ) |>
      # fill forward to carry last known value into the gap
      fill(prev_present_tea, prev_present_ebo, prev_year, .direction = "down") |>
      # fill backward to get the next known value
      mutate(
        next_present_tea  = if_else(!missing, tea,  NA_real_),
        next_present_ebo  = if_else(!missing, established_business_ownership, NA_real_),
        next_year         = if_else(!missing, year, NA_real_)
      ) |>
      fill(next_present_tea, next_present_ebo, next_year, .direction = "up") |>
      ungroup() |>
      # keep only the first missing row of each gap run to define one bridge per gap
      filter(missing) |>
      group_by(economy) |>
      mutate(gap_run = cumsum(year == min(year) | lag(!missing, default = TRUE))) |>
      group_by(economy, gap_run) |>
      slice(1) |>
      ungroup() |>
      filter(!is.na(prev_year) & !is.na(next_year))
    
    p <- ggplot() +
      
      # --- Shaded ribbon (only over present data segments) ---
      geom_ribbon(
        data = df_present,
        aes(x = year, ymin = established_business_ownership, ymax = tea,
            fill = economy, group = interaction(economy, segment_id)),
        alpha = 0.2
      ) +
      
      # --- Dashed grey bridge across each gap (TEA line) ---
      geom_segment(
        data = bridge_segs,
        aes(x = prev_year, xend = next_year,
            y = prev_present_tea, yend = next_present_tea),
        color = "grey60", linetype = "dashed", linewidth = 0.7
      ) +
      
      # --- Dashed grey bridge across each gap (EBO line) ---
      geom_segment(
        data = bridge_segs,
        aes(x = prev_year, xend = next_year,
            y = prev_present_ebo, yend = next_present_ebo),
        color = "grey60", linetype = "dashed", linewidth = 0.7
      ) +
      
      # --- Solid lines (TEA dashed, EBO solid) — BREAK at NAs ---
      geom_line(
        data = df_present,
        aes(x = year, y = tea, color = economy,
            group = interaction(economy, segment_id)),
        linetype = "dashed", linewidth = 0.9
      ) +
      geom_line(
        data = df_present,
        aes(x = year, y = established_business_ownership, color = economy,
            group = interaction(economy, segment_id)),
        linewidth = 1.3
      ) +
      
      # --- Open circles at gap boundary points (explicit missing signal) ---
      geom_point(
        data = gap_points,
        aes(x = year, y = tea, color = economy),
        shape = 1, size = 3.5, stroke = 1.2, show.legend = FALSE
      ) +
      geom_point(
        data = gap_points,
        aes(x = year, y = established_business_ownership, color = economy),
        shape = 1, size = 3.5, stroke = 1.2, show.legend = FALSE
      ) +
      
      facet_wrap(~economy, scales = "free_y") +
      scale_color_manual(values = pal7) +
      scale_fill_manual(values  = pal7) +
      labs(
        x       = "Year",
        y       = "Rate (%)",
        caption = "○ Open circles & dashed grey bridge = missing data period (Law of Closure)"
      ) +
      theme_minimal() +
      theme(
        strip.text   = element_text(face = "bold", size = 13),
        plot.caption = element_text(size = 9, color = "grey50", hjust = 0)
      )
    
    ggplotly(p)
  })
  
  # -------------------------------------------------------
  # CHART 2: Pipeline Funnel
  # CHANGE 2 & 3: axis labels, legend
  # -------------------------------------------------------
  output$funnelPlot <- renderPlotly({
    df <- filtered_data() |> group_by(economy) |>
      summarise(
        Intention  = mean(entrepreneurial_intentions, na.rm=T),
        `New Startup` = mean(tea, na.rm=T),
        Survival   = mean(established_business_ownership, na.rm=T),
        .groups="drop"
      ) |>
      pivot_longer(-economy, names_to="Stage", values_to="Rate") |>
      mutate(Stage = factor(Stage, levels = c("Intention","New Startup","Survival")))
    
    p <- ggplot(df, aes(x=Stage, y=Rate, color=economy, group=economy)) +
      geom_line(linewidth=1) +
      geom_point(size=3) +
      scale_color_manual(values=pal7, name="Country") +
      labs(x = "Entrepreneurship Stage", y = "Average Rate (%)") +
      theme_minimal()
    ggplotly(p)
  })
  
  # -------------------------------------------------------
  # CHART 3: Bubble — Finance × Survival × Gov Support
  # CHANGE 2 & 3
  # -------------------------------------------------------
  output$bubblePlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(
      x    = financing_for_entrepreneurs,
      y    = established_business_ownership,
      size = governmental_support_and_policies,
      color = economy
    )) +
      geom_point(alpha = 0.6) +
      { if (input$show_trend) geom_smooth(aes(group=1), method="lm", color="#1B2A4A", se=F, linetype="dashed", linewidth=0.8) } +
      scale_color_manual(values = pal7, name = "Country") +
      scale_size_continuous(name = "Gov. Support Score") +
      labs(
        x = "Financing Availability Score",
        y = "Established Business Rate (%)"
      ) +
      theme_minimal()
    ggplotly(p)
  })
  
  # -------------------------------------------------------
  # CHART 4: Scatter — Fear vs. Capability
  # CHANGE 2 & 3
  # -------------------------------------------------------
  output$scatterPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(
      x     = fear_of_failure_rate,
      y     = perceived_capabilities,
      color = economy
    )) +
      geom_point(size = 3.5) +
      { if (input$show_trend) geom_smooth(aes(group=1), method="lm", color="#1B2A4A", se=F, linetype="dashed", linewidth=0.8) } +
      scale_color_manual(values = pal7, name = "Country") +
      labs(
        x = "Fear of Failure Rate (%)",
        y = "Perceived Capability to Start a Business (%)"
      ) +
      theme_minimal()
    ggplotly(p)
  })
  
  # -------------------------------------------------------
  # CHART 5: Slope Graph
  # CHANGE 2 & 3
  # -------------------------------------------------------
  output$slopePlot <- renderPlotly({
    df <- filtered_data() |> filter(year %in% c(min(year), max(year)))
    p <- ggplot(df, aes(
      x     = as.factor(year),
      y     = established_business_ownership,
      group = economy,
      color = economy
    )) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 4) +
      scale_color_manual(values = pal7, name = "Country") +
      labs(
        x = "Year (First vs. Latest Available)",
        y = "Established Business Rate (%)"
      ) +
      theme_minimal()
    ggplotly(p)
  })
  
  # -------------------------------------------------------
  # CHART 6: Heatmap
  # CHANGE 2 & 3
  # -------------------------------------------------------
  output$heatmapPlot <- renderPlotly({
    df <- filtered_data() |>
      group_by(economy, year) |>
      summarise(gap = mean(survival_gap, na.rm=T), .groups="drop")
    
    p <- ggplot(df, aes(x=factor(year), y=economy, fill=gap)) +
      geom_tile(color="white") +
      scale_fill_gradient2(
        low      = "#2A9D8F",
        mid      = "#F7F9FC",
        high     = "#E63946",
        midpoint = median(df$gap, na.rm=T),
        name     = "Gap (%)"
      ) +
      labs(x = "Year", y = "Country") +
      theme_minimal()
    ggplotly(p)
  })
  
  # -------------------------------------------------------
  # CHART 7: Box Plot
  # CHANGE 2 & 3
  # -------------------------------------------------------
  output$boxPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(
      x    = reorder(economy, survival_gap),
      y    = survival_gap,
      fill = economy
    )) +
      geom_boxplot(alpha=0.7) +
      coord_flip() +
      scale_fill_manual(values=rep(pal7, 5), name="Country") +
      labs(x = "Country", y = "Startup–Survival Gap (%)") +
      theme_minimal()
    ggplotly(p)
  })
  
  # -------------------------------------------------------
  # CHART 8: Stacked Bar
  # CHANGE 2 & 3
  # -------------------------------------------------------
  output$stackedBarPlot <- renderPlotly({
    df <- filtered_data() |> group_by(economy) |>
      summarise(
        Intentions   = mean(entrepreneurial_intentions, na.rm=T),
        `New Startups (TEA)` = mean(tea, na.rm=T),
        `Established Businesses` = mean(established_business_ownership, na.rm=T),
        .groups="drop"
      ) |>
      pivot_longer(-economy, names_to="Stage", values_to="Rate") |>
      mutate(Stage = factor(Stage, levels = c("Intentions","New Startups (TEA)","Established Businesses")))
    
    p <- ggplot(df, aes(x=reorder(economy, Rate), y=Rate, fill=Stage)) +
      geom_bar(stat="identity", position="dodge") +
      scale_fill_manual(
        values = c("Intentions"="#1B2A4A", "New Startups (TEA)"="#457B9D", "Established Businesses"="#2A9D8F"),
        name   = "Stage"
      ) +
      coord_flip() +
      labs(x = "Country", y = "Average Rate (%)") +
      theme_minimal()
    ggplotly(p) |> layout(legend=list(orientation="h", x=0.1, y=-0.2))
  })
  
  output$data_table <- renderDT({
    filtered_data() |> mutate(across(where(is.numeric), ~round(., 1)))
  }, options = list(pageLength = 10, scrollX = TRUE))
}

# ==========================================
# 6. Launch App
# ==========================================
shinyApp(ui, server)