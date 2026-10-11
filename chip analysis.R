# ============================================================
# AI CHIP MARKET ANALYTICS DASHBOARD
# COMPLETE WORKING VERSION
# ============================================================


# ============================================================
# 1. INSTALL PACKAGES
# ============================================================

packages <- c(
  "shiny",
  "ggplot2",
  "DT"
)

for (pkg in packages) {
  
  if (!requireNamespace(pkg, quietly = TRUE)) {
    
    install.packages(
      pkg,
      repos = "https://cloud.r-project.org"
    )
    
  }
  
}


# ============================================================
# 2. LOAD PACKAGES
# ============================================================

library(shiny)
library(ggplot2)
library(DT)


# ============================================================
# 3. SELECT CSV FILE
# ============================================================

csv_file <- file.choose()


# ============================================================
# 4. READ CSV
# ============================================================

chip_data <- read.csv(
  csv_file,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c(
    "",
    "NA",
    "N/A",
    "null",
    "NULL"
  )
)


# ============================================================
# 5. SHOW ORIGINAL COLUMN NAMES
# ============================================================

cat("\nOriginal CSV columns:\n")
print(names(chip_data))
cat("\n")


# ============================================================
# 6. RENAME COLUMNS CORRECTLY
# ============================================================

names(chip_data) <- c(
  "YEAR",
  "CHIP_NAME",
  "VENDOR",
  "LAUNCH_DATE",
  "MEMORY_GB",
  "FP16_TFLOPS",
  "TDP_WATTS",
  "ESTIMATED_SHIPMENTS",
  "ESTIMATED_ASP_USD",
  "ESTIMATED_REVENUE_USD"
)


# ============================================================
# 7. CONVERT NUMERIC COLUMNS
# ============================================================

numeric_columns <- c(
  "YEAR",
  "MEMORY_GB",
  "FP16_TFLOPS",
  "TDP_WATTS",
  "ESTIMATED_SHIPMENTS",
  "ESTIMATED_ASP_USD",
  "ESTIMATED_REVENUE_USD"
)


for (col in numeric_columns) {
  
  chip_data[[col]] <- suppressWarnings(
    as.numeric(
      gsub(
        ",",
        "",
        as.character(
          chip_data[[col]]
        )
      )
    )
  )
  
}


# ============================================================
# 8. CLEAN TEXT DATA
# ============================================================

chip_data$CHIP_NAME <- trimws(
  as.character(
    chip_data$CHIP_NAME
  )
)


chip_data$VENDOR <- trimws(
  as.character(
    chip_data$VENDOR
  )
)


# ============================================================
# 9. REMOVE EMPTY RECORDS
# ============================================================

chip_data <- chip_data[
  !is.na(chip_data$YEAR) &
    !is.na(chip_data$CHIP_NAME) &
    !is.na(chip_data$VENDOR) &
    chip_data$CHIP_NAME != "" &
    chip_data$VENDOR != "",
  ,
  drop = FALSE
]


# ============================================================
# 10. CHECK DATASET
# ============================================================

if (nrow(chip_data) == 0) {
  
  stop(
    "No valid records found in the CSV file."
  )
  
}


cat("\n====================================\n")
cat("DATASET LOADED SUCCESSFULLY\n")
cat("====================================\n")
cat("Rows:", nrow(chip_data), "\n")
cat("Columns:", ncol(chip_data), "\n")
cat("====================================\n\n")


# ============================================================
# 11. HELPER FUNCTIONS
# ============================================================

safe_sum <- function(x) {
  
  if (
    length(x) == 0 ||
    all(is.na(x))
  ) {
    return(NA_real_)
  }
  
  sum(
    x,
    na.rm = TRUE
  )
  
}


safe_mean <- function(x) {
  
  if (
    length(x) == 0 ||
    all(is.na(x))
  ) {
    return(NA_real_)
  }
  
  mean(
    x,
    na.rm = TRUE
  )
  
}


format_number <- function(
    x,
    digits = 0
) {
  
  if (
    length(x) == 0 ||
    all(is.na(x))
  ) {
    return("N/A")
  }
  
  format(
    round(
      x[1],
      digits
    ),
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
  
}


# ============================================================
# 12. USER INTERFACE
# ============================================================

ui <- fluidPage(
  
  # ----------------------------------------------------------
  # STYLE
  # ----------------------------------------------------------
  
  tags$head(
    
    tags$title(
      "AI Chip Market Analytics"
    ),
    
    tags$style(
      HTML(
        "
        body {
          background-color: #f4f7fb;
          font-family: Arial, sans-serif;
        }

        .dashboard-header {
          background: linear-gradient(
            135deg,
            #17365d,
            #2878a8
          );

          color: white;

          padding: 28px;

          margin: -15px -15px 20px -15px;

          border-radius: 0 0 14px 14px;
        }

        .dashboard-header h1 {
          margin-top: 0;
          font-weight: bold;
        }

        .dashboard-header p {
          margin-bottom: 0;
          color: #e4eef7;
        }

        .kpi-card {
          background: white;

          border-radius: 12px;

          padding: 18px;

          margin-bottom: 18px;

          min-height: 105px;

          border-left: 5px solid #2878a8;

          box-shadow:
            0 3px 10px
            rgba(0,0,0,0.08);
        }

        .kpi-label {
          color: #6b7785;

          font-size: 13px;

          margin-bottom: 10px;
        }

        .kpi-value {
          color: #17365d;

          font-size: 23px;

          font-weight: bold;
        }

        .chart-box {
          background: white;

          border-radius: 12px;

          padding: 18px;

          margin-bottom: 20px;

          border: 1px solid #e2e8f0;

          box-shadow:
            0 2px 8px
            rgba(0,0,0,0.05);
        }

        .chart-title {
          color: #17365d;

          font-size: 17px;

          font-weight: bold;

          margin-bottom: 15px;
        }

        .btn-primary {
          background-color: #2878a8;

          border-color: #2878a8;
        }

        .btn-primary:hover {
          background-color: #17365d;

          border-color: #17365d;
        }
        "
      )
    )
    
  ),
  
  
  # ----------------------------------------------------------
  # HEADER
  # ----------------------------------------------------------
  
  div(
    class = "dashboard-header",
    
    h1(
      "AI Chip Market Analytics Dashboard"
    ),
    
    p(
      "Interactive analysis of AI chip market, vendors, performance and revenue"
    )
    
  ),
  
  
  # ----------------------------------------------------------
  # SIDEBAR LAYOUT
  # ----------------------------------------------------------
  
  sidebarLayout(
    
    # ========================================================
    # SIDEBAR
    # ========================================================
    
    sidebarPanel(
      
      width = 3,
      
      h4(
        "Dashboard Filters"
      ),
      
      hr(),
      
      
      selectInput(
        
        inputId = "year_filter",
        
        label = "Select Year",
        
        choices = c(
          "All Years" = "ALL",
          sort(
            unique(
              chip_data$YEAR
            )
          )
        ),
        
        selected = "ALL"
        
      ),
      
      
      selectInput(
        
        inputId = "vendor_filter",
        
        label = "Select Vendor",
        
        choices = c(
          "All Vendors" = "ALL",
          sort(
            unique(
              chip_data$VENDOR
            )
          )
        ),
        
        selected = "ALL"
        
      ),
      
      
      uiOutput(
        "chip_filter_ui"
      ),
      
      
      br(),
      
      
      actionButton(
        
        inputId = "reset_filters",
        
        label = "Reset Filters",
        
        class = "btn-primary",
        
        width = "100%"
        
      ),
      
      
      hr(),
      
      
      helpText(
        "All charts and KPI cards automatically update when filters are changed."
      )
      
    ),
    
    
    # ========================================================
    # MAIN PANEL
    # ========================================================
    
    mainPanel(
      
      width = 9,
      
      
      # ======================================================
      # KPI CARDS
      # ======================================================
      
      fluidRow(
        
        column(
          
          3,
          
          div(
            class = "kpi-card",
            
            div(
              class = "kpi-label",
              "TOTAL RECORDS"
            ),
            
            div(
              class = "kpi-value",
              
              textOutput(
                "kpi_records",
                inline = TRUE
              )
              
            )
            
          )
          
        ),
        
        
        column(
          
          3,
          
          div(
            class = "kpi-card",
            
            div(
              class = "kpi-label",
              "NUMBER OF VENDORS"
            ),
            
            div(
              class = "kpi-value",
              
              textOutput(
                "kpi_vendors",
                inline = TRUE
              )
              
            )
            
          )
          
        ),
        
        
        column(
          
          3,
          
          div(
            class = "kpi-card",
            
            div(
              class = "kpi-label",
              "ESTIMATED REVENUE"
            ),
            
            div(
              class = "kpi-value",
              
              textOutput(
                "kpi_revenue",
                inline = TRUE
              )
              
            )
            
          )
          
        ),
        
        
        column(
          
          3,
          
          div(
            class = "kpi-card",
            
            div(
              class = "kpi-label",
              "AVERAGE FP16 TFLOPS"
            ),
            
            div(
              class = "kpi-value",
              
              textOutput(
                "kpi_fp16",
                inline = TRUE
              )
              
            )
            
          )
          
        )
        
      ),
      
      
      # ======================================================
      # DASHBOARD TABS
      # ======================================================
      
      tabsetPanel(
        
        # ====================================================
        # OVERVIEW
        # ====================================================
        
        tabPanel(
          
          "Overview",
          
          br(),
          
          
          fluidRow(
            
            column(
              
              6,
              
              div(
                class = "chart-box",
                
                div(
                  class = "chart-title",
                  "Estimated Revenue by Year"
                ),
                
                plotOutput(
                  "year_revenue_plot",
                  height = "320px"
                )
                
              )
              
            ),
            
            
            column(
              
              6,
              
              div(
                class = "chart-box",
                
                div(
                  class = "chart-title",
                  "Estimated Revenue by Vendor"
                ),
                
                plotOutput(
                  "vendor_revenue_plot",
                  height = "320px"
                )
                
              )
              
            )
            
          ),
          
          
          fluidRow(
            
            column(
              
              6,
              
              div(
                class = "chart-box",
                
                div(
                  class = "chart-title",
                  "Estimated Shipments by Vendor"
                ),
                
                plotOutput(
                  "vendor_shipments_plot",
                  height = "320px"
                )
                
              )
              
            ),
            
            
            column(
              
              6,
              
              div(
                class = "chart-box",
                
                div(
                  class = "chart-title",
                  "Memory vs FP16 Performance"
                ),
                
                plotOutput(
                  "memory_performance_plot",
                  height = "320px"
                )
                
              )
              
            )
            
          )
          
        ),
        
        
        # ====================================================
        # VENDOR ANALYSIS
        # ====================================================
        
        tabPanel(
          
          "Vendor Analysis",
          
          br(),
          
          
          div(
            class = "chart-box",
            
            div(
              class = "chart-title",
              "Vendor Summary"
            ),
            
            DTOutput(
              "vendor_summary_table"
            )
            
          ),
          
          
          div(
            class = "chart-box",
            
            div(
              class = "chart-title",
              "Average Selling Price by Vendor"
            ),
            
            plotOutput(
              "asp_plot",
              height = "400px"
            )
            
          )
          
        ),
        
        
        # ====================================================
        # CHIP PERFORMANCE
        # ====================================================
        
        tabPanel(
          
          "Chip Performance",
          
          br(),
          
          
          fluidRow(
            
            column(
              
              6,
              
              div(
                class = "chart-box",
                
                div(
                  class = "chart-title",
                  "FP16 Performance by Chip"
                ),
                
                plotOutput(
                  "chip_fp16_plot",
                  height = "420px"
                )
                
              )
              
            ),
            
            
            column(
              
              6,
              
              div(
                class = "chart-box",
                
                div(
                  class = "chart-title",
                  "Power Consumption by Chip"
                ),
                
                plotOutput(
                  "chip_power_plot",
                  height = "420px"
                )
                
              )
              
            )
            
          ),
          
          
          div(
            class = "chart-box",
            
            div(
              class = "chart-title",
              "Revenue vs Estimated Shipments"
            ),
            
            plotOutput(
              "revenue_shipments_plot",
              height = "400px"
            )
            
          )
          
        ),
        
        
        # ====================================================
        # DATASET
        # ====================================================
        
        tabPanel(
          
          "Dataset",
          
          br(),
          
          
          div(
            class = "chart-box",
            
            div(
              class = "chart-title",
              "AI Chip Market Dataset"
            ),
            
            DTOutput(
              "dataset_table"
            )
            
          )
          
        )
        
      )
      
    )
    
  )
  
)


# ============================================================
# 13. SERVER
# ============================================================

server <- function(
    input,
    output,
    session
) {
  
  
  # ==========================================================
  # CHIP FILTER
  # ==========================================================
  
  output$chip_filter_ui <- renderUI({
    
    d <- chip_data
    
    
    if (
      !is.null(input$year_filter) &&
      input$year_filter != "ALL"
    ) {
      
      d <- d[
        d$YEAR ==
          as.numeric(
            input$year_filter
          ),
        ,
        drop = FALSE
      ]
      
    }
    
    
    if (
      !is.null(input$vendor_filter) &&
      input$vendor_filter != "ALL"
    ) {
      
      d <- d[
        d$VENDOR ==
          input$vendor_filter,
        ,
        drop = FALSE
      ]
      
    }
    
    
    chips <- sort(
      unique(
        d$CHIP_NAME
      )
    )
    
    
    selectInput(
      
      "chip_filter",
      
      "Select Chip",
      
      choices = c(
        "All Chips" = "ALL",
        chips
      ),
      
      selected = "ALL"
      
    )
    
  })
  
  
  # ==========================================================
  # RESET BUTTON
  # ==========================================================
  
  observeEvent(
    
    input$reset_filters,
    
    {
      
      updateSelectInput(
        session,
        "year_filter",
        selected = "ALL"
      )
      
      
      updateSelectInput(
        session,
        "vendor_filter",
        selected = "ALL"
      )
      
      
      updateSelectInput(
        session,
        "chip_filter",
        selected = "ALL"
      )
      
    }
    
  )
  
  
  # ==========================================================
  # FILTERED DATA
  # ==========================================================
  
  filtered_data <- reactive({
    
    d <- chip_data
    
    
    # YEAR
    
    if (
      !is.null(input$year_filter) &&
      input$year_filter != "ALL"
    ) {
      
      d <- d[
        d$YEAR ==
          as.numeric(
            input$year_filter
          ),
        ,
        drop = FALSE
      ]
      
    }
    
    
    # VENDOR
    
    if (
      !is.null(input$vendor_filter) &&
      input$vendor_filter != "ALL"
    ) {
      
      d <- d[
        d$VENDOR ==
          input$vendor_filter,
        ,
        drop = FALSE
      ]
      
    }
    
    
    # CHIP
    
    if (
      !is.null(input$chip_filter) &&
      input$chip_filter != "ALL"
    ) {
      
      d <- d[
        d$CHIP_NAME ==
          input$chip_filter,
        ,
        drop = FALSE
      ]
      
    }
    
    
    d
    
  })
  
  
  # ==========================================================
  # KPI - RECORDS
  # ==========================================================
  
  output$kpi_records <- renderText({
    
    format_number(
      nrow(
        filtered_data()
      ),
      0
    )
    
  })
  
  
  # ==========================================================
  # KPI - VENDORS
  # ==========================================================
  
  output$kpi_vendors <- renderText({
    
    format_number(
      
      length(
        unique(
          filtered_data()$VENDOR
        )
      ),
      
      0
      
    )
    
  })
  
  
  # ==========================================================
  # KPI - REVENUE
  # ==========================================================
  
  output$kpi_revenue <- renderText({
    
    format_number(
      
      safe_sum(
        filtered_data()$ESTIMATED_REVENUE_USD
      ),
      
      1
      
    )
    
  })
  
  
  # ==========================================================
  # KPI - FP16
  # ==========================================================
  
  output$kpi_fp16 <- renderText({
    
    format_number(
      
      safe_mean(
        filtered_data()$FP16_TFLOPS
      ),
      
      2
      
    )
    
  })
  
  
  # ==========================================================
  # REVENUE BY YEAR
  # ==========================================================
  
  output$year_revenue_plot <- renderPlot({
    
    d <- filtered_data()
    
    
    d <- d[
      !is.na(d$YEAR) &
        !is.na(
          d$ESTIMATED_REVENUE_USD
        ),
      ,
      drop = FALSE
    ]
    
    
    validate(
      need(
        nrow(d) > 0,
        "No revenue data available."
      )
    )
    
    
    summary_data <- aggregate(
      
      ESTIMATED_REVENUE_USD ~ YEAR,
      
      data = d,
      
      FUN = sum,
      
      na.rm = TRUE
      
    )
    
    
    ggplot(
      
      summary_data,
      
      aes(
        x = YEAR,
        y = ESTIMATED_REVENUE_USD
      )
      
    ) +
      
      geom_line(
        linewidth = 1.2
      ) +
      
      geom_point(
        size = 3
      ) +
      
      labs(
        
        x = "Year",
        
        y = "Estimated Revenue"
        
      ) +
      
      theme_minimal(
        base_size = 12
      )
    
  })
  
  
  # ==========================================================
  # REVENUE BY VENDOR
  # ==========================================================
  
  output$vendor_revenue_plot <- renderPlot({
    
    d <- filtered_data()
    
    
    d <- d[
      !is.na(d$VENDOR) &
        !is.na(
          d$ESTIMATED_REVENUE_USD
        ),
      ,
      drop = FALSE
    ]
    
    
    validate(
      need(
        nrow(d) > 0,
        "No revenue data available."
      )
    )
    
    
    summary_data <- aggregate(
      
      ESTIMATED_REVENUE_USD ~ VENDOR,
      
      data = d,
      
      FUN = sum,
      
      na.rm = TRUE
      
    )
    
    
    summary_data <- summary_data[
      order(
        summary_data$ESTIMATED_REVENUE_USD
      ),
    ]
    
    
    ggplot(
      
      summary_data,
      
      aes(
        
        x = reorder(
          VENDOR,
          ESTIMATED_REVENUE_USD
        ),
        
        y = ESTIMATED_REVENUE_USD
        
      )
      
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      labs(
        
        x = "Vendor",
        
        y = "Estimated Revenue"
        
      ) +
      
      theme_minimal(
        base_size = 12
      )
    
  })
  
  
  # ==========================================================
  # SHIPMENTS BY VENDOR
  # ==========================================================
  
  output$vendor_shipments_plot <- renderPlot({
    
    d <- filtered_data()
    
    
    d <- d[
      !is.na(d$VENDOR) &
        !is.na(
          d$ESTIMATED_SHIPMENTS
        ),
      ,
      drop = FALSE
    ]
    
    
    validate(
      need(
        nrow(d) > 0,
        "No shipment data available."
      )
    )
    
    
    summary_data <- aggregate(
      
      ESTIMATED_SHIPMENTS ~ VENDOR,
      
      data = d,
      
      FUN = sum,
      
      na.rm = TRUE
      
    )
    
    
    summary_data <- summary_data[
      order(
        summary_data$ESTIMATED_SHIPMENTS
      ),
    ]
    
    
    ggplot(
      
      summary_data,
      
      aes(
        
        x = reorder(
          VENDOR,
          ESTIMATED_SHIPMENTS
        ),
        
        y = ESTIMATED_SHIPMENTS
        
      )
      
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      labs(
        
        x = "Vendor",
        
        y = "Estimated Shipments"
        
      ) +
      
      theme_minimal(
        base_size = 12
      )
    
  })
  
  
  # ==========================================================
  # MEMORY VS FP16
  # ==========================================================
  
  output$memory_performance_plot <- renderPlot({
    
    d <- filtered_data()
    
    
    d <- d[
      !is.na(d$MEMORY_GB) &
        !is.na(d$FP16_TFLOPS),
      ,
      drop = FALSE
    ]
    
    
    validate(
      need(
        nrow(d) > 0,
        "No performance data available."
      )
    )
    
    
    ggplot(
      
      d,
      
      aes(
        x = MEMORY_GB,
        y = FP16_TFLOPS,
        color = VENDOR
      )
      
    ) +
      
      geom_point(
        size = 2.5,
        alpha = 0.75
      ) +
      
      labs(
        
        x = "Memory (GB)",
        
        y = "FP16 TFLOPS",
        
        color = "Vendor"
        
      ) +
      
      theme_minimal(
        base_size = 12
      )
    
  })
  
  
  # ==========================================================
  # VENDOR SUMMARY
  # ==========================================================
  
  output$vendor_summary_table <- renderDT({
    
    d <- filtered_data()
    
    
    validate(
      need(
        nrow(d) > 0,
        "No matching records."
      )
    )
    
    
    summary_data <- aggregate(
      
      cbind(
        
        ESTIMATED_SHIPMENTS,
        
        ESTIMATED_REVENUE_USD,
        
        ESTIMATED_ASP_USD,
        
        FP16_TFLOPS,
        
        MEMORY_GB,
        
        TDP_WATTS
        
      )
      
      ~ VENDOR,
      
      data = d,
      
      FUN = safe_mean
      
    )
    
    
    names(summary_data) <- c(
      
      "Vendor",
      
      "Average Shipments",
      
      "Average Revenue",
      
      "Average ASP USD",
      
      "Average FP16 TFLOPS",
      
      "Average Memory GB",
      
      "Average TDP Watts"
      
    )
    
    
    datatable(
      
      summary_data,
      
      rownames = FALSE,
      
      options = list(
        
        pageLength = 8,
        
        scrollX = TRUE,
        
        autoWidth = TRUE
        
      )
      
    ) |>
      
      formatRound(
        
        columns = 2:ncol(
          summary_data
        ),
        
        digits = 2
        
      )
    
  })
  
  
  # ==========================================================
  # ASP BY VENDOR
  # ==========================================================
  
  output$asp_plot <- renderPlot({
    
    d <- filtered_data()
    
    
    d <- d[
      !is.na(d$VENDOR) &
        !is.na(
          d$ESTIMATED_ASP_USD
        ),
      ,
      drop = FALSE
    ]
    
    
    validate(
      need(
        nrow(d) > 0,
        "No ASP data available."
      )
    )
    
    
    summary_data <- aggregate(
      
      ESTIMATED_ASP_USD ~ VENDOR,
      
      data = d,
      
      FUN = safe_mean
      
    )
    
    
    summary_data <- summary_data[
      order(
        summary_data$ESTIMATED_ASP_USD
      ),
    ]
    
    
    ggplot(
      
      summary_data,
      
      aes(
        
        x = reorder(
          VENDOR,
          ESTIMATED_ASP_USD
        ),
        
        y = ESTIMATED_ASP_USD
        
      )
      
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      labs(
        
        x = "Vendor",
        
        y = "Average ASP (USD)"
        
      ) +
      
      theme_minimal(
        base_size = 12
      )
    
  })
  
  
  # ==========================================================
  # FP16 BY CHIP
  # ==========================================================
  
  output$chip_fp16_plot <- renderPlot({
    
    d <- filtered_data()
    
    
    d <- d[
      !is.na(d$CHIP_NAME) &
        !is.na(d$FP16_TFLOPS),
      ,
      drop = FALSE
    ]
    
    
    validate(
      need(
        nrow(d) > 0,
        "No FP16 data available."
      )
    )
    
    
    summary_data <- aggregate(
      
      FP16_TFLOPS ~ CHIP_NAME,
      
      data = d,
      
      FUN = safe_mean
      
    )
    
    
    summary_data <- summary_data[
      order(
        summary_data$FP16_TFLOPS
      ),
    ]
    
    
    ggplot(
      
      summary_data,
      
      aes(
        
        x = reorder(
          CHIP_NAME,
          FP16_TFLOPS
        ),
        
        y = FP16_TFLOPS
        
      )
      
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      labs(
        
        x = "Chip",
        
        y = "Average FP16 TFLOPS"
        
      ) +
      
      theme_minimal(
        base_size = 10
      )
    
  })
  
  
  # ==========================================================
  # POWER BY CHIP
  # ==========================================================
  
  output$chip_power_plot <- renderPlot({
    
    d <- filtered_data()
    
    
    d <- d[
      !is.na(d$CHIP_NAME) &
        !is.na(d$TDP_WATTS),
      ,
      drop = FALSE
    ]
    
    
    validate(
      need(
        nrow(d) > 0,
        "No power data available."
      )
    )
    
    
    summary_data <- aggregate(
      
      TDP_WATTS ~ CHIP_NAME,
      
      data = d,
      
      FUN = safe_mean
      
    )
    
    
    summary_data <- summary_data[
      order(
        summary_data$TDP_WATTS
      ),
    ]
    
    
    ggplot(
      
      summary_data,
      
      aes(
        
        x = reorder(
          CHIP_NAME,
          TDP_WATTS
        ),
        
        y = TDP_WATTS
        
      )
      
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      labs(
        
        x = "Chip",
        
        y = "Average TDP Watts"
        
      ) +
      
      theme_minimal(
        base_size = 10
      )
    
  })
  
  
  # ==========================================================
  # REVENUE VS SHIPMENTS
  # ==========================================================
  
  output$revenue_shipments_plot <- renderPlot({
    
    d <- filtered_data()
    
    
    d <- d[
      !is.na(
        d$ESTIMATED_SHIPMENTS
      ) &
        !is.na(
          d$ESTIMATED_REVENUE_USD
        ),
      ,
      drop = FALSE
    ]
    
    
    validate(
      need(
        nrow(d) > 0,
        "No revenue/shipment data available."
      )
    )
    
    
    ggplot(
      
      d,
      
      aes(
        
        x = ESTIMATED_SHIPMENTS,
        
        y = ESTIMATED_REVENUE_USD,
        
        color = VENDOR
        
      )
      
    ) +
      
      geom_point(
        size = 2.5,
        alpha = 0.75
      ) +
      
      labs(
        
        x = "Estimated Shipments",
        
        y = "Estimated Revenue",
        
        color = "Vendor"
        
      ) +
      
      theme_minimal(
        base_size = 12
      )
    
  })
  
  
  # ==========================================================
  # DATASET TABLE
  # ==========================================================
  
  output$dataset_table <- renderDT({
    
    datatable(
      
      filtered_data(),
      
      rownames = FALSE,
      
      filter = "top",
      
      options = list(
        
        pageLength = 10,
        
        scrollX = TRUE,
        
        autoWidth = TRUE
        
      )
      
    )
    
  })
  
}


# ============================================================
# 14. START SHINY APPLICATION
# ============================================================

shiny::shinyApp(
  
  ui = ui,
  
  server = server
  
)