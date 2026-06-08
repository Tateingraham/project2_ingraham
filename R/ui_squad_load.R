squad_load_ui <- function(gps) {
  tabPanel(
    "Squad Load Management",
    
    h3("Squad Load Management"),
    p(
      "Use this tab to monitor weekly squad load distributions, position-level loading patterns, and ACWR risk zones."
    ),
    
    wellPanel(
      fluidRow(
        column(
          width = 4,
          selectInput(
            "squad_week",
            "Select Week",
            choices = sort(unique(gps$week)),
            selected = max(gps$week, na.rm = TRUE)
          )
        ),
        
        column(
          width = 4,
          selectInput(
            "squad_position",
            "Position Filter",
            choices = c("All", sort(unique(gps$position))),
            selected = "All"
          )
        ),
        
        column(
          width = 4,
          br(),
          downloadButton(
            "download_squad_summary",
            "Download Summary Table"
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 6,
        
        wellPanel(
          h4("Weekly Load Distribution by Position", style = "font-weight: 700; margin-bottom: 4px;"),
          p(
            "Violin plot shows the distribution of player weekly load by position for the selected week.",
            style = "margin-top: 0px; color: #555;"
          ),
          plotlyOutput("weekly_load_distribution", height = "450px")
        )
      ),
      
      column(
        width = 6,
        
        wellPanel(
          h4("ACWR Monitoring", style = "font-weight: 700; margin-bottom: 4px;"),
          p(
            "Each dot represents one player's weekly ACWR. The shaded band marks the 0.8–1.3 optimal loading zone. Early weeks may show innacurate points until enough prior load history is available.",
            style = "margin-top: 0px; color: #555;"
          ),
          plotlyOutput("acwr_dot_chart", height = "450px")
        )
      )
    ),
    
    wellPanel(
      h4("Squad Weekly Summary Table", style = "font-weight: 700; margin-bottom: 4px;"),
      p(
        "Summary table includes weekly load, position, ACWR, and readiness context for the selected filters.",
        style = "margin-top: 0px; color: #555;"
      ),
      DTOutput("squad_summary_table")
    )
  )
}