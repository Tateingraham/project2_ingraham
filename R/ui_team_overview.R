team_overview_ui <- function(gps) {
  tabPanel(
    "Team Overview",
    
    fluidRow(
      column(
        width = 4,
        
        wellPanel(
          selectInput(
            "date",
            "Select Date",
            choices = as.character(sort(unique(gps$date))),
            selected = as.character(min(gps$date, na.rm = TRUE))
          )
        ),
        
        fluidRow(
          column(4, uiOutput("green_count")),
          column(4, uiOutput("amber_count")),
          column(4, uiOutput("red_count"))
        ),
        
        br(),
        
        wellPanel(
          h4("Priority Follow-Up List", style = "font-weight: 700; margin-bottom: 6px;"),
          uiOutput("red_flag_players"),
          tags$small("Listed players are Red based on suppressed HRV, high fatigue, or high soreness.")
        ),
        
        wellPanel(
          h4(textOutput("selected_date_title"), style = "font-weight: 700; margin-bottom: 6px;"),
          div(
            class = "compact-table",
            DTOutput("preview_table", width = "100%")
          )
        )
      ),
      
      column(
        width = 8,
        
        h3("Team Readiness Snapshot"),
        
        h4("HRV Readiness Traffic-Light Grid"),
        p("Use this grid to scan daily readiness status across the roster. Dashed lines separate training weeks. Click on player/date square to highlight them in Readiness Table"),
        plotlyOutput("readiness_grid", height = "535px"),
        
        br(),
        
        div(
          style = "margin-top: 0px; margin-bottom: 2px;",
          h4("Team Mean Daily Load Trend", style = "margin-bottom: 2px;"),
          p(
            "Bars show daily mean load, the blue line shows the smoothed trend, and red diamonds mark game days.",
            style = "margin-top: 0px; margin-bottom: 0px;"
          )
        ),
        
        plotlyOutput("team_load_trend", height = "425px")
      )
    )
  )
}