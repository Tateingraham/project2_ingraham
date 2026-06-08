player_deep_dive_ui <- function(gps) {
  tabPanel(
    "Player Deep Dive",
    
    h3("Player Deep Dive"),
    p("Use this tab to review an individual player's load, HRV response, and wellness profile across the selected date range."),
    
    wellPanel(
      fluidRow(
        column(
          width = 4,
          selectInput(
            "player",
            "Select Player",
            choices = sort(unique(gps$player_id)),
            selected = sort(unique(gps$player_id))[1]
          )
        ),
        
        column(
          width = 5,
          dateRangeInput(
            "player_date_range",
            "Date Range",
            start = min(gps$date, na.rm = TRUE),
            end = max(gps$date, na.rm = TRUE),
            min = min(gps$date, na.rm = TRUE),
            max = max(gps$date, na.rm = TRUE)
          )
        ),
        
        column(
          width = 3,
          radioButtons(
            "player_metric",
            "Primary View",
            choices = c(
              "Load + HRV" = "load_hrv",
              "Wellness" = "wellness"
            ),
            selected = "load_hrv",
            inline = TRUE
          )
        )
      )
    ),
    
    conditionalPanel(
      condition = "input.player_metric == 'load_hrv'",
      
      wellPanel(
        h4("Session Load and HRV Profile", style = "font-weight: 700; margin-bottom: 4px;"),
        p(
          "Bars show session load. The blue line shows HRV z-score scaled onto the load axis; hover for exact HRV values.",
          style = "margin-top: 0px; color: #555;"
        ),
        plotlyOutput("player_load_hrv_plot", height = "600px")
      )
    ),
    
    conditionalPanel(
      condition = "input.player_metric == 'wellness'",
      
      wellPanel(
        h4("Weekly Wellness Profile", style = "font-weight: 700; margin-bottom: 4px;"),
        p(
          "Bars show weekly Hooper Index. The solid line shows average fatigue/stress/soreness strain, and the dashed line shows sleep quality. Lines are scaled to the Hooper axis for visual comparison.",
          style = "margin-top: 0px; color: #555; margin-bottom: 12px;"
        ),
        
        fluidRow(
          column(4, uiOutput("player_hooper_card")),
          column(4, uiOutput("player_sleep_card")),
          column(4, uiOutput("player_strain_card"))
        ),
        
        br(),
        
        div(
          style = "display:flex; gap:28px; align-items:center; margin-bottom:10px; font-size:13px; color:#333;",
          
          div(
            span(style = "display:inline-block; width:24px; height:10px; background:#D6E2F1; margin-right:7px; vertical-align:middle;"),
            "Hooper Index"
          ),
          
          div(
            span(style = "display:inline-block; width:26px; height:0px; border-top:4px dashed #2E7D32; margin-right:7px; vertical-align:middle;"),
            "Sleep Quality"
          ),
          
          div(
            span(style = "display:inline-block; width:26px; height:4px; background:#C58B00; margin-right:7px; vertical-align:middle;"),
            "Average Strain Score"
          )
        ),
        
        fluidRow(
          column(
            width = 8,
            plotlyOutput("player_wellness_plot", height = "500px")
          ),
          
          column(
            width = 4,
            
            wellPanel(
              h4("Weekly Wellness Radar", style = "font-weight: 700; margin-bottom: 4px;"),
              
              selectInput(
                "radar_week",
                "Radar Week",
                choices = sort(unique(gps$week)),
                selected = min(gps$week, na.rm = TRUE)
              ),
              
              p(
                "Radar chart shows the selected week's wellness component profile. Hooper Index is summarized in the card and trend chart. Fatigue, stress, and soreness use 1–7 scales; sleep quality uses a 1–5 scale.",
                style = "font-size: 12px; color: #555;"
              ),
              
              plotlyOutput("player_hooper_radar", height = "330px")
            )
          )
        )
      )
    )
  )
}
