library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(zoo)
library(scales)
library(viridis)

load_data <- function() {
  read_csv("data/NYR_sim_load_hrv_wk1-7.csv", show_col_types = FALSE) %>%
    mutate(
      date = lubridate::mdy(date),
      daily_load = session_au,
      high_speed_pct = high_speed_distance_m / total_distance_m,
      readiness_status = case_when(
        hrv_zscore >= -0.5 & hooper_index_total <= median(hooper_index_total, na.rm = TRUE) ~ "Green",
        hrv_zscore < -1.0 | perceived_fatigue_1to7 >= 6 | perceived_soreness_1to7 >= 6 ~ "Red",
        TRUE ~ "Amber"
      )
    ) %>%
    arrange(player_id, date)
}

gps <- load_data()

ui <- fluidPage(
  titlePanel("Project 2: Hockey Readiness and Load Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "date",
        "Select Date",
        choices = sort(unique(gps$date)),
        selected = max(gps$date, na.rm = TRUE)
      )
    ),
    mainPanel(
      h3("Dashboard setup successful"),
      DTOutput("preview_table")
    )
  )
)

server <- function(input, output, session) {
  output$preview_table <- renderDT({
    gps %>%
      filter(date == input$date) %>%
      select(
        player_id,
        position,
        session_type,
        daily_load,
        hrv_zscore,
        hooper_index_total,
        readiness_status
      )
  })
}

shinyApp(ui, server)