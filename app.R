source("R/packages.R")
source("R/data_prep.R")

source("R/ui_team_overview.R")
source("R/ui_player_deep_dive.R")
source("R/ui_squad_load.R")
source("R/ui_practitioner_guide.R")

source("R/server_team_overview.R")
source("R/server_player_deep_dive.R")
source("R/server_squad_load.R")

gps <- load_data()

ui <- fluidPage(
  titlePanel("Hockey Readiness and Load Dashboard"),
  tags$style(HTML("
  .compact-table {
    font-size: 12px;
    white-space: nowrap;
  }

  .compact-table table.dataTable tbody td {
    white-space: nowrap;
    padding: 5px 5px;
    line-height: 1.25;
  }

  .compact-table table.dataTable thead th {
    white-space: nowrap;
    padding: 5px 5px;
    line-height: 1.25;
  }

  .compact-table .dataTables_wrapper {
    margin-top: 0px;
  }
")),
  
  tabsetPanel(
    practitioner_guide_ui(),
    team_overview_ui(gps),
    player_deep_dive_ui(gps),
    squad_load_ui(gps)
    )
)


server <- function(input, output, session) {
  team_overview_server(input, output, session, gps)
  player_deep_dive_server(input, output, session, gps)
  squad_load_server(input, output, session, gps)
}

shinyApp(ui, server)