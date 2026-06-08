practitioner_guide_ui <- function() {
  tabPanel(
    "Practitioner Guide",
    
    h3("Practitioner Guide & Communication"),
    
    wellPanel(
      h4("Intended User", style = "font-weight: 700;"),
      p(
        "This dashboard is designed for a professional hockey performance staff, including strength and conditioning coaches, sport scientists, athletic trainers, and performance directors. The intended user is someone responsible for monitoring daily readiness, interpreting training load responses, and communicating athlete status to coaches and support staff."
      )
    ),
    
    wellPanel(
      h4("Use-Case Narrative", style = "font-weight: 700;"),
      p(
        "The dashboard supports daily and weekly performance monitoring across a simulated hockey roster. The main workflow begins with a team-level readiness scan, moves into individual player investigation when a concern appears, and finishes with squad-level load management to identify position-based loading patterns and ACWR risk zones."
      ),
      p(
        "A practitioner can use the dashboard before a training session or staff meeting to identify players who may need follow-up, understand whether low readiness is related to wellness or training load, and communicate clear action points to the coaching staff."
      )
    ),
    
    fluidRow(
      column(
        width = 4,
        wellPanel(
          h4("Tab 1: Team Overview", style = "font-weight: 700;"),
          tags$ul(
            tags$li("Provides a roster-wide readiness snapshot."),
            tags$li("Date selector updates readiness cards, follow-up list, and detail table."),
            tags$li("Traffic-light heat map shows Green, Amber, and Red readiness status across the full monitoring period."),
            tags$li("Clicking a heat-map tile updates the selected date and highlights the clicked player in the table."),
            tags$li("Team mean daily load trend shows squad loading patterns and game-day markers.")
          )
        )
      ),
      
      column(
        width = 4,
        wellPanel(
          h4("Tab 2: Player Deep Dive", style = "font-weight: 700;"),
          tags$ul(
            tags$li("Allows staff to select an individual player and date range."),
            tags$li("Load + HRV view shows session AU bars with HRV z-score trend."),
            tags$li("Wellness view summarizes Hooper Index, sleep quality, and strain score using KPI cards."),
            tags$li("Weekly wellness trend shows how total wellness strain changes over time."),
            tags$li("Radar chart displays the selected week's wellness component profile.")
          )
        )
      ),
      
      column(
        width = 4,
        wellPanel(
          h4("Tab 3: Squad Load Management", style = "font-weight: 700;"),
          tags$ul(
            tags$li("Monitors squad-level weekly load distribution by position."),
            tags$li("Violin plots show how weekly load varies across position groups."),
            tags$li("ACWR dot chart identifies players in low, optimal, or high workload zones."),
            tags$li("The 0.8–1.3 ACWR band is treated as the optimal loading zone."),
            tags$li("Filtered summary table can be downloaded for staff reporting.")
          )
        )
      )
    ),
    
    wellPanel(
      h4("Communication Notes for Practitioners", style = "font-weight: 700;"),
      tags$ul(
        tags$li("Green status suggests the player is generally responding well to current load."),
        tags$li("Amber status suggests the player may need monitoring, conversation, or modified exposure depending on context."),
        tags$li("Red status does not automatically mean the athlete should be removed from training, but it should trigger follow-up from performance or medical staff."),
        tags$li("ACWR should be interpreted alongside athlete history, positional demands, game schedule, and coaching context."),
        tags$li("Early ACWR weeks may show insufficient baseline because enough prior weekly load history is not yet available.")
      )
    )
  )
}