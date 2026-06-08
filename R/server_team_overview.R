team_overview_server <- function(input, output, session, gps) {
  selected_date_reactive <- reactive({
    as.Date(input$date)
  })
  
  selected_day_data <- reactive({
    gps %>%
      filter(date == selected_date_reactive())
  })
  
  clicked_player <- reactive({
    click <- plotly::event_data("plotly_click", source = "readiness_grid")
    
    if (is.null(click) || is.null(click$key)) {
      return(NULL)
    }
    
    key_parts <- strsplit(as.character(click$key[1]), "\\|\\|")[[1]]
    
    if (length(key_parts) < 1) {
      return(NULL)
    }
    
    key_parts[1]
  })
  
  observeEvent(
    plotly::event_data("plotly_click", source = "readiness_grid"),
    {
      click <- plotly::event_data("plotly_click", source = "readiness_grid")
      
      if (is.null(click) || is.null(click$key)) {
        return(NULL)
      }
      
      key_parts <- strsplit(as.character(click$key[1]), "\\|\\|")[[1]]
      
      if (length(key_parts) < 2) {
        return(NULL)
      }
      
      clicked_date <- key_parts[2]
      
      updateSelectInput(
        session = session,
        inputId = "date",
        choices = as.character(sort(unique(gps$date))),
        selected = clicked_date
      )
    },
    ignoreInit = TRUE
  )
  
  output$selected_date_title <- renderText({
    paste(
      "Selected Date Readiness:",
      format(selected_date_reactive(), "%b %d, %Y")
    )
  })
  
  output$green_count <- renderUI({
    n <- selected_day_data() %>% filter(readiness_status == "Green") %>% nrow()
    
    div(
      style = "background:#D9F0D3; padding:8px; border-radius:8px; text-align:center;",
      h4(n, style = "margin:0; font-weight:800;"),
      tags$small("Green")
    )
  })
  
  output$amber_count <- renderUI({
    n <- selected_day_data() %>% filter(readiness_status == "Amber") %>% nrow()
    
    div(
      style = "background:#FEE08B; padding:8px; border-radius:8px; text-align:center;",
      h4(n, style = "margin:0; font-weight:800;"),
      tags$small("Amber")
    )
  })
  
  output$red_count <- renderUI({
    n <- selected_day_data() %>% filter(readiness_status == "Red") %>% nrow()
    
    div(
      style = "background:#F4A6A6; padding:8px; border-radius:8px; text-align:center;",
      h4(n, style = "margin:0; font-weight:800;"),
      tags$small("Red")
    )
  })
  
  output$red_flag_players <- renderUI({
    red_players <- selected_day_data() %>%
      filter(readiness_status == "Red") %>%
      arrange(hrv_zscore) %>%
      select(player_id, position, hrv_zscore, daily_load)
    
    if (nrow(red_players) == 0) {
      return(
        div(
          style = "color:#2E7D32; font-weight:600;",
          "No red-status players on selected date."
        )
      )
    }
    
    tags$ul(
      style = "padding-left: 18px; margin-bottom: 0;",
      lapply(seq_len(nrow(red_players)), function(i) {
        tags$li(
          paste0(
            red_players$player_id[i],
            " — ",
            red_players$position[i],
            " | HRV: ",
            round(red_players$hrv_zscore[i], 2),
            " | Load: ",
            round(red_players$daily_load[i], 0),
            " AU"
          )
        )
      })
    )
  })
  output$readiness_grid <- renderPlotly({
    grid_data <- gps %>%
      filter(!is.na(date)) %>%
      mutate(
        status_value = case_when(
          readiness_status == "Green" ~ 3,
          readiness_status == "Amber" ~ 2,
          readiness_status == "Red" ~ 1,
          TRUE ~ NA_real_
        ),
        hover_text = paste0(
          "Player: ", player_id,
          "<br>Position: ", position,
          "<br>Date: ", format(date, "%b %d, %Y"),
          "<br>Status: ", readiness_status,
          "<br>HRV z-score: ", round(hrv_zscore, 2),
          "<br>Hooper Index: ", hooper_index_total,
          "<br>Session: ", session_type,
          "<br>Load: ", round(daily_load, 0), " AU"
        )
      )
    week_breaks <- seq(
      from = min(grid_data$date, na.rm = TRUE),
      to = max(grid_data$date, na.rm = TRUE),
      by = "1 week"
    )
    p <- ggplot(
      grid_data,
      aes(
        x = date,
        y = fct_rev(player_id),
        fill = readiness_status,
        text = hover_text,
        key = paste(player_id, as.character(date), sep = "||")
      )
    ) +
      geom_tile(color = "white", linewidth = 0.35) +
      geom_vline(
        xintercept = week_breaks,
        linetype = "dashed",
        color = "gray40",
        linewidth = 0.4,
        alpha = 0.7
      ) +
      scale_fill_manual(
        values = c(
          "Green" = "#D9F0D3",
          "Amber" = "#FEE08B",
          "Red" = "#F4A6A6"
        ),
        name = "Readiness"
      ) +
      scale_x_date(
        date_breaks = "1 week",
        date_labels = "%b %d"
      ) +
      labs(
        title = NULL,
        x = "Date",
        y = "Player"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = "text", source = "readiness_grid")
  })
  output$team_load_trend <- renderPlotly({
    
    team_daily <- gps %>%
      filter(training_day == TRUE | game_day == TRUE) %>%
      group_by(date) %>%
      summarise(
        team_mean_load = mean(daily_load, na.rm = TRUE),
        team_total_load = sum(daily_load, na.rm = TRUE),
        session_type = paste(unique(session_type), collapse = ", "),
        n_players = n_distinct(player_id),
        .groups = "drop"
      ) %>%
      arrange(date) %>%
      mutate(
        game_day = stringr::str_detect(session_type, "Game"),
        hover_text = paste0(
          "Date: ", format(date, "%b %d, %Y"),
          "<br>Session Type: ", session_type,
          "<br>Mean Load: ", round(team_mean_load, 0), " AU",
          "<br>Total Team Load: ", round(team_total_load, 0), " AU",
          "<br>Players Recorded: ", n_players
        )
      )
    
    p <- ggplot(team_daily, aes(x = date, y = team_mean_load)) +
      geom_col(
        aes(text = hover_text),
        fill = "#D6E2F1",
        alpha = 0.85,
        width = 0.85
      ) +
      geom_smooth(
        aes(group = 1),
        method = "loess",
        se = FALSE,
        color = "#2C5985",
        linewidth = 1.5,
        span = 0.45
      ) +
      geom_point(
        data = team_daily %>% filter(game_day),
        aes(
          x = date,
          y = team_mean_load,
          text = hover_text
        ),
        inherit.aes = FALSE,
        color = "#B2182B",
        size = 4,
        shape = 18
      ) +
      scale_x_date(
        date_breaks = "1 week",
        date_labels = "%b %d",
        expand = expansion(mult = c(0.01, 0.03))
      ) +
      scale_y_continuous(
        labels = scales::comma,
        breaks = seq(250, 500, by = 50),
        expand = expansion(mult = c(0.03, 0.06))
      ) +
      coord_cartesian(
        ylim = c(250, 500),
        clip = "off"
      ) +
      labs(
        title = NULL,
        subtitle = NULL,
        x = "Date",
        y = "Mean Daily Load (AU)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 17),
        plot.subtitle = element_text(size = 11, color = "gray30"),
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hovermode = "closest",
        margin = list(l = 60, r = 25, t = 25, b = 100)
      )
  })
  
  output$preview_table <- renderDT({
    
    selected_date <- selected_date_reactive()
    selected_player <- clicked_player()
    
    readiness_table <- gps %>%
      filter(date == selected_date) %>%
      select(
        player_id,
        position,
        session_type,
        daily_load,
        hrv_zscore,
        readiness_status
      ) %>%
      mutate(
        clicked = if (is.null(selected_player)) {
          ""
        } else {
          if_else(player_id == selected_player, "Selected", "")
        },
        daily_load = round(daily_load, 0),
        hrv_zscore = round(hrv_zscore, 2)
      ) %>%
      rename(
        Player = player_id,
        Pos = position,
        Session = session_type,
        Load = daily_load,
        HRV = hrv_zscore,
        Status = readiness_status,
        Clicked = clicked
      ) %>%
      arrange(desc(Clicked), Status, desc(HRV))
    
    datatable(
      readiness_table,
      rownames = FALSE,
      options = list(
        pageLength = 23,
        paging = FALSE,
        ordering = TRUE,
        dom = "t",
        scrollX = TRUE,
        autoWidth = FALSE,
        columnDefs = list(
          list(width = "95px", targets = 0),
          list(width = "45px", targets = 1),
          list(width = "70px", targets = 2),
          list(width = "55px", targets = 3),
          list(width = "55px", targets = 4),
          list(width = "70px", targets = 5),
          list(visible = FALSE, targets = 6)
        )
      )
    ) %>%
      formatStyle(
        "Status",
        backgroundColor = styleEqual(
          c("Green", "Amber", "Red"),
          c("#D9F0D3", "#FEE08B", "#F4A6A6")
        ),
        fontWeight = "bold"
      ) %>%
      formatStyle(
        "Clicked",
        target = "row",
        backgroundColor = styleEqual(
          "Selected",
          "#E8F1FB"
        )
      )
  })
}