squad_load_server <- function(input, output, session, gps) {
  
  squad_filtered_week <- reactive({
    
    data <- gps %>%
      filter(week == as.numeric(input$squad_week))
    
    if (input$squad_position != "All") {
      data <- data %>%
        filter(position == input$squad_position)
    }
    
    data
  })
  
  weekly_player_load <- reactive({
    
    weekly_all <- gps %>%
      group_by(player_id, position, week) %>%
      summarise(
        weekly_load = sum(daily_load, na.rm = TRUE),
        mean_daily_load = mean(daily_load, na.rm = TRUE),
        sessions_recorded = n(),
        mean_hrv = mean(hrv_zscore, na.rm = TRUE),
        mean_hooper = mean(hooper_index_total, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(player_id, week) %>%
      group_by(player_id) %>%
      mutate(
        chronic_load = slider::slide_dbl(
          weekly_load,
          mean,
          .before = 3,
          .complete = FALSE,
          na.rm = TRUE
        ),
        acwr = if_else(
          chronic_load > 0,
          weekly_load / chronic_load,
          NA_real_
        )
      ) %>%
      ungroup()
    
    filtered <- weekly_all %>%
      filter(week == as.numeric(input$squad_week))
    
    if (input$squad_position != "All") {
      filtered <- filtered %>%
        filter(position == input$squad_position)
    }
    
    filtered %>%
      mutate(
        hover_text = paste0(
          "Player: ", player_id,
          "<br>Position: ", position,
          "<br>Week: ", week,
          "<br>Weekly Load: ", round(weekly_load, 0), " AU",
          "<br>Chronic Load: ", round(chronic_load, 0), " AU",
          "<br>ACWR: ", round(acwr, 2),
          "<br>Mean HRV z-score: ", round(mean_hrv, 2),
          "<br>Mean Hooper: ", round(mean_hooper, 1),
          "<br>Sessions Recorded: ", sessions_recorded
        )
      )
  })
  
  output$weekly_load_distribution <- renderPlotly({
    
    load_data <- weekly_player_load() %>%
      mutate(
        position = as.factor(position),
        hover_text = paste0(
          "Player: ", player_id,
          "<br>Position: ", position,
          "<br>Week: ", week,
          "<br>Weekly Load: ", round(weekly_load, 0), " AU",
          "<br>Mean Daily Load: ", round(mean_daily_load, 0), " AU",
          "<br>Sessions Recorded: ", sessions_recorded
        )
      )
    
    p <- ggplot(
      load_data,
      aes(
        x = position,
        y = weekly_load
      )
    ) +
      geom_violin(
        aes(fill = position),
        alpha = 0.55,
        trim = FALSE,
        color = "gray45"
      ) +
      geom_boxplot(
        width = 0.12,
        outlier.shape = NA,
        alpha = 0.75,
        color = "gray25"
      ) +
      geom_point(
        aes(text = hover_text),
        position = position_jitter(width = 0.06, height = 0),
        size = 2.4,
        alpha = 0.85,
        color = "#111111"
      ) +
      labs(
        title = NULL,
        x = "Position",
        y = "Weekly Load (AU)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hovermode = "closest",
        margin = list(l = 65, r = 25, t = 20, b = 70)
      )
  })
  
  output$acwr_dot_chart <- renderPlotly({
    
    acwr_data <- weekly_player_load() %>%
      mutate(
        acwr_status = case_when(
          acwr < 0.8 ~ "Low",
          acwr >= 0.8 & acwr <= 1.3 ~ "Optimal",
          acwr > 1.3 ~ "High",
          TRUE ~ "Missing"
        ),
        hover_text = paste0(
          "Player: ", player_id,
          "<br>Position: ", position,
          "<br>Week: ", week,
          "<br>Weekly Load: ", round(weekly_load, 0), " AU",
          "<br>Chronic Load: ", round(chronic_load, 0), " AU",
          "<br>ACWR: ", round(acwr, 2),
          "<br>Status: ", acwr_status
        )
      )
    
    p <- ggplot(
      acwr_data,
      aes(
        x = reorder(player_id, acwr),
        y = acwr,
        text = hover_text
      )
    ) +
      annotate(
        "rect",
        xmin = -Inf,
        xmax = Inf,
        ymin = 0.8,
        ymax = 1.3,
        fill = "#D9F0D3",
        alpha = 0.45
      ) +
      geom_hline(
        yintercept = 0.8,
        linetype = "dashed",
        color = "#2E7D32",
        linewidth = 0.6
      ) +
      geom_hline(
        yintercept = 1.3,
        linetype = "dashed",
        color = "#B2182B",
        linewidth = 0.6
      ) +
      geom_point(
        aes(color = acwr_status),
        size = 3.4,
        alpha = 0.9
      ) +
      scale_color_manual(
        values = c(
          "Low" = "#6BAED6",
          "Optimal" = "#2E7D32",
          "High" = "#B2182B",
          "Missing" = "gray60"
        ),
        name = "ACWR Zone"
      ) +
      coord_flip() +
      labs(
        title = NULL,
        x = "Player",
        y = "ACWR"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hovermode = "closest",
        margin = list(l = 105, r = 25, t = 20, b = 120),
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.25
        )
      )
  })
  
  output$squad_summary_table <- renderDT({
    
    summary_table <- weekly_player_load() %>%
      mutate(
        acwr_zone = case_when(
          is.na(acwr) ~ "Insufficient baseline",
          acwr < 0.8 ~ "Low",
          acwr >= 0.8 & acwr <= 1.3 ~ "Optimal",
          acwr > 1.3 ~ "High"
        )
      ) %>%
      select(
        player_id,
        position,
        week,
        weekly_load,
        chronic_load,
        acwr,
        acwr_zone,
        mean_hrv,
        mean_hooper,
        sessions_recorded
      ) %>%
      mutate(
        weekly_load = round(weekly_load, 0),
        chronic_load = round(chronic_load, 0),
        acwr = round(acwr, 2),
        mean_hrv = round(mean_hrv, 2),
        mean_hooper = round(mean_hooper, 1)
      ) %>%
      rename(
        Player = player_id,
        Pos = position,
        Week = week,
        `Weekly Load` = weekly_load,
        `Chronic Load` = chronic_load,
        ACWR = acwr,
        `ACWR Zone` = acwr_zone,
        `Mean HRV` = mean_hrv,
        `Mean Hooper` = mean_hooper,
        Sessions = sessions_recorded
      ) %>%
      arrange(desc(`Weekly Load`))
    
    datatable(
      summary_table,
      rownames = FALSE,
      class = "compact stripe hover",
      options = list(
        pageLength = 10,
        lengthChange = FALSE,
        searching = FALSE,
        scrollX = FALSE,
        dom = "tip",
        autoWidth = FALSE,
        columnDefs = list(
          list(width = "140px", targets = 0),
          list(width = "50px", targets = 1),
          list(width = "50px", targets = 2),
          list(width = "95px", targets = 3),
          list(width = "100px", targets = 4),
          list(width = "70px", targets = 5),
          list(width = "115px", targets = 6),
          list(width = "85px", targets = 7),
          list(width = "100px", targets = 8),
          list(width = "70px", targets = 9)
        )
      )
    ) %>%
      formatStyle(
        columns = names(summary_table),
        fontSize = "12px",
        padding = "4px"
      ) %>%
      formatStyle(
        "Player",
        fontWeight = "600"
      ) %>%
      formatStyle(
        "Weekly Load",
        fontWeight = "600"
      ) %>%
      formatStyle(
        "ACWR",
        fontWeight = "600"
      ) %>%
      formatStyle(
        "ACWR Zone",
        backgroundColor = styleEqual(
          c("Low", "Optimal", "High", "Insufficient baseline"),
          c("#D6EAF8", "#D9F0D3", "#F4A6A6", "#EFEFEF")
        ),
        color = styleEqual(
          c("Low", "Optimal", "High", "Insufficient baseline"),
          c("#1F4E79", "#1B5E20", "#8B1A1A", "#555555")
        ),
        fontWeight = "bold"
      )
  })
  
  output$download_squad_summary <- downloadHandler(
    
    filename = function() {
      paste0(
        "squad_load_summary_week_",
        input$squad_week,
        "_",
        input$squad_position,
        ".csv"
      )
    },
    
    content = function(file) {
      
      download_table <- weekly_player_load() %>%
        mutate(
          acwr_zone = case_when(
            is.na(acwr) ~ "Insufficient baseline",
            acwr < 0.8 ~ "Low",
            acwr >= 0.8 & acwr <= 1.3 ~ "Optimal",
            acwr > 1.3 ~ "High"
          )
        ) %>%
        select(
          player_id,
          position,
          week,
          weekly_load,
          chronic_load,
          acwr,
          acwr_zone,
          mean_hrv,
          mean_hooper,
          sessions_recorded
        ) %>%
        mutate(
          weekly_load = round(weekly_load, 0),
          chronic_load = round(chronic_load, 0),
          acwr = round(acwr, 2),
          mean_hrv = round(mean_hrv, 2),
          mean_hooper = round(mean_hooper, 1)
        ) %>%
        rename(
          Player = player_id,
          Position = position,
          Week = week,
          Weekly_Load_AU = weekly_load,
          Chronic_Load_AU = chronic_load,
          ACWR = acwr,
          ACWR_Zone = acwr_zone,
          Mean_HRV_Z = mean_hrv,
          Mean_Hooper = mean_hooper,
          Sessions = sessions_recorded
        )
      
      write.csv(download_table, file, row.names = FALSE)
    }
  )
}