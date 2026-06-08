player_deep_dive_server <- function(input, output, session, gps) {
  
  selected_player_data <- reactive({
    gps %>%
      filter(
        player_id == input$player,
        date >= input$player_date_range[1],
        date <= input$player_date_range[2]
      ) %>%
      arrange(date) %>%
      mutate(
        hover_text = paste0(
          "Date: ", format(date, "%b %d, %Y"),
          "<br>Session: ", session_type,
          "<br>Load: ", round(daily_load, 0), " AU",
          "<br>HRV z-score: ", round(hrv_zscore, 2),
          "<br>Hooper Index: ", hooper_index_total,
          "<br>Sleep: ", sleep_duration_hrs, " hrs",
          "<br>Fatigue: ", perceived_fatigue_1to7,
          "<br>Soreness: ", perceived_soreness_1to7
        )
      )
  })
  player_wellness_summary <- reactive({
    
    selected_player_data() %>%
      summarise(
        avg_hooper = mean(hooper_index_total, na.rm = TRUE),
        avg_sleep = mean(sleep_quality_1to5, na.rm = TRUE),
        avg_strain = mean(
          rowMeans(
            across(c(perceived_fatigue_1to7, perceived_stress_1to7, perceived_soreness_1to7)),
            na.rm = TRUE
          ),
          na.rm = TRUE
        )
      )
  })
  
  output$player_hooper_card <- renderUI({
    
    x <- player_wellness_summary()
    
    div(
      style = "background:#F3F6FA; border-left:5px solid #2C5985; padding:12px; border-radius:8px;",
      tags$small("Average Hooper Index", style = "color:#555; font-weight:700;"),
      h3(round(x$avg_hooper, 1), style = "margin:4px 0 0 0; font-weight:800;"),
      tags$small("Total wellness strain")
    )
  })
  
  output$player_sleep_card <- renderUI({
    
    x <- player_wellness_summary()
    
    div(
      style = "background:#F3F8F3; border-left:5px solid #2E7D32; padding:12px; border-radius:8px;",
      tags$small("Average Sleep Quality", style = "color:#555; font-weight:700;"),
      h3(round(x$avg_sleep, 1), style = "margin:4px 0 0 0; font-weight:800;"),
      tags$small("1–5 scale")
    )
  })
  
  output$player_strain_card <- renderUI({
    
    x <- player_wellness_summary()
    
    div(
      style = "background:#FFF8E6; border-left:5px solid #C58B00; padding:12px; border-radius:8px;",
      tags$small("Average Strain Score", style = "color:#555; font-weight:700;"),
      h3(round(x$avg_strain, 1), style = "margin:4px 0 0 0; font-weight:800;"),
      tags$small("Fatigue, stress, soreness")
    )
  })
  output$player_load_hrv_plot <- renderPlotly({
    
    player_data <- selected_player_data() %>%
      mutate(
        session_label = case_when(
          game_day ~ "Game",
          training_day ~ "Training",
          TRUE ~ "Off / Travel"
        ),
        hover_text = paste0(
          "Date: ", format(date, "%b %d, %Y"),
          "<br>Session: ", session_type,
          "<br>Load: ", round(daily_load, 0), " AU",
          "<br>HRV z-score: ", round(hrv_zscore, 2),
          "<br>Hooper Index: ", hooper_index_total
        )
      )
    
    p <- ggplot(player_data, aes(x = date)) +
      geom_col(
        aes(y = daily_load, text = hover_text),
        fill = "#D6E2F1",
        alpha = 0.85,
        width = 0.8
      ) +
      geom_line(
        aes(y = (hrv_zscore + 3) * 90, group = 1, text = hover_text),
        color = "#2C5985",
        linewidth = 1.2
      ) +
      geom_point(
        aes(y = (hrv_zscore + 3) * 90, text = hover_text),
        color = "#2C5985",
        size = 2.2
      ) +
      geom_point(
        data = player_data %>% filter(game_day),
        aes(y = daily_load, text = hover_text),
        color = "#B2182B",
        size = 3.5,
        shape = 18
      ) +
      scale_x_date(
        date_breaks = "1 week",
        date_labels = "%b %d",
        expand = expansion(mult = c(0.01, 0.03))
      ) +
      scale_y_continuous(
        name = "Session Load (AU)",
        sec.axis = sec_axis(
          trans = ~ (. / 90) - 3,
          name = "HRV z-score"
        ),
        expand = expansion(mult = c(0.03, 0.08))
      ) +
      labs(
        title = NULL,
        x = "Date"
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
        autosize = TRUE,
        margin = list(l = 70, r = 45, t = 20, b = 70)
      )
  })
  output$player_wellness_plot <- renderPlotly({
    
    wellness_weekly <- selected_player_data() %>%
      group_by(week) %>%
      summarise(
        fatigue = mean(perceived_fatigue_1to7, na.rm = TRUE),
        stress = mean(perceived_stress_1to7, na.rm = TRUE),
        soreness = mean(perceived_soreness_1to7, na.rm = TRUE),
        sleep_quality = mean(sleep_quality_1to5, na.rm = TRUE),
        hooper = mean(hooper_index_total, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        strain_average = rowMeans(
          across(c(fatigue, stress, soreness)),
          na.rm = TRUE
        ),
        hover_text = paste0(
          "Week: ", week,
          "<br>Hooper Index: ", round(hooper, 1),
          "<br>Fatigue: ", round(fatigue, 1),
          "<br>Stress: ", round(stress, 1),
          "<br>Soreness: ", round(soreness, 1),
          "<br>Sleep Quality: ", round(sleep_quality, 1),
          "<br>Strain Avg: ", round(strain_average, 1)
        )
      )
    
    p <- ggplot(wellness_weekly, aes(x = week)) +
      geom_col(
        aes(y = hooper, text = hover_text),
        fill = "#D6E2F1",
        alpha = 0.9,
        width = 0.65
      ) +
      geom_line(
        aes(y = strain_average * 4.2, group = 1, text = hover_text),
        color = "#C58B00",
        linewidth = 1.4
      ) +
      geom_point(
        aes(y = strain_average * 4.2, text = hover_text),
        color = "#C58B00",
        size = 3
      ) +
      geom_line(
        aes(y = sleep_quality * 4.2, group = 1, text = hover_text),
        color = "#2E7D32",
        linewidth = 1.2,
        linetype = "dashed"
      ) +
      geom_point(
        aes(y = sleep_quality * 4.2, text = hover_text),
        color = "#2E7D32",
        size = 2.8
      ) +
      scale_x_continuous(
        breaks = sort(unique(wellness_weekly$week))
      ) +
      scale_y_continuous(
        name = "Hooper Index",
        sec.axis = sec_axis(
          trans = ~ . / 4.2,
          name = "Strain / Sleep Quality"
        ),
        expand = expansion(mult = c(0.02, 0.08))
      ) +
      labs(
        title = NULL,
        x = "Week"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.margin = margin(5, 10, 5, 5)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hovermode = "closest",
        autosize = TRUE,
        margin = list(l = 65, r = 65, t = 10, b = 100),
        annotations = list(
          list(
            x = 0.01,
            y = 1.08,
            xref = "paper",
            yref = "paper",
            text = "<b>Bars:</b> Hooper Index &nbsp;&nbsp; <b>Gold:</b> fatigue/stress/soreness average &nbsp;&nbsp; <b>Green dashed:</b> sleep quality",
            showarrow = FALSE,
            align = "left",
            font = list(size = 12, color = "#444")
          )
        )
      )
  })
  output$player_hooper_radar <- renderPlotly({
    
    radar_data <- selected_player_data() %>%
      filter(week == as.numeric(input$radar_week)) %>%
      summarise(
        Fatigue = mean(perceived_fatigue_1to7, na.rm = TRUE),
        Stress = mean(perceived_stress_1to7, na.rm = TRUE),
        Soreness = mean(perceived_soreness_1to7, na.rm = TRUE),
        `Sleep Quality` = mean(sleep_quality_1to5, na.rm = TRUE),
        Hooper = mean(hooper_index_total, na.rm = TRUE),
        .groups = "drop"
      )
    
    radar_long <- tibble(
      metric = c("Fatigue", "Stress", "Soreness", "Sleep Quality"),
      value = c(
        radar_data$Fatigue,
        radar_data$Stress,
        radar_data$Soreness,
        radar_data$`Sleep Quality`
      )
    ) %>%
      mutate(
        metric = factor(
          metric,
          levels = c("Fatigue", "Stress", "Soreness", "Sleep Quality")
        ),
        hover_text = paste0(
          metric,
          ": ",
          round(value, 2),
          "<br>Week: ",
          input$radar_week,
          "<br>Hooper Index: ",
          round(radar_data$Hooper, 1)
        )
      )
    
    # Repeat the first row at the end so the radar shape closes cleanly
    radar_closed <- bind_rows(
      radar_long,
      radar_long %>% slice(1)
    )
    
    plot_ly(
      radar_closed,
      type = "scatterpolar",
      mode = "lines+markers",
      r = ~value,
      theta = ~metric,
      fill = "toself",
      fillcolor = "rgba(197, 139, 0, 0.22)",
      line = list(
        color = "#2C5985",
        width = 3,
        shape = "linear"
      ),
      marker = list(
        color = "#2C5985",
        size = 7,
        line = list(
          color = "white",
          width = 1.5
        )
      ),
      text = ~hover_text,
      hoverinfo = "text"
    ) %>%
      layout(
        polar = list(
          bgcolor = "rgba(0,0,0,0)",
          radialaxis = list(
            visible = TRUE,
            range = c(0, 7),
            tickvals = c(0, 2, 4, 6),
            tickfont = list(size = 10, color = "#666"),
            gridcolor = "rgba(0,0,0,0.18)",
            linecolor = "rgba(0,0,0,0.25)"
          ),
          angularaxis = list(
            tickfont = list(size = 11, color = "#333"),
            gridcolor = "rgba(0,0,0,0.15)",
            linecolor = "rgba(0,0,0,0.25)"
          )
        ),
        showlegend = FALSE,
        margin = list(l = 35, r = 35, t = 10, b = 25)
      )
  })
}
