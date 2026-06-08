load_data <- function() {
  read_csv("data/NYR_sim_load_hrv_wk1-7.csv", show_col_types = FALSE) %>%
    mutate(
      date = lubridate::mdy(date),
      
      total_distance_m = replace_na(total_distance_m, 0),
      high_speed_distance_m = replace_na(high_speed_distance_m, 0),
      session_rpe = replace_na(session_rpe, 0),
      session_duration_min = replace_na(session_duration_min, 0),
      session_au = replace_na(session_au, 0),
      
      daily_load = if_else(
        session_type %in% c("Off", "Travel"),
        0,
        session_au
      ),
      
      high_speed_pct = if_else(
        total_distance_m > 0,
        high_speed_distance_m / total_distance_m,
        0
      ),
      
      training_day = !session_type %in% c("Off", "Travel"),
      game_day = session_type == "Game",
      date_label = format(date, "%b %d, %Y"),
      
      readiness_status = case_when(
        hrv_zscore <= -1.0 |
          perceived_fatigue_1to7 >= 6 |
          perceived_soreness_1to7 >= 6 ~ "Red",
        
        hrv_zscore <= -0.5 |
          hooper_index_total >= 17 ~ "Amber",
        
        TRUE ~ "Green"
      )
    ) %>%
    arrange(player_id, date)
}
