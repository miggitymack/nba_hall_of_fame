source("global.R")

season_stats = read.csv('./dataset/Seasons_Stats.csv')

shinyServer(function(input, output) {
  selected_player = reactive({
    season_stats %>% filter(., Player == input$selected_player) %>%
      group_by(Player) %>%
      summarise(
        Position = Pos,
        Season = n(),
        Mean_G = mean(G, na.rm = T),
        Mean_PER = mean(PER, na.rm = T),
        Mean_PTS = mean(PTS, na.rm = T),
        Mean_TRB = mean(TRB, na.rm = T),
        Mean_ORB = mean(ORB., na.rm = T),
        Mean_DRB = mean(DRB., na.rm = T),
        Mean_AST = mean(AST, na.rm = T),
        Mean_FG = mean(FG., na.rm = T),
        Mean_eFG = mean(eFG., na.rm = T),
        Mean_X3P = mean(X3P., na.rm = T),
        Mean_TOV = mean(TOV, na.rm = T),
        Mean_VORP = mean(VORP, na.rm = T),
        Mean_MP = mean(MP, na.rm = T)
      ) %>% slice_head(., order_by(Mean_G), 1)
  })
  
  era_mean = reactive({
    season_stats %>% filter(.,
                            Year >= input$Year[1] &
                              Year <= input$Year[2] & Pos == input$selected_player) %>%
      arrange(., desc(PER)) %>% distinct(., Player, .keep_all = T) %>%
      slice_head(., n = 20) %>%
      group_by(Pos) %>%
      summarise(
        Player = "Era",
        Position = Pos,
        Season = n(),
        Mean_G = mean(G, na.rm = T),
        Mean_PER = mean(PER, na.rm = T),
        Mean_PTS = mean(PTS, na.rm = T),
        Mean_TRB = mean(TRB, na.rm = T),
        Mean_ORB = mean(ORB., na.rm = T),
        Mean_DRB = mean(DRB., na.rm = T),
        Mean_AST = mean(AST, na.rm = T),
        Mean_FG = mean(FG., na.rm = T),
        Mean_eFG = mean(eFG., na.rm = T),
        Mean_X3P = mean(X3P., na.rm = T),
        Mean_TOV = mean(TOV, na.rm = T),
        Mean_VORP = mean(VORP, na.rm = T),
        Mean_MP = mean(MP, na.rm = T)
      ) %>% slice_head(., order_by(Mean_G), 1)
  })
  
  avg_player_mean = reactive({
    season_stats %>% filter(.,
                            Year >= input$Year[1] &
                              Year <= input$Year[2] & Pos == input$selected_player ) %>% group_by(Pos) %>% summarise(
                                Player = "Average",
                                Position = Pos,
                                Season = n(),
                                Mean_G = mean(G, na.rm = T),
                                Mean_PER = mean(PER, na.rm = T),
                                Mean_PTS = mean(PTS, na.rm = T),
                                Mean_TRB = mean(TRB, na.rm = T),
                                Mean_ORB = mean(ORB., na.rm = T),
                                Mean_DRB = mean(DRB., na.rm = T),
                                Mean_AST = mean(AST, na.rm = T),
                                Mean_FG = mean(FG., na.rm = T),
                                Mean_eFG = mean(eFG., na.rm = T),
                                Mean_X3P = mean(X3P., na.rm = T),
                                Mean_TOV = mean(TOV, na.rm = T),
                                Mean_VORP = mean(VORP, na.rm = T),
                                Mean_MP = mean(MP, na.rm = T)
                              ) %>% slice_head(., order_by(Mean_G), 1)
  })
  
  mj_mean = season_stats %>% filter(., Player == "Michael Jordan*") %>%
    group_by(Player) %>%
    summarise(
      Position = Pos,
      Season = n(),
      Mean_G = mean(G, na.rm = T),
      Mean_PER = mean(PER, na.rm = T),
      Mean_PTS = mean(PTS, na.rm = T),
      Mean_TRB = mean(TRB, na.rm = T),
      Mean_ORB = mean(ORB., na.rm = T),
      Mean_DRB = mean(DRB., na.rm = T),
      Mean_AST = mean(AST, na.rm = T),
      Mean_FG = mean(FG., na.rm = T),
      Mean_eFG = mean(eFG., na.rm = T),
      Mean_X3P = mean(X3P., na.rm = T),
      Mean_TOV = mean(TOV, na.rm = T),
      Mean_VORP = mean(VORP, na.rm = T),
      Mean_MP = mean(MP, na.rm = T)
    ) %>% slice_head(., order_by(Mean_G), 1)
  
  combined_df = reactive({
    bind_rows(selected_player(), mj_mean, avg_player_mean(), era_mean())
  })
  
  
  output$stat_compare = renderPlot({
    combined_df() %>% ggplot(aes(x = Player, y = input$player_stat)) + geom_col(fill =
                                                                                 "lightblue")
  })
  
  output$table = renderTable({
    combined_df()
  })
  
})