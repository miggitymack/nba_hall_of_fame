library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(shinydashboard)

season_stats = read.csv('./dataset/Seasons_Stats.csv')

players = unique(sort(season_stats$Player))
player_stat = c(
  "PER",
  "Points",
  "Offensive rebounds",
  "Defensive Rebounds",
  "Total Rebounds",
  "Assists",
  "Field Goal Percentage",
  "Effective Field Goal %",
  "3-Point Field Goal %",
  "Turnovers",
  "Assist-to-Turnover Ratio",
  "VORP",
  "Minutes Per Game"
)
compare = c("Michael Jordan", "Top 10 in era", "Average Player in era")