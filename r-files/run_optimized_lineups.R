library(lpSolve)
library(odbc)
library(DBI)
library(tidyverse)
select <- dplyr::select

source(here::here("r-files", "optimizer.R"))

con <- DBI::dbConnect(drv = RMySQL::MySQL(),
                      dbname = "nhl",
                      username    = 'admin',
                      password    = "guerrillas",
                      host = "database-1.cyhyxhbkkfox.us-east-2.rds.amazonaws.com",
                      port = 3306)
contest_q <- tbl(con, "CONTEST_ID")
lineups_q <- tbl(con, "LINEUPS")
player_stats_q <- tbl(con, "PLAYER_STATS")
players_q <- tbl(con, "PLAYER")

player_data_q %>%
  filter(contest_id == 101704840) %>%
  left_join(player_q, 'playerId') %>%
  select(player_id = playerId, player = fullName,
         team = currentTeamId, salary,
         fpts_proj = projPoints, position) %>%
  collect() %>%
  mutate(row_id = 1:n(),
         position = case_when(position %in% c("RW","LW") ~ "W",
                              TRUE ~ position),
         player_id = as.character(player_id),
         team = as.character(team),
         salary = as.integer(salary)) ->
  test_data

## example for 101704840 
contest_id <- 101704840 
player_stats_q %>%
  filter(contest_id == !!contest_id ) %>%
  collect() ->
  data

data %>%
  filter(
    playerId %in% getOptNHL(data$actualPoints, data$position, data$salary, data$playerId)
  ) %>%
  left_join(players_q %>% collect(), by = "playerId") %>%
  select(fullName, salary, position, ownership, actualPoints)

player_stats_q %>%
  filter(projPoints > 1) %>%
  collect() %>%
  ggplot(aes(projPoints, actualPoints))+
  geom_point(alpha = 0.1) +
  geom_smooth()