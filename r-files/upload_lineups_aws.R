library(tidyverse)
source(here::here("r-files",'scrape_fl_lineups.R'))


library(odbc)
library(DBI)
library(tidyverse)
select <- dplyr::select

con <- DBI::dbConnect(drv = RMySQL::MySQL(),
                      dbname = "nhl",
                      username    = 'admin',
                      password    = "guerrillas",
                      host = "database-1.cyhyxhbkkfox.us-east-2.rds.amazonaws.com",
                      port = 3306)


dates <- seq( as.Date("2021-03-16"), as.Date("2021-04-09"), by = 'day')

uploadDataContestDate <- function(list_o_dates) {
  for(i in 1:length(list_o_dates)) {
    date <- as.character(lubridate::as_date(list_o_dates[i]))
    date_underscored <- paste(
      lubridate::month(date),
      lubridate::day(date),
      lubridate::year(date),
      sep  = "_"
    )
    
    contest_group_url <- paste0(
      "https://www.fantasylabs.com/api/ownership-contestgroups/4/4/",date_underscored,"/"
    )
    if(httr::GET(contest_group_url) %>%
       content(type = 'text',encoding = "UTF-8") != "[]") {
      # get the contest_group_id
      contest_group_id <- getContestGroupId(date)
      # get the contest_id of largest contest
      contestId <- getContestId(contest_group = contest_group_id$ContestGroupId)
      # get player results from that date
      player_stats <- getPlayerData(contestId = contestId$contest_id, date = date)
      # get all lineups from that night
      lineups <- getContestLineups(contest_id = contestId$contest_id, date = date)
      
      
      ## need to put 5 things into DB everytime
      # contest_group_id tibble
      # contest_id tibble
      # lineups tibble
      # player_data tibble
      # probably some sort of player Id reference table
      
      ## add new rows to each table in db
      dbWriteTable(con, "CONTEST_GROUP", contest_group_id,
                   row.names = F, overwrite = F, append = T)
      dbWriteTable(con, "CONTEST_ID", contestId,
                   row.names = F, overwrite = F, append = T)
      dbWriteTable(con, "LINEUPS", lineups %>%
                     group_by(contest_id) %>%
                     mutate(lineup_id = 1:n()),
                   row.names = F, overwrite = F, append = T)
      dbWriteTable(con, "PLAYER_STATS", player_stats %>% 
                     select(playerId, salary, position, currentTeamId, homeVisitor, favDog, projPoints,
                            ownership, actualPoints, contest_id),
                   row.names = F, overwrite = F, append = T)
      dbWriteTable(con, "TEAM", player_stats %>%
                     distinct(team = currentTeam, teamId = currentTeamId),
                   row.names = F, overwrite = F, append = T)
      dbWriteTable(con, "PLAYER", player_stats %>%
                     distinct(playerId, firstName, lastName, fullName),
                   row.names = F, overwrite = F, append = T)
      
      paste0("done with ", list_o_dates[i])
    } else {
      paste0("Skipping ", date)
    }
  }
  paste0(list_o_dates[i])
}



