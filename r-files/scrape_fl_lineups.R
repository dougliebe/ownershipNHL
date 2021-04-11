### Use to read lineup data off FL
## also read results/ownership from same url
library(tidyverse)
library(tidyjson)
library(httr)
library(jsonlite)

## need the largest contest group id from a given date
getContestGroupId <- function(date) {
  date_underscored <- paste(
    lubridate::month(date),
    lubridate::day(date),
    lubridate::year(date),
    sep  = "_"
  )
  
  contest_group_url <- paste0(
    "https://www.fantasylabs.com/api/ownership-contestgroups/4/4/",date_underscored,"/"
  )
  
  httr::GET(contest_group_url) %>%
    content(type = 'text',encoding = "UTF-8") ->
    json_file_cg
  
  json_file_cg %>%
    tidyjson::gather_array() %>%
    spread_all() %>% 
    arrange(desc(GameCount)) %>% 
    as_tibble() %>%
    select(-document.id, -array.index) %>%
    slice(1) ->
    contest_group_id
  
  return(contest_group_id)
  
}

getContestId <- function(contest_group) {
  ## will be the contest group from the group_id result df
  contests_url <- paste0("https://service.fantasylabs.com/live-contests/?sport=NHL&contest_group_id=",
                         contest_group)
  httr::GET(contests_url) %>%
    content(type = 'text',encoding = "UTF-8") ->
    json_contests
  json_contests %>%
    enter_object('live_contests') %>%
    gather_array() %>%
    spread_all %>%
    as_tibble() %>%
    select(-document.id, -array.index) %>%
    mutate(contest_group_id = contest_group) %>%
    arrange(desc(contest_size)) %>% 
    slice(1) ->
    contest_id
  return(contest_id)
}


## using contest group id + date, we can return the lineups from that contest
getContestLineups <- function(contest_id, date) {
  date_collapsed <- as.character(date) %>%
    str_replace_all("-","")
  
  lu_url <- paste0("https://dh5nxc6yx3kwy.cloudfront.net/contests/nhl/",
                     date_collapsed,"/",contest_id,"/lineups/")
  # lu_url <- "https://service.fantasylabs.com/live/events/?sport_id=3&contest_group_id=55202"
  file <- httr::GET(lu_url)
  
  file %>%
    content(type = 'text',encoding = "UTF-8") ->
    json_file
  
  json_file %>%
    enter_object("lineups") %>%
    gather_object('name') %>%
    spread_values(correlatedPlayers = jnumber("correlatedPlayers"),
                  favoriteCt = jnumber("favoriteCt"),
                  totalSalary = jnumber("totalSalary"))  %>%
    enter_object('lineupPlayers') %>%
    spread_all %>%
    as_tibble() %>%
    mutate(contest_id = contest_id) %>%
    select(-document.id, -name) ->
    lineups_data
  
  return(lineups_data)
}

## we can also return the own, proj pts, actual points, sal from contest id
getPlayerData <- function(contestId, date) {
  date_collapsed <- as.character(date) %>%
    str_replace_all("-","")
  
  data_url <- paste0("https://dh5nxc6yx3kwy.cloudfront.net/contests/nhl/",
                     date_collapsed,"/",contestId,"/data/")
  
  file <- httr::GET(data_url)
  
  file %>%
    content(type = 'text',encoding = "UTF-8") ->
    json_file_data
  
  json_file_data %>%
    enter_object("players") %>%
    gather_object('name') %>%
    # gather_object() %>% json_types %>% count(name, type)
    # gather_object('name') %>%
    # enter_object('lineupPlayers') %>%
    spread_all %>%
    as_tibble() %>%
    mutate(contest_id = contestId)  %>%
    select(-document.id, -name) ->
    player_data
  
  return(player_data)

}

