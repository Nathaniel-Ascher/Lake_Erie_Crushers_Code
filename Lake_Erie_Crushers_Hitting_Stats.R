library(rvest)
library(dplyr)
library(stringr)

create_player_stats_data_frames_extended <- function(url) {
  page <- read_html(url)
  
  basic_table <- page %>% html_node('table[data-col-name="ab"]')
  extended_table <- page %>% html_node('table[data-col-name="pa"]')
  fielding_table <- page %>% html_node('table[data-col-name="po"]')
  
  basic_rows <- basic_table %>% html_nodes("tr")
  extended_rows <- extended_table %>% html_nodes("tr")
  fielding_rows <- fielding_table %>% html_nodes("tr")
  
  player_data_frames <- list()
  
  for (i in 2:length(basic_rows)) {
    player_data_frames <- process_basic_stats(basic_rows[i], player_data_frames)
  }
  
  for (i in 2:length(extended_rows)) {
    player_data_frames <- process_extended_stats(extended_rows[i], player_data_frames)
  }
  
  for (i in 2:length(fielding_rows)) {
    player_data_frames <- process_fielding_stats(fielding_rows[i], player_data_frames)
  }
  
  return(player_data_frames)
}

process_basic_stats <- function(row, player_data_frames) {
  basic_data <- row %>% html_nodes('td') %>% html_text()
  player_href <- html_nodes(row, 'td a') %>% html_attr('href')
  player_identifier <- str_extract(player_href, "(?<=players/)[^/]+")
  
  if (is.null(player_identifier) || is.na(as.numeric(basic_data[7])) || as.numeric(basic_data[7]) == 0) {
    return(player_data_frames)
  }
  
  player_name_clean <- gsub("[^[:alnum:]]", "_", player_identifier)
  df_name <- paste0(player_name_clean, "_2024_Hitting_Stats")
  
  basic_stats_values <- as.numeric(basic_data[5:19])
  basic_stats_values[is.na(basic_stats_values)] <- 0
  ops_value <- basic_stats_values[14] + basic_stats_values[15]
  
  stat_names <- c("G", "AB", "R", "H", "2B", "3B", "HR", "RBI", "BB", "K", "SB", "CS", "AVG", "OBP", "SLG", "OPS")
  basic_stats_df <- data.frame(Stat = stat_names, Value = c(basic_stats_values[1:15], ops_value))
  
  player_data <- list()
  player_data$basic <- basic_stats_df
  
  player_data_frames[[df_name]] <- player_data
  
  return(player_data_frames)
}


process_extended_stats <- function(row, player_data_frames) {
  extended_data <- row %>% html_nodes('td') %>% html_text()
  player_href <- html_nodes(row, 'td a') %>% html_attr('href')
  player_identifier <- str_extract(player_href, "(?<=players/)[^/]+")
  
  if (is.null(player_identifier)) {
    return(player_data_frames)
  }
  
  player_name_clean <- gsub("[^[:alnum:]]", "_", player_identifier)
  df_name <- paste0(player_name_clean, "_2024_Hitting_Stats")
  
  extended_stats_values <- as.numeric(extended_data[c(6, 7, 8, 9, 11, 12, 13, 15)])
  extended_stat_names <- c("HBP", "SF", "SH", "TB", "HDP", "GO", "FO", "PA")
  extended_stats_df <- data.frame(Stat = extended_stat_names, Value = extended_stats_values)
  
  if (df_name %in% names(player_data_frames)) {
    player_data_frames[[df_name]]$extended <- extended_stats_df
  }
  
  return(player_data_frames)
}

process_fielding_stats <- function(row, player_data_frames) {
  fielding_data <- row %>% html_nodes('td') %>% html_text()
  player_href <- html_nodes(row, 'td a') %>% html_attr('href')
  player_identifier <- str_extract(player_href, "(?<=players/)[^/]+")
  
  if (is.null(player_identifier)) {
    return(player_data_frames)
  }
  
  player_name_clean <- gsub("[^[:alnum:]]", "_", player_identifier)
  df_name <- paste0(player_name_clean, "_2024_Hitting_Stats")
  
  fielding_stats_values <- as.numeric(fielding_data[c(6, 7, 8, 9, 11, 12, 13, 15, 16)])
  fielding_stat_names <- c("TC", "PO", "A", "E", "DP", "SBA", "RCS", "PB", "CI")
  fielding_stats_df <- data.frame(Stat = fielding_stat_names, Value = fielding_stats_values)
  
  if (df_name %in% names(player_data_frames)) {
    player_data_frames[[df_name]]$fielding <- fielding_stats_df
  }
  
  return(player_data_frames)
}

Lake_Erie_Crushers_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/lakeeriecrushers?view=lineup"
Lake_Erie_Crushers_Hitters_Stats_List_2024 <- create_player_stats_data_frames_extended(Lake_Erie_Crushers_Url_2024)

Windy_City_Thunderbolts_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/windycitythunderbolts?view=lineup"
Windy_City_Thunderbolts_Hitters_Stats_List_2024 <- create_player_stats_data_frames_extended(Windy_City_Thunderbolts_Url_2024)

New_England_Knockouts_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/newenglandknockouts?view=lineup"
New_England_Knockouts_Hitters_Stats_List_2024 <- create_player_stats_data_frames_extended(New_England_Knockouts_Url_2024)

Joliet_Slammers_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/jolietslammers?view=lineup"
Joliet_Slammers_Hitters_Stats_List_2024 <- create_player_stats_data_frames_extended(Joliet_Slammers_Url_2024)

Washington_Wild_Things_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/washingtonwildthings?view=lineup"
Washington_Wild_Things_Hitters_Stats_List_2024 <- create_player_stats_data_frames_extended(Washington_Wild_Things_Url_2024)

Quebec_Capitales_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/quebeccapitales?view=lineup"
Quebec_Capitales_Hitters_Stats_List_2024 <- create_player_stats_data_frames_extended(Quebec_Capitales_Url_2024)

Ottawa_Titans_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/ottawatitans?view=lineup"
Ottawa_Titans_Hitters_Stats_List_2024 <- create_player_stats_data_frames_extended(Ottawa_Titans_Url_2024)

Evansville_Otters_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/evansvilleotters?view=lineup"
Evansville_Otters_Hitters_Stats_List_2024 <- create_player_stats_data_frames_extended(Evansville_Otters_Url_2024)

Florence_Yalls_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/florenceyalls?view=lineup"
Florence_Yalls_Hitters_Stats_List_2024 <- create_player_stats_data_frames_extended(Florence_Yalls_Url_2024)

Sussex_County_Miners_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/sussexcountyminers?view=lineup"
Sussex_County_Miners_Hitters_Stats_List_2024 <- create_player_stats_data_frames_extended(Sussex_County_Miners_Url_2024)

New_York_Boulders_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/newyorkboulders?view=lineup"
New_York_Boulders_Hitters_Stats_List_2024 <- create_player_stats_data_frames_extended(New_York_Boulders_Url_2024)

Trois_Rivieres_Aigles_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/troisrivieresaigles?view=lineup"
Trois_Rivieres_Aigles_Hitters_Stats_List_2024 <- create_player_stats_data_frames_extended(Trois_Rivieres_Aigles_Url_2024)

Schaumburg_Boomers_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/schaumburgboomers?view=lineup"
Schaumburg_Boomers_Hitters_Stats_List_2024 <- create_player_stats_data_frames_extended(Schaumburg_Boomers_Url_2024)

Tri_City_Valleycats_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/tricityvalleycats?view=lineup"
Tri_City_Valleycats_Hitters_Stats_List_2024 <- create_player_stats_data_frames_extended(Tri_City_Valleycats_Url_2024)

Gateway_Grizzlies_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/gatewaygrizzlies?view=lineup"
Gateway_Grizzlies_Hitters_Stats_List_2024 <- create_player_stats_data_frames_extended(Gateway_Grizzlies_Url_2024)

New_Jersey_Jackals_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/newjerseyjackals?view=lineup"
New_Jersey_Jackals_Hitters_Stats_List_2024 <- create_player_stats_data_frames_extended(New_Jersey_Jackals_Url_2024)

scrape_stat_data_general <- function(url, column_index, is_weighted = FALSE) {
  library(rvest)
  library(dplyr)
  
  print(paste("Reading URL:", url))
  page <- read_html(url)
  
  table <- page %>% html_node('table[data-col-name="avg"]')
  if (is.null(table)) {
    print("No table found with the specified data-col-name='avg'. Please check the HTML structure.")
    return(NULL)
  }
  
  rows <- table %>% html_nodes("tr")
  print("Extracting team names and stats...")
  team_names <- rows %>% html_nodes("td.pinned-col.text a") %>% html_text()
  stats <- rows %>% html_nodes(sprintf("td:nth-child(%d)", column_index)) %>% html_text()
  
  valid_entries <- team_names != ""
  team_names <- team_names[valid_entries]
  stats <- as.numeric(stats[valid_entries])
  
  if (any(is.na(stats))) {
    print("NA values found in stats. Check extraction and conversion.")
    return(NULL)
  }
  
  team_names <- head(team_names, 16)
  stats <- head(stats, 16)
  
  data <- data.frame(Team = team_names, Stat = stats)
  
  if (is_weighted) {
    abs <- rows %>% html_nodes("td:nth-child(4)") %>% html_text()
    abs <- as.numeric(abs[valid_entries])
    abs <- head(abs, 16)
    
    east_teams <- c("New Jersey Jackals", "Ottawa Titans", "Trois-Rivières Aigles", 
                    "New York Boulders", "Tri-City ValleyCats", "New England Knockouts", 
                    "Sussex County Miners", "Québec Capitales")
    west_teams <- setdiff(team_names, east_teams)
    
    east_weighted_sum <- sum(data$Stat[data$Team %in% east_teams] * abs[data$Team %in% east_teams]) / sum(abs[data$Team %in% east_teams])
    west_weighted_sum <- sum(data$Stat[data$Team %in% west_teams] * abs[data$Team %in% west_teams]) / sum(abs[data$Team %in% west_teams])
    total_weighted_sum <- sum(data$Stat * abs) / sum(abs)
    
    data <- rbind(data, data.frame(Team = "Frontier League East", Stat = east_weighted_sum))
    data <- rbind(data, data.frame(Team = "Frontier League West", Stat = west_weighted_sum))
    data <- rbind(data, data.frame(Team = "Frontier League Total", Stat = total_weighted_sum))
  } else {
    east_teams <- c("New Jersey Jackals", "Ottawa Titans", "Trois-Rivières Aigles", 
                    "New York Boulders", "Tri-City ValleyCats", "New England Knockouts", 
                    "Sussex County Miners", "Québec Capitales")
    west_teams <- setdiff(team_names, east_teams)
    
    east_sum <- sum(data$Stat[data$Team %in% east_teams])
    west_sum <- sum(data$Stat[data$Team %in% west_teams])
    total_sum <- sum(data$Stat)
    
    data <- rbind(data, data.frame(Team = "Frontier League East", Stat = east_sum))
    data <- rbind(data, data.frame(Team = "Frontier League West", Stat = west_sum))
    data <- rbind(data, data.frame(Team = "Frontier League Total", Stat = total_sum))
  }
  
  print("Function completed successfully.")
  return(data)
}



scrape_stat_data_extended <- function(url, column_index) {
  page <- read_html(url)
  
  table <- page %>% html_node('table[data-col-name="pa"]')
  rows <- table %>% html_nodes("tr")
  
  team_names <- rows %>% html_nodes("td.pinned-col.text a") %>% html_text()
  stats <- rows %>% html_nodes(sprintf("td:nth-child(%d)", column_index)) %>% html_text()
  
  valid_entries <- team_names != ""
  team_names <- team_names[valid_entries]
  stats <- as.numeric(stats[valid_entries])
  
  team_names <- head(team_names, 16)
  stats <- head(stats, 16)
  
  east_teams <- c("New Jersey Jackals", "Ottawa Titans", "Trois-Rivières Aigles", 
                  "New York Boulders", "Tri-City ValleyCats", "New England Knockouts", 
                  "Sussex County Miners", "Québec Capitales")
  west_teams <- setdiff(team_names, east_teams)
  
  data <- data.frame(Team = team_names, Stat = stats)
  
  east_sum <- sum(data$Stat[data$Team %in% east_teams])
  west_sum <- sum(data$Stat[data$Team %in% west_teams])
  total_sum <- sum(data$Stat)
  
  data <- rbind(data, data.frame(Team = "Frontier League East", Stat = east_sum))
  data <- rbind(data, data.frame(Team = "Frontier League West", Stat = west_sum))
  data <- rbind(data, data.frame(Team = "Frontier League Total", Stat = total_sum))  
  
  return(data)
}

scrape_stat_data_pitching <- function(url, column_index, is_weighted = FALSE) {
  page <- read_html(url)
  
  table <- page %>% html_node('table[data-col-name="era"]')
  rows <- table %>% html_nodes("tr")
  
  team_names <- rows %>% html_nodes("td.pinned-col.text a") %>% html_text()
  stats <- rows %>% html_nodes(sprintf("td:nth-child(%d)", column_index)) %>% html_text()
  
  valid_entries <- team_names != ""
  team_names <- team_names[valid_entries]
  stats <- as.numeric(stats[valid_entries])
  
  team_names <- head(team_names, 16)
  stats <- head(stats, 16)
  
  east_teams <- c("New Jersey Jackals", "Ottawa Titans", "Trois-Rivières Aigles", 
                  "New York Boulders", "Tri-City ValleyCats", "New England Knockouts", 
                  "Sussex County Miners", "Québec Capitales")
  west_teams <- setdiff(team_names, east_teams)
  
  data <- data.frame(Team = team_names, Stat = stats)
  
  if (is_weighted) {
    ips <- rows %>% html_nodes("td:nth-child(4)") %>% html_text()
    ips <- as.numeric(ips[valid_entries])
    ips <- head(ips, 16)
    
    east_weighted_sum <- sum(data$Stat[data$Team %in% east_teams] * ips[data$Team %in% east_teams]) / sum(ips[data$Team %in% east_teams])
    west_weighted_sum <- sum(data$Stat[data$Team %in% west_teams] * ips[data$Team %in% west_teams]) / sum(ips[data$Team %in% west_teams])
    total_weighted_sum <- sum(data$Stat * ips) / sum(ips) 
    
    data <- rbind(data, data.frame(Team = "Frontier League East", Stat = east_weighted_sum))
    data <- rbind(data, data.frame(Team = "Frontier League West", Stat = west_weighted_sum))
    data <- rbind(data, data.frame(Team = "Frontier League Total", Stat = total_weighted_sum))  
  } else {
    east_sum <- sum(data$Stat[data$Team %in% east_teams])
    west_sum <- sum(data$Stat[data$Team %in% west_teams])
    total_sum <- sum(data$Stat)  
    
    data <- rbind(data, data.frame(Team = "Frontier League East", Stat = east_sum))
    data <- rbind(data, data.frame(Team = "Frontier League West", Stat = west_sum))
    data <- rbind(data, data.frame(Team = "Frontier League Total", Stat = total_sum))  
  }
  
  return(data)
}

scrape_stat_data_fielding <- function(url, column_index, is_weighted = FALSE) {
  page <- read_html(url)
  
  table <- page %>% html_node('table[data-col-name="fpct"]')
  rows <- table %>% html_nodes("tr")
  
  team_names <- rows %>% html_nodes("td.pinned-col.text a") %>% html_text()
  stats <- rows %>% html_nodes(sprintf("td:nth-child(%d)", column_index)) %>% html_text()
  
  valid_entries <- team_names != ""
  team_names <- team_names[valid_entries]
  stats <- as.numeric(stats[valid_entries])
  
  team_names <- head(team_names, 16)
  stats <- head(stats, 16)
  
  east_teams <- c("New Jersey Jackals", "Ottawa Titans", "Trois-Rivières Aigles", 
                  "New York Boulders", "Tri-City ValleyCats", "New England Knockouts", 
                  "Sussex County Miners", "Québec Capitales")
  west_teams <- setdiff(team_names, east_teams)
  
  data <- data.frame(Team = team_names, Stat = stats)
  
  if (is_weighted) {
    tcs <- rows %>% html_nodes("td:nth-child(4)") %>% html_text()
    tcs <- as.numeric(tcs[valid_entries])
    tcs <- head(tcs, 16)
    
    east_weighted_sum <- sum(data$Stat[data$Team %in% east_teams] * tcs[data$Team %in% east_teams]) / sum(tcs[data$Team %in% east_teams])
    west_weighted_sum <- sum(data$Stat[data$Team %in% west_teams] * tcs[data$Team %in% west_teams]) / sum(tcs[data$Team %in% west_teams])
    total_weighted_sum <- sum(data$Stat * tcs) / sum(tcs) 
    
    
    data <- rbind(data, data.frame(Team = "Frontier League East", Stat = east_weighted_sum))
    data <- rbind(data, data.frame(Team = "Frontier League West", Stat = west_weighted_sum))
    data <- rbind(data, data.frame(Team = "Frontier League Total", Stat = total_weighted_sum))  
    
  } else {
    east_sum <- sum(data$Stat[data$Team %in% east_teams])
    west_sum <- sum(data$Stat[data$Team %in% west_teams])
    total_sum <- sum(data$Stat)  
    
    data <- rbind(data, data.frame(Team = "Frontier League East", Stat = east_sum))
    data <- rbind(data, data.frame(Team = "Frontier League West", Stat = west_sum))
    data <- rbind(data, data.frame(Team = "Frontier League Total", Stat = total_sum))  
  }
  
  return(data)
}

url_general_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams?sort=avg&r=0&pos=h"

All_Teams_GP_2024 <- scrape_stat_data_general(url_general_2024, 3) %>% rename(GP = Stat)
All_Teams_Hitting_AB_2024 <- scrape_stat_data_general(url_general_2024, 4) %>% rename(AB = Stat)
All_Teams_Hitting_Runs_2024 <- scrape_stat_data_general(url_general_2024, 5) %>% rename(Run = Stat)
All_Teams_Hitting_Hits_2024 <- scrape_stat_data_general(url_general_2024, 6) %>% rename(Hit = Stat)
All_Teams_Hitting_Doubles_2024 <- scrape_stat_data_general(url_general_2024, 7) %>% rename(Double = Stat)
All_Teams_Hitting_Triples_2024 <- scrape_stat_data_general(url_general_2024, 8) %>% rename(Triple = Stat)
All_Teams_Hitting_HRs_2024 <- scrape_stat_data_general(url_general_2024, 9) %>% rename(HR = Stat)
All_Teams_Hitting_RBIs_2024 <- scrape_stat_data_general(url_general_2024, 10) %>% rename(RBI = Stat)
All_Teams_Hitting_BBs_2024 <- scrape_stat_data_general(url_general_2024, 11) %>% rename(BB = Stat)
All_Teams_Hitting_Ks_2024 <- scrape_stat_data_general(url_general_2024, 12) %>% rename(K = Stat)
All_Teams_Hitting_SBs_2024 <- scrape_stat_data_general(url_general_2024, 13) %>% rename(SB = Stat)
All_Teams_Hitting_CSs_2024 <- scrape_stat_data_general(url_general_2024, 14) %>% rename(CS = Stat)
All_Teams_Hitting_AVG_2024 <- scrape_stat_data_general(url_general_2024, 15, is_weighted = TRUE) %>% rename(AVG = Stat)
All_Teams_Hitting_OBP_2024 <- scrape_stat_data_general(url_general_2024, 16, is_weighted = TRUE) %>% rename(OBP = Stat)
All_Teams_Hitting_SLG_2024 <- scrape_stat_data_general(url_general_2024, 17, is_weighted = TRUE) %>% rename(SLG = Stat)
All_Teams_Hitting_OPS_2024 <- merge(All_Teams_Hitting_OBP_2024, All_Teams_Hitting_SLG_2024, by = "Team") %>%
  mutate(OPS = OBP + SLG) %>%
  select(Team, OPS)

All_Teams_Hitting_Singles_2024 <- merge(All_Teams_Hitting_Hits_2024, All_Teams_Hitting_Doubles_2024, by = "Team")
All_Teams_Hitting_Singles_2024 <- merge(All_Teams_Hitting_Singles_2024, All_Teams_Hitting_Triples_2024, by = "Team")
All_Teams_Hitting_Singles_2024 <- merge(All_Teams_Hitting_Singles_2024, All_Teams_Hitting_HRs_2024, by = "Team")

All_Teams_Hitting_Singles_2024 <- All_Teams_Hitting_Singles_2024 %>%
  mutate(Single = Hit - Double - Triple - HR) %>%
  select(Team, Single)


url_extended_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams?sort=pa&r=0&pos=eh"

All_Teams_Hitting_HBP_2024 <- scrape_stat_data_extended(url_extended_2024, 4) %>% rename(HBP = Stat)
All_Teams_Hitting_SF_2024 <- scrape_stat_data_extended(url_extended_2024, 5) %>% rename(SF = Stat)
All_Teams_Hitting_SH_2024 <- scrape_stat_data_extended(url_extended_2024, 6) %>% rename(SH = Stat)
All_Teams_Hitting_TB_2024 <- scrape_stat_data_extended(url_extended_2024, 7) %>% rename(TB = Stat)
All_Teams_Hitting_XBH_2024 <- scrape_stat_data_extended(url_extended_2024, 8) %>% rename(XBH = Stat)
All_Teams_Hitting_HDP_2024 <- scrape_stat_data_extended(url_extended_2024, 9) %>% rename(HDP = Stat)
All_Teams_Hitting_GO_2024 <- scrape_stat_data_extended(url_extended_2024, 10) %>% rename(GO = Stat)
All_Teams_Hitting_FO_2024 <- scrape_stat_data_extended(url_extended_2024, 11) %>% rename(FO = Stat)
All_Teams_Hitting_PA_2024 <- scrape_stat_data_extended(url_extended_2024, 13) %>% rename(PA = Stat)

All_Teams_Hitting_BB_HBP_2024 <- merge(All_Teams_Hitting_BBs_2024, All_Teams_Hitting_HBP_2024, by = "Team")

All_Teams_Hitting_BB_HBP_2024 <- All_Teams_Hitting_BB_HBP_2024 %>%
  mutate(BB_HBP = BB + HBP) %>%
  select(Team, BB_HBP)

## wOBA calculation 
library(rvest)
library(dplyr)
library(stringr)

create_player_stats_data_frames_extended <- function(url) {
  page <- read_html(url)
  
  basic_table <- page %>% html_node('table[data-col-name="ab"]')
  extended_table <- page %>% html_node('table[data-col-name="pa"]')
  fielding_table <- page %>% html_node('table[data-col-name="po"]')
  
  basic_rows <- basic_table %>% html_nodes("tr")
  extended_rows <- extended_table %>% html_nodes("tr")
  fielding_rows <- fielding_table %>% html_nodes("tr")
  
  player_data_frames <- list()
  
  for (i in 2:length(basic_rows)) {
    player_data_frames <- process_basic_stats(basic_rows[i], player_data_frames)
  }
  
  for (i in 2:length(extended_rows)) {
    player_data_frames <- process_extended_stats(extended_rows[i], player_data_frames)
  }
  
  for (i in 2:length(fielding_rows)) {
    player_data_frames <- process_fielding_stats(fielding_rows[i], player_data_frames)
  }
  
  return(player_data_frames)
}

process_basic_stats <- function(row, player_data_frames) {
  basic_data <- row %>% html_nodes('td') %>% html_text()
  player_href <- html_nodes(row, 'td a') %>% html_attr('href')
  player_identifier <- str_extract(player_href, "(?<=players/)[^/]+")
  
  if (is.null(player_identifier) || as.numeric(basic_data[7]) == 0) {
    return(player_data_frames)
  }
  
  player_name_clean <- gsub("[^[:alnum:]]", "_", player_identifier)
  df_name <- paste0(player_name_clean, "_2024_Hitting_Stats")
  
  basic_stats_values <- as.numeric(basic_data[5:19])
  ops_value <- basic_stats_values[14] + basic_stats_values[15]
  
  stat_names <- c("G", "AB", "R", "H", "2B", "3B", "HR", "RBI", "BB", "K", "SB", "CS", "AVG", "OBP", "SLG", "OPS")
  basic_stats_df <- data.frame(Stat = stat_names, Value = c(basic_stats_values[1:15], ops_value))
  
  player_data <- list()
  player_data$basic <- basic_stats_df
  
  player_data_frames[[df_name]] <- player_data
  
  return(player_data_frames)
}

process_extended_stats <- function(row, player_data_frames) {
  extended_data <- row %>% html_nodes('td') %>% html_text()
  player_href <- html_nodes(row, 'td a') %>% html_attr('href')
  player_identifier <- str_extract(player_href, "(?<=players/)[^/]+")
  
  if (is.null(player_identifier)) {
    return(player_data_frames)
  }
  
  player_name_clean <- gsub("[^[:alnum:]]", "_", player_identifier)
  df_name <- paste0(player_name_clean, "_2024_Hitting_Stats")
  
  extended_stats_values <- as.numeric(extended_data[c(6, 7, 8, 9, 11, 12, 13, 15)])
  extended_stat_names <- c("HBP", "SF", "SH", "TB", "HDP", "GO", "FO", "PA")
  extended_stats_df <- data.frame(Stat = extended_stat_names, Value = extended_stats_values)
  
  if (df_name %in% names(player_data_frames)) {
    player_data_frames[[df_name]]$extended <- extended_stats_df
  }
  
  return(player_data_frames)
}

process_fielding_stats <- function(row, player_data_frames) {
  fielding_data <- row %>% html_nodes('td') %>% html_text()
  player_href <- html_nodes(row, 'td a') %>% html_attr('href')
  player_identifier <- str_extract(player_href, "(?<=players/)[^/]+")
  
  if (is.null(player_identifier)) {
    return(player_data_frames)
  }
  
  player_name_clean <- gsub("[^[:alnum:]]", "_", player_identifier)
  df_name <- paste0(player_name_clean, "_2024_Hitting_Stats")
  
  fielding_stats_values <- as.numeric(fielding_data[c(6, 7, 8, 9, 11, 12, 13, 15, 16)])
  fielding_stat_names <- c("TC", "PO", "A", "E", "DP", "SBA", "RCS", "PB", "CI")
  fielding_stats_df <- data.frame(Stat = fielding_stat_names, Value = fielding_stats_values)
  
  if (df_name %in% names(player_data_frames)) {
    player_data_frames[[df_name]]$fielding <- fielding_stats_df
  }
  
  return(player_data_frames)
}

get_east_west_stats <- function(df) {
  df[df$Team %in% c("Frontier League East", "Frontier League West"), ]
}

east_west_AB_2024 <- get_east_west_stats(All_Teams_Hitting_AB_2024)
east_west_Hits_2024 <- get_east_west_stats(All_Teams_Hitting_Hits_2024)
east_west_Doubles_2024 <- get_east_west_stats(All_Teams_Hitting_Doubles_2024)
east_west_Triples_2024 <- get_east_west_stats(All_Teams_Hitting_Triples_2024)
east_west_HRs_2024 <- get_east_west_stats(All_Teams_Hitting_HRs_2024)
east_west_BB_HBP_2024 <- get_east_west_stats(All_Teams_Hitting_BB_HBP_2024)
east_west_Ks_2024 <- get_east_west_stats(All_Teams_Hitting_Ks_2024)
east_west_SFs_2024 <- get_east_west_stats(All_Teams_Hitting_SF_2024)
east_west_Singles_2024 <- get_east_west_stats(All_Teams_Hitting_Singles_2024)

wOBA_east_west_constants_stats_2024 <- Reduce(function(x, y) merge(x, y, by = "Team", all = TRUE), 
                                              list(east_west_AB_2024, east_west_Hits_2024, east_west_Doubles_2024, 
                                                   east_west_Triples_2024, east_west_HRs_2024, east_west_BB_HBP_2024, 
                                                   east_west_Ks_2024, east_west_SFs_2024, east_west_Singles_2024))

names(wOBA_east_west_constants_stats_2024)[2:ncol(wOBA_east_west_constants_stats_2024)] <- c("AB", "Hits", "Doubles", "Triples", "HRs", "BB_HBP", "Ks","SFs", "Singles")

wOBA_east_west_constants_stats_2024

get_league_wide_stats <- function(df) {
  df[df$Team=="Frontier League Total", ]
}

frontier_AB_2024 <- get_league_wide_stats(All_Teams_Hitting_AB_2024)
frontier_Hits_2024 <- get_league_wide_stats(All_Teams_Hitting_Hits_2024)
frontier_Doubles_2024 <- get_league_wide_stats(All_Teams_Hitting_Doubles_2024)
frontier_Triples_2024 <- get_league_wide_stats(All_Teams_Hitting_Triples_2024)
frontier_HRs_2024 <- get_league_wide_stats(All_Teams_Hitting_HRs_2024)
frontier_BB_HBP_2024 <- get_league_wide_stats(All_Teams_Hitting_BB_HBP_2024)
frontier_Ks_2024 <- get_league_wide_stats(All_Teams_Hitting_Ks_2024)
frontier_SFs_2024 <- get_league_wide_stats(All_Teams_Hitting_SF_2024)
frontier_Singles_2024 <- get_league_wide_stats(All_Teams_Hitting_Singles_2024)


wOBA_frontier_constants_stats_2024 <- Reduce(function(x, y) merge(x, y, by = "Team", all = TRUE), 
                                             list(frontier_AB_2024, frontier_Hits_2024, frontier_Doubles_2024, 
                                                  frontier_Triples_2024, frontier_HRs_2024, frontier_BB_HBP_2024, frontier_Ks_2024, frontier_SFs_2024, frontier_Singles_2024))

names(wOBA_frontier_constants_stats_2024)[2:ncol(wOBA_frontier_constants_stats_2024)] <- c("AB", "Hits", "Doubles", "Triples", "HRs", "BB_HBP", "Ks","SFs", "Singles")

wOBA_frontier_constants_stats_2024

calculate_east_west_wide_wOBA <- function(wOBA_constants_east_2024, wOBA_constants_west_2024, wOBA_east_west_constants_stats_2024) {
  
  wOBA_constants_east_2024 <- c(".732",".852","1.142","1.437","1.857") 
  wOBA_constants_west_2024 <- c(".702",".820","1.105","1.399","1.828")
  
  wOBA_constants_east_2024 <- as.numeric(wOBA_constants_east_2024)
  wOBA_constants_west_2024 <- as.numeric(wOBA_constants_west_2024)
  
  BB_HBP_constant_east <- wOBA_constants_east_2024[1]
  Singles_constant_east <- wOBA_constants_east_2024[2]
  Doubles_constant_east <- wOBA_constants_east_2024[3]
  Triples_constant_east <- wOBA_constants_east_2024[4]
  HRs_constant_east <- wOBA_constants_east_2024[5]
  
  BB_HBP_constant_west <- wOBA_constants_west_2024[1]
  Singles_constant_west <- wOBA_constants_west_2024[2]
  Doubles_constant_west <- wOBA_constants_west_2024[3]
  Triples_constant_west <- wOBA_constants_west_2024[4]
  HRs_constant_west <- wOBA_constants_west_2024[5]
  
  calculate_wOBA <- function(df, constants) {
    with(df, {
      numerator = (BB_HBP * constants[1] +
                     Singles * constants[2] +
                     Doubles * constants[3] +
                     Triples * constants[4] +
                     HRs * constants[5])
      denominator = AB + BB_HBP + SFs
      return(numerator / denominator)
    })
  }
  
  wOBA_east <- calculate_wOBA(wOBA_east_west_constants_stats_2024[wOBA_east_west_constants_stats_2024$Team == "Frontier League East", ], wOBA_constants_east_2024)
  wOBA_west <- calculate_wOBA(wOBA_east_west_constants_stats_2024[wOBA_east_west_constants_stats_2024$Team == "Frontier League West", ], wOBA_constants_west_2024)
  
  return(list("East" = wOBA_east, "West" = wOBA_west))
}

east_west_wide_wOBA_2024 <- calculate_east_west_wide_wOBA(wOBA_constants_east_2024,wOBA_constants_west_2024,wOBA_east_west_constants_stats_2024)

east_west_wide_wOBA_2024

calculate_frontier_wide_wOBA <- function(wOBA_frontier_constants_stats_2024, wOBA_constants_frontier_2024) {
  wOBA_constants_frontier_2024 <- as.numeric(c(".717",".836","1.124","1.418","1.843"))
  
  frontier_stats_2024 <- wOBA_frontier_constants_stats_2024[wOBA_frontier_constants_stats_2024$Team == "Frontier League Total", ]
  
  calculate_wOBA <- function(df, constants) {
    with(df, {
      numerator = (BB_HBP * constants[1] +
                     Singles * constants[2] +
                     Doubles * constants[3] +
                     Triples * constants[4] +
                     HRs * constants[5])
      denominator = AB + BB_HBP + SFs
      return(numerator / denominator)
    })
  }
  
  wOBA_frontier_2024 <- calculate_wOBA(frontier_stats_2024, wOBA_constants_frontier_2024)
  
  return(wOBA_frontier_2024)
}

frontier_wide_wOBA_2024 <- calculate_frontier_wide_wOBA(wOBA_frontier_constants_stats_2024, wOBA_constants_frontier_2024)
frontier_wide_wOBA_2024

scale_wOBA_constants <- function(All_Teams_Hitting_OBP_2024, frontier_wide_wOBA_2024, wOBA_constants_frontier_2024) {
  
  wOBA_constants_frontier_2024 <- as.numeric(c(".717",".836","1.124","1.418","1.843"))
  
  scale_constants <- function(OBP, league_wOBA, constants) {
    ratio <- OBP / league_wOBA
    scaled_constants <- constants * ratio
    return(scaled_constants)
  }
  
  OBP_frontier_2024 <- All_Teams_Hitting_OBP_2024[All_Teams_Hitting_OBP_2024$Team == "Frontier League Total", "OBP"]
  
  scaled_constants_frontier_2024 <- scale_constants(OBP_frontier_2024, frontier_wide_wOBA_2024, wOBA_constants_frontier_2024)
  
  scaled_constants_df <- data.frame(
    Team = "Frontier League Total",
    BB_HBP = scaled_constants_frontier_2024[1],
    Singles = scaled_constants_frontier_2024[2],
    Doubles = scaled_constants_frontier_2024[3],
    Triples = scaled_constants_frontier_2024[4],
    HRs = scaled_constants_frontier_2024[5])
  
  return(scaled_constants_df)
}

scaled_wOBA_constants_2024 <- scale_wOBA_constants(All_Teams_Hitting_OBP_2024, frontier_wide_wOBA_2024, wOBA_constants_frontier_2024)

calculate_woba_and_store <- function(player_stats_list, scaled_wOBA_constants_2024) {
  results <- data.frame(Player = character(), Team = character(), wOBA = numeric(), stringsAsFactors = FALSE)
  
  for (player_stat_name in names(player_stats_list)) {
    player_data <- player_stats_list[[player_stat_name]]
    
    if (!"basic" %in% names(player_data) || !"extended" %in% names(player_data)) {
      next 
    }
    
    team_name <- str_extract(player_stat_name, "^[^_]+")
    constants <- scaled_wOBA_constants_2024
    
    basic_stats <- player_data[["basic"]]
    extended_stats <- player_data[["extended"]]
    
    walks <- ifelse(is.na(as.numeric(basic_stats[basic_stats$Stat == "BB", "Value"])), 0, as.numeric(basic_stats[basic_stats$Stat == "BB", "Value"]))
    hbp <- ifelse(is.na(as.numeric(extended_stats[extended_stats$Stat == "HBP", "Value"])), 0, as.numeric(extended_stats[extended_stats$Stat == "HBP", "Value"]))
    singles <- ifelse(is.na(as.numeric(basic_stats[basic_stats$Stat == "H", "Value"])), 0, as.numeric(basic_stats[basic_stats$Stat == "H", "Value"])) - sum(ifelse(is.na(as.numeric(basic_stats[basic_stats$Stat %in% c("2B", "3B", "HR"), "Value"])), 0, as.numeric(basic_stats[basic_stats$Stat %in% c("2B", "3B", "HR"), "Value"])), na.rm = TRUE)
    doubles <- ifelse(is.na(as.numeric(basic_stats[basic_stats$Stat == "2B", "Value"])), 0, as.numeric(basic_stats[basic_stats$Stat == "2B", "Value"]))
    triples <- ifelse(is.na(as.numeric(basic_stats[basic_stats$Stat == "3B", "Value"])), 0, as.numeric(basic_stats[basic_stats$Stat == "3B", "Value"]))
    homeruns <- ifelse(is.na(as.numeric(basic_stats[basic_stats$Stat == "HR", "Value"])), 0, as.numeric(basic_stats[basic_stats$Stat == "HR", "Value"]))
    at_bats <- ifelse(is.na(as.numeric(basic_stats[basic_stats$Stat == "AB", "Value"])), 0, as.numeric(basic_stats[basic_stats$Stat == "AB", "Value"]))
    sac_flies <- ifelse(is.na(as.numeric(extended_stats[extended_stats$Stat == "SF", "Value"])), 0, as.numeric(extended_stats[extended_stats$Stat == "SF", "Value"]))
    
    BB_HBP_constant <- as.numeric(constants["BB_HBP"])
    single_constant <- as.numeric(constants["Singles"])
    double_constant <- as.numeric(constants["Doubles"])
    triple_constant <- as.numeric(constants["Triples"])
    HR_constant <- as.numeric(constants["HRs"])
    
    woba_numerator <- ((walks + hbp) *  BB_HBP_constant) + (singles * single_constant) + (doubles * double_constant) + (triples * triple_constant) + (homeruns * HR_constant)
    woba_denominator <- at_bats + walks + sac_flies + hbp
    
    player_woba <- woba_numerator / woba_denominator
    
    player_name <- gsub("_2024_Hitting_Stats", "", player_stat_name)
    results <- rbind(results, data.frame(Player = player_name, wOBA = player_woba))
  }
  
  return(results)
}

Lake_Erie_Crushers_Hitters_Stats_wOBA_2024 <- calculate_woba_and_store(Lake_Erie_Crushers_Hitters_Stats_List_2024, scaled_wOBA_constants_2024) 
Windy_City_Thunderbolts_Hitters_Stats_wOBA_2024 <- calculate_woba_and_store(Windy_City_Thunderbolts_Hitters_Stats_List_2024, scaled_wOBA_constants_2024) 
New_England_Knockouts_Hitters_Stats_wOBA_2024 <- calculate_woba_and_store(New_England_Knockouts_Hitters_Stats_List_2024, scaled_wOBA_constants_2024)
Joliet_Slammers_Hitters_Stats_wOBA_2024 <- calculate_woba_and_store(Joliet_Slammers_Hitters_Stats_List_2024, scaled_wOBA_constants_2024)
Washington_Wild_Things_Hitters_Stats_wOBA_2024 <- calculate_woba_and_store(Washington_Wild_Things_Hitters_Stats_List_2024, scaled_wOBA_constants_2024)
Quebec_Capitales_Hitters_Stats_wOBA_2024 <- calculate_woba_and_store(Quebec_Capitales_Hitters_Stats_List_2024, scaled_wOBA_constants_2024)
Ottawa_Titans_Hitters_Stats_wOBA_2024 <- calculate_woba_and_store(Ottawa_Titans_Hitters_Stats_List_2024, scaled_wOBA_constants_2024)
Evansville_Otters_Hitters_Stats_wOBA_2024 <- calculate_woba_and_store(Evansville_Otters_Hitters_Stats_List_2024, scaled_wOBA_constants_2024)
Florence_Yalls_Hitters_Stats_wOBA_2024 <- calculate_woba_and_store(Florence_Yalls_Hitters_Stats_List_2024, scaled_wOBA_constants_2024)
Sussex_County_Miners_Hitters_Stats_wOBA_2024 <- calculate_woba_and_store(Sussex_County_Miners_Hitters_Stats_List_2024, scaled_wOBA_constants_2024)
New_York_Boulders_Hitters_Stats_wOBA_2024 <- calculate_woba_and_store(New_York_Boulders_Hitters_Stats_List_2024, scaled_wOBA_constants_2024)
Trois_Rivieres_Aigles_Hitters_Stats_wOBA_2024 <- calculate_woba_and_store(Trois_Rivieres_Aigles_Hitters_Stats_List_2024, scaled_wOBA_constants_2024)
Schaumburg_Boomers_Hitters_Stats_wOBA_2024 <- calculate_woba_and_store(Schaumburg_Boomers_Hitters_Stats_List_2024, scaled_wOBA_constants_2024)
Tri_City_Valleycats_Hitters_Stats_wOBA_2024 <- calculate_woba_and_store(Tri_City_Valleycats_Hitters_Stats_List_2024, scaled_wOBA_constants_2024)
Gateway_Grizzlies_Hitters_Stats_wOBA_2024 <- calculate_woba_and_store(Gateway_Grizzlies_Hitters_Stats_List_2024, scaled_wOBA_constants_2024)
New_Jersey_Jackals_Hitters_Stats_wOBA_2024 <- calculate_woba_and_store(New_Jersey_Jackals_Hitters_Stats_List_2024, scaled_wOBA_constants_2024)

remove_blank_players <- function(df_list) {
  lapply(df_list, function(df) {
    df %>% filter(Player != "")
  })
}

df_list <- list(
  Lake_Erie_Crushers_Hitters_Stats_wOBA_2024,
  Windy_City_Thunderbolts_Hitters_Stats_wOBA_2024,
  New_England_Knockouts_Hitters_Stats_wOBA_2024,
  Joliet_Slammers_Hitters_Stats_wOBA_2024,
  Washington_Wild_Things_Hitters_Stats_wOBA_2024,
  Quebec_Capitales_Hitters_Stats_wOBA_2024,
  Ottawa_Titans_Hitters_Stats_wOBA_2024,
  Evansville_Otters_Hitters_Stats_wOBA_2024,
  Florence_Yalls_Hitters_Stats_wOBA_2024,
  Sussex_County_Miners_Hitters_Stats_wOBA_2024,
  New_York_Boulders_Hitters_Stats_wOBA_2024,
  Trois_Rivieres_Aigles_Hitters_Stats_wOBA_2024,
  Schaumburg_Boomers_Hitters_Stats_wOBA_2024,
  Tri_City_Valleycats_Hitters_Stats_wOBA_2024,
  Gateway_Grizzlies_Hitters_Stats_wOBA_2024,
  New_Jersey_Jackals_Hitters_Stats_wOBA_2024
)

cleaned_df_list <- remove_blank_players(df_list)

Lake_Erie_Crushers_Hitters_Stats_wOBA_2024 <- cleaned_df_list[[1]]
Windy_City_Thunderbolts_Hitters_Stats_wOBA_2024 <- cleaned_df_list[[2]]
New_England_Knockouts_Hitters_Stats_wOBA_2024 <- cleaned_df_list[[3]]
Joliet_Slammers_Hitters_Stats_wOBA_2024 <- cleaned_df_list[[4]]
Washington_Wild_Things_Hitters_Stats_wOBA_2024 <- cleaned_df_list[[5]]
Quebec_Capitales_Hitters_Stats_wOBA_2024 <- cleaned_df_list[[6]]
Ottawa_Titans_Hitters_Stats_wOBA_2024 <- cleaned_df_list[[7]]
Evansville_Otters_Hitters_Stats_wOBA_2024 <- cleaned_df_list[[8]]
Florence_Yalls_Hitters_Stats_wOBA_2024 <- cleaned_df_list[[9]]
Sussex_County_Miners_Hitters_Stats_wOBA_2024 <- cleaned_df_list[[10]]
New_York_Boulders_Hitters_Stats_wOBA_2024 <- cleaned_df_list[[11]]
Trois_Rivieres_Aigles_Hitters_Stats_wOBA_2024 <- cleaned_df_list[[12]]
Schaumburg_Boomers_Hitters_Stats_wOBA_2024 <- cleaned_df_list[[13]]
Tri_City_Valleycats_Hitters_Stats_wOBA_2024 <- cleaned_df_list[[14]]
Gateway_Grizzlies_Hitters_Stats_wOBA_2024 <- cleaned_df_list[[15]]
New_Jersey_Jackals_Hitters_Stats_wOBA_2024 <- cleaned_df_list[[16]]


##Creating data frame with desired stats for each player (with wOBA):

combine_player_stats_with_wOBA <- function(player_stats_list, team_wOBA_df) {
  combined_player_stats_list <- list()
  
  for (player_stat_name in names(player_stats_list)) {
    player_data <- player_stats_list[[player_stat_name]]
    
    if (!"basic" %in% names(player_data) || !"extended" %in% names(player_data)) {
      next
    }
    
    basic_stats <- player_data$basic
    extended_stats <- player_data$extended
    
    player_name <- gsub("_2024_Hitting_Stats", "", player_stat_name)
    player_wOBA_row <- team_wOBA_df[team_wOBA_df$Player == player_name, ]
    
    if (nrow(player_wOBA_row) == 0) {
      player_wOBA_value <- NA 
    } else {
      player_wOBA_value <- player_wOBA_row$wOBA
    }
    
    relevant_basic_stats <- basic_stats[basic_stats$Stat %in% c("G", "AB", "R", "H", "2B", "3B", "HR", "RBI", "BB", "K", "SB", "CS", "AVG", "OBP", "SLG", "OPS"), ]
    relevant_extended_stats <- extended_stats[extended_stats$Stat %in% c("HBP","SF","SH","TB","HDP","GO", "FO", "PA"), ]
    
    combined_stats <- rbind(relevant_basic_stats, relevant_extended_stats)
    combined_stats <- rbind(combined_stats, data.frame(Stat = "wOBA", Value = player_wOBA_value))
    
    combined_player_stats_list[[player_name]] <- combined_stats
  }
  
  return(combined_player_stats_list)
}

Lake_Erie_Crushers_Combined_Stats_2024 <- combine_player_stats_with_wOBA(Lake_Erie_Crushers_Hitters_Stats_List_2024, Lake_Erie_Crushers_Hitters_Stats_wOBA_2024)
Windy_City_Thunderbolts_Combined_Stats_2024 <- combine_player_stats_with_wOBA(Windy_City_Thunderbolts_Hitters_Stats_List_2024, Windy_City_Thunderbolts_Hitters_Stats_wOBA_2024)
New_England_Knockouts_Combined_Stats_2024 <- combine_player_stats_with_wOBA(New_England_Knockouts_Hitters_Stats_List_2024, New_England_Knockouts_Hitters_Stats_wOBA_2024)
Joliet_Slammers_Combined_Stats_2024 <- combine_player_stats_with_wOBA(Joliet_Slammers_Hitters_Stats_List_2024, Joliet_Slammers_Hitters_Stats_wOBA_2024)
Washington_Wild_Things_Combined_Stats_2024 <- combine_player_stats_with_wOBA(Washington_Wild_Things_Hitters_Stats_List_2024, Washington_Wild_Things_Hitters_Stats_wOBA_2024)
Quebec_Capitales_Combined_Stats_2024 <- combine_player_stats_with_wOBA(Quebec_Capitales_Hitters_Stats_List_2024, Quebec_Capitales_Hitters_Stats_wOBA_2024)
Ottawa_Titans_Combined_Stats_2024 <- combine_player_stats_with_wOBA(Ottawa_Titans_Hitters_Stats_List_2024, Ottawa_Titans_Hitters_Stats_wOBA_2024)
Evansville_Otters_Combined_Stats_2024 <- combine_player_stats_with_wOBA(Evansville_Otters_Hitters_Stats_List_2024, Evansville_Otters_Hitters_Stats_wOBA_2024)
Florence_Yalls_Combined_Stats_2024 <- combine_player_stats_with_wOBA(Florence_Yalls_Hitters_Stats_List_2024, Florence_Yalls_Hitters_Stats_wOBA_2024)
Sussex_County_Miners_Combined_Stats_2024 <- combine_player_stats_with_wOBA(Sussex_County_Miners_Hitters_Stats_List_2024, Sussex_County_Miners_Hitters_Stats_wOBA_2024)
New_York_Boulders_Combined_Stats_2024 <- combine_player_stats_with_wOBA(New_York_Boulders_Hitters_Stats_List_2024, New_York_Boulders_Hitters_Stats_wOBA_2024)
Trois_Rivieres_Aigles_Combined_Stats_2024 <- combine_player_stats_with_wOBA(Trois_Rivieres_Aigles_Hitters_Stats_List_2024, Trois_Rivieres_Aigles_Hitters_Stats_wOBA_2024)
Schaumburg_Boomers_Combined_Stats_2024 <- combine_player_stats_with_wOBA(Schaumburg_Boomers_Hitters_Stats_List_2024, Schaumburg_Boomers_Hitters_Stats_wOBA_2024)
Tri_City_Valleycats_Combined_Stats_2024 <- combine_player_stats_with_wOBA(Tri_City_Valleycats_Hitters_Stats_List_2024, Tri_City_Valleycats_Hitters_Stats_wOBA_2024)
Gateway_Grizzlies_Combined_Stats_2024 <- combine_player_stats_with_wOBA(Gateway_Grizzlies_Hitters_Stats_List_2024, Gateway_Grizzlies_Hitters_Stats_wOBA_2024)
New_Jersey_Jackals_Combined_Stats_2024 <- combine_player_stats_with_wOBA(New_Jersey_Jackals_Hitters_Stats_List_2024, New_Jersey_Jackals_Hitters_Stats_wOBA_2024)

##wRAA and WRC Calculation: 

calculate_frontier_wide_wOBA_2 <- function(wOBA_frontier_constants_stats_2024, wOBA_constants_frontier_2024) {
  wOBA_constants_frontier_2024 <- as.numeric(c(".717",".836","1.124","1.418","1.843"))
  
  frontier_stats_2024 <- wOBA_frontier_constants_stats_2024[wOBA_frontier_constants_stats_2024$Team == "Frontier League Total", ]
  
  calculate_wOBA <- function(df, constants) {
    with(df, {
      numerator = (BB_HBP * constants[1] +
                     Singles * constants[2] +
                     Doubles * constants[3] +
                     Triples * constants[4] +
                     HRs * constants[5])
      denominator = AB + BB_HBP + SFs
      return(numerator / denominator)
    })
  }
  
  wOBA_frontier_2024 <- calculate_wOBA(frontier_stats_2024, wOBA_constants_frontier_2024)
  
  return(wOBA_frontier_2024)
}

frontier_wide_wOBA_2024 <- calculate_frontier_wide_wOBA_2(wOBA_frontier_constants_stats_2024, wOBA_constants_frontier_2024)
frontier_wide_wOBA_2024

scale_wOBA_constants_2 <- function(All_Teams_Hitting_OBP_2024, frontier_wide_wOBA_2024) {
  wOBA_constants_frontier_2024 <- as.numeric(c(".717",".836","1.124","1.418","1.843"))
  
  wOBA_constants_frontier_2024 <- as.numeric(wOBA_constants_frontier_2024)
  
  ratio_frontier <- OBP_east_2024 / league_wide_wOBA_2024$East
  
  return(list(East = ratio_east, West = ratio_west))
}

OBP_frontier_2024 <- All_Teams_Hitting_OBP_2024[All_Teams_Hitting_OBP_2024$Team == "Frontier League Total", "OBP"]

scale_wOBA_constants_2 <- function(OBP_frontier_2024, frontier_wide_wOBA_2024, wOBA_constants_frontier_2024) {
  
  wOBA_constants_frontier_2024 <- as.numeric(c(".717",".836","1.124","1.418","1.843"))
  
  wOBA_constants_frontier_2024 <- as.numeric(wOBA_constants_frontier_2024)
  
  ratio_frontier_league_total <- OBP_frontier_2024 / frontier_wide_wOBA_2024
  
  return(ratio_frontier_league_total)
}


wOBA_scale_2024 <- scale_wOBA_constants_2(OBP_frontier_2024, frontier_wide_wOBA_2024, wOBA_constants_frontier_2024)


league_R <- list(East = All_Teams_Hitting_Runs_2024[All_Teams_Hitting_Runs_2024$Team == "Frontier League East", "Run"], 
                 West = All_Teams_Hitting_Runs_2024[All_Teams_Hitting_Runs_2024$Team == "Frontier League West", "Run"],
                 Frontier_League_Total = All_Teams_Hitting_Runs_2024[All_Teams_Hitting_Runs_2024$Team == "Frontier League Total", "Run"])

league_PA <- list(East = All_Teams_Hitting_PA_2024[All_Teams_Hitting_Runs_2024$Team == "Frontier League East", "PA"], 
                  West = All_Teams_Hitting_PA_2024[All_Teams_Hitting_Runs_2024$Team == "Frontier League West", "PA"],
                  Frontier_League_Total = All_Teams_Hitting_PA_2024[All_Teams_Hitting_Runs_2024$Team == "Frontier League Total", "PA"])

runs_league <- league_R$Frontier_League_Total
pa_league <- league_PA$Frontier_League_Total

calculate_wRAA_wRC_BaBIP_ISO_K_BB_K_BB_Percent <- function(combined_player_stats, frontier_wide_wOBA_2024, wOBA_scale_2024, runs_league, pa_league) {
  
  for (player_name in names(combined_player_stats)) {
    player_stats <- combined_player_stats[[player_name]]
    
    if ("wOBA" %in% player_stats$Stat && "PA" %in% player_stats$Stat) {
      player_wOBA <- as.numeric(player_stats[player_stats$Stat == "wOBA", "Value"])
      player_PA <- as.numeric(player_stats[player_stats$Stat == "PA", "Value"])
      player_BB <- as.numeric(player_stats[player_stats$Stat == "BB", "Value"])
      player_SLG <- as.numeric(player_stats[player_stats$Stat == "SLG", "Value"])
      player_AVG <- as.numeric(player_stats[player_stats$Stat == "AVG", "Value"])
      player_H <- ifelse(is.na(as.numeric(player_stats[player_stats$Stat == "H", "Value"])), 0, as.numeric(player_stats[player_stats$Stat == "H", "Value"]))
      player_HR <- ifelse(is.na(as.numeric(player_stats[player_stats$Stat == "HR", "Value"])), 0, as.numeric(player_stats[player_stats$Stat == "HR", "Value"]))
      player_AB <- as.numeric(player_stats[player_stats$Stat == "AB", "Value"])
      player_K <- ifelse(is.na(as.numeric(player_stats[player_stats$Stat == "K", "Value"])), 0, as.numeric(player_stats[player_stats$Stat == "K", "Value"]))
      player_SF <- ifelse(is.na(as.numeric(player_stats[player_stats$Stat == "SF", "Value"])), 0, as.numeric(player_stats[player_stats$Stat == "SF", "Value"]))
      player_SH <- ifelse(is.na(as.numeric(player_stats[player_stats$Stat == "SH", "Value"])), 0, as.numeric(player_stats[player_stats$Stat == "SH", "Value"]))
      
      player_wRAA <- ((player_wOBA - frontier_wide_wOBA_2024) / wOBA_scale_2024) * player_PA
      player_wRC <- (((player_wOBA - frontier_wide_wOBA_2024) / wOBA_scale_2024) + (runs_league / pa_league)) * player_PA
      player_BaBIP <- (player_H - player_HR) / (player_AB - player_K - player_HR + player_SF)
      
      player_ISO <- player_SLG - player_AVG
      player_K_Percent <- player_K / player_PA
      player_BB_Percent <- player_BB / player_PA
      player_K_BB_Percent <- player_K_Percent - player_BB_Percent
      
      player_new_stats <- data.frame(
        Stat = c("wRAA", "wRC", "BaBIP", "ISO", "K%", "BB%", "(K-BB)%"), 
        Value = c(player_wRAA, player_wRC, player_BaBIP, player_ISO, player_K_Percent, player_BB_Percent, player_K_BB_Percent)
      )
      
      player_stats <- rbind(player_stats, player_new_stats)
      combined_player_stats[[player_name]] <- player_stats
    }
  }
  return(combined_player_stats)
}


New_Jersey_Jackals_Combined_Stats_2_2024 <- calculate_wRAA_wRC_BaBIP_ISO_K_BB_K_BB_Percent(New_Jersey_Jackals_Combined_Stats_2024, frontier_wide_wOBA_2024, wOBA_scale_2024, runs_league, pa_league)
New_York_Boulders_Combined_Stats_2_2024 <- calculate_wRAA_wRC_BaBIP_ISO_K_BB_K_BB_Percent(New_York_Boulders_Combined_Stats_2024, frontier_wide_wOBA_2024, wOBA_scale_2024, runs_league, pa_league)
Ottawa_Titans_Combined_Stats_2_2024 <- calculate_wRAA_wRC_BaBIP_ISO_K_BB_K_BB_Percent(Ottawa_Titans_Combined_Stats_2024, frontier_wide_wOBA_2024, wOBA_scale_2024, runs_league, pa_league)
Quebec_Capitales_Combined_Stats_2_2024 <- calculate_wRAA_wRC_BaBIP_ISO_K_BB_K_BB_Percent(Quebec_Capitales_Combined_Stats_2024, frontier_wide_wOBA_2024, wOBA_scale_2024, runs_league, pa_league)
Sussex_County_Miners_Combined_Stats_2_2024 <- calculate_wRAA_wRC_BaBIP_ISO_K_BB_K_BB_Percent(Sussex_County_Miners_Combined_Stats_2024, frontier_wide_wOBA_2024, wOBA_scale_2024, runs_league, pa_league)
Trois_Rivieres_Aigles_Combined_Stats_2_2024 <- calculate_wRAA_wRC_BaBIP_ISO_K_BB_K_BB_Percent(Trois_Rivieres_Aigles_Combined_Stats_2024, frontier_wide_wOBA_2024, wOBA_scale_2024, runs_league, pa_league)
Tri_City_Valleycats_Combined_Stats_2_2024 <- calculate_wRAA_wRC_BaBIP_ISO_K_BB_K_BB_Percent(Tri_City_Valleycats_Combined_Stats_2024, frontier_wide_wOBA_2024, wOBA_scale_2024, runs_league, pa_league)
New_England_Knockouts_Combined_Stats_2_2024 <- calculate_wRAA_wRC_BaBIP_ISO_K_BB_K_BB_Percent(New_England_Knockouts_Combined_Stats_2024, frontier_wide_wOBA_2024, wOBA_scale_2024, runs_league, pa_league)
Lake_Erie_Crushers_Combined_Stats_2_2024 <- calculate_wRAA_wRC_BaBIP_ISO_K_BB_K_BB_Percent(Lake_Erie_Crushers_Combined_Stats_2024, frontier_wide_wOBA_2024, wOBA_scale_2024, runs_league, pa_league)
Evansville_Otters_Combined_Stats_2_2024 <- calculate_wRAA_wRC_BaBIP_ISO_K_BB_K_BB_Percent(Evansville_Otters_Combined_Stats_2024, frontier_wide_wOBA_2024, wOBA_scale_2024, runs_league, pa_league)
Florence_Yalls_Combined_Stats_2_2024 <- calculate_wRAA_wRC_BaBIP_ISO_K_BB_K_BB_Percent(Florence_Yalls_Combined_Stats_2024, frontier_wide_wOBA_2024, wOBA_scale_2024, runs_league, pa_league)
Gateway_Grizzlies_Combined_Stats_2_2024 <- calculate_wRAA_wRC_BaBIP_ISO_K_BB_K_BB_Percent(Gateway_Grizzlies_Combined_Stats_2024, frontier_wide_wOBA_2024, wOBA_scale_2024, runs_league, pa_league)
Joliet_Slammers_Combined_Stats_2_2024 <- calculate_wRAA_wRC_BaBIP_ISO_K_BB_K_BB_Percent(Joliet_Slammers_Combined_Stats_2024, frontier_wide_wOBA_2024, wOBA_scale_2024, runs_league, pa_league)
Schaumburg_Boomers_Combined_Stats_2_2024 <- calculate_wRAA_wRC_BaBIP_ISO_K_BB_K_BB_Percent(Schaumburg_Boomers_Combined_Stats_2024, frontier_wide_wOBA_2024, wOBA_scale_2024, runs_league, pa_league)
Washington_Wild_Things_Combined_Stats_2_2024 <- calculate_wRAA_wRC_BaBIP_ISO_K_BB_K_BB_Percent(Washington_Wild_Things_Combined_Stats_2024, frontier_wide_wOBA_2024, wOBA_scale_2024, runs_league, pa_league)
Windy_City_Thunderbolts_Combined_Stats_2_2024 <- calculate_wRAA_wRC_BaBIP_ISO_K_BB_K_BB_Percent(Windy_City_Thunderbolts_Combined_Stats_2024, frontier_wide_wOBA_2024, wOBA_scale_2024, runs_league, pa_league)

##Ballpark Factor Calculation

##Calculating 2022 stats 

scrape_stat_data_general <- function(url, column_index, is_weighted = FALSE) {
  library(rvest)
  
  page <- read_html(url)
  rows <- page %>% html_nodes("table.table.table-hover.table-sm tr")
  
  team_names <- rows %>% html_nodes("td.pinned-col.text a") %>% html_text()
  stats <- rows %>% html_nodes(sprintf("td:nth-child(%d)", column_index)) %>% html_text()
  
  valid_entries <- team_names != ""
  team_names <- team_names[valid_entries]
  stats <- as.numeric(stats[valid_entries])
  
  team_names <- head(team_names, 16)
  stats <- head(stats, 16)
  
  east_teams <- c("New Jersey Jackals", "Ottawa Titans", "Trois-Rivières Aigles", "New York Boulders", "Tri-City ValleyCats", "Empire State Greys", "Sussex County Miners", "Québec Capitales")
  west_teams <- setdiff(team_names, east_teams)
  
  data <- data.frame(Team = team_names, Stat = stats)
  
  if (is_weighted) {
    abs <- rows %>% html_nodes("td:nth-child(4)") %>% html_text()
    abs <- as.numeric(abs[valid_entries])
    abs <- head(abs, 16)
    
    east_weighted_sum <- sum(data$Stat[data$Team %in% east_teams] * abs[data$Team %in% east_teams]) / sum(abs[data$Team %in% east_teams])
    west_weighted_sum <- sum(data$Stat[data$Team %in% west_teams] * abs[data$Team %in% west_teams]) / sum(abs[data$Team %in% west_teams])
    total_weighted_sum <- sum(data$Stat * abs) / sum(abs) 
    
    data <- rbind(data, data.frame(Team = "Frontier League East", Stat = east_weighted_sum))
    data <- rbind(data, data.frame(Team = "Frontier League West", Stat = west_weighted_sum))
    data <- rbind(data, data.frame(Team = "Frontier League Total", Stat = total_weighted_sum))  
  } else {
    east_sum <- sum(data$Stat[data$Team %in% east_teams])
    west_sum <- sum(data$Stat[data$Team %in% west_teams])
    total_sum <- sum(data$Stat)  
    
    data <- rbind(data, data.frame(Team = "Frontier League East", Stat = east_sum))
    data <- rbind(data, data.frame(Team = "Frontier League West", Stat = west_sum))
    data <- rbind(data, data.frame(Team = "Frontier League Total", Stat = total_sum))  
  }
  
  return(data)
}


scrape_stat_data_extended <- function(url, column_index) {
  page <- read_html(url)
  
  table <- page %>% html_node('table[data-col-name="pa"]')
  rows <- table %>% html_nodes("tr")
  
  team_names <- rows %>% html_nodes("td.pinned-col.text a") %>% html_text()
  stats <- rows %>% html_nodes(sprintf("td:nth-child(%d)", column_index)) %>% html_text()
  
  valid_entries <- team_names != ""
  team_names <- team_names[valid_entries]
  stats <- as.numeric(stats[valid_entries])
  
  team_names <- head(team_names, 16)
  stats <- head(stats, 16)
  
  east_teams <- c("New Jersey Jackals", "Ottawa Titans", "Trois-Rivières Aigles", 
                  "New York Boulders", "Tri-City ValleyCats", "Empire State Greys", 
                  "Sussex County Miners", "Québec Capitales")
  west_teams <- setdiff(team_names, east_teams)
  
  data <- data.frame(Team = team_names, Stat = stats)
  
  east_sum <- sum(data$Stat[data$Team %in% east_teams])
  west_sum <- sum(data$Stat[data$Team %in% west_teams])
  total_sum <- sum(data$Stat)
  
  data <- rbind(data, data.frame(Team = "Frontier League East", Stat = east_sum))
  data <- rbind(data, data.frame(Team = "Frontier League West", Stat = west_sum))
  data <- rbind(data, data.frame(Team = "Frontier League Total", Stat = total_sum))  
  
  return(data)
}

scrape_stat_data_pitching <- function(url, column_index, is_weighted = FALSE) {
  page <- read_html(url)
  
  table <- page %>% html_node('table[data-col-name="era"]')
  rows <- table %>% html_nodes("tr")
  
  team_names <- rows %>% html_nodes("td.pinned-col.text a") %>% html_text()
  stats <- rows %>% html_nodes(sprintf("td:nth-child(%d)", column_index)) %>% html_text()
  
  valid_entries <- team_names != ""
  team_names <- team_names[valid_entries]
  stats <- as.numeric(stats[valid_entries])
  
  team_names <- head(team_names, 16)
  stats <- head(stats, 16)
  
  east_teams <- c("New Jersey Jackals", "Ottawa Titans", "Trois-Rivières Aigles", 
                  "New York Boulders", "Tri-City ValleyCats", "Empire State Greys", 
                  "Sussex County Miners", "Québec Capitales")
  west_teams <- setdiff(team_names, east_teams)
  
  data <- data.frame(Team = team_names, Stat = stats)
  
  if (is_weighted) {
    ips <- rows %>% html_nodes("td:nth-child(4)") %>% html_text()
    ips <- as.numeric(ips[valid_entries])
    ips <- head(ips, 16)
    
    east_weighted_sum <- sum(data$Stat[data$Team %in% east_teams] * ips[data$Team %in% east_teams]) / sum(ips[data$Team %in% east_teams])
    west_weighted_sum <- sum(data$Stat[data$Team %in% west_teams] * ips[data$Team %in% west_teams]) / sum(ips[data$Team %in% west_teams])
    total_weighted_sum <- sum(data$Stat * ips) / sum(ips) 
    
    data <- rbind(data, data.frame(Team = "Frontier League East", Stat = east_weighted_sum))
    data <- rbind(data, data.frame(Team = "Frontier League West", Stat = west_weighted_sum))
    data <- rbind(data, data.frame(Team = "Frontier League Total", Stat = total_weighted_sum))  
  } else {
    east_sum <- sum(data$Stat[data$Team %in% east_teams])
    west_sum <- sum(data$Stat[data$Team %in% west_teams])
    total_sum <- sum(data$Stat)  
    
    data <- rbind(data, data.frame(Team = "Frontier League East", Stat = east_sum))
    data <- rbind(data, data.frame(Team = "Frontier League West", Stat = west_sum))
    data <- rbind(data, data.frame(Team = "Frontier League Total", Stat = total_sum))  
  }
  
  return(data)
}

scrape_stat_data_fielding <- function(url, column_index, is_weighted = FALSE) {
  page <- read_html(url)
  
  table <- page %>% html_node('table[data-col-name="fpct"]')
  rows <- table %>% html_nodes("tr")
  
  team_names <- rows %>% html_nodes("td.pinned-col.text a") %>% html_text()
  stats <- rows %>% html_nodes(sprintf("td:nth-child(%d)", column_index)) %>% html_text()
  
  valid_entries <- team_names != ""
  team_names <- team_names[valid_entries]
  stats <- as.numeric(stats[valid_entries])
  
  team_names <- head(team_names, 16)
  stats <- head(stats, 16)
  
  east_teams <- c("New Jersey Jackals", "Ottawa Titans", "Trois-Rivières Aigles", 
                  "New York Boulders", "Tri-City ValleyCats", "Empire State Greys", 
                  "Sussex County Miners", "Québec Capitales")
  west_teams <- setdiff(team_names, east_teams)
  
  data <- data.frame(Team = team_names, Stat = stats)
  
  if (is_weighted) {
    tcs <- rows %>% html_nodes("td:nth-child(4)") %>% html_text()
    tcs <- as.numeric(tcs[valid_entries])
    tcs <- head(tcs, 16)
    
    east_weighted_sum <- sum(data$Stat[data$Team %in% east_teams] * tcs[data$Team %in% east_teams]) / sum(tcs[data$Team %in% east_teams])
    west_weighted_sum <- sum(data$Stat[data$Team %in% west_teams] * tcs[data$Team %in% west_teams]) / sum(tcs[data$Team %in% west_teams])
    total_weighted_sum <- sum(data$Stat * tcs) / sum(tcs) 
    
    
    data <- rbind(data, data.frame(Team = "Frontier League East", Stat = east_weighted_sum))
    data <- rbind(data, data.frame(Team = "Frontier League West", Stat = west_weighted_sum))
    data <- rbind(data, data.frame(Team = "Frontier League Total", Stat = total_weighted_sum))  
    
  } else {
    east_sum <- sum(data$Stat[data$Team %in% east_teams])
    west_sum <- sum(data$Stat[data$Team %in% west_teams])
    total_sum <- sum(data$Stat)  
    
    data <- rbind(data, data.frame(Team = "Frontier League East", Stat = east_sum))
    data <- rbind(data, data.frame(Team = "Frontier League West", Stat = west_sum))
    data <- rbind(data, data.frame(Team = "Frontier League Total", Stat = total_sum))  
  }
  
  return(data)
}

url_general_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams?sort=avg&r=0&pos=h"

All_Teams_GP_2022 <- scrape_stat_data_general(url_general_2022, 3) %>% rename(GP = Stat)
All_Teams_Hitting_AB_2022 <- scrape_stat_data_general(url_general_2022, 4) %>% rename(AB = Stat)
All_Teams_Hitting_Runs_2022 <- scrape_stat_data_general(url_general_2022, 5) %>% rename(Run = Stat)
All_Teams_Hitting_Hits_2022 <- scrape_stat_data_general(url_general_2022, 6) %>% rename(Hit = Stat)
All_Teams_Hitting_Doubles_2022 <- scrape_stat_data_general(url_general_2022, 7) %>% rename(Double = Stat)
All_Teams_Hitting_Triples_2022 <- scrape_stat_data_general(url_general_2022, 8) %>% rename(Triple = Stat)
All_Teams_Hitting_HRs_2022 <- scrape_stat_data_general(url_general_2022, 9) %>% rename(HR = Stat)
All_Teams_Hitting_RBIs_2022 <- scrape_stat_data_general(url_general_2022, 10) %>% rename(RBI = Stat)
All_Teams_Hitting_BBs_2022 <- scrape_stat_data_general(url_general_2022, 11) %>% rename(BB = Stat)
All_Teams_Hitting_Ks_2022 <- scrape_stat_data_general(url_general_2022, 12) %>% rename(K = Stat)
All_Teams_Hitting_SBs_2022 <- scrape_stat_data_general(url_general_2022, 13) %>% rename(SB = Stat)
All_Teams_Hitting_CSs_2022 <- scrape_stat_data_general(url_general_2022, 14) %>% rename(CS = Stat)
All_Teams_Hitting_AVG_2022 <- scrape_stat_data_general(url_general_2022, 15, is_weighted = TRUE) %>% rename(AVG = Stat)
All_Teams_Hitting_OBP_2022 <- scrape_stat_data_general(url_general_2022, 16, is_weighted = TRUE) %>% rename(OBP = Stat)
All_Teams_Hitting_SLG_2022 <- scrape_stat_data_general(url_general_2022, 17, is_weighted = TRUE) %>% rename(SLG = Stat)
All_Teams_Hitting_OPS_2022 <- merge(All_Teams_Hitting_OBP_2022, All_Teams_Hitting_SLG_2022, by = "Team") %>%
  mutate(OPS = OBP + SLG) %>%
  select(Team, OPS)
All_Teams_Hitting_Singles_2022 <- merge(All_Teams_Hitting_Hits_2022, All_Teams_Hitting_Doubles_2022, by = "Team")
All_Teams_Hitting_Singles_2022 <- merge(All_Teams_Hitting_Singles_2022, All_Teams_Hitting_Triples_2022, by = "Team")
All_Teams_Hitting_Singles_2022 <- merge(All_Teams_Hitting_Singles_2022, All_Teams_Hitting_HRs_2022, by = "Team")

All_Teams_Hitting_Singles_2022 <- All_Teams_Hitting_Singles_2022 %>%
  mutate(Single = Hit - Double - Triple - HR) %>%
  select(Team, Single)

url_extended_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams?sort=pa&r=0&pos=eh"

All_Teams_Hitting_HBP_2022 <- scrape_stat_data_extended(url_extended_2022, 4) %>% rename(HBP = Stat)
All_Teams_Hitting_SF_2022 <- scrape_stat_data_extended(url_extended_2022, 5) %>% rename(SF = Stat)
All_Teams_Hitting_SH_2022 <- scrape_stat_data_extended(url_extended_2022, 6) %>% rename(SH = Stat)
All_Teams_Hitting_TB_2022 <- scrape_stat_data_extended(url_extended_2022, 7) %>% rename(TB = Stat)
All_Teams_Hitting_XBH_2022 <- scrape_stat_data_extended(url_extended_2022, 8) %>% rename(XBH = Stat)
All_Teams_Hitting_HDP_2022 <- scrape_stat_data_extended(url_extended_2022, 9) %>% rename(HDP = Stat)
All_Teams_Hitting_GO_2022 <- scrape_stat_data_extended(url_extended_2022, 10) %>% rename(GO = Stat)
All_Teams_Hitting_FO_2022 <- scrape_stat_data_extended(url_extended_2022, 11) %>% rename(FO = Stat)
All_Teams_Hitting_PA_2022 <- scrape_stat_data_extended(url_extended_2022, 13) %>% rename(PA = Stat)

All_Teams_Hitting_BB_HBP_2022 <- merge(All_Teams_Hitting_BBs_2022, All_Teams_Hitting_HBP_2022, by = "Team")

All_Teams_Hitting_BB_HBP_2022 <- All_Teams_Hitting_BB_HBP_2022 %>%
  mutate(BB_HBP = BB + HBP) %>%
  select(Team, BB_HBP)


##Calculating 2022 and 2023 hitting stats 

library(rvest)
library(dplyr)
library(stringr)
scrape_home_away_stats_dataframe <- function(url) {
  
  page <- read_html(url)
  
  stats_container <- page %>% html_node("#gamelog-splits-h") %>% html_table(fill = TRUE)
  
  stat_labels <- c("GP", "AB", "R", "H", "2B", "3B", "HR", "RBI", "BB", "K", "SB", "CS", "AVG", "OBP", "SLG")
  
  home_stats <- as.numeric(sapply(stats_container[which(stats_container[,1] == "Home"), -1], function(x) gsub("[^0-9.-]", "", x)))
  away_stats <- as.numeric(sapply(stats_container[which(stats_container[,1] == "Away"), -1], function(x) gsub("[^0-9.-]", "", x)))
  
  team_stats_df <- data.frame(Stat = stat_labels, Home = home_stats, Away = away_stats, stringsAsFactors = FALSE)
  
  return(team_stats_df)
}

lake_erie_crushers_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/lakeeriecrushers?view=splits"
lake_erie_crushers_home_away_stats_2022 <- scrape_home_away_stats_dataframe(lake_erie_crushers_home_away_url_2022)
lake_erie_crushers_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/lakeeriecrushers?view=splits"
lake_erie_crushers_home_away_stats_2023 <- scrape_home_away_stats_dataframe(lake_erie_crushers_home_away_url_2023)
windy_city_thunderbolts_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/windycitythunderbolts?view=splits"
windy_city_thunderbolts_home_away_stats_2022 <- scrape_home_away_stats_dataframe(windy_city_thunderbolts_home_away_url_2022)
windy_city_thunderbolts_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/windycitythunderbolts?view=splits"
windy_city_thunderbolts_home_away_stats_2023 <- scrape_home_away_stats_dataframe(windy_city_thunderbolts_home_away_url_2023)
empire_state_greys_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/empirestategreys?view=splits"
empire_state_greys_home_away_stats_2022 <- scrape_home_away_stats_dataframe(empire_state_greys_home_away_url_2022)
empire_state_greys_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/empirestategreys?view=splits"
empire_state_greys_home_away_stats_2023 <- scrape_home_away_stats_dataframe(empire_state_greys_home_away_url_2023)

joliet_slammers_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/jolietslammers?view=splits"
joliet_slammers_home_away_stats_2022 <- scrape_home_away_stats_dataframe(joliet_slammers_home_away_url_2022)
joliet_slammers_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/jolietslammers?view=splits"
joliet_slammers_home_away_stats_2023 <- scrape_home_away_stats_dataframe(joliet_slammers_home_away_url_2023)

washington_wild_things_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/washingtonwildthings?view=splits"
washington_wild_things_home_away_stats_2022 <- scrape_home_away_stats_dataframe(washington_wild_things_home_away_url_2022)
washington_wild_things_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/washingtonwildthings?view=splits"
washington_wild_things_home_away_stats_2023 <- scrape_home_away_stats_dataframe(washington_wild_things_home_away_url_2023)

quebec_capitales_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/quebeccapitales?view=splits"
quebec_capitales_home_away_stats_2022 <- scrape_home_away_stats_dataframe(quebec_capitales_home_away_url_2022)
quebec_capitales_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/quebeccapitales?view=splits"
quebec_capitales_home_away_stats_2023 <- scrape_home_away_stats_dataframe(quebec_capitales_home_away_url_2023)

ottawa_titans_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/ottawatitans?view=splits"
ottawa_titans_home_away_stats_2022 <- scrape_home_away_stats_dataframe(ottawa_titans_home_away_url_2022)
ottawa_titans_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/ottawatitans?view=splits"
ottawa_titans_home_away_stats_2023 <- scrape_home_away_stats_dataframe(ottawa_titans_home_away_url_2023)

evansville_otters_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/evansvilleotters?view=splits"
evansville_otters_home_away_stats_2022 <- scrape_home_away_stats_dataframe(evansville_otters_home_away_url_2022)
evansville_otters_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/evansvilleotters?view=splits"
evansville_otters_home_away_stats_2023 <- scrape_home_away_stats_dataframe(evansville_otters_home_away_url_2023)

florence_yalls_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/florenceyalls?view=splits"
florence_yalls_home_away_stats_2022 <- scrape_home_away_stats_dataframe(florence_yalls_home_away_url_2022)
florence_yalls_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/florenceyalls?view=splits"
florence_yalls_home_away_stats_2023 <- scrape_home_away_stats_dataframe(florence_yalls_home_away_url_2023)

sussex_county_miners_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/sussexcountyminers?view=splits"
sussex_county_miners_home_away_stats_2022 <- scrape_home_away_stats_dataframe(sussex_county_miners_home_away_url_2022)
sussex_county_miners_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/sussexcountyminers?view=splits"
sussex_county_miners_home_away_stats_2023 <- scrape_home_away_stats_dataframe(sussex_county_miners_home_away_url_2023)

new_york_boulders_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/newyorkboulders?view=splits"
new_york_boulders_home_away_stats_2022 <- scrape_home_away_stats_dataframe(new_york_boulders_home_away_url_2022)
new_york_boulders_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/newyorkboulders?view=splits"
new_york_boulders_home_away_stats_2023 <- scrape_home_away_stats_dataframe(new_york_boulders_home_away_url_2023)

trois_rivieres_aigles_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/troisrivieresaigles?view=splits"
trois_rivieres_aigles_home_away_stats_2022 <- scrape_home_away_stats_dataframe(trois_rivieres_aigles_home_away_url_2022)
trois_rivieres_aigles_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/troisrivieresaigles?view=splits"
trois_rivieres_aigles_home_away_stats_2023 <- scrape_home_away_stats_dataframe(trois_rivieres_aigles_home_away_url_2023)

schaumburg_boomers_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/schaumburgboomers?view=splits"
schaumburg_boomers_home_away_stats_2022 <- scrape_home_away_stats_dataframe(schaumburg_boomers_home_away_url_2022)
schaumburg_boomers_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/schaumburgboomers?view=splits"
schaumburg_boomers_home_away_stats_2023 <- scrape_home_away_stats_dataframe(schaumburg_boomers_home_away_url_2023)

tri_city_valleycats_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/tricityvalleycats?view=splits"
tri_city_valleycats_home_away_stats_2022 <- scrape_home_away_stats_dataframe(tri_city_valleycats_home_away_url_2022)
tri_city_valleycats_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/tricityvalleycats?view=splits"
tri_city_valleycats_home_away_stats_2023 <- scrape_home_away_stats_dataframe(tri_city_valleycats_home_away_url_2023)

gateway_grizzlies_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/gatewaygrizzlies?view=splits"
gateway_grizzlies_home_away_stats_2022 <- scrape_home_away_stats_dataframe(gateway_grizzlies_home_away_url_2022)
gateway_grizzlies_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/gatewaygrizzlies?view=splits"
gateway_grizzlies_home_away_stats_2023 <- scrape_home_away_stats_dataframe(gateway_grizzlies_home_away_url_2023)

new_jersey_jackals_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/newjerseyjackals?view=splits"
new_jersey_jackals_home_away_stats_2022 <- scrape_home_away_stats_dataframe(new_jersey_jackals_home_away_url_2022)
new_jersey_jackals_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/newjerseyjackals?view=splits"
new_jersey_jackals_home_away_stats_2023 <- scrape_home_away_stats_dataframe(new_jersey_jackals_home_away_url_2023)

##Calculating 2022 and 2023 pitching stats 

library(rvest)
library(dplyr)
library(stringr)
scrape_home_away_stats_pitching_dataframe <- function(url) {
  
  page <- read_html(url)
  
  stats_container <- page %>% html_node("#gamelog-splits-p") %>% html_table(fill = TRUE)
  
  stat_labels <- c("GP", "IP", "H", "R", "ER", "BB", "K", "K/9", "HR", "ERA", "WHIP")
  
  home_stats_pitching <- as.numeric(sapply(stats_container[which(stats_container[,1] == "Home"), -1], function(x) gsub("[^0-9.-]", "", x)))
  away_stats_pitching <- as.numeric(sapply(stats_container[which(stats_container[,1] == "Away"), -1], function(x) gsub("[^0-9.-]", "", x)))
  
  team_stats_df <- data.frame(Stat = stat_labels, Home = home_stats_pitching, Away = away_stats_pitching, stringsAsFactors = FALSE)
  
  return(team_stats_df)
}
lake_erie_crushers_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/lakeeriecrushers?view=splits"
lake_erie_crushers_home_away_stats_pitching_2022 <- scrape_home_away_stats_pitching_dataframe(lake_erie_crushers_home_away_url_2022)
lake_erie_crushers_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/lakeeriecrushers?view=splits"
lake_erie_crushers_home_away_stats_pitching_2023 <- scrape_home_away_stats_pitching_dataframe(lake_erie_crushers_home_away_url_2023)

windy_city_thunderbolts_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/windycitythunderbolts?view=splits"
windy_city_thunderbolts_home_away_stats_pitching_2022 <- scrape_home_away_stats_pitching_dataframe(windy_city_thunderbolts_home_away_url_2022)
windy_city_thunderbolts_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/windycitythunderbolts?view=splits"
windy_city_thunderbolts_home_away_stats_pitching_2023 <- scrape_home_away_stats_pitching_dataframe(windy_city_thunderbolts_home_away_url_2023)

empire_state_greys_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/empirestategreys?view=splits"
empire_state_greys_home_away_stats_pitching_2022 <- scrape_home_away_stats_pitching_dataframe(empire_state_greys_home_away_url_2022)
empire_state_greys_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/empirestategreys?view=splits"
empire_state_greys_home_away_stats_pitching_2023 <- scrape_home_away_stats_pitching_dataframe(empire_state_greys_home_away_url_2023)

joliet_slammers_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/jolietslammers?view=splits"
joliet_slammers_home_away_stats_pitching_2022 <- scrape_home_away_stats_pitching_dataframe(joliet_slammers_home_away_url_2022)
joliet_slammers_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/jolietslammers?view=splits"
joliet_slammers_home_away_stats_pitching_2023 <- scrape_home_away_stats_pitching_dataframe(joliet_slammers_home_away_url_2023)

washington_wild_things_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/washingtonwildthings?view=splits"
washington_wild_things_home_away_stats_pitching_2022 <- scrape_home_away_stats_pitching_dataframe(washington_wild_things_home_away_url_2022)
washington_wild_things_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/washingtonwildthings?view=splits"
washington_wild_things_home_away_stats_pitching_2023 <- scrape_home_away_stats_pitching_dataframe(washington_wild_things_home_away_url_2023)

quebec_capitales_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/quebeccapitales?view=splits"
quebec_capitales_home_away_stats_pitching_2022 <- scrape_home_away_stats_pitching_dataframe(quebec_capitales_home_away_url_2022)
quebec_capitales_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/quebeccapitales?view=splits"
quebec_capitales_home_away_stats_pitching_2023 <- scrape_home_away_stats_pitching_dataframe(quebec_capitales_home_away_url_2023)

ottawa_titans_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/ottawatitans?view=splits"
ottawa_titans_home_away_stats_pitching_2022 <- scrape_home_away_stats_pitching_dataframe(ottawa_titans_home_away_url_2022)
ottawa_titans_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/ottawatitans?view=splits"
ottawa_titans_home_away_stats_pitching_2023 <- scrape_home_away_stats_pitching_dataframe(ottawa_titans_home_away_url_2023)

evansville_otters_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/evansvilleotters?view=splits"
evansville_otters_home_away_stats_pitching_2022 <- scrape_home_away_stats_pitching_dataframe(evansville_otters_home_away_url_2022)
evansville_otters_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/evansvilleotters?view=splits"
evansville_otters_home_away_stats_pitching_2023 <- scrape_home_away_stats_pitching_dataframe(evansville_otters_home_away_url_2023)

florence_yalls_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/florenceyalls?view=splits"
florence_yalls_home_away_stats_pitching_2022 <- scrape_home_away_stats_pitching_dataframe(florence_yalls_home_away_url_2022)
florence_yalls_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/florenceyalls?view=splits"
florence_yalls_home_away_stats_pitching_2023 <- scrape_home_away_stats_pitching_dataframe(florence_yalls_home_away_url_2023)

sussex_county_miners_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/sussexcountyminers?view=splits"
sussex_county_miners_home_away_stats_pitching_2022 <- scrape_home_away_stats_pitching_dataframe(sussex_county_miners_home_away_url_2022)
sussex_county_miners_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/sussexcountyminers?view=splits"
sussex_county_miners_home_away_stats_pitching_2023 <- scrape_home_away_stats_pitching_dataframe(sussex_county_miners_home_away_url_2023)

new_york_boulders_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/newyorkboulders?view=splits"
new_york_boulders_home_away_stats_pitching_2022 <- scrape_home_away_stats_pitching_dataframe(new_york_boulders_home_away_url_2022)
new_york_boulders_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/newyorkboulders?view=splits"
new_york_boulders_home_away_stats_pitching_2023 <- scrape_home_away_stats_pitching_dataframe(new_york_boulders_home_away_url_2023)

trois_rivieres_aigles_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/troisrivieresaigles?view=splits"
trois_rivieres_aigles_home_away_stats_pitching_2022 <- scrape_home_away_stats_pitching_dataframe(trois_rivieres_aigles_home_away_url_2022)
trois_rivieres_aigles_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/troisrivieresaigles?view=splits"
trois_rivieres_aigles_home_away_stats_pitching_2023 <- scrape_home_away_stats_pitching_dataframe(trois_rivieres_aigles_home_away_url_2023)

schaumburg_boomers_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/schaumburgboomers?view=splits"
schaumburg_boomers_home_away_stats_pitching_2022 <- scrape_home_away_stats_pitching_dataframe(schaumburg_boomers_home_away_url_2022)
schaumburg_boomers_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/schaumburgboomers?view=splits"
schaumburg_boomers_home_away_stats_pitching_2023 <- scrape_home_away_stats_pitching_dataframe(schaumburg_boomers_home_away_url_2023)

tri_city_valleycats_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/tricityvalleycats?view=splits"
tri_city_valleycats_home_away_stats_pitching_2022 <- scrape_home_away_stats_pitching_dataframe(tri_city_valleycats_home_away_url_2022)
tri_city_valleycats_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/tricityvalleycats?view=splits"
tri_city_valleycats_home_away_stats_pitching_2023 <- scrape_home_away_stats_pitching_dataframe(tri_city_valleycats_home_away_url_2023)

gateway_grizzlies_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/gatewaygrizzlies?view=splits"
gateway_grizzlies_home_away_stats_pitching_2022 <- scrape_home_away_stats_pitching_dataframe(gateway_grizzlies_home_away_url_2022)
gateway_grizzlies_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/gatewaygrizzlies?view=splits"
gateway_grizzlies_home_away_stats_pitching_2023 <- scrape_home_away_stats_pitching_dataframe(gateway_grizzlies_home_away_url_2023)

new_jersey_jackals_home_away_url_2022 <- "https://www.frontierleague.com/sports/bsb/2021-22/teams/newjerseyjackals?view=splits"
new_jersey_jackals_home_away_stats_pitching_2022 <- scrape_home_away_stats_pitching_dataframe(new_jersey_jackals_home_away_url_2022)
new_jersey_jackals_home_away_url_2023 <- "https://www.frontierleague.com/sports/bsb/2022-23/teams/newjerseyjackals?view=splits"
new_jersey_jackals_home_away_stats_pitching_2023 <- scrape_home_away_stats_pitching_dataframe(new_jersey_jackals_home_away_url_2023)

### Calculating Ball Park Factor for the 2022 and 2023 seasons 

calculate_total_games <- function(hitting_stats_2022, hitting_stats_2023) {
  
  games_played_home <- sum(
    hitting_stats_2022[which(hitting_stats_2022$Stat == "GP"), "Home"],
    hitting_stats_2023[which(hitting_stats_2023$Stat == "GP"), "Home"]
  )
  
  games_played_away <- sum(
    hitting_stats_2022[which(hitting_stats_2022$Stat == "GP"), "Away"],
    hitting_stats_2023[which(hitting_stats_2023$Stat == "GP"), "Away"]
  )
  
  total_games_df <- data.frame(
    Location = c("Home", "Away"),
    Games = c(games_played_home, games_played_away)
  )
  
  return(total_games_df)
}

joliet_slammers_games <- calculate_total_games(joliet_slammers_home_away_stats_2022, joliet_slammers_home_away_stats_2023)
washington_wild_things_games <- calculate_total_games(washington_wild_things_home_away_stats_2022, washington_wild_things_home_away_stats_2023)
quebec_capitales_games <- calculate_total_games(quebec_capitales_home_away_stats_2022, quebec_capitales_home_away_stats_2023)
ottawa_titans_games <- calculate_total_games(ottawa_titans_home_away_stats_2022, ottawa_titans_home_away_stats_2023)
evansville_otters_games <- calculate_total_games(evansville_otters_home_away_stats_2022, evansville_otters_home_away_stats_2023)
florence_yalls_games <- calculate_total_games(florence_yalls_home_away_stats_2022, florence_yalls_home_away_stats_2023)
sussex_county_miners_games <- calculate_total_games(sussex_county_miners_home_away_stats_2022, sussex_county_miners_home_away_stats_2023)
new_york_boulders_games <- calculate_total_games(new_york_boulders_home_away_stats_2022, new_york_boulders_home_away_stats_2023)
trois_rivieres_aigles_games <- calculate_total_games(trois_rivieres_aigles_home_away_stats_2022, trois_rivieres_aigles_home_away_stats_2023)
schaumburg_boomers_games <- calculate_total_games(schaumburg_boomers_home_away_stats_2022, schaumburg_boomers_home_away_stats_2023)
tri_city_valleycats_games <- calculate_total_games(tri_city_valleycats_home_away_stats_2022, tri_city_valleycats_home_away_stats_2023)
gateway_grizzlies_games <- calculate_total_games(gateway_grizzlies_home_away_stats_2022, gateway_grizzlies_home_away_stats_2023)
new_jersey_jackals_games <- calculate_total_games(new_jersey_jackals_home_away_stats_2022, new_jersey_jackals_home_away_stats_2023)
lake_erie_crushers_games <- calculate_total_games(lake_erie_crushers_home_away_stats_2022, lake_erie_crushers_home_away_stats_2023)
empire_state_greys_games <- calculate_total_games(empire_state_greys_home_away_stats_2022, empire_state_greys_home_away_stats_2023)
windy_city_thunderbolts_games <- calculate_total_games(windy_city_thunderbolts_home_away_stats_2022, windy_city_thunderbolts_home_away_stats_2023)


calculate_ballpark_factor_adjusted <- function(hitting_stats_2022, hitting_stats_2023, pitching_stats_2022, pitching_stats_2023, team_name) {
  
  if (team_name == "new_jersey_jackals") {
    runs_scored_home <- hitting_stats_2023[which(hitting_stats_2023$Stat == "R"), "Home"] +
      pitching_stats_2023[which(pitching_stats_2023$Stat == "R"), "Home"]
    
    runs_scored_away <- hitting_stats_2023[which(hitting_stats_2023$Stat == "R"), "Away"] +
      pitching_stats_2023[which(pitching_stats_2023$Stat == "R"), "Away"]
    
    games_played_home <- hitting_stats_2023[which(hitting_stats_2023$Stat == "GP"), "Home"]
    games_played_away <- hitting_stats_2023[which(hitting_stats_2023$Stat == "GP"), "Away"]
  } else {
    runs_scored_home <- sum(
      hitting_stats_2022[which(hitting_stats_2022$Stat == "R"), "Home"],
      hitting_stats_2023[which(hitting_stats_2023$Stat == "R"), "Home"],
      pitching_stats_2022[which(pitching_stats_2022$Stat == "R"), "Home"],
      pitching_stats_2023[which(pitching_stats_2023$Stat == "R"), "Home"]
    )
    
    runs_scored_away <- sum(
      hitting_stats_2022[which(hitting_stats_2022$Stat == "R"), "Away"],
      hitting_stats_2023[which(hitting_stats_2023$Stat == "R"), "Away"],
      pitching_stats_2022[which(pitching_stats_2022$Stat == "R"), "Away"],
      pitching_stats_2023[which(pitching_stats_2023$Stat == "R"), "Away"]
    )
    
    games_played_home <- sum(
      hitting_stats_2022[which(hitting_stats_2022$Stat == "GP"), "Home"],
      hitting_stats_2023[which(hitting_stats_2023$Stat == "GP"), "Home"]
    )
    
    games_played_away <- sum(
      hitting_stats_2022[which(hitting_stats_2022$Stat == "GP"), "Away"],
      hitting_stats_2023[which(hitting_stats_2023$Stat == "GP"), "Away"]
    )
  }
  
  game_difference <- games_played_home - games_played_away
  
  adjusted_runs_away <- runs_scored_away + (runs_scored_away / games_played_away) * game_difference
  
  ballpark_factor <- runs_scored_home / adjusted_runs_away
  
  return(ballpark_factor)
}

joliet_slammers_ballpark_factor <- calculate_ballpark_factor_adjusted(
  joliet_slammers_home_away_stats_2022, joliet_slammers_home_away_stats_2023,
  joliet_slammers_home_away_stats_pitching_2022, joliet_slammers_home_away_stats_pitching_2023,
  "joliet_slammers"
)

washington_wild_things_ballpark_factor <- calculate_ballpark_factor_adjusted(
  washington_wild_things_home_away_stats_2022, washington_wild_things_home_away_stats_2023,
  washington_wild_things_home_away_stats_pitching_2022, washington_wild_things_home_away_stats_pitching_2023,
  "washington_wild_things"
)

quebec_capitales_ballpark_factor <- calculate_ballpark_factor_adjusted(
  quebec_capitales_home_away_stats_2022, quebec_capitales_home_away_stats_2023,
  quebec_capitales_home_away_stats_pitching_2022, quebec_capitales_home_away_stats_pitching_2023,
  "quebec_capitales"
)

ottawa_titans_ballpark_factor <- calculate_ballpark_factor_adjusted(
  ottawa_titans_home_away_stats_2022, ottawa_titans_home_away_stats_2023,
  ottawa_titans_home_away_stats_pitching_2022, ottawa_titans_home_away_stats_pitching_2023,
  "ottawa_titans"
)

empire_state_greys_ballpark_factor <- 1.00

evansville_otters_ballpark_factor <- calculate_ballpark_factor_adjusted(
  evansville_otters_home_away_stats_2022, evansville_otters_home_away_stats_2023,
  evansville_otters_home_away_stats_pitching_2022, evansville_otters_home_away_stats_pitching_2023,
  "evansville_otters"
)

florence_yalls_ballpark_factor <- calculate_ballpark_factor_adjusted(
  florence_yalls_home_away_stats_2022, florence_yalls_home_away_stats_2023,
  florence_yalls_home_away_stats_pitching_2022, florence_yalls_home_away_stats_pitching_2023,
  "florence_yalls"
)

sussex_county_miners_ballpark_factor <- calculate_ballpark_factor_adjusted(
  sussex_county_miners_home_away_stats_2022, sussex_county_miners_home_away_stats_2023,
  sussex_county_miners_home_away_stats_pitching_2022, sussex_county_miners_home_away_stats_pitching_2023,
  "sussex_county_miners"
)

new_york_boulders_ballpark_factor <- calculate_ballpark_factor_adjusted(
  new_york_boulders_home_away_stats_2022, new_york_boulders_home_away_stats_2023,
  new_york_boulders_home_away_stats_pitching_2022, new_york_boulders_home_away_stats_pitching_2023,
  "new_york_boulders"
)

trois_rivieres_aigles_ballpark_factor <- calculate_ballpark_factor_adjusted(
  trois_rivieres_aigles_home_away_stats_2022, trois_rivieres_aigles_home_away_stats_2023,
  trois_rivieres_aigles_home_away_stats_pitching_2022, trois_rivieres_aigles_home_away_stats_pitching_2023,
  "trois_rivieres_aigles"
)

schaumburg_boomers_ballpark_factor <- calculate_ballpark_factor_adjusted(
  schaumburg_boomers_home_away_stats_2022, schaumburg_boomers_home_away_stats_2023,
  schaumburg_boomers_home_away_stats_pitching_2022, schaumburg_boomers_home_away_stats_pitching_2023,
  "schaumburg_boomers"
)

tri_city_valleycats_ballpark_factor <- calculate_ballpark_factor_adjusted(
  tri_city_valleycats_home_away_stats_2022, tri_city_valleycats_home_away_stats_2023,
  tri_city_valleycats_home_away_stats_pitching_2022, tri_city_valleycats_home_away_stats_pitching_2023,
  "tri_city_valleycats"
)

gateway_grizzlies_ballpark_factor <- calculate_ballpark_factor_adjusted(
  gateway_grizzlies_home_away_stats_2022, gateway_grizzlies_home_away_stats_2023,
  gateway_grizzlies_home_away_stats_pitching_2022, gateway_grizzlies_home_away_stats_pitching_2023,
  "gateway_grizzlies"
)

new_jersey_jackals_ballpark_factor <- calculate_ballpark_factor_adjusted(
  new_jersey_jackals_home_away_stats_2022, new_jersey_jackals_home_away_stats_2023,
  new_jersey_jackals_home_away_stats_pitching_2022, new_jersey_jackals_home_away_stats_pitching_2023,
  "new_jersey_jackals"
)

lake_erie_crushers_ballpark_factor <- calculate_ballpark_factor_adjusted(
  lake_erie_crushers_home_away_stats_2022, lake_erie_crushers_home_away_stats_2023,
  lake_erie_crushers_home_away_stats_pitching_2022, lake_erie_crushers_home_away_stats_pitching_2023,
  "lake_erie_crushers"
)

empire_state_greys_ballpark_factor <- calculate_ballpark_factor_adjusted(
  empire_state_greys_home_away_stats_2022, empire_state_greys_home_away_stats_2023,
  empire_state_greys_home_away_stats_pitching_2022, empire_state_greys_home_away_stats_pitching_2023,
  "empire_state_greys"
)

windy_city_thunderbolts_ballpark_factor <- calculate_ballpark_factor_adjusted(
  windy_city_thunderbolts_home_away_stats_2022, windy_city_thunderbolts_home_away_stats_2023,
  windy_city_thunderbolts_home_away_stats_pitching_2022, windy_city_thunderbolts_home_away_stats_pitching_2023,
  "windy_city_thunderbolts"
)

###START HERE:
joliet_slammers_ballpark_factor <- 1

washington_wild_things_ballpark_factor <- 1

quebec_capitales_ballpark_factor <- 1

ottawa_titans_ballpark_factor <- 1

evansville_otters_ballpark_factor <- 1

florence_yalls_ballpark_factor <- 1

sussex_county_miners_ballpark_factor <- 1

new_york_boulders_ballpark_factor <- 1

trois_rivieres_aigles_ballpark_factor <- 1

schaumburg_boomers_ballpark_factor <- 1

tri_city_valleycats_ballpark_factor <- 1

gateway_grizzlies_ballpark_factor <- 1

new_jersey_jackals_ballpark_factor <- 1

lake_erie_crushers_ballpark_factor <- 1

empire_state_greys_ballpark_factor <- 1

windy_city_thunderbolts_ballpark_factor <- 1

###OPS+

frontier_league_wide_wOBA_2024 <- calculate_frontier_wide_wOBA_2(wOBA_frontier_constants_stats_2024, wOBA_constants_frontier_2024)

OBP_frontier_2024 <- All_Teams_Hitting_OBP_2024[All_Teams_Hitting_OBP_2024$Team == "Frontier League Total", "OBP"]

SLG_frontier_2024 <- All_Teams_Hitting_SLG_2024[All_Teams_Hitting_SLG_2024$Team == "Frontier League Total", "SLG"]

ballpark_factors <- list(
  "joliet_slammers" = joliet_slammers_ballpark_factor,
  "washington_wild_things" = washington_wild_things_ballpark_factor,
  "quebec_capitales" = quebec_capitales_ballpark_factor,
  "ottawa_titans" = ottawa_titans_ballpark_factor,
  "evansville_otters" = evansville_otters_ballpark_factor,
  "florence_yalls" = florence_yalls_ballpark_factor,
  "sussex_county_miners" = sussex_county_miners_ballpark_factor,
  "new_york_boulders" = new_york_boulders_ballpark_factor,
  "trois_rivieres_aigles" = trois_rivieres_aigles_ballpark_factor,
  "schaumburg_boomers" = schaumburg_boomers_ballpark_factor,
  "tri_city_valleycats" = tri_city_valleycats_ballpark_factor,
  "gateway_grizzlies" = gateway_grizzlies_ballpark_factor,
  "new_jersey_jackals" = new_jersey_jackals_ballpark_factor,
  "lake_erie_crushers" = lake_erie_crushers_ballpark_factor,
  "empire_state_greys" = empire_state_greys_ballpark_factor,
  "windy_city_thunderbolts" = windy_city_thunderbolts_ballpark_factor
)

calculate_ops_plus <- function(combined_stats, OBP_frontier_2024, SLG_frontier_2024, team_BPF) {
  team_BPF <- 1 
  combined_stats_with_ops_plus <- lapply(combined_stats, function(player_stats) {
    if ("OPS" %in% player_stats$Stat) {
      player_OBP <- as.numeric(player_stats[player_stats$Stat == "OBP", "Value"])
      player_SLG <- as.numeric(player_stats[player_stats$Stat == "SLG", "Value"])
      
      player_OPS_plus <- 100 * (((player_OBP / OBP_frontier_2024) + (player_SLG / SLG_frontier_2024) - 1) / team_BPF)
      
      player_stats <- rbind(player_stats, data.frame(Stat = "OPS+", Value = player_OPS_plus, stringsAsFactors = FALSE))
    }
    return(player_stats)
  })
  
  return(combined_stats_with_ops_plus)
}

New_Jersey_Jackals_Combined_Stats_3_2024 <- calculate_ops_plus(
  New_Jersey_Jackals_Combined_Stats_2_2024, 
  OBP_frontier_2024, 
  SLG_frontier_2024, 
  ballpark_factors[["new_jersey_jackals"]]
)

New_York_Boulders_Combined_Stats_3_2024 <- calculate_ops_plus(
  New_York_Boulders_Combined_Stats_2_2024, 
  OBP_frontier_2024, 
  SLG_frontier_2024, 
  ballpark_factors[["new_york_boulders"]]
)

Ottawa_Titans_Combined_Stats_3_2024 <- calculate_ops_plus(
  Ottawa_Titans_Combined_Stats_2_2024, 
  OBP_frontier_2024, 
  SLG_frontier_2024, 
  ballpark_factors[["ottawa_titans"]]
)

Quebec_Capitales_Combined_Stats_3_2024 <- calculate_ops_plus(
  Quebec_Capitales_Combined_Stats_2_2024, 
  OBP_frontier_2024, 
  SLG_frontier_2024, 
  ballpark_factors[["quebec_capitales"]]
)

Sussex_County_Miners_Combined_Stats_3_2024 <- calculate_ops_plus(
  Sussex_County_Miners_Combined_Stats_2_2024, 
  OBP_frontier_2024, 
  SLG_frontier_2024, 
  ballpark_factors[["sussex_county_miners"]]
)

Trois_Rivieres_Aigles_Combined_Stats_3_2024 <- calculate_ops_plus(
  Trois_Rivieres_Aigles_Combined_Stats_2_2024, 
  OBP_frontier_2024, 
  SLG_frontier_2024, 
  ballpark_factors[["trois_rivieres_aigles"]]
)

Tri_City_Valleycats_Combined_Stats_3_2024 <- calculate_ops_plus(
  Tri_City_Valleycats_Combined_Stats_2_2024, 
  OBP_frontier_2024, 
  SLG_frontier_2024, 
  ballpark_factors[["tri_city_valleycats"]]
)

New_England_Knockouts_Combined_Stats_3_2024 <- calculate_ops_plus(
  New_England_Knockouts_Combined_Stats_2_2024, 
  OBP_frontier_2024, 
  SLG_frontier_2024, 
  ballpark_factors[["empire_state_greys"]]
)

Lake_Erie_Crushers_Combined_Stats_3_2024 <- calculate_ops_plus(
  Lake_Erie_Crushers_Combined_Stats_2_2024, 
  OBP_frontier_2024, 
  SLG_frontier_2024, 
  ballpark_factors[["lake_erie_crushers"]]
)

Evansville_Otters_Combined_Stats_3_2024 <- calculate_ops_plus(
  Evansville_Otters_Combined_Stats_2_2024, 
  OBP_frontier_2024, 
  SLG_frontier_2024, 
  ballpark_factors[["evansville_otters"]]
)

Florence_Yalls_Combined_Stats_3_2024 <- calculate_ops_plus(
  Florence_Yalls_Combined_Stats_2_2024, 
  OBP_frontier_2024, 
  SLG_frontier_2024, 
  ballpark_factors[["florence_yalls"]]
)

Gateway_Grizzlies_Combined_Stats_3_2024 <- calculate_ops_plus(
  Gateway_Grizzlies_Combined_Stats_2_2024, 
  OBP_frontier_2024, 
  SLG_frontier_2024, 
  ballpark_factors[["gateway_grizzlies"]]
)

Joliet_Slammers_Combined_Stats_3_2024 <- calculate_ops_plus(
  Joliet_Slammers_Combined_Stats_2_2024, 
  OBP_frontier_2024, 
  SLG_frontier_2024, 
  ballpark_factors[["joliet_slammers"]]
)

Schaumburg_Boomers_Combined_Stats_3_2024 <- calculate_ops_plus(
  Schaumburg_Boomers_Combined_Stats_2_2024, 
  OBP_frontier_2024, 
  SLG_frontier_2024, 
  ballpark_factors[["schaumburg_boomers"]]
)

Washington_Wild_Things_Combined_Stats_3_2024 <- calculate_ops_plus(
  Washington_Wild_Things_Combined_Stats_2_2024, 
  OBP_frontier_2024, 
  SLG_frontier_2024, 
  ballpark_factors[["washington_wild_things"]]
)

Windy_City_Thunderbolts_Combined_Stats_3_2024 <- calculate_ops_plus(
  Windy_City_Thunderbolts_Combined_Stats_2_2024, 
  OBP_frontier_2024, 
  SLG_frontier_2024, 
  ballpark_factors[["windy_city_thunderbolts"]]
)

### East and West wRC calculations 

runs_league <- league_R$Frontier_League_Total
pa_league <- league_PA$Frontier_League_Total
pa_east <- league_PA$East
pa_west <- league_PA$West
league_R_PA <- runs_league/pa_league

calculate_division_wRC <- function(east_west_wide_wOBA_2024, frontier_wide_wOBA_2024, wOBA_scale_2024, runs_league, pa_league) {
  wRC_east <- (((east_west_wide_wOBA_2024$East - frontier_wide_wOBA_2024) / wOBA_scale_2024) + (league_R_PA)) * pa_east
  
  wRC_west <- (((east_west_wide_wOBA_2024$West - frontier_wide_wOBA_2024) / wOBA_scale_2024) + (league_R_PA)) * pa_west
  
  division_wRC <- list(East = wRC_east, West = wRC_west)
  return(division_wRC)
}

runs_league <- league_R$Frontier_League_Total
pa_league_data_frame <- league_PA
pa_east <- league_PA$East
pa_west <- league_PA$West
league_R_PA <- runs_league/pa_league

division_wRC <- calculate_division_wRC(east_west_wide_wOBA_2024, frontier_wide_wOBA_2024, wOBA_scale_2024, runs_league, pa_league)

ballpark_factors <- list(
  "joliet_slammers" = joliet_slammers_ballpark_factor,
  "washington_wild_things" = washington_wild_things_ballpark_factor,
  "quebec_capitales" = quebec_capitales_ballpark_factor,
  "ottawa_titans" = ottawa_titans_ballpark_factor,
  "evansville_otters" = evansville_otters_ballpark_factor,
  "florence_yalls" = florence_yalls_ballpark_factor,
  "sussex_county_miners" = sussex_county_miners_ballpark_factor,
  "new_york_boulders" = new_york_boulders_ballpark_factor,
  "trois_rivieres_aigles" = trois_rivieres_aigles_ballpark_factor,
  "schaumburg_boomers" = schaumburg_boomers_ballpark_factor,
  "tri_city_valleycats" = tri_city_valleycats_ballpark_factor,
  "gateway_grizzlies" = gateway_grizzlies_ballpark_factor,
  "new_jersey_jackals" = new_jersey_jackals_ballpark_factor,
  "lake_erie_crushers" = lake_erie_crushers_ballpark_factor,
  "empire_state_greys" = empire_state_greys_ballpark_factor,
  "windy_city_thunderbolts" = windy_city_thunderbolts_ballpark_factor
)

calculate_wRC_plus_for_team <- function(team_name, team_combined_stats, ballpark_factors, division_wRC, league_R_PA,pa_league_data_frame) {
  options(scipen = 999)
  
  is_team_east <- team_name %in% c("new_jersey_jackals", "new_york_boulders", "new_england_knockouts", "ottawa_titans", "trois_rivieres_aigles", "quebec_capitales", "sussex_county_miners", "tri_city_valleycats")
  team_division_wRC <- ifelse(is_team_east, division_wRC$East, division_wRC$West)
  league_PA <- ifelse(is_team_east, pa_league_data_frame$East, pa_league_data_frame$West)
  team_BPF <- 1 ##ballpark_factors[[team_name]]
  
  team_combined_stats_with_wRC_plus <- lapply(team_combined_stats, function(player_stats) {
    wRAA_row <- player_stats[player_stats$Stat == "wRAA", ]
    PA_row <- player_stats[player_stats$Stat == "PA", ]
    
    if (nrow(wRAA_row) > 0 && nrow(PA_row) > 0) {
      player_wRAA <- as.numeric(wRAA_row$Value)
      player_PA <- as.numeric(PA_row$Value)
      
      player_wRC_plus <- (((player_wRAA/ player_PA) + league_R_PA + (league_R_PA - (team_BPF * league_R_PA))) / (team_division_wRC / league_PA)) * 100
      
      player_stats <- rbind(player_stats, data.frame(Stat = "wRC+", Value = player_wRC_plus, stringsAsFactors = FALSE))
    }
    return(player_stats)
  })
  
  return(team_combined_stats_with_wRC_plus)
}


New_Jersey_Jackals_Combined_Stats_4_2024 <- calculate_wRC_plus_for_team("new_jersey_jackals",
                                                                        New_Jersey_Jackals_Combined_Stats_3_2024, 
                                                                        ballpark_factors, 
                                                                        division_wRC,
                                                                        league_R_PA,
                                                                        pa_league_data_frame)

New_York_Boulders_Combined_Stats_4_2024 <- calculate_wRC_plus_for_team("new_york_boulders",
                                                                       New_York_Boulders_Combined_Stats_3_2024, 
                                                                       ballpark_factors, 
                                                                       division_wRC,
                                                                       league_R_PA,
                                                                       pa_league_data_frame)

New_England_Knockouts_Combined_Stats_4_2024 <- calculate_wRC_plus_for_team("empire_state_greys",
                                                                           New_England_Knockouts_Combined_Stats_3_2024, 
                                                                           ballpark_factors, 
                                                                           division_wRC,
                                                                           league_R_PA,
                                                                           pa_league_data_frame)

Ottawa_Titans_Combined_Stats_4_2024 <- calculate_wRC_plus_for_team("ottawa_titans",
                                                                   Ottawa_Titans_Combined_Stats_3_2024, 
                                                                   ballpark_factors, 
                                                                   division_wRC,
                                                                   league_R_PA,
                                                                   pa_league_data_frame)

Trois_Rivieres_Aigles_Combined_Stats_4_2024 <- calculate_wRC_plus_for_team("trois_rivieres_aigles",
                                                                           Trois_Rivieres_Aigles_Combined_Stats_3_2024, 
                                                                           ballpark_factors, 
                                                                           division_wRC,
                                                                           league_R_PA,
                                                                           pa_league_data_frame)

Quebec_Capitales_Combined_Stats_4_2024 <- calculate_wRC_plus_for_team("quebec_capitales",
                                                                      Quebec_Capitales_Combined_Stats_3_2024, 
                                                                      ballpark_factors, 
                                                                      division_wRC,
                                                                      league_R_PA,
                                                                      pa_league_data_frame)

Sussex_County_Miners_Combined_Stats_4_2024 <- calculate_wRC_plus_for_team("sussex_county_miners",
                                                                          Sussex_County_Miners_Combined_Stats_3_2024, 
                                                                          ballpark_factors, 
                                                                          division_wRC,
                                                                          league_R_PA,
                                                                          pa_league_data_frame)

Tri_City_ValleyCats_Combined_Stats_4_2024 <- calculate_wRC_plus_for_team("tri_city_valleycats",
                                                                         Tri_City_Valleycats_Combined_Stats_3_2024, 
                                                                         ballpark_factors, 
                                                                         division_wRC,
                                                                         league_R_PA,
                                                                         pa_league_data_frame)

Lake_Erie_Crushers_Combined_Stats_4_2024 <- calculate_wRC_plus_for_team("lake_erie_crushers",
                                                                        Lake_Erie_Crushers_Combined_Stats_3_2024, 
                                                                        ballpark_factors, 
                                                                        division_wRC,
                                                                        league_R_PA,
                                                                        pa_league_data_frame)

Evansville_Otters_Combined_Stats_4_2024 <- calculate_wRC_plus_for_team("evansville_otters",
                                                                       Evansville_Otters_Combined_Stats_3_2024, 
                                                                       ballpark_factors, 
                                                                       division_wRC,
                                                                       league_R_PA,
                                                                       pa_league_data_frame)

Florence_Yalls_Combined_Stats_4_2024 <- calculate_wRC_plus_for_team("florence_yalls",
                                                                    Florence_Yalls_Combined_Stats_3_2024, 
                                                                    ballpark_factors, 
                                                                    division_wRC,
                                                                    league_R_PA,
                                                                    pa_league_data_frame)

Gateway_Grizzlies_Combined_Stats_4_2024 <- calculate_wRC_plus_for_team("gateway_grizzlies",
                                                                       Gateway_Grizzlies_Combined_Stats_3_2024, 
                                                                       ballpark_factors, 
                                                                       division_wRC,
                                                                       league_R_PA,
                                                                       pa_league_data_frame)

Joliet_Slammers_Combined_Stats_4_2024 <- calculate_wRC_plus_for_team("joliet_slammers",
                                                                     Joliet_Slammers_Combined_Stats_3_2024, 
                                                                     ballpark_factors, 
                                                                     division_wRC,
                                                                     league_R_PA,
                                                                     pa_league_data_frame)

Schaumburg_Boomers_Combined_Stats_4_2024 <- calculate_wRC_plus_for_team("schaumburg_boomers",
                                                                        Schaumburg_Boomers_Combined_Stats_3_2024, 
                                                                        ballpark_factors, 
                                                                        division_wRC,
                                                                        league_R_PA,
                                                                        pa_league_data_frame)

Washington_Wild_Things_Combined_Stats_4_2024 <- calculate_wRC_plus_for_team("washington_wild_things",
                                                                            Washington_Wild_Things_Combined_Stats_3_2024, 
                                                                            ballpark_factors, 
                                                                            division_wRC,
                                                                            league_R_PA,
                                                                            pa_league_data_frame)

Windy_City_ThunderBolts_Combined_Stats_4_2024 <- calculate_wRC_plus_for_team("windy_city_thunderbolts",
                                                                             Windy_City_Thunderbolts_Combined_Stats_3_2024, 
                                                                             ballpark_factors, 
                                                                             division_wRC,
                                                                             league_R_PA,
                                                                             pa_league_data_frame)


### Putting these stats into the SQL Database:

library(RSQLite)

process_team_stats <- function(team_data, team_name, conn) {
  table_name <- paste(team_name, "2024_Hitting_Stats", sep="_")
  sql_drop_table <- paste0("DROP TABLE IF EXISTS `", table_name, "`")
  dbExecute(conn, sql_drop_table)
  
  all_stats <- unique(unlist(lapply(team_data, function(df) df$Stat)))
  stat_columns <- paste0('`', all_stats, '` REAL', collapse = ', ')
  sql_create_table <- paste0("CREATE TABLE `", table_name, "` (`Player` TEXT, ", stat_columns, ")")
  dbExecute(conn, sql_create_table)
  
  format_for_sql <- function(player_data, player_name) {
    stats_df <- setNames(data.frame(matrix(ncol = length(all_stats), nrow = 1, data = NA)), all_stats)
    for (i in seq_along(player_data$Stat)) {
      stat_name <- player_data$Stat[i]
      stats_df[1, stat_name] <- player_data$Value[i]
    }
    stats_df$Player <- player_name
    return(stats_df)
  }
  
  for (player_name in names(team_data)) {
    player_data <- team_data[[player_name]]
    formatted_data <- format_for_sql(player_data, player_name)
    dbWriteTable(conn, table_name, formatted_data, append = TRUE, row.names = FALSE)
  }
}

conn <- dbConnect(RSQLite::SQLite(), dbname = "/Users/nathanielascher/Downloads/Frontier League 2024 Season.db")

teams_data <- list(
  New_Jersey_Jackals_Combined_Stats_4_2024,
  New_York_Boulders_Combined_Stats_4_2024,
  New_England_Knockouts_Combined_Stats_4_2024,
  Ottawa_Titans_Combined_Stats_4_2024,
  Trois_Rivieres_Aigles_Combined_Stats_4_2024,
  Quebec_Capitales_Combined_Stats_4_2024,
  Sussex_County_Miners_Combined_Stats_4_2024,
  Tri_City_ValleyCats_Combined_Stats_4_2024,
  Lake_Erie_Crushers_Combined_Stats_4_2024,
  Evansville_Otters_Combined_Stats_4_2024,
  Florence_Yalls_Combined_Stats_4_2024,
  Gateway_Grizzlies_Combined_Stats_4_2024,
  Joliet_Slammers_Combined_Stats_4_2024,
  Schaumburg_Boomers_Combined_Stats_4_2024,
  Washington_Wild_Things_Combined_Stats_4_2024,
  Windy_City_ThunderBolts_Combined_Stats_4_2024
)

team_names <- c(
  "New_Jersey_Jackals",
  "New_York_Boulders",
  "New_England_Knockouts",
  "Ottawa_Titans",
  "Trois_Rivieres_Aigles",
  "Quebec_Capitales",
  "Sussex_County_Miners",
  "Tri_City_ValleyCats",
  "Lake_Erie_Crushers",
  "Evansville_Otters",
  "Florence_Yalls",
  "Gateway_Grizzlies",
  "Joliet_Slammers",
  "Schaumburg_Boomers",
  "Washington_Wild_Things",
  "Windy_City_ThunderBolts"
)

for (i in seq_along(teams_data)) {
  process_team_stats(teams_data[[i]], team_names[i], conn)
}

dbDisconnect(conn)

###All Teams' Hitters

library(RSQLite)

insert_all_teams_stats <- function(teams_data, conn) {
  all_stats <- unique(unlist(lapply(teams_data, function(team) unlist(lapply(team, function(df) df$Stat)))))
  stat_columns <- paste0('`', all_stats, '` REAL', collapse = ', ')
  dbExecute(conn, "DROP TABLE IF EXISTS `All_Teams_Hitters_2024_Hitting_Stats`")
  dbExecute(conn, paste0("CREATE TABLE `All_Teams_Hitters_2024_Hitting_Stats` (`Player` TEXT, `Team` TEXT, ", stat_columns, ")"))
  
  format_for_sql <- function(player_data, player_name) {
    stats_df <- setNames(data.frame(matrix(ncol = length(all_stats), nrow = 1, data = NA)), all_stats)
    for (i in seq_along(player_data$Stat)) {
      stats_df[1, player_data$Stat[i]] <- player_data$Value[i]
    }
    stats_df$Player <- player_name
    return(stats_df)
  }
  
  for (i in seq_along(teams_data)) {
    team_data <- teams_data[[i]]
    for (player_name in names(team_data)) {
      player_data <- team_data[[player_name]]
      formatted_data <- format_for_sql(player_data, player_name)
      formatted_data$Team <- team_names[i]
      dbWriteTable(conn, "All_Teams_Hitters_2024_Hitting_Stats", formatted_data, append = TRUE, row.names = FALSE)
    }
  }
}

conn <- dbConnect(RSQLite::SQLite(), dbname = "/Users/nathanielascher/Downloads/Frontier League 2024 Season.db")

teams_data <- list(
  New_Jersey_Jackals_Combined_Stats_4_2024,
  New_York_Boulders_Combined_Stats_4_2024,
  New_England_Knockouts_Combined_Stats_4_2024,
  Ottawa_Titans_Combined_Stats_4_2024,
  Trois_Rivieres_Aigles_Combined_Stats_4_2024,
  Quebec_Capitales_Combined_Stats_4_2024,
  Sussex_County_Miners_Combined_Stats_4_2024,
  Tri_City_ValleyCats_Combined_Stats_4_2024,
  Lake_Erie_Crushers_Combined_Stats_4_2024,
  Evansville_Otters_Combined_Stats_4_2024,
  Florence_Yalls_Combined_Stats_4_2024,
  Gateway_Grizzlies_Combined_Stats_4_2024,
  Joliet_Slammers_Combined_Stats_4_2024,
  Schaumburg_Boomers_Combined_Stats_4_2024,
  Washington_Wild_Things_Combined_Stats_4_2024,
  Windy_City_ThunderBolts_Combined_Stats_4_2024
)

team_names <- c(
  "New_Jersey_Jackals",
  "New_York_Boulders",
  "New_England_Knockouts",
  "Ottawa_Titans",
  "Trois_Rivieres_Aigles",
  "Quebec_Capitales",
  "Sussex_County_Miners",
  "Tri_City_ValleyCats",
  "Lake_Erie_Crushers",
  "Evansville_Otters",
  "Florence_Yalls",
  "Gateway_Grizzlies",
  "Joliet_Slammers",
  "Schaumburg_Boomers",
  "Washington_Wild_Things",
  "Windy_City_ThunderBolts"
)

insert_all_teams_stats(teams_data, conn)

dbDisconnect(conn)

###East Teams' Hitters

library(RSQLite)

insert_east_teams_stats <- function(teams_data, conn) {
  all_stats <- unique(unlist(lapply(teams_data, function(team) unlist(lapply(team, function(df) df$Stat)))))
  stat_columns <- paste0('`', all_stats, '` REAL', collapse = ', ')
  dbExecute(conn, "DROP TABLE IF EXISTS `East_Teams_Hitters_2024_Hitting_Stats`")
  dbExecute(conn, paste0("CREATE TABLE `East_Teams_Hitters_2024_Hitting_Stats` (`Player` TEXT, `Team` TEXT, ", stat_columns, ")"))
  
  format_for_sql <- function(player_data, player_name) {
    stats_df <- setNames(data.frame(matrix(ncol = length(all_stats), nrow = 1, data = NA)), all_stats)
    for (i in seq_along(player_data$Stat)) {
      stats_df[1, player_data$Stat[i]] <- player_data$Value[i]
    }
    stats_df$Player <- player_name
    return(stats_df)
  }
  
  for (i in seq_along(teams_data)) {
    team_data <- teams_data[[i]]
    for (player_name in names(team_data)) {
      player_data <- team_data[[player_name]]
      formatted_data <- format_for_sql(player_data, player_name)
      formatted_data$Team <- team_names[i]
      dbWriteTable(conn, "East_Teams_Hitters_2024_Hitting_Stats", formatted_data, append = TRUE, row.names = FALSE)
    }
  }
}

conn <- dbConnect(RSQLite::SQLite(), dbname = "/Users/nathanielascher/Downloads/Frontier League 2024 Season.db")

teams_data <- list(
  New_Jersey_Jackals_Combined_Stats_4_2024,
  New_York_Boulders_Combined_Stats_4_2024,
  New_England_Knockouts_Combined_Stats_4_2024,
  Ottawa_Titans_Combined_Stats_4_2024,
  Trois_Rivieres_Aigles_Combined_Stats_4_2024,
  Quebec_Capitales_Combined_Stats_4_2024,
  Sussex_County_Miners_Combined_Stats_4_2024,
  Tri_City_ValleyCats_Combined_Stats_4_2024)

team_names <- c(
  "New_Jersey_Jackals",
  "New_York_Boulders",
  "New_England_Knockouts",
  "Ottawa_Titans",
  "Trois_Rivieres_Aigles",
  "Quebec_Capitales",
  "Sussex_County_Miners",
  "Tri_City_ValleyCats")

insert_east_teams_stats(teams_data, conn)

dbDisconnect(conn)

###West Teams' Hitters

library(RSQLite)

insert_west_teams_stats <- function(teams_data, conn) {
  all_stats <- unique(unlist(lapply(teams_data, function(team) unlist(lapply(team, function(df) df$Stat)))))
  stat_columns <- paste0('`', all_stats, '` REAL', collapse = ', ')
  dbExecute(conn, "DROP TABLE IF EXISTS `West_Teams_Hitters_2024_Hitting_Stats`")
  dbExecute(conn, paste0("CREATE TABLE `West_Teams_Hitters_2024_Hitting_Stats` (`Player` TEXT, `Team` TEXT, ", stat_columns, ")"))
  
  format_for_sql <- function(player_data, player_name) {
    stats_df <- setNames(data.frame(matrix(ncol = length(all_stats), nrow = 1, data = NA)), all_stats)
    for (i in seq_along(player_data$Stat)) {
      stats_df[1, player_data$Stat[i]] <- player_data$Value[i]
    }
    stats_df$Player <- player_name
    return(stats_df)
  }
  
  for (i in seq_along(teams_data)) {
    team_data <- teams_data[[i]]
    for (player_name in names(team_data)) {
      player_data <- team_data[[player_name]]
      formatted_data <- format_for_sql(player_data, player_name)
      formatted_data$Team <- team_names[i]
      dbWriteTable(conn, "West_Teams_Hitters_2024_Hitting_Stats", formatted_data, append = TRUE, row.names = FALSE)
    }
  }
}

conn <- dbConnect(RSQLite::SQLite(), dbname = "/Users/nathanielascher/Downloads/Frontier League 2024 Season.db")

teams_data <- list(
  Lake_Erie_Crushers_Combined_Stats_4_2024,
  Evansville_Otters_Combined_Stats_4_2024,
  Florence_Yalls_Combined_Stats_4_2024,
  Gateway_Grizzlies_Combined_Stats_4_2024,
  Joliet_Slammers_Combined_Stats_4_2024,
  Schaumburg_Boomers_Combined_Stats_4_2024,
  Washington_Wild_Things_Combined_Stats_4_2024,
  Windy_City_ThunderBolts_Combined_Stats_4_2024)

team_names <- c(
  "Lake_Erie_Crushers",
  "Evansville_Otters",
  "Florence_Yalls",
  "Gateway_Grizzlies",
  "Joliet_Slammers",
  "Schaumburg_Boomers",
  "Washington_Wild_Things",
  "Windy_City_ThunderBolts")

insert_west_teams_stats(teams_data, conn)

dbDisconnect(conn)


##Hitter Split Stats

library(dplyr)
library(RSQLite)

calculateHittersStatsvsRightiesLefties <- function(dbname, startDate, endDate, BB_HBP_Constant, Single_Constant, Double_Constant, Triple_Constant, HomeRun_Constant) {
  conn <- dbConnect(RSQLite::SQLite(), dbname = dbname)
  data <- dbReadTable(conn, "Lake Erie Crushers 2024")
  dbDisconnect(conn)
  
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  startDate <- as.Date(startDate, format = "%m/%d/%Y")
  endDate <- as.Date(endDate, format = "%m/%d/%Y")
  
  data_filtered <- data %>%
    filter(Date >= startDate, Date <= endDate, BatterTeam == "Lake Erie Crushers", TaggedPitchType != "") %>%
    select(Batter, PitcherThrows, PitchCall, PlayResult, KorBB, HitType)
  
  stats <- data_filtered %>%
    group_by(Batter, PitcherThrows) %>%
    summarise(
      PA = sum(PitchCall %in% c("InPlay", "HitByPitch") | KorBB %in% c("Strikeout", "Walk"), na.rm = TRUE),
      Walks_HBP = sum(KorBB == "Walk" | PitchCall == "HitByPitch", na.rm = TRUE),
      Sacrifices = sum(PlayResult == "Sacrifice", na.rm = TRUE),
      SacrificeFlies = sum(PlayResult == "Sacrifice" & HitType != "Bunt", na.rm = TRUE),
      TotalBases = sum(1 * (PlayResult == "Single") + 2 * (PlayResult == "Double") + 3 * (PlayResult == "Triple") + 4 * (PlayResult == "HomeRun"), na.rm = TRUE),
      Singles = sum(PlayResult == "Single", na.rm = TRUE),
      Doubles = sum(PlayResult == "Double", na.rm = TRUE),
      Triples = sum(PlayResult == "Triple", na.rm = TRUE),
      HomeRuns = sum(PlayResult == "HomeRun", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Hits = Singles + Doubles + Triples + HomeRuns,
      ABs = PA - Walks_HBP - Sacrifices,
      BattingAverage = Hits / ABs,
      OBP = (Hits + Walks_HBP) / (ABs + Walks_HBP + SacrificeFlies),
      SLG = TotalBases / ABs,
      OPS = OBP + SLG,
      wOBA = (BB_HBP_Constant * Walks_HBP + Single_Constant * Singles +
                Double_Constant * Doubles + Triple_Constant * Triples +
                HomeRun_Constant * HomeRuns) / PA
    )
  
  return(stats)
}

Crushers_Hitters_Stats_LeftiesRighties <- calculateHittersStatsvsRightiesLefties("/Users/nathanielascher/Downloads/Frontier League 2024 Season.db", "5/09/2024", "9/03/2024", .717,.836,1.124,1.418,1.843)

calculateHittersStatsvsRightiesLeftiesandbyPitchType <- function(dbname, startDate, endDate, BB_HBP_Constant, Single_Constant, Double_Constant, Triple_Constant, HomeRun_Constant) {
  conn <- dbConnect(RSQLite::SQLite(), dbname = dbname)
  data <- dbReadTable(conn, "Lake Erie Crushers 2024")
  dbDisconnect(conn)
  
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  startDate <- as.Date(startDate, format = "%m/%d/%Y")
  endDate <- as.Date(endDate, format = "%m/%d/%Y")
  
  data_filtered <- data %>%
    filter(Date >= startDate, Date <= endDate, BatterTeam == "Lake Erie Crushers", TaggedPitchType != "") %>%
    select(Batter, PitcherThrows, TaggedPitchType, PitchCall, PlayResult, KorBB, HitType)
  
  stats <- data_filtered %>%
    group_by(Batter, PitcherThrows, TaggedPitchType) %>%
    summarise(
      PA = sum(PitchCall %in% c("InPlay", "HitByPitch") | KorBB %in% c("Strikeout", "Walk"), na.rm = TRUE),
      Walks_HBP = sum(KorBB == "Walk" | PitchCall == "HitByPitch", na.rm = TRUE),
      Sacrifices = sum(PlayResult == "Sacrifice", na.rm = TRUE),
      SacrificeFlies = sum(PlayResult == "Sacrifice" & HitType != "Bunt", na.rm = TRUE),
      TotalBases = sum(1 * (PlayResult == "Single") + 2 * (PlayResult == "Double") + 3 * (PlayResult == "Triple") + 4 * (PlayResult == "HomeRun"), na.rm = TRUE),
      Singles = sum(PlayResult == "Single", na.rm = TRUE),
      Doubles = sum(PlayResult == "Double", na.rm = TRUE),
      Triples = sum(PlayResult == "Triple", na.rm = TRUE),
      HomeRuns = sum(PlayResult == "HomeRun", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Hits = Singles + Doubles + Triples + HomeRuns,
      ABs = PA - Walks_HBP - Sacrifices,
      BattingAverage = Hits / ABs,
      OBP = (Hits + Walks_HBP) / (ABs + Walks_HBP + SacrificeFlies),
      SLG = TotalBases / ABs,
      OPS = OBP + SLG,
      wOBA = (BB_HBP_Constant * Walks_HBP + Single_Constant * Singles +
                Double_Constant * Doubles + Triple_Constant * Triples +
                HomeRun_Constant * HomeRuns) / PA
    )
  
  return(stats)
}

Crushers_Hitters_Stats_LeftiesRightiesperPitchType <- calculateHittersStatsvsRightiesLeftiesandbyPitchType("/Users/nathanielascher/Downloads/Frontier League 2024 Season.db", "5/09/2024", "9/03/2024", .717,.836,1.124,1.418,1.843)

calculateHittersStatsbyPitchType <- function(dbname, startDate, endDate, BB_HBP_Constant, Single_Constant, Double_Constant, Triple_Constant, HomeRun_Constant) {
  conn <- dbConnect(RSQLite::SQLite(), dbname = dbname)
  data <- dbReadTable(conn, "Lake Erie Crushers 2024")
  dbDisconnect(conn)
  
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  startDate <- as.Date(startDate, format = "%m/%d/%Y")
  endDate <- as.Date(endDate, format = "%m/%d/%Y")
  
  data_filtered <- data %>%
    filter(Date >= startDate, Date <= endDate, BatterTeam == "Lake Erie Crushers", TaggedPitchType != "") %>%
    select(Batter,TaggedPitchType, PitchCall, PlayResult, KorBB, HitType)
  
  stats <- data_filtered %>%
    group_by(Batter, TaggedPitchType) %>%
    summarise(
      PA = sum(PitchCall %in% c("InPlay", "HitByPitch") | KorBB %in% c("Strikeout", "Walk"), na.rm = TRUE),
      Walks_HBP = sum(KorBB == "Walk" | PitchCall == "HitByPitch", na.rm = TRUE),
      Sacrifices = sum(PlayResult == "Sacrifice", na.rm = TRUE),
      SacrificeFlies = sum(PlayResult == "Sacrifice" & HitType != "Bunt", na.rm = TRUE),
      TotalBases = sum(1 * (PlayResult == "Single") + 2 * (PlayResult == "Double") + 3 * (PlayResult == "Triple") + 4 * (PlayResult == "HomeRun"), na.rm = TRUE),
      Singles = sum(PlayResult == "Single", na.rm = TRUE),
      Doubles = sum(PlayResult == "Double", na.rm = TRUE),
      Triples = sum(PlayResult == "Triple", na.rm = TRUE),
      HomeRuns = sum(PlayResult == "HomeRun", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Hits = Singles + Doubles + Triples + HomeRuns,
      ABs = PA - Walks_HBP - Sacrifices,
      BattingAverage = Hits / ABs,
      OBP = (Hits + Walks_HBP) / (ABs + Walks_HBP + SacrificeFlies),
      SLG = TotalBases / ABs,
      OPS = OBP + SLG,
      wOBA = (BB_HBP_Constant * Walks_HBP + Single_Constant * Singles +
                Double_Constant * Doubles + Triple_Constant * Triples +
                HomeRun_Constant * HomeRuns) / PA
    )
  
  return(stats)
}

Crushers_Hitters_Stats_perPitchType <- calculateHittersStatsbyPitchType("/Users/nathanielascher/Downloads/Frontier League 2024 Season.db", "5/09/2024", "9/03/2024", .717,.836,1.124,1.418,1.843)

calculateHittersStatsbyCount <- function(dbname, startDate, endDate, BB_HBP_Constant, Single_Constant, Double_Constant, Triple_Constant, HomeRun_Constant) {
  conn <- dbConnect(RSQLite::SQLite(), dbname = dbname)
  data <- dbReadTable(conn, "Lake Erie Crushers 2024")
  dbDisconnect(conn)
  
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  startDate <- as.Date(startDate, format = "%m/%d/%Y")
  endDate <- as.Date(endDate, format = "%m/%d/%Y")
  
  data_filtered <- data %>%
    filter(Date >= startDate, Date <= endDate, BatterTeam == "Lake Erie Crushers", TaggedPitchType != "") %>%
    select(Batter,TaggedPitchType, Balls, Strikes,PitchCall, PlayResult, KorBB, HitType)
  
  stats <- data_filtered %>%
    group_by(Batter, Balls, Strikes) %>%
    summarise(
      PA = sum(PitchCall %in% c("InPlay", "HitByPitch") | KorBB %in% c("Strikeout", "Walk"), na.rm = TRUE),
      Walks_HBP = sum(KorBB == "Walk" | PitchCall == "HitByPitch", na.rm = TRUE),
      Sacrifices = sum(PlayResult == "Sacrifice", na.rm = TRUE),
      SacrificeFlies = sum(PlayResult == "Sacrifice" & HitType != "Bunt", na.rm = TRUE),
      TotalBases = sum(1 * (PlayResult == "Single") + 2 * (PlayResult == "Double") + 3 * (PlayResult == "Triple") + 4 * (PlayResult == "HomeRun"), na.rm = TRUE),
      Singles = sum(PlayResult == "Single", na.rm = TRUE),
      Doubles = sum(PlayResult == "Double", na.rm = TRUE),
      Triples = sum(PlayResult == "Triple", na.rm = TRUE),
      HomeRuns = sum(PlayResult == "HomeRun", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Hits = Singles + Doubles + Triples + HomeRuns,
      ABs = PA - Walks_HBP - Sacrifices,
      BattingAverage = Hits / ABs,
      OBP = (Hits + Walks_HBP) / (ABs + Walks_HBP + SacrificeFlies),
      SLG = TotalBases / ABs,
      OPS = OBP + SLG,
      wOBA = (BB_HBP_Constant * Walks_HBP + Single_Constant * Singles +
                Double_Constant * Doubles + Triple_Constant * Triples +
                HomeRun_Constant * HomeRuns) / PA
    )
  
  return(stats)
}

Crushers_Hitters_Stats_byCount <- calculateHittersStatsbyCount("/Users/nathanielascher/Downloads/Frontier League 2024 Season.db", "5/09/2024", "9/03/2024", .717,.836,1.124,1.418,1.843)

calculateHittersStatsvsSpeedFastballSinker <- function(dbname, startDate, endDate, BB_HBP_Constant, Single_Constant, Double_Constant, Triple_Constant, HomeRun_Constant) {
  conn <- dbConnect(RSQLite::SQLite(), dbname = dbname)
  data <- dbReadTable(conn, "Lake Erie Crushers 2024")
  dbDisconnect(conn)
  
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  startDate <- as.Date(startDate, format = "%m/%d/%Y")
  endDate <- as.Date(endDate, format = "%m/%d/%Y")
  
  data_filtered <- data %>%
    filter(Date >= startDate, Date <= endDate, BatterTeam == "Lake Erie Crushers", TaggedPitchType %in% c("Fastball", "Sinker")) %>%
    select(Batter, TaggedPitchType, RelSpeed, PitchCall, PlayResult, KorBB, HitType) %>%
    mutate(SpeedCategory = ifelse(RelSpeed >= 90, "90Plus", "Less90"))
  
  stats <- data_filtered %>%
    group_by(Batter, SpeedCategory) %>%
    summarise(
      PA = sum(PitchCall %in% c("InPlay", "HitByPitch") | KorBB %in% c("Strikeout", "Walk"), na.rm = TRUE),
      Walks_HBP = sum(KorBB == "Walk" | PitchCall == "HitByPitch", na.rm = TRUE),
      Sacrifices = sum(PlayResult == "Sacrifice", na.rm = TRUE),
      SacrificeFlies = sum(PlayResult == "Sacrifice" & HitType != "Bunt", na.rm = TRUE),
      TotalBases = sum(1 * (PlayResult == "Single") + 2 * (PlayResult == "Double") + 3 * (PlayResult == "Triple") + 4 * (PlayResult == "HomeRun"), na.rm = TRUE),
      Singles = sum(PlayResult == "Single", na.rm = TRUE),
      Doubles = sum(PlayResult == "Double", na.rm = TRUE),
      Triples = sum(PlayResult == "Triple", na.rm = TRUE),
      HomeRuns = sum(PlayResult == "HomeRun", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Hits = Singles + Doubles + Triples + HomeRuns,
      ABs = PA - Walks_HBP - Sacrifices,
      BattingAverage = Hits / ABs,
      OBP = (Hits + Walks_HBP) / (ABs + Walks_HBP + SacrificeFlies),
      SLG = TotalBases / ABs,
      OPS = OBP + SLG,
      wOBA = (BB_HBP_Constant * Walks_HBP + Single_Constant * Singles +
                Double_Constant * Doubles + Triple_Constant * Triples +
                HomeRun_Constant * HomeRuns) / PA
    )
  
  return(stats)
}

Crushers_Hitters_Stats_90_FastballSinker <- calculateHittersStatsvsSpeedFastballSinker("/Users/nathanielascher/Downloads/Frontier League 2024 Season.db", "5/09/2024", "9/03/2024", .717,.836,1.124,1.418,1.843)

###LEADOFF STATS

calculateHittersStatswhenLeadoff <- function(dbname, startDate, endDate, BB_HBP_Constant, Single_Constant, Double_Constant, Triple_Constant, HomeRun_Constant) {
  conn <- dbConnect(RSQLite::SQLite(), dbname = dbname)
  data <- dbReadTable(conn, "Lake Erie Crushers 2024")
  dbDisconnect(conn)
  
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  startDate <- as.Date(startDate, format = "%m/%d/%Y")
  endDate <- as.Date(endDate, format = "%m/%d/%Y")
  
  data_filtered <- data %>%
    filter(Date >= startDate, Date <= endDate, BatterTeam == "Lake Erie Crushers", TaggedPitchType != "", L1 == 1)
  
  calculate_stats <- function(df, grouping_label, group_by_pitcher_throws = TRUE) {
    if (group_by_pitcher_throws) {
      df <- df %>%
        group_by(Batter, PitcherThrows)
    } else {
      df <- df %>%
        group_by(Batter)
    }
    
    df %>%
      summarise(
        PA = sum(PitchCall %in% c("InPlay", "HitByPitch") | KorBB %in% c("Strikeout", "Walk"), na.rm = TRUE),
        Walks_HBP = sum(KorBB == "Walk" | PitchCall == "HitByPitch", na.rm = TRUE),
        Sacrifices = sum(PlayResult == "Sacrifice", na.rm = TRUE),
        SacrificeFlies = sum(PlayResult == "Sacrifice" & HitType != "Bunt", na.rm = TRUE),
        TotalBases = sum(1 * (PlayResult == "Single") + 2 * (PlayResult == "Double") + 3 * (PlayResult == "Triple") + 4 * (PlayResult == "HomeRun"), na.rm = TRUE),
        Singles = sum(PlayResult == "Single", na.rm = TRUE),
        Doubles = sum(PlayResult == "Double", na.rm = TRUE),
        Triples = sum(PlayResult == "Triple", na.rm = TRUE),
        HomeRuns = sum(PlayResult == "HomeRun", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Hits = Singles + Doubles + Triples + HomeRuns,
        ABs = PA - Walks_HBP - Sacrifices,
        BattingAverage = Hits / ABs,
        OBP = (Hits + Walks_HBP) / (ABs + Walks_HBP + SacrificeFlies),
        SLG = TotalBases / ABs,
        OPS = OBP + SLG,
        wOBA = (BB_HBP_Constant * Walks_HBP + Single_Constant * Singles +
                  Double_Constant * Doubles + Triple_Constant * Triples +
                  HomeRun_Constant * HomeRuns) / PA,
        Grouping = grouping_label
      )
  }
  
  stats_leadoff_overall <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right")), "Leadoff Overall", group_by_pitcher_throws = FALSE)
  stats_leadoff_lefties <- calculate_stats(filter(data_filtered, PitcherThrows == "Left"), "Leadoff vs Lefties")
  stats_leadoff_righties <- calculate_stats(filter(data_filtered, PitcherThrows == "Right"), "Leadoff vs Righties")
  stats_leadoff_lefty_starters <- calculate_stats(filter(data_filtered, PitcherThrows == "Left", Starter == 1), "Leadoff vs Lefty Starters")
  stats_leadoff_righty_starters <- calculate_stats(filter(data_filtered, PitcherThrows == "Right", Starter == 1), "Leadoff vs Righty Starters")
  stats_leadoff_starters <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right"), Starter == 1), "Leadoff vs Starters", group_by_pitcher_throws = FALSE)
  stats_leadoff_relievers <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right"), Reliever == 1), "Leadoff vs Relievers", group_by_pitcher_throws = FALSE)
  
  combined_stats <- bind_rows(
    stats_leadoff_overall,
    stats_leadoff_lefties,
    stats_leadoff_righties,
    stats_leadoff_lefty_starters,
    stats_leadoff_righty_starters,
    stats_leadoff_starters,
    stats_leadoff_relievers
  )
  
  return(combined_stats)
}

Crushers_Hitters_Stats_whenLeadoff <- calculateHittersStatswhenLeadoff("/Users/nathanielascher/Downloads/Frontier League 2024 Season.db", "5/09/2024", "9/03/2024", .717,.836,1.124,1.418,1.843)

Crushers_Hitters_Stats_whenLeadoff_file_path <- "/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenLeadoff.csv"

write_csv(Crushers_Hitters_Stats_whenLeadoff, Crushers_Hitters_Stats_whenLeadoff_file_path)

###2-Hole STATS

calculateHittersStatswhenHittingSecond <- function(dbname, startDate, endDate, BB_HBP_Constant, Single_Constant, Double_Constant, Triple_Constant, HomeRun_Constant) {
  conn <- dbConnect(RSQLite::SQLite(), dbname = dbname)
  data <- dbReadTable(conn, "Lake Erie Crushers 2024")
  dbDisconnect(conn)
  
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  startDate <- as.Date(startDate, format = "%m/%d/%Y")
  endDate <- as.Date(endDate, format = "%m/%d/%Y")
  
  data_filtered <- data %>%
    filter(Date >= startDate, Date <= endDate, BatterTeam == "Lake Erie Crushers", TaggedPitchType != "", L2 == 1)
  
  calculate_stats <- function(df, grouping_label, group_by_pitcher_throws = TRUE) {
    if (group_by_pitcher_throws) {
      df <- df %>%
        group_by(Batter, PitcherThrows)
    } else {
      df <- df %>%
        group_by(Batter)
    }
    
    df %>%
      summarise(
        PA = sum(PitchCall %in% c("InPlay", "HitByPitch") | KorBB %in% c("Strikeout", "Walk"), na.rm = TRUE),
        Walks_HBP = sum(KorBB == "Walk" | PitchCall == "HitByPitch", na.rm = TRUE),
        Sacrifices = sum(PlayResult == "Sacrifice", na.rm = TRUE),
        SacrificeFlies = sum(PlayResult == "Sacrifice" & HitType != "Bunt", na.rm = TRUE),
        TotalBases = sum(1 * (PlayResult == "Single") + 2 * (PlayResult == "Double") + 3 * (PlayResult == "Triple") + 4 * (PlayResult == "HomeRun"), na.rm = TRUE),
        Singles = sum(PlayResult == "Single", na.rm = TRUE),
        Doubles = sum(PlayResult == "Double", na.rm = TRUE),
        Triples = sum(PlayResult == "Triple", na.rm = TRUE),
        HomeRuns = sum(PlayResult == "HomeRun", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Hits = Singles + Doubles + Triples + HomeRuns,
        ABs = PA - Walks_HBP - Sacrifices,
        BattingAverage = Hits / ABs,
        OBP = (Hits + Walks_HBP) / (ABs + Walks_HBP + SacrificeFlies),
        SLG = TotalBases / ABs,
        OPS = OBP + SLG,
        wOBA = (BB_HBP_Constant * Walks_HBP + Single_Constant * Singles +
                  Double_Constant * Doubles + Triple_Constant * Triples +
                  HomeRun_Constant * HomeRuns) / PA,
        Grouping = grouping_label
      )
  }
  
  stats_whenhittingsecond_overall <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right")), "Hitting Second Overall", group_by_pitcher_throws = FALSE)
  stats_whenhittingsecond_lefties <- calculate_stats(filter(data_filtered, PitcherThrows == "Left"), "Hitting Second vs Lefties")
  stats_whenhittingsecond_righties <- calculate_stats(filter(data_filtered, PitcherThrows == "Right"), "Hitting Second vs Righties")
  stats_whenhittingsecond_lefty_starters <- calculate_stats(filter(data_filtered, PitcherThrows == "Left", Starter == 1), "Hitting Second vs Lefty Starters")
  stats_whenhittingsecond_righty_starters <- calculate_stats(filter(data_filtered, PitcherThrows == "Right", Starter == 1), "Hitting Second vs Righty Starters")
  stats_whenhittingsecond_starters <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right"), Starter == 1), "Hitting Second vs Starters", group_by_pitcher_throws = FALSE)
  stats_whenhittingsecond_relievers <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right"), Reliever == 1), "Hitting Second vs Relievers", group_by_pitcher_throws = FALSE)
  
  combined_stats <- bind_rows(
    stats_whenhittingsecond_overall,
    stats_whenhittingsecond_lefties,
    stats_whenhittingsecond_righties,
    stats_whenhittingsecond_lefty_starters,
    stats_whenhittingsecond_righty_starters,
    stats_whenhittingsecond_starters,
    stats_whenhittingsecond_relievers
  )
  
  return(combined_stats)
}

Crushers_Hitters_Stats_whenHittingSecond <- calculateHittersStatswhenHittingSecond("/Users/nathanielascher/Downloads/Frontier League 2024 Season.db", "5/09/2024", "9/03/2024", .717,.836,1.124,1.418,1.843)

Crushers_Hitters_Stats_whenHittingSecond_file_path <- "/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenHittingSecond.csv"

write_csv(Crushers_Hitters_Stats_whenHittingSecond, Crushers_Hitters_Stats_whenHittingSecond_file_path)

###3-Hole STATS

calculateHittersStatswhenHittingThird <- function(dbname, startDate, endDate, BB_HBP_Constant, Single_Constant, Double_Constant, Triple_Constant, HomeRun_Constant) {
  conn <- dbConnect(RSQLite::SQLite(), dbname = dbname)
  data <- dbReadTable(conn, "Lake Erie Crushers 2024")
  dbDisconnect(conn)
  
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  startDate <- as.Date(startDate, format = "%m/%d/%Y")
  endDate <- as.Date(endDate, format = "%m/%d/%Y")
  
  data_filtered <- data %>%
    filter(Date >= startDate, Date <= endDate, BatterTeam == "Lake Erie Crushers", TaggedPitchType != "", L3 == 1)
  
  calculate_stats <- function(df, grouping_label, group_by_pitcher_throws = TRUE) {
    if (group_by_pitcher_throws) {
      df <- df %>%
        group_by(Batter, PitcherThrows)
    } else {
      df <- df %>%
        group_by(Batter)
    }
    
    df %>%
      summarise(
        PA = sum(PitchCall %in% c("InPlay", "HitByPitch") | KorBB %in% c("Strikeout", "Walk"), na.rm = TRUE),
        Walks_HBP = sum(KorBB == "Walk" | PitchCall == "HitByPitch", na.rm = TRUE),
        Sacrifices = sum(PlayResult == "Sacrifice", na.rm = TRUE),
        SacrificeFlies = sum(PlayResult == "Sacrifice" & HitType != "Bunt", na.rm = TRUE),
        TotalBases = sum(1 * (PlayResult == "Single") + 2 * (PlayResult == "Double") + 3 * (PlayResult == "Triple") + 4 * (PlayResult == "HomeRun"), na.rm = TRUE),
        Singles = sum(PlayResult == "Single", na.rm = TRUE),
        Doubles = sum(PlayResult == "Double", na.rm = TRUE),
        Triples = sum(PlayResult == "Triple", na.rm = TRUE),
        HomeRuns = sum(PlayResult == "HomeRun", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Hits = Singles + Doubles + Triples + HomeRuns,
        ABs = PA - Walks_HBP - Sacrifices,
        BattingAverage = Hits / ABs,
        OBP = (Hits + Walks_HBP) / (ABs + Walks_HBP + SacrificeFlies),
        SLG = TotalBases / ABs,
        OPS = OBP + SLG,
        wOBA = (BB_HBP_Constant * Walks_HBP + Single_Constant * Singles +
                  Double_Constant * Doubles + Triple_Constant * Triples +
                  HomeRun_Constant * HomeRuns) / PA,
        Grouping = grouping_label
      )
  }
  
  stats_whenhittingthird_overall <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right")), "Hitting Third Overall", group_by_pitcher_throws = FALSE)
  stats_whenhittingthird_lefties <- calculate_stats(filter(data_filtered, PitcherThrows == "Left"), "Hitting Third vs Lefties")
  stats_whenhittingthird_righties <- calculate_stats(filter(data_filtered, PitcherThrows == "Right"), "Hitting Third vs Righties")
  stats_whenhittingthird_lefty_starters <- calculate_stats(filter(data_filtered, PitcherThrows == "Left", Starter == 1), "Hitting Third vs Lefty Starters")
  stats_whenhittingthird_righty_starters <- calculate_stats(filter(data_filtered, PitcherThrows == "Right", Starter == 1), "Hitting Third vs Righty Starters")
  stats_whenhittingthird_starters <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right"), Starter == 1), "Hitting Third vs Starters", group_by_pitcher_throws = FALSE)
  stats_whenhittingthird_relievers <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right"), Reliever == 1), "Hitting Third vs Relievers", group_by_pitcher_throws = FALSE)
  
  combined_stats <- bind_rows(
    stats_whenhittingthird_overall,
    stats_whenhittingthird_lefties,
    stats_whenhittingthird_righties,
    stats_whenhittingthird_lefty_starters,
    stats_whenhittingthird_righty_starters,
    stats_whenhittingthird_starters,
    stats_whenhittingthird_relievers
  )
  
  return(combined_stats)
}

Crushers_Hitters_Stats_whenHittingThird <- calculateHittersStatswhenHittingThird("/Users/nathanielascher/Downloads/Frontier League 2024 Season.db", "5/09/2024", "9/03/2024", .717,.836,1.124,1.418,1.843)

Crushers_Hitters_Stats_whenHittingThird_file_path <- "/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenHittingThird.csv"

write_csv(Crushers_Hitters_Stats_whenHittingThird, Crushers_Hitters_Stats_whenHittingThird_file_path)

###4-Hole STATS

calculateHittersStatswhenHittingFourth <- function(dbname, startDate, endDate, BB_HBP_Constant, Single_Constant, Double_Constant, Triple_Constant, HomeRun_Constant) {
  conn <- dbConnect(RSQLite::SQLite(), dbname = dbname)
  data <- dbReadTable(conn, "Lake Erie Crushers 2024")
  dbDisconnect(conn)
  
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  startDate <- as.Date(startDate, format = "%m/%d/%Y")
  endDate <- as.Date(endDate, format = "%m/%d/%Y")
  
  data_filtered <- data %>%
    filter(Date >= startDate, Date <= endDate, BatterTeam == "Lake Erie Crushers", TaggedPitchType != "", L4 == 1)
  
  calculate_stats <- function(df, grouping_label, group_by_pitcher_throws = TRUE) {
    if (group_by_pitcher_throws) {
      df <- df %>%
        group_by(Batter, PitcherThrows)
    } else {
      df <- df %>%
        group_by(Batter)
    }
    
    df %>%
      summarise(
        PA = sum(PitchCall %in% c("InPlay", "HitByPitch") | KorBB %in% c("Strikeout", "Walk"), na.rm = TRUE),
        Walks_HBP = sum(KorBB == "Walk" | PitchCall == "HitByPitch", na.rm = TRUE),
        Sacrifices = sum(PlayResult == "Sacrifice", na.rm = TRUE),
        SacrificeFlies = sum(PlayResult == "Sacrifice" & HitType != "Bunt", na.rm = TRUE),
        TotalBases = sum(1 * (PlayResult == "Single") + 2 * (PlayResult == "Double") + 3 * (PlayResult == "Triple") + 4 * (PlayResult == "HomeRun"), na.rm = TRUE),
        Singles = sum(PlayResult == "Single", na.rm = TRUE),
        Doubles = sum(PlayResult == "Double", na.rm = TRUE),
        Triples = sum(PlayResult == "Triple", na.rm = TRUE),
        HomeRuns = sum(PlayResult == "HomeRun", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Hits = Singles + Doubles + Triples + HomeRuns,
        ABs = PA - Walks_HBP - Sacrifices,
        BattingAverage = Hits / ABs,
        OBP = (Hits + Walks_HBP) / (ABs + Walks_HBP + SacrificeFlies),
        SLG = TotalBases / ABs,
        OPS = OBP + SLG,
        wOBA = (BB_HBP_Constant * Walks_HBP + Single_Constant * Singles +
                  Double_Constant * Doubles + Triple_Constant * Triples +
                  HomeRun_Constant * HomeRuns) / PA,
        Grouping = grouping_label
      )
  }
  
  stats_whenhittingfourth_overall <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right")), "Hitting Fourth Overall", group_by_pitcher_throws = FALSE)
  stats_whenhittingfourth_lefties <- calculate_stats(filter(data_filtered, PitcherThrows == "Left"), "Hitting Fourth vs Lefties")
  stats_whenhittingfourth_righties <- calculate_stats(filter(data_filtered, PitcherThrows == "Right"), "Hitting Fourth vs Righties")
  stats_whenhittingfourth_lefty_starters <- calculate_stats(filter(data_filtered, PitcherThrows == "Left", Starter == 1), "Hitting Fourth vs Lefty Starters")
  stats_whenhittingfourth_righty_starters <- calculate_stats(filter(data_filtered, PitcherThrows == "Right", Starter == 1), "Hitting Fourth vs Righty Starters")
  stats_whenhittingfourth_starters <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right"), Starter == 1), "Hitting Fourth vs Starters", group_by_pitcher_throws = FALSE)
  stats_whenhittingfourth_relievers <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right"), Reliever == 1), "Hitting Fourth vs Relievers", group_by_pitcher_throws = FALSE)
  
  combined_stats <- bind_rows(
    stats_whenhittingfourth_overall,
    stats_whenhittingfourth_lefties,
    stats_whenhittingfourth_righties,
    stats_whenhittingfourth_lefty_starters,
    stats_whenhittingfourth_righty_starters,
    stats_whenhittingfourth_starters,
    stats_whenhittingfourth_relievers
  )
  
  return(combined_stats)
}

Crushers_Hitters_Stats_whenHittingFourth <- calculateHittersStatswhenHittingFourth("/Users/nathanielascher/Downloads/Frontier League 2024 Season.db", "5/09/2024", "9/03/2024", .717,.836,1.124,1.418,1.843)

Crushers_Hitters_Stats_whenHittingFourth_file_path <- "/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenHittingFourth.csv"

write_csv(Crushers_Hitters_Stats_whenHittingFourth, Crushers_Hitters_Stats_whenHittingFourth_file_path)

###5-Hole STATS

calculateHittersStatswhenHittingFifth <- function(dbname, startDate, endDate, BB_HBP_Constant, Single_Constant, Double_Constant, Triple_Constant, HomeRun_Constant) {
  conn <- dbConnect(RSQLite::SQLite(), dbname = dbname)
  data <- dbReadTable(conn, "Lake Erie Crushers 2024")
  dbDisconnect(conn)
  
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  startDate <- as.Date(startDate, format = "%m/%d/%Y")
  endDate <- as.Date(endDate, format = "%m/%d/%Y")
  
  data_filtered <- data %>%
    filter(Date >= startDate, Date <= endDate, BatterTeam == "Lake Erie Crushers", TaggedPitchType != "", L5 == 1)
  
  calculate_stats <- function(df, grouping_label, group_by_pitcher_throws = TRUE) {
    if (group_by_pitcher_throws) {
      df <- df %>%
        group_by(Batter, PitcherThrows)
    } else {
      df <- df %>%
        group_by(Batter)
    }
    
    df %>%
      summarise(
        PA = sum(PitchCall %in% c("InPlay", "HitByPitch") | KorBB %in% c("Strikeout", "Walk"), na.rm = TRUE),
        Walks_HBP = sum(KorBB == "Walk" | PitchCall == "HitByPitch", na.rm = TRUE),
        Sacrifices = sum(PlayResult == "Sacrifice", na.rm = TRUE),
        SacrificeFlies = sum(PlayResult == "Sacrifice" & HitType != "Bunt", na.rm = TRUE),
        TotalBases = sum(1 * (PlayResult == "Single") + 2 * (PlayResult == "Double") + 3 * (PlayResult == "Triple") + 4 * (PlayResult == "HomeRun"), na.rm = TRUE),
        Singles = sum(PlayResult == "Single", na.rm = TRUE),
        Doubles = sum(PlayResult == "Double", na.rm = TRUE),
        Triples = sum(PlayResult == "Triple", na.rm = TRUE),
        HomeRuns = sum(PlayResult == "HomeRun", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Hits = Singles + Doubles + Triples + HomeRuns,
        ABs = PA - Walks_HBP - Sacrifices,
        BattingAverage = Hits / ABs,
        OBP = (Hits + Walks_HBP) / (ABs + Walks_HBP + SacrificeFlies),
        SLG = TotalBases / ABs,
        OPS = OBP + SLG,
        wOBA = (BB_HBP_Constant * Walks_HBP + Single_Constant * Singles +
                  Double_Constant * Doubles + Triple_Constant * Triples +
                  HomeRun_Constant * HomeRuns) / PA,
        Grouping = grouping_label
      )
  }
  
  stats_whenhittingfifth_overall <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right")), "Hitting Fifth Overall", group_by_pitcher_throws = FALSE)
  stats_whenhittingfifth_lefties <- calculate_stats(filter(data_filtered, PitcherThrows == "Left"), "Hitting Fifth vs Lefties")
  stats_whenhittingfifth_righties <- calculate_stats(filter(data_filtered, PitcherThrows == "Right"), "Hitting Fifth vs Righties")
  stats_whenhittingfifth_lefty_starters <- calculate_stats(filter(data_filtered, PitcherThrows == "Left", Starter == 1), "Hitting Fifth vs Lefty Starters")
  stats_whenhittingfifth_righty_starters <- calculate_stats(filter(data_filtered, PitcherThrows == "Right", Starter == 1), "Hitting Fifth vs Righty Starters")
  stats_whenhittingfifth_starters <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right"), Starter == 1), "Hitting Fifth vs Starters", group_by_pitcher_throws = FALSE)
  stats_whenhittingfifth_relievers <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right"), Reliever == 1), "Hitting Fifth vs Relievers", group_by_pitcher_throws = FALSE)
  
  combined_stats <- bind_rows(
    stats_whenhittingfifth_overall,
    stats_whenhittingfifth_lefties,
    stats_whenhittingfifth_righties,
    stats_whenhittingfifth_lefty_starters,
    stats_whenhittingfifth_righty_starters,
    stats_whenhittingfifth_starters,
    stats_whenhittingfifth_relievers
  )
  
  return(combined_stats)
}

Crushers_Hitters_Stats_whenHittingFifth <- calculateHittersStatswhenHittingFifth("/Users/nathanielascher/Downloads/Frontier League 2024 Season.db", "5/09/2024", "9/03/2024", .717,.836,1.124,1.418,1.843)

Crushers_Hitters_Stats_whenHittingFifth_file_path <- "/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenHittingFifth.csv"

write_csv(Crushers_Hitters_Stats_whenHittingFifth, Crushers_Hitters_Stats_whenHittingFifth_file_path)

###6-Hole STATS

calculateHittersStatswhenHittingSixth <- function(dbname, startDate, endDate, BB_HBP_Constant, Single_Constant, Double_Constant, Triple_Constant, HomeRun_Constant) {
  conn <- dbConnect(RSQLite::SQLite(), dbname = dbname)
  data <- dbReadTable(conn, "Lake Erie Crushers 2024")
  dbDisconnect(conn)
  
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  startDate <- as.Date(startDate, format = "%m/%d/%Y")
  endDate <- as.Date(endDate, format = "%m/%d/%Y")
  
  data_filtered <- data %>%
    filter(Date >= startDate, Date <= endDate, BatterTeam == "Lake Erie Crushers", TaggedPitchType != "", L6 == 1)
  
  calculate_stats <- function(df, grouping_label, group_by_pitcher_throws = TRUE) {
    if (group_by_pitcher_throws) {
      df <- df %>%
        group_by(Batter, PitcherThrows)
    } else {
      df <- df %>%
        group_by(Batter)
    }
    
    df %>%
      summarise(
        PA = sum(PitchCall %in% c("InPlay", "HitByPitch") | KorBB %in% c("Strikeout", "Walk"), na.rm = TRUE),
        Walks_HBP = sum(KorBB == "Walk" | PitchCall == "HitByPitch", na.rm = TRUE),
        Sacrifices = sum(PlayResult == "Sacrifice", na.rm = TRUE),
        SacrificeFlies = sum(PlayResult == "Sacrifice" & HitType != "Bunt", na.rm = TRUE),
        TotalBases = sum(1 * (PlayResult == "Single") + 2 * (PlayResult == "Double") + 3 * (PlayResult == "Triple") + 4 * (PlayResult == "HomeRun"), na.rm = TRUE),
        Singles = sum(PlayResult == "Single", na.rm = TRUE),
        Doubles = sum(PlayResult == "Double", na.rm = TRUE),
        Triples = sum(PlayResult == "Triple", na.rm = TRUE),
        HomeRuns = sum(PlayResult == "HomeRun", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Hits = Singles + Doubles + Triples + HomeRuns,
        ABs = PA - Walks_HBP - Sacrifices,
        BattingAverage = Hits / ABs,
        OBP = (Hits + Walks_HBP) / (ABs + Walks_HBP + SacrificeFlies),
        SLG = TotalBases / ABs,
        OPS = OBP + SLG,
        wOBA = (BB_HBP_Constant * Walks_HBP + Single_Constant * Singles +
                  Double_Constant * Doubles + Triple_Constant * Triples +
                  HomeRun_Constant * HomeRuns) / PA,
        Grouping = grouping_label
      )
  }
  
  stats_whenhittingsixth_overall <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right")), "Hitting Sixth Overall", group_by_pitcher_throws = FALSE)
  stats_whenhittingsixth_lefties <- calculate_stats(filter(data_filtered, PitcherThrows == "Left"), "Hitting Sixth vs Lefties")
  stats_whenhittingsixth_righties <- calculate_stats(filter(data_filtered, PitcherThrows == "Right"), "Hitting Sixth vs Righties")
  stats_whenhittingsixth_lefty_starters <- calculate_stats(filter(data_filtered, PitcherThrows == "Left", Starter == 1), "Hitting Sixth vs Lefty Starters")
  stats_whenhittingsixth_righty_starters <- calculate_stats(filter(data_filtered, PitcherThrows == "Right", Starter == 1), "Hitting Sixth vs Righty Starters")
  stats_whenhittingsixth_starters <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right"), Starter == 1), "Hitting Sixth vs Starters", group_by_pitcher_throws = FALSE)
  stats_whenhittingsixth_relievers <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right"), Reliever == 1), "Hitting Sixth vs Relievers", group_by_pitcher_throws = FALSE)
  
  combined_stats <- bind_rows(
    stats_whenhittingsixth_overall,
    stats_whenhittingsixth_lefties,
    stats_whenhittingsixth_righties,
    stats_whenhittingsixth_lefty_starters,
    stats_whenhittingsixth_righty_starters,
    stats_whenhittingsixth_starters,
    stats_whenhittingsixth_relievers
  )
  
  return(combined_stats)
}

Crushers_Hitters_Stats_whenHittingSixth <- calculateHittersStatswhenHittingSixth("/Users/nathanielascher/Downloads/Frontier League 2024 Season.db", "5/09/2024", "9/03/2024", .717,.836,1.124,1.418,1.843)

Crushers_Hitters_Stats_whenHittingSixth_file_path <- "/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenHittingSixth.csv"

write_csv(Crushers_Hitters_Stats_whenHittingSixth, Crushers_Hitters_Stats_whenHittingSixth_file_path)

###7-Hole STATS

calculateHittersStatswhenHittingSeventh <- function(dbname, startDate, endDate, BB_HBP_Constant, Single_Constant, Double_Constant, Triple_Constant, HomeRun_Constant) {
  conn <- dbConnect(RSQLite::SQLite(), dbname = dbname)
  data <- dbReadTable(conn, "Lake Erie Crushers 2024")
  dbDisconnect(conn)
  
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  startDate <- as.Date(startDate, format = "%m/%d/%Y")
  endDate <- as.Date(endDate, format = "%m/%d/%Y")
  
  data_filtered <- data %>%
    filter(Date >= startDate, Date <= endDate, BatterTeam == "Lake Erie Crushers", TaggedPitchType != "", L7 == 1)
  
  calculate_stats <- function(df, grouping_label, group_by_pitcher_throws = TRUE) {
    if (group_by_pitcher_throws) {
      df <- df %>%
        group_by(Batter, PitcherThrows)
    } else {
      df <- df %>%
        group_by(Batter)
    }
    
    df %>%
      summarise(
        PA = sum(PitchCall %in% c("InPlay", "HitByPitch") | KorBB %in% c("Strikeout", "Walk"), na.rm = TRUE),
        Walks_HBP = sum(KorBB == "Walk" | PitchCall == "HitByPitch", na.rm = TRUE),
        Sacrifices = sum(PlayResult == "Sacrifice", na.rm = TRUE),
        SacrificeFlies = sum(PlayResult == "Sacrifice" & HitType != "Bunt", na.rm = TRUE),
        TotalBases = sum(1 * (PlayResult == "Single") + 2 * (PlayResult == "Double") + 3 * (PlayResult == "Triple") + 4 * (PlayResult == "HomeRun"), na.rm = TRUE),
        Singles = sum(PlayResult == "Single", na.rm = TRUE),
        Doubles = sum(PlayResult == "Double", na.rm = TRUE),
        Triples = sum(PlayResult == "Triple", na.rm = TRUE),
        HomeRuns = sum(PlayResult == "HomeRun", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Hits = Singles + Doubles + Triples + HomeRuns,
        ABs = PA - Walks_HBP - Sacrifices,
        BattingAverage = Hits / ABs,
        OBP = (Hits + Walks_HBP) / (ABs + Walks_HBP + SacrificeFlies),
        SLG = TotalBases / ABs,
        OPS = OBP + SLG,
        wOBA = (BB_HBP_Constant * Walks_HBP + Single_Constant * Singles +
                  Double_Constant * Doubles + Triple_Constant * Triples +
                  HomeRun_Constant * HomeRuns) / PA,
        Grouping = grouping_label
      )
  }
  
  stats_whenhittingseventh_overall <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right")), "Hitting Seventh Overall", group_by_pitcher_throws = FALSE)
  stats_whenhittingseventh_lefties <- calculate_stats(filter(data_filtered, PitcherThrows == "Left"), "Hitting Seventh vs Lefties")
  stats_whenhittingseventh_righties <- calculate_stats(filter(data_filtered, PitcherThrows == "Right"), "Hitting Seventh vs Righties")
  stats_whenhittingseventh_lefty_starters <- calculate_stats(filter(data_filtered, PitcherThrows == "Left", Starter == 1), "Hitting Seventh vs Lefty Starters")
  stats_whenhittingseventh_righty_starters <- calculate_stats(filter(data_filtered, PitcherThrows == "Right", Starter == 1), "Hitting Seventh vs Righty Starters")
  stats_whenhittingseventh_starters <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right"), Starter == 1), "Hitting Seventh vs Starters", group_by_pitcher_throws = FALSE)
  stats_whenhittingseventh_relievers <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right"), Reliever == 1), "Hitting Seventh vs Relievers", group_by_pitcher_throws = FALSE)
  
  combined_stats <- bind_rows(
    stats_whenhittingseventh_overall,
    stats_whenhittingseventh_lefties,
    stats_whenhittingseventh_righties,
    stats_whenhittingseventh_lefty_starters,
    stats_whenhittingseventh_righty_starters,
    stats_whenhittingseventh_starters,
    stats_whenhittingseventh_relievers
  )
  
  return(combined_stats)
}

Crushers_Hitters_Stats_whenHittingSeventh <- calculateHittersStatswhenHittingSeventh("/Users/nathanielascher/Downloads/Frontier League 2024 Season.db", "5/09/2024", "9/03/2024", .717,.836,1.124,1.418,1.843)

Crushers_Hitters_Stats_whenHittingSeventh_file_path <- "/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenHittingSeventh.csv"

write_csv(Crushers_Hitters_Stats_whenHittingSeventh, Crushers_Hitters_Stats_whenHittingSeventh_file_path)

###8-Hole STATS

calculateHittersStatswhenHittingEighth <- function(dbname, startDate, endDate, BB_HBP_Constant, Single_Constant, Double_Constant, Triple_Constant, HomeRun_Constant) {
  conn <- dbConnect(RSQLite::SQLite(), dbname = dbname)
  data <- dbReadTable(conn, "Lake Erie Crushers 2024")
  dbDisconnect(conn)
  
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  startDate <- as.Date(startDate, format = "%m/%d/%Y")
  endDate <- as.Date(endDate, format = "%m/%d/%Y")
  
  data_filtered <- data %>%
    filter(Date >= startDate, Date <= endDate, BatterTeam == "Lake Erie Crushers", TaggedPitchType != "", L8 == 1)
  
  calculate_stats <- function(df, grouping_label, group_by_pitcher_throws = TRUE) {
    if (group_by_pitcher_throws) {
      df <- df %>%
        group_by(Batter, PitcherThrows)
    } else {
      df <- df %>%
        group_by(Batter)
    }
    
    df %>%
      summarise(
        PA = sum(PitchCall %in% c("InPlay", "HitByPitch") | KorBB %in% c("Strikeout", "Walk"), na.rm = TRUE),
        Walks_HBP = sum(KorBB == "Walk" | PitchCall == "HitByPitch", na.rm = TRUE),
        Sacrifices = sum(PlayResult == "Sacrifice", na.rm = TRUE),
        SacrificeFlies = sum(PlayResult == "Sacrifice" & HitType != "Bunt", na.rm = TRUE),
        TotalBases = sum(1 * (PlayResult == "Single") + 2 * (PlayResult == "Double") + 3 * (PlayResult == "Triple") + 4 * (PlayResult == "HomeRun"), na.rm = TRUE),
        Singles = sum(PlayResult == "Single", na.rm = TRUE),
        Doubles = sum(PlayResult == "Double", na.rm = TRUE),
        Triples = sum(PlayResult == "Triple", na.rm = TRUE),
        HomeRuns = sum(PlayResult == "HomeRun", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Hits = Singles + Doubles + Triples + HomeRuns,
        ABs = PA - Walks_HBP - Sacrifices,
        BattingAverage = Hits / ABs,
        OBP = (Hits + Walks_HBP) / (ABs + Walks_HBP + SacrificeFlies),
        SLG = TotalBases / ABs,
        OPS = OBP + SLG,
        wOBA = (BB_HBP_Constant * Walks_HBP + Single_Constant * Singles +
                  Double_Constant * Doubles + Triple_Constant * Triples +
                  HomeRun_Constant * HomeRuns) / PA,
        Grouping = grouping_label
      )
  }
  
  stats_whenhittingeighth_overall <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right")), "Hitting Eighth Overall", group_by_pitcher_throws = FALSE)
  stats_whenhittingeighth_lefties <- calculate_stats(filter(data_filtered, PitcherThrows == "Left"), "Hitting Eighth vs Lefties")
  stats_whenhittingeighth_righties <- calculate_stats(filter(data_filtered, PitcherThrows == "Right"), "Hitting Eighth vs Righties")
  stats_whenhittingeighth_lefty_starters <- calculate_stats(filter(data_filtered, PitcherThrows == "Left", Starter == 1), "Hitting Eighth vs Lefty Starters")
  stats_whenhittingeighth_righty_starters <- calculate_stats(filter(data_filtered, PitcherThrows == "Right", Starter == 1), "Hitting Eighth vs Righty Starters")
  stats_whenhittingeighth_starters <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right"), Starter == 1), "Hitting Eighth vs Starters", group_by_pitcher_throws = FALSE)
  stats_whenhittingeighth_relievers <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right"), Reliever == 1), "Hitting Eighth vs Relievers", group_by_pitcher_throws = FALSE)
  
  combined_stats <- bind_rows(
    stats_whenhittingeighth_overall,
    stats_whenhittingeighth_lefties,
    stats_whenhittingeighth_righties,
    stats_whenhittingeighth_lefty_starters,
    stats_whenhittingeighth_righty_starters,
    stats_whenhittingeighth_starters,
    stats_whenhittingeighth_relievers
  )
  
  return(combined_stats)
}

Crushers_Hitters_Stats_whenHittingEighth <- calculateHittersStatswhenHittingEighth("/Users/nathanielascher/Downloads/Frontier League 2024 Season.db", "5/09/2024", "9/03/2024", .717,.836,1.124,1.418,1.843)

Crushers_Hitters_Stats_whenHittingEighth_file_path <- "/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenHittingEighth.csv"

write_csv(Crushers_Hitters_Stats_whenHittingEighth, Crushers_Hitters_Stats_whenHittingEighth_file_path)

###9-Hole STATS

calculateHittersStatswhenHittingNinth <- function(dbname, startDate, endDate, BB_HBP_Constant, Single_Constant, Double_Constant, Triple_Constant, HomeRun_Constant) {
  conn <- dbConnect(RSQLite::SQLite(), dbname = dbname)
  data <- dbReadTable(conn, "Lake Erie Crushers 2024")
  dbDisconnect(conn)
  
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  startDate <- as.Date(startDate, format = "%m/%d/%Y")
  endDate <- as.Date(endDate, format = "%m/%d/%Y")
  
  data_filtered <- data %>%
    filter(Date >= startDate, Date <= endDate, BatterTeam == "Lake Erie Crushers", TaggedPitchType != "", L9 == 1)
  
  calculate_stats <- function(df, grouping_label, group_by_pitcher_throws = TRUE) {
    if (group_by_pitcher_throws) {
      df <- df %>%
        group_by(Batter, PitcherThrows)
    } else {
      df <- df %>%
        group_by(Batter)
    }
    
    df %>%
      summarise(
        PA = sum(PitchCall %in% c("InPlay", "HitByPitch") | KorBB %in% c("Strikeout", "Walk"), na.rm = TRUE),
        Walks_HBP = sum(KorBB == "Walk" | PitchCall == "HitByPitch", na.rm = TRUE),
        Sacrifices = sum(PlayResult == "Sacrifice", na.rm = TRUE),
        SacrificeFlies = sum(PlayResult == "Sacrifice" & HitType != "Bunt", na.rm = TRUE),
        TotalBases = sum(1 * (PlayResult == "Single") + 2 * (PlayResult == "Double") + 3 * (PlayResult == "Triple") + 4 * (PlayResult == "HomeRun"), na.rm = TRUE),
        Singles = sum(PlayResult == "Single", na.rm = TRUE),
        Doubles = sum(PlayResult == "Double", na.rm = TRUE),
        Triples = sum(PlayResult == "Triple", na.rm = TRUE),
        HomeRuns = sum(PlayResult == "HomeRun", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Hits = Singles + Doubles + Triples + HomeRuns,
        ABs = PA - Walks_HBP - Sacrifices,
        BattingAverage = Hits / ABs,
        OBP = (Hits + Walks_HBP) / (ABs + Walks_HBP + SacrificeFlies),
        SLG = TotalBases / ABs,
        OPS = OBP + SLG,
        wOBA = (BB_HBP_Constant * Walks_HBP + Single_Constant * Singles +
                  Double_Constant * Doubles + Triple_Constant * Triples +
                  HomeRun_Constant * HomeRuns) / PA,
        Grouping = grouping_label
      )
  }
  
  stats_whenhittingninth_overall <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right")), "Hitting Ninth Overall", group_by_pitcher_throws = FALSE)
  stats_whenhittingninth_lefties <- calculate_stats(filter(data_filtered, PitcherThrows == "Left"), "Hitting Ninth vs Lefties")
  stats_whenhittingninth_righties <- calculate_stats(filter(data_filtered, PitcherThrows == "Right"), "Hitting Ninth vs Righties")
  stats_whenhittingninth_lefty_starters <- calculate_stats(filter(data_filtered, PitcherThrows == "Left", Starter == 1), "Hitting Ninth vs Lefty Starters")
  stats_whenhittingninth_righty_starters <- calculate_stats(filter(data_filtered, PitcherThrows == "Right", Starter == 1), "Hitting Ninth vs Righty Starters")
  stats_whenhittingninth_starters <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right"), Starter == 1), "Hitting Ninth vs Starters", group_by_pitcher_throws = FALSE)
  stats_whenhittingninth_relievers <- calculate_stats(filter(data_filtered, PitcherThrows %in% c("Left", "Right"), Reliever == 1), "Hitting Ninth vs Relievers", group_by_pitcher_throws = FALSE)
  
  combined_stats <- bind_rows(
    stats_whenhittingninth_overall,
    stats_whenhittingninth_lefties,
    stats_whenhittingninth_righties,
    stats_whenhittingninth_lefty_starters,
    stats_whenhittingninth_righty_starters,
    stats_whenhittingninth_starters,
    stats_whenhittingninth_relievers
  )
  
  return(combined_stats)
}

Crushers_Hitters_Stats_whenHittingNinth <- calculateHittersStatswhenHittingNinth("/Users/nathanielascher/Downloads/Frontier League 2024 Season.db", "5/09/2024", "9/03/2024", .717,.836,1.124,1.418,1.843)

Crushers_Hitters_Stats_whenHittingNinth_file_path <- "/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenHittingNinth.csv"

write_csv(Crushers_Hitters_Stats_whenHittingNinth, Crushers_Hitters_Stats_whenHittingNinth_file_path)

#install.packages("DBI")
#install.packages("RSQLite") 
library(DBI)
library(RSQLite)

data <- read.csv("/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenLeadoff.csv")
conn <- dbConnect(RSQLite::SQLite(), dbname = "/Users/nathanielascher/Downloads/Frontier League 2024 Season.db")
dbWriteTable(conn, "Lake Erie Crushers Hitters Stats When Hitting Leadoff", data, overwrite = TRUE, row.names = FALSE)
dbDisconnect(conn)

data <- read.csv("/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenHittingSecond.csv")
conn <- dbConnect(RSQLite::SQLite(), dbname = "/Users/nathanielascher/Downloads/Frontier League 2024 Season.db")
dbWriteTable(conn, "Lake Erie Crushers Hitters Stats When Hitting Second", data, overwrite = TRUE, row.names = FALSE)
dbDisconnect(conn)

data <- read.csv("/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenHittingThird.csv")
conn <- dbConnect(RSQLite::SQLite(), dbname = "/Users/nathanielascher/Downloads/Frontier League 2024 Season.db")
dbWriteTable(conn, "Lake Erie Crushers Hitters Stats When Hitting Third", data, overwrite = TRUE, row.names = FALSE)
dbDisconnect(conn)

data <- read.csv("/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenHittingFourth.csv")
conn <- dbConnect(RSQLite::SQLite(), dbname = "/Users/nathanielascher/Downloads/Frontier League 2024 Season.db")
dbWriteTable(conn, "Lake Erie Crushers Hitters Stats When Hitting Fourth", data, overwrite = TRUE, row.names = FALSE)
dbDisconnect(conn)

data <- read.csv("/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenHittingFifth.csv")
conn <- dbConnect(RSQLite::SQLite(), dbname = "/Users/nathanielascher/Downloads/Frontier League 2024 Season.db")
dbWriteTable(conn, "Lake Erie Crushers Hitters Stats When Hitting Fifth", data, overwrite = TRUE, row.names = FALSE)
dbDisconnect(conn)

data <- read.csv("/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenHittingSixth.csv")
conn <- dbConnect(RSQLite::SQLite(), dbname = "/Users/nathanielascher/Downloads/Frontier League 2024 Season.db")
dbWriteTable(conn, "Lake Erie Crushers Hitters Stats When Hitting Sixth", data, overwrite = TRUE, row.names = FALSE)
dbDisconnect(conn)

data <- read.csv("/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenHittingSeventh.csv")
conn <- dbConnect(RSQLite::SQLite(), dbname = "/Users/nathanielascher/Downloads/Frontier League 2024 Season.db")
dbWriteTable(conn, "Lake Erie Crushers Hitters Stats When Hitting Seventh", data, overwrite = TRUE, row.names = FALSE)
dbDisconnect(conn)

data <- read.csv("/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenHittingEighth.csv")
conn <- dbConnect(RSQLite::SQLite(), dbname = "/Users/nathanielascher/Downloads/Frontier League 2024 Season.db")
dbWriteTable(conn, "Lake Erie Crushers Hitters Stats When Hitting Eighth", data, overwrite = TRUE, row.names = FALSE)
dbDisconnect(conn)

data <- read.csv("/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenHittingNinth.csv")
conn <- dbConnect(RSQLite::SQLite(), dbname = "/Users/nathanielascher/Downloads/Frontier League 2024 Season.db")
dbWriteTable(conn, "Lake Erie Crushers Hitters Stats When Hitting Ninth", data, overwrite = TRUE, row.names = FALSE)
dbDisconnect(conn)

leadoff_data <- read.csv("/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenLeadoff.csv")
second_data <- read.csv("/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenHittingSecond.csv")
third_data <- read.csv("/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenHittingThird.csv")
fourth_data <- read.csv("/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenHittingFourth.csv")
fifth_data <- read.csv("/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenHittingFifth.csv")
sixth_data <- read.csv("/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenHittingSixth.csv")
seventh_data <- read.csv("/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenHittingSeventh.csv")
eighth_data <- read.csv("/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenHittingEighth.csv")
ninth_data <- read.csv("/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_whenHittingNinth.csv")

combined_data <- bind_rows(
  leadoff_data,
  second_data,
  third_data,
  fourth_data,
  fifth_data,
  sixth_data,
  seventh_data,
  eighth_data,
  ninth_data
)

combined_file_path <- "/Users/nathanielascher/Downloads/Crushers_Hitters_Stats_by_Lineup_Position.csv"
write_csv(combined_data, combined_file_path)

conn <- dbConnect(RSQLite::SQLite(), dbname = "/Users/nathanielascher/Downloads/Frontier League 2024 Season.db")
dbWriteTable(conn, "Lake Erie Crushers Hitters Stats by Lineup Position", combined_data, overwrite = TRUE, row.names = FALSE)
dbDisconnect(conn)

