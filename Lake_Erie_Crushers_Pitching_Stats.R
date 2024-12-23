library(rvest)
library(dplyr)
library(stringr)

create_pitcher_stats_data_frames_extended <- function(url) {
  page <- read_html(url)
  
  basic_table <- page %>% html_node('table[data-col-name="ip"]')
  
  basic_rows <- basic_table %>% html_nodes("tr")
  
  player_data_frames <- list()
  
  for (i in 2:length(basic_rows)) {
    player_data_frames <- process_basic_stats(basic_rows[i], player_data_frames)
  }
  
  return(player_data_frames)
}

process_basic_stats <- function(row, player_data_frames) {
  basic_data <- row %>% html_nodes('td') %>% html_text()
  player_href <- html_nodes(row, 'td a') %>% html_attr('href')
  player_identifier <- str_extract(player_href, "(?<=players/)[^/]+")
  
  if (is.null(player_identifier)) {
    return(player_data_frames)
  }
  
  player_name_clean <- gsub("[^[:alnum:]]", "_", player_identifier)
  df_name <- paste0(player_name_clean, "_2024_Pitching_Stats")
  
  basic_stats_values <- as.numeric(basic_data[5:20])
  
  if (is.na(basic_stats_values[1]) || basic_stats_values[1] == 0) {
    return(player_data_frames)
  }
  
  stat_names <- c("APP", "GS", "W", "L", "SV", "CG", "IP", "H", "R", "ER", "BB", "K", "K/9", "HR", "ERA", "WHIP")
  basic_stats_df <- data.frame(Stat = stat_names, Value = basic_stats_values)
  
  player_data <- list()
  player_data$basic <- basic_stats_df
  
  player_data_frames[[df_name]] <- player_data
  
  return(player_data_frames)
}

Lake_Erie_Crushers_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/lakeeriecrushers?view=lineup"
Lake_Erie_Crushers_Pitchers_Stats_List_2024 <- create_pitcher_stats_data_frames_extended(Lake_Erie_Crushers_Url_2024)

Windy_City_Thunderbolts_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/windycitythunderbolts?view=lineup"
Windy_City_Thunderbolts_Pitchers_Stats_List_2024 <- create_pitcher_stats_data_frames_extended(Windy_City_Thunderbolts_Url_2024)

New_England_Knockouts_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/newenglandknockouts?view=lineup"
New_England_Knockouts_Stats_List_2024 <- create_pitcher_stats_data_frames_extended(New_England_Knockouts_Url_2024)

Joliet_Slammers_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/jolietslammers?view=lineup"
Joliet_Slammers_Pitchers_Stats_List_2024 <- create_pitcher_stats_data_frames_extended(Joliet_Slammers_Url_2024)

Washington_Wild_Things_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/washingtonwildthings?view=lineup"
Washington_Wild_Things_Pitchers_Stats_List_2024 <- create_pitcher_stats_data_frames_extended(Washington_Wild_Things_Url_2024)

Quebec_Capitales_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/quebeccapitales?view=lineup"
Quebec_Capitales_Pitchers_Stats_List_2024 <- create_pitcher_stats_data_frames_extended(Quebec_Capitales_Url_2024)

Ottawa_Titans_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/ottawatitans?view=lineup"
Ottawa_Titans_Pitchers_Stats_List_2024 <- create_pitcher_stats_data_frames_extended(Ottawa_Titans_Url_2024)

Evansville_Otters_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/evansvilleotters?view=lineup"
Evansville_Otters_Pitchers_Stats_List_2024 <- create_pitcher_stats_data_frames_extended(Evansville_Otters_Url_2024)

Florence_Yalls_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/florenceyalls?view=lineup"
Florence_Yalls_Pitchers_Stats_List_2024 <- create_pitcher_stats_data_frames_extended(Florence_Yalls_Url_2024)

Sussex_County_Miners_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/sussexcountyminers?view=lineup"
Sussex_County_Miners_Pitchers_Stats_List_2024 <- create_pitcher_stats_data_frames_extended(Sussex_County_Miners_Url_2024)

New_York_Boulders_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/newyorkboulders?view=lineup"
New_York_Boulders_Pitchers_Stats_List_2024 <- create_pitcher_stats_data_frames_extended(New_York_Boulders_Url_2024)

Trois_Rivieres_Aigles_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/troisrivieresaigles?view=lineup"
Trois_Rivieres_Aigles_Pitchers_Stats_List_2024 <- create_pitcher_stats_data_frames_extended(Trois_Rivieres_Aigles_Url_2024)

Schaumburg_Boomers_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/schaumburgboomers?view=lineup"
Schaumburg_Boomers_Pitchers_Stats_List_2024 <- create_pitcher_stats_data_frames_extended(Schaumburg_Boomers_Url_2024)

Tri_City_Valleycats_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/tricityvalleycats?view=lineup"
Tri_City_Valleycats_Pitchers_Stats_List_2024 <- create_pitcher_stats_data_frames_extended(Tri_City_Valleycats_Url_2024)

Gateway_Grizzlies_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/gatewaygrizzlies?view=lineup"
Gateway_Grizzlies_Pitchers_Stats_List_2024 <- create_pitcher_stats_data_frames_extended(Gateway_Grizzlies_Url_2024)

New_Jersey_Jackals_Url_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams/newjerseyjackals?view=lineup"
New_Jersey_Jackals_Pitchers_Stats_List_2024 <- create_pitcher_stats_data_frames_extended(New_Jersey_Jackals_Url_2024)


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

url_pitching_general_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams?sort=era&r=0&pos=p"

All_Teams_GP_2024 <- scrape_stat_data_pitching(url_pitching_general_2024, 3) %>% rename(GP = Stat)
All_Teams_Pitching_IP_2024 <- scrape_stat_data_pitching(url_pitching_general_2024, 4) %>% rename(IP = Stat)
All_Teams_Pitching_H_2024 <- scrape_stat_data_pitching(url_pitching_general_2024, 5) %>% rename(H = Stat)
All_Teams_Pitching_R_2024 <- scrape_stat_data_pitching(url_pitching_general_2024, 6) %>% rename(R = Stat)
All_Teams_Pitching_ER_2024 <- scrape_stat_data_pitching(url_pitching_general_2024, 7) %>% rename(ER = Stat)
All_Teams_Pitching_BB_2024 <- scrape_stat_data_pitching(url_pitching_general_2024, 8) %>% rename(BB = Stat)
All_Teams_Pitching_K_2024 <- scrape_stat_data_pitching(url_pitching_general_2024, 9) %>% rename(K = Stat)
All_Teams_Pitching_HR_2024 <- scrape_stat_data_pitching(url_pitching_general_2024, 11) %>% rename(HR = Stat)
All_Teams_Pitching_ERA_2024 <- scrape_stat_data_pitching(url_pitching_general_2024, 12,is_weighted = TRUE) %>% rename(ERA = Stat)
All_Teams_Pitching_WHIP_2024 <- scrape_stat_data_pitching(url_pitching_general_2024, 13,is_weighted = TRUE) %>% rename(WHIP = Stat)

###UPDATING PITCHERS HBP stats. MUST BE DONE MANUALLY
update_pitchers_hbp <- function(Pitchers_Stats_List, player_ids, hbps) {
  if(length(player_ids) != length(hbps)) {
    stop("The length of player_ids and hbps must be the same.")
  }
  
  player_ids_full <- paste(player_ids, "_2024_Pitching_Stats", sep = "")
  
  for(i in seq_along(player_ids_full)) {
    player_id_full <- player_ids_full[i]
    hbp_total <- hbps[i]
    
    if(player_id_full %in% names(Pitchers_Stats_List)) {
      pitcher_stats <- Pitchers_Stats_List[[player_id_full]]$basic
      if("HBP" %in% pitcher_stats$Stat) {
        hbp_row_index <- which(pitcher_stats$Stat == "HBP")
        pitcher_stats$Value[hbp_row_index] <- hbp_total
      } else {
        new_row <- data.frame(Stat = "HBP", Value = hbp_total, stringsAsFactors = FALSE)
        pitcher_stats <- rbind(pitcher_stats, new_row)
      }
      
      Pitchers_Stats_List[[player_id_full]]$basic <- pitcher_stats
    } else {
      cat(sprintf("Player ID '%s' not found in Pitchers_Stats_List.\n", player_id_full))
    }
  }
  
  return(Pitchers_Stats_List)
}

Lake_Erie_Crushers_player_ids_2024 <- c("jackeisenbargerw3w5", "anthonyescobaropje", "mattmulhearn7rlh","leonardorodriguezle7m","kennypierson4mn3","braydenbonnerheyv","christianscafidii0z1","calcarverb8tt","enriqueozoa8dq9","sammytavarezmzu9","trevorkunclguj2","mattvalin73i5","darrienragins3hga","garrettcoe3j3t","pedroechemendia4fn0")
Lake_Erie_Crushers_hbps_2024 <- c(7, 9, 6,6,2,0,2,0,0,10,1,3,6,2,4)
Lake_Erie_Crushers_Pitchers_Stats_List_2024 <- update_pitchers_hbp(Lake_Erie_Crushers_Pitchers_Stats_List_2024, Lake_Erie_Crushers_player_ids_2024, Lake_Erie_Crushers_hbps_2024)

New_England_Knockouts_player_ids_2024 <- c("mattcronincb4s", "liamosullivan2joc", "benseilerx1t3","heisellbarojwys","richardbrito9u2d","caseybargo7ho1","bradvanasdlen04t7","nicdaniels18z2","reevesmartinhdv5","dangoggin0xbh","lukewilliamsonmeh9","jamesmarinangynf","brendanbellijcy","dallasvaughn9r87","courtneymacksfn5","andersoncomase7ei")
New_England_Knockouts_hbps_2024 <- c(5, 4, 1,2,4,6,8,5,1,1,2,1,0,0,0,2)
New_England_Knockouts_Pitchers_Stats_List_2024 <- update_pitchers_hbp(New_England_Knockouts_Stats_List_2024, New_England_Knockouts_player_ids_2024, New_England_Knockouts_hbps_2024)

Evansville_Otters_player_ids_2024 <- c("zachsmithpvs7", "tylerrasfdjo", "parkerbrahmss6wy","bradenscott2k8l","jameskrickmajo","johancastillozmgh","tylerdriverk3s3","leonidelacruzozh5","tristanharvinouud","grifhughesutoz","michaelmcavene7rn0","patminerz89j","jonbeymerrna5","caseydelgadoeutp","pavinparksh8s5","jacobmeyerfq7o","terancemarin850e","chipkorbachermx16","ryanwiltsebm7q")
Evansville_Otters_hbps_2024 <- c(4, 2, 9,7,0,0,5,5,4,3,3,1,5,2,0,0,3,3,0)
Evansville_Otters_Pitchers_Stats_List_2024 <- update_pitchers_hbp(Evansville_Otters_Pitchers_Stats_List_2024, Evansville_Otters_player_ids_2024, Evansville_Otters_hbps_2024)

Florence_Yalls_player_ids_2024 <- c("ryanwatsonku0w", "blakeloubierkmng", "jonaikervillalobosims9", "gunnargroens6j2", "joshhudginsqmub", "joekemlage9jij", "rossthompsonwtsi", "shanegustafson272d", "kentklyman4eok", "ryanstecklinedlqc", "benterwilliger8xo0", "darrienwilliamse3ga","gagebihmcedq","edgarmartineztplx","logancampbelln07x","ayurakobayashiliit","jeremiahburks5xfl","alexwagnertgdn","reedsmithkdov","randyabshierewy8","dantechiricommdj","carterpoiryi7yv","cameronpferrer38r3","tygoodapvi","evanwebster64hd")
Florence_Yalls_hbps_2024 <- c(2, 2, 2,1,2,3,2,1,6,1,1,2,1,5,0,2,0,1,3,2,1,8,0,1,0)
Florence_Yalls_Pitchers_Stats_List_2024 <- update_pitchers_hbp(Florence_Yalls_Pitchers_Stats_List_2024, Florence_Yalls_player_ids_2024, Florence_Yalls_hbps_2024)

Gateway_Grizzlies_player_ids_2024 <- c("deylenmileycrj4", "rynemooreo7ul", "collinsullivanr8i7", "matthickey2m11", "osvaldoberriosovxq", "joelcondreay5ll0", "alverydelossantosfwg0", "parkerjohnsonhhh1", "alecwhaleyw9vn", "lukasveinbergs54s3", "nategarkowyxs7","nathanialtatek3rg","tylercornettlikj","teagueconradgo32","gabeholt42an","colebrannenmlz6","jaredhenry5q7g","markvierling1zs5","gagevailesdmz4","tatewargoz7zn","clarkedavenportb5gj","kalebhillvm9s","leonidelacruzenzc","keegancollettiq5e","justingoossenbrown6p1s")
Gateway_Grizzlies_hbps_2024 <- c(6, 5, 5,7,0,1,4,0,4,10,0,3,7,7,0,0,0,0,2,1,1,2,7,0,11)
Gateway_Grizzlies_Pitchers_Stats_List_2024 <- update_pitchers_hbp(Gateway_Grizzlies_Pitchers_Stats_List_2024, Gateway_Grizzlies_player_ids_2024, Gateway_Grizzlies_hbps_2024)

Joliet_Slammers_player_ids_2024 <- c("zacwestcott2xx7", "johnlundgrengecv", "rickycastroas30", "dwaynemarshall050c", "jakearmstrongkwwk", "iandellemanpf99", "zachgracezpov", "davidharrisonmq8r", "nickbautistafra0", "andrewdietzkneb", "caldjuraskovicosh5", "nicoodonnelloq8s","cameronsmith2mxp","chasegockelvk3e","giovannigarbellae53g","frankplesacptlc","colewesneskimnak","brettsanchezudli","genoencina3ywg","kevinfeevc1p","jacobmorinl41y","greysonlindermanl1p0","mattwalkerjg7o","landonsmiddyhkmu")
Joliet_Slammers_hbps_2024 <- c(14, 2, 0,3,8,1,7,6,4,0,0,1,1,1,0,1,0,9,8,1,1,1,0,2)
Joliet_Slammers_Pitchers_Stats_List_2024 <- update_pitchers_hbp(Joliet_Slammers_Pitchers_Stats_List_2024, Joliet_Slammers_player_ids_2024, Joliet_Slammers_hbps_2024)

New_Jersey_Jackals_player_ids_2024 <- c("yuhisakobtwt", "michaelgutierrez1puw", "ausitnparsley911c", "joetesta0ji1", "jalontysonlong3umc", "dazoncole4do3", "mikedesanti5mq3", "chrismormilekeeh", "ronielraudesf216", "dannysullivan63d9", "ryanvelazqueztf7e","jakekuchmaner945a","edgarrodriguezofyb","wilfredovalenzuelaloyt","ianconcevitchnjym","franknigroho3i","nickhohensteinafr3","andrewkramerbng6","nilorijo9po8","dylansabiaxndi","joeacostaacj7","alexbarkerqz31","jesusrosarioz6ac","reinaldodepaulal3ew")
New_Jersey_Jackals_hbps_2024 <- c(3, 2, 2,8,1,2,1,4,0,2,2,2,6,1,1,0,0,1,0,0,1,0,0,0)
New_Jersey_Jackals_Pitchers_Stats_List_2024 <- update_pitchers_hbp(New_Jersey_Jackals_Pitchers_Stats_List_2024,New_Jersey_Jackals_player_ids_2024,New_Jersey_Jackals_hbps_2024)

New_York_Boulders_player_ids_2024 <- c("aarondona9wir", "brandonbackmanfzzy", "garrettcooperc5xz", "jacksoncunninghamxwg9", "blainetraxelpzka", "mitchellsengerwlfd", "coltoneasterwood7r40", "dylansmith7bub", "peterallegro52a2", "erikstockyy05","jonahjenkinsph6p","bretterwinxkfk","tommyhughess640")
New_York_Boulders_hbps_2024 <- c(1, 3, 1,1,2,3,3,3,0,3,1,0,2)
New_York_Boulders_Pitchers_Stats_List_2024 <- update_pitchers_hbp(New_York_Boulders_Pitchers_Stats_List_2024,New_York_Boulders_player_ids_2024,New_York_Boulders_hbps_2024)

Ottawa_Titans_player_ids_2024 <- c("grantlarsonvmtl", "tylerjandronc26s", "shanegrayfnam", "mattdallasvju5", "erasmopinaleshy5b", "mattvoelkerrvz2", "mclainharris373a", "kylewhitec5mp", "brelynjonesdo5j", "scottprinseoby", "bryanpenaugm7","jakesanfordvbjt","brookswaltonfod2")
Ottawa_Titans_hbps_2024 <- c(2, 3, 1,1,1,0,0,2,0,1,3,0,1)
Ottawa_Titans_Pitchers_Stats_List_2024 <- update_pitchers_hbp(Ottawa_Titans_Pitchers_Stats_List_2024, Ottawa_Titans_player_ids_2024,Ottawa_Titans_hbps_2024)

Quebec_Capitales_player_ids_2024 <- c("samryann7zl", "jamesbradwell45qx", "yusneilpadronhry3", "ryansandberg3w6m", "harleygollertv02f", "frankmoscatiello94o6", "ryokohigashiz66w", "brandonmarklundcbgs", "austinmarozas2rqt", "franklinparrax661","abdielsaldanafm1n","thibaultmercadiermk6b","liamdoolan9vc9","jacksonsigmanxrkv","emileboiesurnv")
Quebec_Capitales_hbps_2024 <- c(4, 1, 0,1,0,0,1,1,2,0,0,1,1,0,0)
Quebec_Capitales_Pitchers_Stats_List_2024 <- update_pitchers_hbp(Quebec_Capitales_Pitchers_Stats_List_2024,Quebec_Capitales_player_ids_2024,Quebec_Capitales_hbps_2024)

Schaumburg_Boomers_player_ids_2024 <- c("jacksonhickert2y3q", "johnwilson2026", "colecookraxg", "aaronglicksteinqpdo", "brendanknollmtkv", "cristianlopez880p", "jacobsmithmxon", "matthelwigi3xs", "dallaswoolfolk2f39", "jakejoycemu3v", "mitchwhiteboc4", "dylanstutsmanqvon","cameronzunkelffow","petertveitephpf","colestallingsphfy","hambletonolivermne6","nolanpender03q9","tyleryotkewich2c2i","daiveyonwhittleyml5","danielparet69l5","antoniofrias9rrf")
Schaumburg_Boomers_hbps_2024 <- c(8, 2, 5,6,5,6,5,2,4,1,4,2,1,1,2,1,0,0,0,0,2)
Schaumburg_Boomers_Pitchers_Stats_List_2024 <- update_pitchers_hbp(Schaumburg_Boomers_Pitchers_Stats_List_2024,Schaumburg_Boomers_player_ids_2024,Schaumburg_Boomers_hbps_2024)

Sussex_County_Miners_player_ids_2024 <- c("jimmyboycedtc3", "tylerthorntonprq2", "mikereagan1i41", "markmoclair24fp", "estonstullx8p0", "bobbycurry4jqs", "billyparsonsmxtw", "mattstilsze0", "joseledesmajr4y7u", "tylerlunekef98b", "noahrobinsonpo7n", "ronnievoacolop9qs","angelcespedesii70","robbiehittok63","kellenbrothersjgb5","charlieneuweilerxdib")
Sussex_County_Miners_hbps_2024 <- c(3, 3, 3,2,1,2,2,1,6,0,0,0,1,0,3,0)
Sussex_County_Miners_Pitchers_Stats_List_2024 <- update_pitchers_hbp(Sussex_County_Miners_Pitchers_Stats_List_2024,Sussex_County_Miners_player_ids_2024,Sussex_County_Miners_hbps_2024)

Tri_City_Valleycats_player_ids_2024 <- c("wesalbertmgm6", "jhonvargasjt8x", "rolandoherediabustoszwft", "danbeebee9wm", "eastonklein2yze", "chascywinufyx", "nathanmedrano5riy", "arlomarynczakr6si", "ginosabatinerfc7", "gregblackmano4m2","austindillxhn6","zekewoodzjln","josefelixckdc")
Tri_City_Valleycats_hbps_2024 <- c(3, 4, 3,2,1,1,1,0,0,1,2,2,2)
Tri_City_Valleycats_Pitchers_Stats_List_2024 <- update_pitchers_hbp(Tri_City_Valleycats_Pitchers_Stats_List_2024,Tri_City_Valleycats_player_ids_2024,Tri_City_Valleycats_hbps_2024)

Trois_Rivieres_Aigles_player_ids_2024 <- c("osmangutierrezf5tg", "tuckersmith3ftf", "didiervargaspkyv", "michellmilianox7ej", "sampoliquinzs3d", "hernandomejiaagdl", "braedenallemannjrhg", "harryrutkowskig70k", "shawnatamanchukspp3","loganhofmannw7oz","jesentherrian5z72","brendondadsonubrx","koseinaitobpqq","eddysavoiensv3","carterraffield7k4q")
Trois_Rivieres_Aigles_hbps_2024 <- c(1, 1, 3,2,0,1,2,0,1,1,2,1,2,0,0)
Trois_Rivieres_Aigles_Pitchers_Stats_List_2024 <- update_pitchers_hbp(Trois_Rivieres_Aigles_Pitchers_Stats_List_2024,Trois_Rivieres_Aigles_player_ids_2024,Trois_Rivieres_Aigles_hbps_2024)

Washington_Wild_Things_player_ids_2024 <- c("jordandivaleriog66u", "malikbarringtonzago", "kobefosterghlb", "zachkirby7hci", "justingoossenbrown4dxa", "gyeongjukimkzr5", "nickmacdonaldwl7a", "alexcarrillokgfj", "darielfregioqu6f", "frankiegiuliano883x", "hunterstevenshxsq", "lukasyoungxbgf","christianjamesoe14","liampulsipher2zdc","ryanmunoz7ex5","aaronforrestiofz","brendannaili9zw","marlonperezqg7s")
Washington_Wild_Things_hbps_2024 <- c(4, 7, 11,4,5,0,2,6,5,6,1,0,6,0,0,0,5,0)
Washington_Wild_Things_Pitchers_Stats_List_2024 <- update_pitchers_hbp(Washington_Wild_Things_Pitchers_Stats_List_2024,Washington_Wild_Things_player_ids_2024,Washington_Wild_Things_hbps_2024)

Windy_City_Thunderbolts_player_ids_2024 <- c("kadelancourvz1x","mattswilleypess","willarmbruesterluby", "kevinquotbuddiequotpindel4ve0", "johnmikolaicyk92nj", "jackmahoneyz2z3", "tylernaumannykc3", "michaelbarkerfgag", "derrickedingtonl7ah", "erichildebrandjkhc", "gregduncandmzv", "justinmillerspy9", "tylerlaporte7o8s", "taylorsugga5cg","coltonhunttsq93","hunterdupuy1u5e","ruddygomezlyc0","jacobnewman4r1n","christiankuzemkara6m")
Windy_City_Thunderbolts_hbps_2024 <- c(0,1,3, 1, 2,7,2,3,0,0,0,0,1,4,0,0,0,0,0)
Windy_City_Thunderbolts_Pitchers_Stats_List_2024 <- update_pitchers_hbp(Windy_City_Thunderbolts_Pitchers_Stats_List_2024,Windy_City_Thunderbolts_player_ids_2024,Windy_City_Thunderbolts_hbps_2024)

### FIP Calculation 

calculate_frontier_league_era <- function(df_er, df_ip) {
  total_er <- df_er[df_er$Team == "Frontier League Total",]$ER
  
  total_ip <- df_ip[df_ip$Team == "Frontier League Total",]$IP
  
  if (total_ip == 0) {
    return(NA) 
  }
  era <- (total_er / total_ip) * 9
  return(era)
}

frontier_league_era_2024 <- calculate_frontier_league_era(All_Teams_Pitching_ER_2024, All_Teams_Pitching_IP_2024)

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

url_extended_2024 <- "https://www.frontierleague.com/sports/bsb/2023-24/teams?sort=pa&r=0&pos=eh"

All_Teams_Hitting_HBP_2024 <- scrape_stat_data_extended(url_extended_2024, 4) %>% rename(HBP = Stat)

calculate_frontier_league_constant <- function() {
  lgHR = All_Teams_Pitching_HR_2024[All_Teams_Pitching_HR_2024$Team == "Frontier League Total", "HR"]
  lgBB = All_Teams_Pitching_BB_2024[All_Teams_Pitching_BB_2024$Team == "Frontier League Total", "BB"]
  lgHBP = All_Teams_Hitting_HBP_2024[All_Teams_Hitting_HBP_2024$Team == "Frontier League Total", "HBP"]
  lgK = All_Teams_Pitching_K_2024[All_Teams_Pitching_K_2024$Team == "Frontier League Total", "K"]
  lgIP = All_Teams_Pitching_IP_2024[All_Teams_Pitching_IP_2024$Team == "Frontier League Total", "IP"]
  
  convert_IP_to_thirds <- function(ip) {
    whole <- floor(ip)
    fraction <- ip - whole
    thirds <- ifelse(fraction == 0.1, 1/3, ifelse(fraction == 0.2, 2/3, 0))
    return(whole + thirds)
  }
  
  lgIP_adjusted <- sapply(lgIP, convert_IP_to_thirds)
  
  lgERA = (All_Teams_Pitching_ER_2024[All_Teams_Pitching_ER_2024$Team == "Frontier League Total", "ER"] /
             lgIP_adjusted) * 9
  
  FIP_constant <- lgERA - (((13*lgHR)+(3*(lgBB+lgHBP))-(2*lgK))/lgIP_adjusted)
  return(FIP_constant)
}

frontier_league_constant_2024 <- calculate_frontier_league_constant()

calculate_and_store_FIP <- function(pitchers_list, FIP_constant) {
  for (player_stat_name in names(pitchers_list)) {
    player_data <- pitchers_list[[player_stat_name]]$basic
    
    necessary_stats <- c("HR", "BB", "HBP", "K", "IP")
    stats <- setNames(vector("numeric", length(necessary_stats)), necessary_stats)
    
    for (stat in necessary_stats) {
      if (stat %in% player_data$Stat) {
        stat_value <- player_data$Value[player_data$Stat == stat]
        if(stat == "IP") {
          stat_value <- sapply(stat_value, function(x) {
            main_innings <- floor(x)
            decimal_part <- x - main_innings
            if (decimal_part > 0) {
              thirds = round(decimal_part * 10) * 1/3
              return(main_innings + thirds)
            } else {
              return(x)
            }
          })
        }
        stats[stat] <- ifelse(is.na(stat_value), 0, sum(as.numeric(stat_value), na.rm = TRUE))
      }
    }
    
    if (stats["IP"] > 0) {
      FIP_value <- ((13 * stats["HR"]) + (3 * (stats["BB"] + stats["HBP"])) - (2 * stats["K"])) / stats["IP"] + FIP_constant
    } else {
      FIP_value <- NA
    }
    
    player_data <- rbind(player_data, data.frame(Stat = "FIP", Value = FIP_value))
    pitchers_list[[player_stat_name]]$basic <- player_data
  }
  
  return(pitchers_list)
}

Lake_Erie_Crushers_Pitchers_Stats_List_2024 <- calculate_and_store_FIP(Lake_Erie_Crushers_Pitchers_Stats_List_2024, frontier_league_constant_2024)
Windy_City_Thunderbolts_Pitchers_Stats_List_2024 <- calculate_and_store_FIP(Windy_City_Thunderbolts_Pitchers_Stats_List_2024, frontier_league_constant_2024)
New_England_Knockouts_Pitchers_Stats_List_2024 <- calculate_and_store_FIP(New_England_Knockouts_Pitchers_Stats_List_2024, frontier_league_constant_2024)
Joliet_Slammers_Pitchers_Stats_List_2024 <- calculate_and_store_FIP(Joliet_Slammers_Pitchers_Stats_List_2024, frontier_league_constant_2024)
Washington_Wild_Things_Pitchers_Stats_List_2024 <- calculate_and_store_FIP(Washington_Wild_Things_Pitchers_Stats_List_2024, frontier_league_constant_2024)
Quebec_Capitales_Pitchers_Stats_List_2024 <- calculate_and_store_FIP(Quebec_Capitales_Pitchers_Stats_List_2024, frontier_league_constant_2024)
Ottawa_Titans_Pitchers_Stats_List_2024 <- calculate_and_store_FIP(Ottawa_Titans_Pitchers_Stats_List_2024, frontier_league_constant_2024)
Evansville_Otters_Pitchers_Stats_List_2024 <- calculate_and_store_FIP(Evansville_Otters_Pitchers_Stats_List_2024, frontier_league_constant_2024)
Florence_Yalls_Pitchers_Stats_List_2024 <- calculate_and_store_FIP(Florence_Yalls_Pitchers_Stats_List_2024, frontier_league_constant_2024)
Sussex_County_Miners_Pitchers_Stats_List_2024 <- calculate_and_store_FIP(Sussex_County_Miners_Pitchers_Stats_List_2024, frontier_league_constant_2024)
New_York_Boulders_Pitchers_Stats_List_2024 <- calculate_and_store_FIP(New_York_Boulders_Pitchers_Stats_List_2024, frontier_league_constant_2024)
Trois_Rivieres_Aigles_Pitchers_Stats_List_2024 <- calculate_and_store_FIP(Trois_Rivieres_Aigles_Pitchers_Stats_List_2024, frontier_league_constant_2024)
Schaumburg_Boomers_Pitchers_Stats_List_2024 <- calculate_and_store_FIP(Schaumburg_Boomers_Pitchers_Stats_List_2024, frontier_league_constant_2024)
Tri_City_Valleycats_Pitchers_Stats_List_2024 <- calculate_and_store_FIP(Tri_City_Valleycats_Pitchers_Stats_List_2024, frontier_league_constant_2024)
Gateway_Grizzlies_Pitchers_Stats_List_2024 <- calculate_and_store_FIP(Gateway_Grizzlies_Pitchers_Stats_List_2024, frontier_league_constant_2024)
New_Jersey_Jackals_Pitchers_Stats_List_2024 <- calculate_and_store_FIP(New_Jersey_Jackals_Pitchers_Stats_List_2024, frontier_league_constant_2024)

### ERA- calculation 

calculate_frontier_league_west_division_era <- function(df_er, df_ip) {
  West_er <- df_er[df_er$Team == "Frontier League West",]$ER
  
  West_ip <- df_ip[df_ip$Team == "Frontier League West",]$IP
  
  if (West_ip == 0) {
    return(NA) 
  }
  era <- (West_er / West_ip) * 9
  return(era)
}

frontier_league_west_era_2024 <- calculate_frontier_league_west_division_era(All_Teams_Pitching_ER_2024, All_Teams_Pitching_IP_2024)

calculate_frontier_league_east_division_era <- function(df_er, df_ip) {
  East_er <- df_er[df_er$Team == "Frontier League East",]$ER
  
  East_ip <- df_ip[df_ip$Team == "Frontier League East",]$IP
  
  if (East_ip == 0) {
    return(NA) 
  }
  era <- (East_er / East_ip) * 9
  return(era)
}

frontier_league_east_era_2024 <- calculate_frontier_league_east_division_era(All_Teams_Pitching_ER_2024, All_Teams_Pitching_IP_2024)

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

new_england_knockouts_ballpark_factor <- 1.00

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


east_teams <- c("New Jersey Jackals", "Ottawa Titans", "Trois-Rivières Aigles", 
                "New York Boulders", "Tri-City ValleyCats", "New England Knockouts", 
                "Sussex County Miners", "Québec Capitales")

calculate_era_minus_for_team <- function(team_pitchers_stats, league_ERA, team_BPF) {
  team_pitchers_stats_with_era_minus <- lapply(team_pitchers_stats, function(pitcher_stats) {
    team_BPF <- 1
    if ("ERA" %in% pitcher_stats$basic$Stat) {
      pitcher_ERA <- as.numeric(pitcher_stats$basic[pitcher_stats$basic$Stat == "ERA", "Value"])
      pitcher_ERA_minus <- ((pitcher_ERA + (pitcher_ERA - (pitcher_ERA * team_BPF))) / league_ERA) * 100
      pitcher_stats$basic <- rbind(pitcher_stats$basic, data.frame(Stat = "ERA-", Value = pitcher_ERA_minus))
    }
    return(pitcher_stats)
  })
  
  return(team_pitchers_stats_with_era_minus)
}

Schaumburg_Boomers_Pitchers_Stats_List_2024 <- calculate_era_minus_for_team(
  Schaumburg_Boomers_Pitchers_Stats_List_2024,
  frontier_league_west_era_2024, 
  schaumburg_boomers_ballpark_factor
)

New_Jersey_Jackals_Pitchers_Stats_List_2024 <- calculate_era_minus_for_team(
  New_Jersey_Jackals_Pitchers_Stats_List_2024,
  frontier_league_east_era_2024,
  new_jersey_jackals_ballpark_factor
)

Ottawa_Titans_Pitchers_Stats_List_2024 <- calculate_era_minus_for_team(
  Ottawa_Titans_Pitchers_Stats_List_2024,
  frontier_league_east_era_2024,
  ottawa_titans_ballpark_factor
)

Trois_Rivieres_Aigles_Pitchers_Stats_List_2024 <- calculate_era_minus_for_team(
  Trois_Rivieres_Aigles_Pitchers_Stats_List_2024,
  frontier_league_east_era_2024,
  trois_rivieres_aigles_ballpark_factor
)

New_York_Boulders_Pitchers_Stats_List_2024 <- calculate_era_minus_for_team(
  New_York_Boulders_Pitchers_Stats_List_2024,
  frontier_league_east_era_2024,
  new_york_boulders_ballpark_factor
)

Tri_City_Valleycats_Pitchers_Stats_List_2024 <- calculate_era_minus_for_team(
  Tri_City_Valleycats_Pitchers_Stats_List_2024,
  frontier_league_east_era_2024,
  tri_city_valleycats_ballpark_factor
)

New_England_Knockouts_Pitchers_Stats_List_2024 <- calculate_era_minus_for_team(
  New_England_Knockouts_Pitchers_Stats_List_2024,
  frontier_league_east_era_2024,
  New_England_Knockouts_ballpark_factor
)

Sussex_County_Miners_Pitchers_Stats_List_2024 <- calculate_era_minus_for_team(
  Sussex_County_Miners_Pitchers_Stats_List_2024,
  frontier_league_east_era_2024,
  sussex_county_miners_ballpark_factor
)

Quebec_Capitales_Pitchers_Stats_List_2024 <- calculate_era_minus_for_team(
  Quebec_Capitales_Pitchers_Stats_List_2024,
  frontier_league_east_era_2024,
  quebec_capitales_ballpark_factor
)

Lake_Erie_Crushers_Pitchers_Stats_List_2024 <- calculate_era_minus_for_team(
  Lake_Erie_Crushers_Pitchers_Stats_List_2024,
  frontier_league_west_era_2024,
  lake_erie_crushers_ballpark_factor
)

Windy_City_Thunderbolts_Pitchers_Stats_List_2024 <- calculate_era_minus_for_team(
  Windy_City_Thunderbolts_Pitchers_Stats_List_2024,
  frontier_league_west_era_2024,
  windy_city_thunderbolts_ballpark_factor
)

Gateway_Grizzlies_Pitchers_Stats_List_2024 <- calculate_era_minus_for_team(
  Gateway_Grizzlies_Pitchers_Stats_List_2024,
  frontier_league_west_era_2024,
  gateway_grizzlies_ballpark_factor
)

Joliet_Slammers_Pitchers_Stats_List_2024 <- calculate_era_minus_for_team(
  Joliet_Slammers_Pitchers_Stats_List_2024,
  frontier_league_west_era_2024,
  joliet_slammers_ballpark_factor
)

Evansville_Otters_Pitchers_Stats_List_2024 <- calculate_era_minus_for_team(
  Evansville_Otters_Pitchers_Stats_List_2024,
  frontier_league_west_era_2024,
  evansville_otters_ballpark_factor
)

Florence_Yalls_Pitchers_Stats_List_2024 <- calculate_era_minus_for_team(
  Florence_Yalls_Pitchers_Stats_List_2024,
  frontier_league_west_era_2024,
  florence_yalls_ballpark_factor
)

Washington_Wild_Things_Pitchers_Stats_List_2024 <- calculate_era_minus_for_team(
  Washington_Wild_Things_Pitchers_Stats_List_2024,
  frontier_league_west_era_2024,
  washington_wild_things_ballpark_factor
)


### Putting these stats into the SQL Database:
library(RSQLite)

process_pitching_stats <- function(team_data, team_name, conn) {
  table_name <- paste(team_name, "2024_Pitching_Stats", sep="_")
  sql_drop_table <- paste0("DROP TABLE IF EXISTS `", table_name, "`")
  dbExecute(conn, sql_drop_table)
  
  first_pitcher_data <- team_data[[1]]$basic
  all_stats <- unique(first_pitcher_data$Stat)
  
  if (!"HBP" %in% all_stats) {
    all_stats <- c(all_stats, "HBP")
  }
  
  stat_columns <- paste0('`', all_stats, '` REAL', collapse = ', ')
  sql_create_table <- paste0("CREATE TABLE `", table_name, "` (`Player` TEXT, ", stat_columns, ")")
  dbExecute(conn, sql_create_table)
  
  format_for_sql <- function(pitcher_data, pitcher_name) {
    stats_df <- setNames(data.frame(matrix(ncol = length(all_stats), nrow = 1, data = NA)), all_stats)
    stats_list <- pitcher_data$basic
    
    for (i in seq_along(stats_list$Stat)) {
      stat_name <- stats_list$Stat[i]
      stats_df[1, stat_name] <- stats_list$Value[i]
    }
    
    if (is.na(stats_df$HBP)) {
      stats_df$HBP <- 0
    }
    
    stats_df$Player <- pitcher_name
    return(stats_df)
  }
  
  for (pitcher_name in names(team_data)) {
    pitcher_basic_data <- team_data[[pitcher_name]]
    formatted_data <- format_for_sql(pitcher_basic_data, pitcher_name)
    dbWriteTable(conn, table_name, formatted_data, append = TRUE, row.names = FALSE)
  }
}

conn <- dbConnect(RSQLite::SQLite(), dbname = "/Users/nathanielascher/Downloads/Frontier League 2024 Season.db")

teams_pitchers_data_2024 <- list(
  Schaumburg_Boomers_Pitchers_Stats_List_2024,
  New_Jersey_Jackals_Pitchers_Stats_List_2024,
  Ottawa_Titans_Pitchers_Stats_List_2024,
  Trois_Rivieres_Aigles_Pitchers_Stats_List_2024,
  New_York_Boulders_Pitchers_Stats_List_2024,
  Tri_City_Valleycats_Pitchers_Stats_List_2024,
  New_England_Knockouts_Pitchers_Stats_List_2024,
  Sussex_County_Miners_Pitchers_Stats_List_2024,
  Quebec_Capitales_Pitchers_Stats_List_2024,
  Lake_Erie_Crushers_Pitchers_Stats_List_2024,
  Windy_City_Thunderbolts_Pitchers_Stats_List_2024,
  Gateway_Grizzlies_Pitchers_Stats_List_2024,
  Joliet_Slammers_Pitchers_Stats_List_2024,
  Evansville_Otters_Pitchers_Stats_List_2024,
  Florence_Yalls_Pitchers_Stats_List_2024,
  Washington_Wild_Things_Pitchers_Stats_List_2024
)

team_names_2024 <- c(
  "Schaumburg_Boomers",
  "New_Jersey_Jackals",
  "Ottawa_Titans",
  "Trois_Rivieres_Aigles",
  "New_York_Boulders",
  "Tri_City_Valleycats",
  "New England Knockouts",
  "Sussex_County_Miners",
  "Quebec_Capitales",
  "Lake_Erie_Crushers",
  "Windy_City_Thunderbolts",
  "Gateway_Grizzlies",
  "Joliet_Slammers",
  "Evansville_Otters",
  "Florence_Yalls",
  "Washington_Wild_Things"
)

for (i in seq_along(teams_pitchers_data_2024)) {
  process_pitching_stats(teams_pitchers_data_2024[[i]], team_names_2024[i], conn)
}

dbDisconnect(conn)

###All Teams' Pitchers

library(RSQLite)

aggregate_all_pitchers_stats <- function(teams_pitchers_data, conn) {
  all_stats <- unique(unlist(lapply(teams_pitchers_data, function(team_data) unlist(lapply(team_data, function(pitcher_data) pitcher_data$basic$Stat)))))
  stat_columns <- paste0('`', all_stats, '` REAL', collapse = ', ')
  dbExecute(conn, "DROP TABLE IF EXISTS `All_Teams_Pitchers_2024_Pitching_Stats`")
  dbExecute(conn, paste0("CREATE TABLE `All_Teams_Pitchers_2024_Pitching_Stats` (`Player` TEXT, ", stat_columns, ")"))
  
  format_for_sql <- function(pitcher_data, pitcher_name) {
    stats_df <- setNames(data.frame(matrix(ncol = length(all_stats), nrow = 1, data = NA)), all_stats)
    stats_list <- pitcher_data$basic
    for (i in seq_along(stats_list$Stat)) {
      stats_df[1, stats_list$Stat[i]] <- stats_list$Value[i]
    }
    stats_df$Player <- pitcher_name
    return(stats_df)
  }
  
  for (team_data in teams_pitchers_data) {
    for (pitcher_name in names(team_data)) {
      pitcher_basic_data <- team_data[[pitcher_name]]
      formatted_data <- format_for_sql(pitcher_basic_data, pitcher_name)
      dbWriteTable(conn, "All_Teams_Pitchers_2024_Pitching_Stats", formatted_data, append = TRUE, row.names = FALSE)
    }
  }
}

conn <- dbConnect(RSQLite::SQLite(), dbname = "/Users/nathanielascher/Downloads/Frontier League 2024 Season.db")

teams_pitchers_data_2024 <- list(
  Schaumburg_Boomers_Pitchers_Stats_List_2024,
  New_Jersey_Jackals_Pitchers_Stats_List_2024,
  Ottawa_Titans_Pitchers_Stats_List_2024,
  Trois_Rivieres_Aigles_Pitchers_Stats_List_2024,
  New_York_Boulders_Pitchers_Stats_List_2024,
  Tri_City_Valleycats_Pitchers_Stats_List_2024,
  New_England_Knockouts_Pitchers_Stats_List_2024,
  Sussex_County_Miners_Pitchers_Stats_List_2024,
  Quebec_Capitales_Pitchers_Stats_List_2024,
  Lake_Erie_Crushers_Pitchers_Stats_List_2024,
  Windy_City_Thunderbolts_Pitchers_Stats_List_2024,
  Gateway_Grizzlies_Pitchers_Stats_List_2024,
  Joliet_Slammers_Pitchers_Stats_List_2024,
  Evansville_Otters_Pitchers_Stats_List_2024,
  Florence_Yalls_Pitchers_Stats_List_2024,
  Washington_Wild_Things_Pitchers_Stats_List_2024
)

aggregate_all_pitchers_stats(teams_pitchers_data_2024, conn)

dbDisconnect(conn)

###East Teams' Pitchers

library(RSQLite)

aggregate_east_pitchers_stats <- function(teams_pitchers_data, conn) {
  all_stats <- unique(unlist(lapply(teams_pitchers_data, function(team_data) unlist(lapply(team_data, function(pitcher_data) pitcher_data$basic$Stat)))))
  stat_columns <- paste0('`', all_stats, '` REAL', collapse = ', ')
  dbExecute(conn, "DROP TABLE IF EXISTS `East_Teams_Pitchers_2024_Pitching_Stats`")
  dbExecute(conn, paste0("CREATE TABLE `East_Teams_Pitchers_2024_Pitching_Stats` (`Player` TEXT, ", stat_columns, ")"))
  
  format_for_sql <- function(pitcher_data, pitcher_name) {
    stats_df <- setNames(data.frame(matrix(ncol = length(all_stats), nrow = 1, data = NA)), all_stats)
    stats_list <- pitcher_data$basic
    for (i in seq_along(stats_list$Stat)) {
      stats_df[1, stats_list$Stat[i]] <- stats_list$Value[i]
    }
    stats_df$Player <- pitcher_name
    return(stats_df)
  }
  
  for (team_data in teams_pitchers_data) {
    for (pitcher_name in names(team_data)) {
      pitcher_basic_data <- team_data[[pitcher_name]]
      formatted_data <- format_for_sql(pitcher_basic_data, pitcher_name)
      dbWriteTable(conn, "East_Teams_Pitchers_2024_Pitching_Stats", formatted_data, append = TRUE, row.names = FALSE)
    }
  }
}

conn <- dbConnect(RSQLite::SQLite(), dbname = "/Users/nathanielascher/Downloads/Frontier League 2024 Season.db")

teams_pitchers_data_2024 <- list(
  New_Jersey_Jackals_Pitchers_Stats_List_2024,
  Ottawa_Titans_Pitchers_Stats_List_2024,
  Trois_Rivieres_Aigles_Pitchers_Stats_List_2024,
  New_York_Boulders_Pitchers_Stats_List_2024,
  Tri_City_Valleycats_Pitchers_Stats_List_2024,
  New_England_Knockouts_Pitchers_Stats_List_2024,
  Sussex_County_Miners_Pitchers_Stats_List_2024,
  Quebec_Capitales_Pitchers_Stats_List_2024)

aggregate_east_pitchers_stats(teams_pitchers_data_2024, conn)

dbDisconnect(conn)

###West Teams' Pitchers

library(RSQLite)

aggregate_west_pitchers_stats <- function(teams_pitchers_data, conn) {
  all_stats <- unique(unlist(lapply(teams_pitchers_data, function(team_data) unlist(lapply(team_data, function(pitcher_data) pitcher_data$basic$Stat)))))
  stat_columns <- paste0('`', all_stats, '` REAL', collapse = ', ')
  dbExecute(conn, "DROP TABLE IF EXISTS `West_Teams_Pitchers_2024_Pitching_Stats`")
  dbExecute(conn, paste0("CREATE TABLE `West_Teams_Pitchers_2024_Pitching_Stats` (`Player` TEXT, ", stat_columns, ")"))
  
  format_for_sql <- function(pitcher_data, pitcher_name) {
    stats_df <- setNames(data.frame(matrix(ncol = length(all_stats), nrow = 1, data = NA)), all_stats)
    stats_list <- pitcher_data$basic
    for (i in seq_along(stats_list$Stat)) {
      stats_df[1, stats_list$Stat[i]] <- stats_list$Value[i]
    }
    stats_df$Player <- pitcher_name
    return(stats_df)
  }
  
  for (team_data in teams_pitchers_data) {
    for (pitcher_name in names(team_data)) {
      pitcher_basic_data <- team_data[[pitcher_name]]
      formatted_data <- format_for_sql(pitcher_basic_data, pitcher_name)
      dbWriteTable(conn, "West_Teams_Pitchers_2024_Pitching_Stats", formatted_data, append = TRUE, row.names = FALSE)
    }
  }
}

conn <- dbConnect(RSQLite::SQLite(), dbname = "/Users/nathanielascher/Downloads/Frontier League 2024 Season.db")

teams_pitchers_data_2024 <- list(
  Schaumburg_Boomers_Pitchers_Stats_List_2024,
  Lake_Erie_Crushers_Pitchers_Stats_List_2024,
  Windy_City_Thunderbolts_Pitchers_Stats_List_2024,
  Gateway_Grizzlies_Pitchers_Stats_List_2024,
  Joliet_Slammers_Pitchers_Stats_List_2024,
  Evansville_Otters_Pitchers_Stats_List_2024,
  Florence_Yalls_Pitchers_Stats_List_2024,
  Washington_Wild_Things_Pitchers_Stats_List_2024)

aggregate_west_pitchers_stats(teams_pitchers_data_2024, conn)

dbDisconnect(conn)

##Pitcher Split Stats
library(dplyr)
library(RSQLite)

calculatePitchingStatsforBatterSide <- function(dbname, startDate, endDate, BB_HBP_Constant, Single_Constant, Double_Constant, Triple_Constant, HomeRun_Constant) {
  conn <- dbConnect(RSQLite::SQLite(), dbname = dbname)
  data <- dbReadTable(conn, "Lake Erie Crushers 2024")
  dbDisconnect(conn)
  
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  startDate <- as.Date(startDate, format = "%m/%d/%Y")
  endDate <- as.Date(endDate, format = "%m/%d/%Y")
  
  data_filtered <- data %>%
    filter(Date >= startDate, Date <= endDate, PitcherTeam == "Lake Erie Crushers", TaggedPitchType != "") %>%
    select(Pitcher, TaggedPitchType, BatterSide, PitchCall, PlayResult, KorBB, HitType)
  
  stats <- data_filtered %>%
    group_by(Pitcher, BatterSide) %>%
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

Crushers_Pitchers_Stats_Per_BatterSide_2024 <- calculatePitchingStatsforBatterSide("/Users/nathanielascher/Downloads/Frontier League 2024 Season.db", "5/09/2024", "9/03/2024", .717,.836,1.124,1.418,1.843)

calculatePitchingStatsforBatterSideandPitchType <- function(dbname, startDate, endDate, BB_HBP_Constant, Single_Constant, Double_Constant, Triple_Constant, HomeRun_Constant) {
  conn <- dbConnect(RSQLite::SQLite(), dbname = dbname)
  data <- dbReadTable(conn, "Lake Erie Crushers 2024")
  dbDisconnect(conn)
  
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  startDate <- as.Date(startDate, format = "%m/%d/%Y")
  endDate <- as.Date(endDate, format = "%m/%d/%Y")
  
  data_filtered <- data %>%
    filter(Date >= startDate, Date <= endDate, PitcherTeam == "Lake Erie Crushers", TaggedPitchType != "") %>%
    select(Pitcher, TaggedPitchType, BatterSide, PitchCall, PlayResult, KorBB, HitType)
  
  stats <- data_filtered %>%
    group_by(Pitcher, BatterSide,TaggedPitchType) %>%
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

Crushers_Pitchers_Stats_Per_BatterSide_PitchType_2024 <- calculatePitchingStatsforBatterSideandPitchType("/Users/nathanielascher/Downloads/Frontier League 2024 Season.db", "5/09/2024", "9/03/2024", .717,.836,1.124,1.418,1.843)

calculatePitchingStats <- function(dbname, startDate, endDate, BB_HBP_Constant, Single_Constant, Double_Constant, Triple_Constant, HomeRun_Constant) {
  conn <- dbConnect(RSQLite::SQLite(), dbname = dbname)
  data <- dbReadTable(conn, "Lake Erie Crushers 2024")
  dbDisconnect(conn)
  
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  startDate <- as.Date(startDate, format = "%m/%d/%Y")
  endDate <- as.Date(endDate, format = "%m/%d/%Y")
  
  data_filtered <- data %>%
    filter(Date >= startDate, Date <= endDate, PitcherTeam == "Lake Erie Crushers", TaggedPitchType != "") %>%
    select(Pitcher, TaggedPitchType, PitchCall, PlayResult, KorBB, HitType)
  
  stats <- data_filtered %>%
    group_by(Pitcher, TaggedPitchType) %>%
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

Crushers_Pitchers_Stats_Per_PitchType_2024 <- calculatePitchingStats("/Users/nathanielascher/Downloads/Frontier League 2024 Season.db", "5/09/2024", "9/03/2024", .717,.836,1.124,1.418,1.843)

calculatePitchingStatsperCount <- function(dbname, startDate, endDate, BB_HBP_Constant, Single_Constant, Double_Constant, Triple_Constant, HomeRun_Constant) {
  conn <- dbConnect(RSQLite::SQLite(), dbname = dbname)
  data <- dbReadTable(conn, "Lake Erie Crushers 2024")
  dbDisconnect(conn)
  
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  startDate <- as.Date(startDate, format = "%m/%d/%Y")
  endDate <- as.Date(endDate, format = "%m/%d/%Y")
  
  data_filtered <- data %>%
    filter(Date >= startDate, Date <= endDate, PitcherTeam == "Lake Erie Crushers", TaggedPitchType != "") %>%
    select(Pitcher, Balls, Strikes, TaggedPitchType, PitchCall, PlayResult, KorBB, HitType)
  
  stats <- data_filtered %>%
    group_by(Pitcher, Balls, Strikes) %>%
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

Crushers_Pitchers_Stats_Per_Count_2024 <- calculatePitchingStatsperCount("/Users/nathanielascher/Downloads/Frontier League 2024 Season.db", "5/09/2024", "9/03/2024", .717,.836,1.124,1.418,1.843)

calculatePitchingStatsperPitchTypeperCount <- function(dbname, startDate, endDate, BB_HBP_Constant, Single_Constant, Double_Constant, Triple_Constant, HomeRun_Constant) {
  conn <- dbConnect(RSQLite::SQLite(), dbname = dbname)
  data <- dbReadTable(conn, "Lake Erie Crushers 2024")
  dbDisconnect(conn)
  
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  startDate <- as.Date(startDate, format = "%m/%d/%Y")
  endDate <- as.Date(endDate, format = "%m/%d/%Y")
  
  data_filtered <- data %>%
    filter(Date >= startDate, Date <= endDate, PitcherTeam == "Lake Erie Crushers", TaggedPitchType != "") %>%
    select(Pitcher, Balls, Strikes, TaggedPitchType, PitchCall, PlayResult, KorBB, HitType)
  
  stats <- data_filtered %>%
    group_by(Pitcher, TaggedPitchType, Balls, Strikes) %>%
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

Crushers_Pitchers_Stats_Per_PitchType_Per_Count_2024 <- calculatePitchingStatsperPitchTypeperCount("/Users/nathanielascher/Downloads/Frontier League 2024 Season.db", "5/09/2024", "9/03/2024", .717,.836,1.124,1.418,1.843)

calculatePitchingStatsforSeventhorLater <- function(dbname, startDate, endDate, BB_HBP_Constant, Single_Constant, Double_Constant, Triple_Constant, HomeRun_Constant) {
  conn <- dbConnect(RSQLite::SQLite(), dbname = dbname)
  data <- dbReadTable(conn, "Lake Erie Crushers 2024")
  dbDisconnect(conn)
  
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  startDate <- as.Date(startDate, format = "%m/%d/%Y")
  endDate <- as.Date(endDate, format = "%m/%d/%Y")
  
  data_filtered <- data %>%
    filter(Date >= startDate, Date <= endDate, PitcherTeam == "Lake Erie Crushers", Inning > 6, TaggedPitchType != "") %>%
    select(Pitcher, Inning, TaggedPitchType, BatterSide, PitchCall, PlayResult, KorBB, HitType)
  
  stats <- data_filtered %>%
    group_by(Pitcher) %>%
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

Crushers_Pitchers_Stats_Seventh_or_Later_2024 <- calculatePitchingStatsforSeventhorLater("/Users/nathanielascher/Downloads/Frontier League 2024 Season.db", "5/09/2024", "9/03/2024", .717,.836,1.124,1.418,1.843)

calculatePitchingStatsforFirstPitch <- function(dbname, startDate, endDate, BB_HBP_Constant, Single_Constant, Double_Constant, Triple_Constant, HomeRun_Constant) {
  conn <- dbConnect(RSQLite::SQLite(), dbname = dbname)
  data <- dbReadTable(conn, "Lake Erie Crushers 2024")
  dbDisconnect(conn)
  
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  startDate <- as.Date(startDate, format = "%m/%d/%Y")
  endDate <- as.Date(endDate, format = "%m/%d/%Y")
  
  data_filtered <- data %>%
    filter(Date >= startDate, Date <= endDate, PitcherTeam == "Lake Erie Crushers", PitchofPA == 1, TaggedPitchType != "") %>%
    select(Pitcher, Inning, TaggedPitchType, BatterSide, PitchCall, PlayResult, KorBB, HitType)
  
  stats <- data_filtered %>%
    group_by(Pitcher) %>%
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

Crushers_Pitchers_Stats_First_Pitch_2024 <- calculatePitchingStatsforFirstPitch("/Users/nathanielascher/Downloads/Frontier League 2024 Season.db", "5/09/2024", "9/03/2024", .717,.836,1.124,1.418,1.843)




