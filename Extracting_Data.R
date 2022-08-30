library(dplyr)
library(textreadr)
library(stringr)
library(rvest)
library(tidyr)
library(purrr)
library(ggmap)
library(readr)
library(data.table)
library(lubridate)

wiki_URL_ATP_World_Tour <- "https://www.atptour.com/en/tournaments"
wiki_URL_ATP_Challenger <- "https://www.atptour.com/en/atp-challenger-tour/calendar"

tournament_atp <- function(x) {
  tournament_atp <- read_html(x) %>%
    html_nodes(xpath = '//*[@class="tourney-results-wrapper"]') %>%
    html_table(fill = TRUE)
  tournament_atp <- rbindlist(tournament_atp)

  temp <- tournament_atp[seq(1,nrow(tournament_atp),2),6] # Problematique challengers
  tournament_atp <- tournament_atp[seq(1,nrow(tournament_atp),2),c(2,5,7)]
  names(tournament_atp) <- c("Details", "Surface", "Vainqueur")

  # Recuperer categorie tournoi (250, 500, etc)
  categorystamp <- read_html(x) %>% #Back to Wikipedia we go
    html_nodes(xpath = '//*[@class="tourney-results-wrapper"]') %>%
    html_nodes("td > img") %>%
    html_attr("src") #Grab its URL location

  icon_categorystamp <- paste0("https://www.atptour.com", categorystamp)

  tournament_atp <-tournament_atp %>% #Take the players data frame
    mutate(icon_categorie = icon_categorystamp)

  df_select_categorystamp <- data.frame(deb = str_locate(categorystamp, "/assets/atpwt/images/tournament/badges/categorystamps_")[,2])
  categorystamp <- substr(categorystamp, df_select_categorystamp[,"deb"]+1, nchar(categorystamp)-4)

  for(i in 1:length(categorystamp)){
    if(categorystamp[i] == "challenger") {
      categorystamp[i] <- temp[i]
    }
  }

  # Recuperer lien joueur atp world tour + photo si possible
  tournament_atp <- tournament_atp %>% #Take the players data frame
    mutate(Categorie = categorystamp)

  tournament_atp$Categorie <- str_replace_all(tournament_atp$Categorie,"  ","")
  tournament_atp$Categorie <- str_replace_all(tournament_atp$Categorie,"\r\n"," ")

  # Renommer les catégories
  select_atp_250 <- (tournament_atp$Categorie == 250)
  select_atp_500 <- (tournament_atp$Categorie == 500)
  select_atp_1000 <- (tournament_atp$Categorie == 1000)
  select_atp_grand_chelem <- (tournament_atp$Categorie == "grandslam")
  tournament_atp[select_atp_250,5] <- "ATP 250"
  tournament_atp[select_atp_500,5] <- "ATP 500"
  tournament_atp[select_atp_1000,5] <- "Masters 1000"
  tournament_atp[select_atp_grand_chelem,5] <- "Grand Chelem"

  # Supprimer les tournois spéciaux, type atp cup
  tournament_atp <- tournament_atp[tournament_atp$Categorie %in% c("ATP 250", "ATP 500", "Masters 1000", "Grand Chelem", "Challenger 50", "Challenger 80", "Challenger 90", "Challenger 100", "Challenger 125"),]

  tournament_atp$Details <- str_replace_all(tournament_atp$Details,"  ","")
  tournament_atp$Details <- str_replace_all(tournament_atp$Details,"\r\n\r\n\r\n\r\n",", ")
  tournament_atp$Details <- str_replace_all(tournament_atp$Details,"\r\n","")

  for(j in 1:length(tournament_atp$Details)){
    if(length(unlist(str_split(tournament_atp$Details,",")[[j]]))==5) {
      tournament_atp$Details[j] <- paste0(unlist(str_split(tournament_atp$Details,",")[[j]])[1],",",unlist(str_split(tournament_atp$Details,",")[[j]])[2],",",unlist(str_split(tournament_atp$Details,",")[[j]])[4],",",unlist(str_split(tournament_atp$Details,",")[[j]])[5], sep=" ")
    }
  }

  tournament_atp$Surface <- str_replace_all(tournament_atp$Surface,"  ","")
  tournament_atp$Surface <- str_replace_all(tournament_atp$Surface,"\r\n\r\n",", ")

  tournament_atp$Vainqueur <- str_replace_all(tournament_atp$Vainqueur,"  ","")
  df_select <- data.frame(deb = str_locate(tournament_atp$Vainqueur, "SGL")[,1], fin = str_locate(tournament_atp$Vainqueur, "DBL")[,1])
  tournament_atp$Vainqueur <- substr(tournament_atp$Vainqueur, df_select[,"deb"]+3, df_select[,"fin"]-1)
  tournament_atp$Vainqueur <- str_replace_all(tournament_atp$Vainqueur,"\r\n","")

  # Supprimer les tournois pas encore déroulés
  tournament_atp <- tournament_atp[is.na(tournament_atp$Vainqueur) == FALSE,]

  tournament_atp <- separate(tournament_atp, col="Details", into=c("Tournoi","Ville","Pays","Date"), sep=",")
  tournament_atp <- separate(tournament_atp, col="Date", into=c("Debut","Fin"), sep="-")

  #Now extracting the players' wiki pages so that I can look up their personal information (place of birth, height, playing position)
  player_links <- read_html(x) %>%
    html_nodes(xpath = '//*[@class="tourney-results-wrapper"]') %>%
    html_nodes("td > div > a") %>%
    html_attr("href") #Extract the redirect link for each player

  player_links <- player_links[seq(1,length(player_links),3)]

  # Recuperer lien joueur atp world tour
  tournament_atp <- tournament_atp %>% #Take the players data frame
    mutate(player_links = player_links)

  # Photo du joueur
  player_image <- function (x) { #Creating a function that will...
    read_html(paste0("https://www.atptour.com",x)) %>%
      html_node(xpath = '//*[@id="playerProfileHero"]') %>%
      html_node("img") %>%
      html_attr("src") #Grab its URL location
  }

  tournament_atp <- mutate(tournament_atp, icon_joueur = map(player_links, player_image)) #Apply the function created above for each player
  tournament_atp$icon_joueur <- paste0("https://www.atptour.com", tournament_atp$icon_joueur)

  player_info <- function(x) { #Creating a function grabbing players' info that would be applied to every player.
    player_info <- read_html(paste0("https://www.atptour.com",x)) %>%
      html_node(xpath = '//*[@class="player-profile-hero-table"]') %>%
      html_table(fill = TRUE) #Turn that info into a table
    player_info <- data.frame(Age=player_info[1,1],Birthplace=player_info[2,1],TurnedPro=player_info[1,2],Play=player_info[2,2],Weight=player_info[1,3],Coach=player_info[2,3],Height=player_info[1,4])
    names(player_info) = c("Age","LieuNaissance","ProDepuis","Style","Poids","Coach","Taille")
    player_info$Age <- substr(player_info$Age,40,41)
    player_info$LieuNaissance <- substr(player_info$LieuNaissance,117,nchar(player_info$LieuNaissance))
    player_info$ProDepuis <- substr(player_info$ProDepuis,nchar(player_info$ProDepuis)-3,nchar(player_info$ProDepuis))
    player_info$Style <- substr(player_info$Style,76,nchar(player_info$Style))
    player_info$Poids <- substr(player_info$Poids, str_locate(player_info$Poids, "\\(")[,1]+1, str_locate(player_info$Poids, "\\)")[,1]-1)
    player_info$Coach  <- str_replace(player_info$Coach, "Coach", "")
    player_info$Coach  <- str_replace_all(player_info$Coach, "\r\n", "")
    player_info$Coach  <- str_replace_all(player_info$Coach, "  ", "")
    player_info$Taille <- substr(player_info$Taille, str_locate(player_info$Taille, "\\(")[,1]+1, str_locate(player_info$Taille, "\\)")[,1]-1)

    return(player_info)
  }

  tournament_atp <- mutate(tournament_atp, player_data = map(player_links, player_info)) %>% unnest(player_data)

  player_nationalite <- function(x) {
    player_info_flag <- read_html(paste0("https://www.atptour.com",x)) %>%
      html_node(xpath = '//*[@class="player-flag-code"]') %>%
      html_text()
  }

  tournament_atp <- mutate(tournament_atp, Nationalite = map(player_links, player_nationalite))

  # For each tournament, we add long and lat
  tournament_atp <- mutate_geocode(tournament_atp, Ville, source = "google", output = "more")
  tournament_atp <- tournament_atp %>%
    select(-type, -loctype, -address, -north, -south, -east, -west) #Remove unneeded columns

  #  Format date
  tournament_atp$Debut <- str_replace_all(tournament_atp$Debut, " ", "")
  tournament_atp$Debut <- str_replace_all(tournament_atp$Debut, "\\.", "/")
  tournament_atp$Debut <- as.Date(tournament_atp$Debut)

  tournament_atp$Fin <- str_replace_all(tournament_atp$Fin, " ", "")
  tournament_atp$Fin <- str_replace_all(tournament_atp$Fin, "\\.", "/")
  tournament_atp$Fin <- as.Date(tournament_atp$Fin)

  # Renommer les surfaces
  select_atp_outdoor_hard <- (tournament_atp$Surface == "Outdoor, Hard")
  select_atp_outdoor_clay <- (tournament_atp$Surface == "Outdoor, Clay")
  select_atp_outdoor_grass <- (tournament_atp$Surface == "Outdoor, Grass")
  select_atp_indoor_hard <- (tournament_atp$Surface == "Indoor, Hard")
  select_atp_indoor_clay <- (tournament_atp$Surface == "Indoor, Clay")
  tournament_atp[select_atp_outdoor_hard,6] <- "Dur ext."
  tournament_atp[select_atp_outdoor_clay,6] <- "TB ext."
  tournament_atp[select_atp_outdoor_grass,6] <- "Gazon"
  tournament_atp[select_atp_indoor_hard,6] <- "Dur int."
  tournament_atp[select_atp_indoor_clay,6] <- "TB int."

  # Renommer les types de jeu
  select_atp_droitier_rev2m <- (tournament_atp$Style == "Right-Handed, Two-Handed Backhand")
  select_atp_droitier_rev1m <- (tournament_atp$Style == "Right-Handed, One-Handed Backhand")
  select_atp_gaucher_rev2m <- (tournament_atp$Style == "Left-Handed, Two-Handed Backhand")
  select_atp_gaucher_rev1m <- (tournament_atp$Style == "Left-Handed, One-Handed Backhand")
  tournament_atp[select_atp_droitier_rev2m,15] <- "Droitier, revers à 2 mains"
  tournament_atp[select_atp_droitier_rev1m,15] <- "Droitier, revers à 1 main"
  tournament_atp[select_atp_gaucher_rev2m,15] <- "Gaucher, revers à 2 mains"
  tournament_atp[select_atp_gaucher_rev1m,15] <- "Gaucher, revers à 1 main"

  tournament_atp <- mutate(tournament_atp,
                           links = paste0("https://www.atptour.com", player_links), #Bringing in player links
                           popup_text = paste0("<center>", #Setting up poopup info
                                               ifelse(!is.na(icon_joueur), paste0("<img src='",icon_joueur,"' width='100'>"), ""),
                                               "</br><b>Vainqueur : </b> ", Vainqueur,
                                               "</br><b>Nationalité : </b> ", Nationalite,
                                               "</br><b>Age : </b> ", Age,
                                               "</br><b>Naissance : </b> ", LieuNaissance,
                                               "</br><b>Prise de raquette :</b> ", Style,
                                               "</br><b>Taille :</b> ", Taille,
                                               "</br><b>Poids :</b> ", Poids,
                                               "</br><b>Coach :</b> ", Coach,
                                               "</br><b>Pro depuis :</b> ", ProDepuis,
                                               "</br><a href='", links, "' target='_blank'>More info...</a></center>"))

  tournament_atp <- mutate(tournament_atp,
                           label_text = paste0("<center>", #Setting up poopup info
                                               " <b>Tournoi : </b> ", Tournoi,",", Ville,",", Pays,
                                               "</br><b>Surface : </b> ", Surface,
                                               "</br><b> Date : </b> ", "Du ", format(Debut,"%d/%m/%Y"), " au ", format(Fin, "%d/%m/%Y"),
                                               "</br><b>Catégorie :</b> ", Categorie, "</center>") %>% lapply(htmltools::HTML))

  return(tournament_atp)
}

tournament_atp_final <- tournament_atp(wiki_URL_ATP_World_Tour)
tournament_atp_challenger_final <- tournament_atp(wiki_URL_ATP_Challenger)
tournament_atp_final <- rbind(tournament_atp_final,tournament_atp_challenger_final)

#Saving for the app
write_rds(tournament_atp_final, "tournament_atp_final.rds")
# ##############################################################
# Récupérer classements ATP 2022 (sortie chaque lundi)
x <- seq(as.Date("2022-01-01"), today(), by = "day")
lundis_2022 <- x[weekdays(x) == "Lundi"]

y <- seq(ymd("2001-01-01"), today(), by="1 day")
lundis_mois_2001 <- y[weekdays(y) == "Lundi" & day(y) <= 7]

ranking_par_date <- function(x) {
  ranking_atp_maj <- c()
  for(i in 1:length(x)) {
    ranking_atp <- read_html(paste0("https://www.atptour.com/en/rankings/singles?rankRange=1-5000&rankDate=",x[i])) %>%
      html_nodes(xpath = '//*[@id="player-rank-detail-ajax"]') %>%
      html_table(fill = TRUE) %>% data.frame()

    player_country <- read_html(paste0("https://www.atptour.com/en/rankings/singles?rankRange=1-5000&rankDate=",x[i])) %>%
      html_nodes(xpath = '//*[@class="country-item"]') %>%
      html_node("img") %>%
      html_attr("alt") #Extract the redirect link for each player

    # Recuperer lien joueur atp world tour
    ranking_atp <- ranking_atp %>% #Take the players data frame
      mutate(player_country = player_country)

    if(nrow(ranking_atp)>0) { # Pas de classement semaine de grand chelem
      ranking_atp <- ranking_atp[,c(1,2,11,4,5,6,9)]
      ranking_atp$Date <- x[i]
      colnames(ranking_atp) <- c("Rang","Diff rang / sem. der.", "Pays", "Joueur", "Age", "Points", "Points à défendre", "Date")
      ranking_atp_maj <- rbind(ranking_atp_maj,ranking_atp)
    }
  }
  ranking_atp_maj[,1] <- str_replace_all(ranking_atp_maj[,1] ,"T","")
  ranking_atp_maj[,2] <- str_replace_all(ranking_atp_maj[,2],",","")
  ranking_atp_maj[,6] <- str_replace_all(ranking_atp_maj[,6],",","")
  ranking_atp_maj[,7] <- str_replace_all(ranking_atp_maj[,7],",","")

  ranking_atp_maj[,1] <- as.integer(ranking_atp_maj[,1])
  ranking_atp_maj[,2] <- as.integer(ranking_atp_maj[,2])
  ranking_atp_maj[,6] <- as.integer(ranking_atp_maj[,6])
  ranking_atp_maj[,7] <- as.integer(ranking_atp_maj[,7])

  # Renommer les pays
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"AHO","Antilles Néerlandaises")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"ALG","Algerie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"AND","Andorre")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"ANT","Antigua et Barbuda")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"ARG","Argentine")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"ARM","Armenie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"ARU","Aruba")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"AUS","Australie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"AUT","Autriche")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"AZE","Azerbaidjan")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"BAH","Bahamas")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"BAR","Barbade")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"BDI","Burundi")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"BEL","Belgique")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"BEN","Benin")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"BER","Bermudes")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"BIH","Bosnie Herzegovine")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"BLR","Bielorussie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"BOL","Bolivie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"BOT","Botswana")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"BRA","Bresil")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"BRN","Brunei")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"BUL","Bulgarie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"BUR","Burkina Faso")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"CAM","Cambodge")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"CAN","Canada")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"CAY","Iles Caymans")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"CHI","Chili")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"CHN","Chine")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"CIV","Cote d'Ivoire")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"CMR","Cameroun")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"COD","Republique democratique du Congo")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"COL","Colombie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"CRC","Costa Rica")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"CRO","Croatie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"CUB","Cuba")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"CUW","Curacao")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"CYP","Chypre")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"CZE","Republique Tcheque")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"DEN","Danemark")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"DOM","Republique Dominicaine")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"ECU","Equateur")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"EGY","Egypte")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"ESA","Salvador")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"ESP","Espagne")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"EST","Estonie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"FIN","Finlande")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"FRA","France")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"GAB","Gabon")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"GBR","Grande-Bretagne")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"GEO","Georgie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"GER","Allemagne")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"GHA","Ghana")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"GRE","Grece")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"GUA","Guatemala")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"GUD","Guadeloupe")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"GUM","Guam")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"HAI","Haiti")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"HKG","Hong Kong")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"HUN","Hongrie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"INA","Indonesie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"IND","Inde")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"IRI","Iran")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"IRL","Irelande")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"ISL","Icelande")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"ISR","Israel")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"ISV","Iles Vierges")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"ITA","Italie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"JAM","Jamaique")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"JOR","Jordanie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"JPN","Japon")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"KAZ","Kazakhstan")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"KEN","Kenya")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"KGZ","Kyrgyzstan")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"KOR","Corée du Sud")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"KSA","Arabie Saoudite")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"KUW","Koweit")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"LAT","Lettonie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"LIB","Liban")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"LIE","Liechenstein")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"LTU","Lituanie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"LUX","Luxembourg")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"MAD","Madagascar")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"MAR","Maroc")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"MAS","Malaysie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"MDA","Moldovie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"MEX","Mexique")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"MKD","Macédoine du Nord")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"MLI","Mali")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"MLT","Malte")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"MNE","Montenegro")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"MON","Monaco")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"MRI","Ile Maurice")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"NAM","Namibie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"NED","Pays-Bas")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"NGR","Nigeria")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"NMI","Iles Mariannes du Nord")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"NOR","Norvege")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"NZL","Nouvelle-Zelande")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"OMA","Oman")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"PAK","Pakistan")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"PAN","Panama")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"PAR","Paraguay")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"PER","Perou")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"PHI","Philippines")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"POL","Pologne")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"POR","Portugal")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"PUR","Porto Rico")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"QAT","Qatar")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"ROM","Roumanie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"ROU","Roumanie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"RSA","Afrique du Sud")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"RUS","Russie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"RWA","Rwanda")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"SEN","Senegal")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"SGP","Singapour")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"SIN","Singapour")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"SLE","Sierra Leone")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"SLO","Slovenie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"SMR","San Marin")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"SOL","Iles Salomons")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"SRB","Serbie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"SRI","Sri Lanka")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"SUD","Soudan")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"SUI","Suisse")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"SVK","Slovaquie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"SWE","Suede")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"SYR","Syrie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"TAN","Tanzanie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"THA","Thailande")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"TJK","Tadjikistan")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"TKM","Turkmenistan")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"TOG","Togo")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"TPE","Taipei")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"TTO","Trinidad et Tobago")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"TUN","Tunisie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"TUR","Turquie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"UAE","Emirats Arabes Unis")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"UGA","Ouganda")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"UKR","Ukraine")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"URU","Uruguay")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"USA","Etats-Unis")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"UZB","Ouzbekistan")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"VEN","Venezuela")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"VIE","Vietnam")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"YUG","Yougoslavie")
  ranking_atp_maj$Pays <- str_replace(ranking_atp_maj$Pays ,"ZIM","Zimbabwe")
  return(ranking_atp_maj)
}

ranking_atp_2022 <- ranking_par_date(lundis_2022)
ranking_atp_mois_2001 <- ranking_par_date(lundis_mois_2001)

#Saving for the app
write_rds(ranking_atp_2022, "ranking_atp_2022.rds")
write_rds(ranking_atp_mois_2001, "ranking_atp_mois_2001.rds")

