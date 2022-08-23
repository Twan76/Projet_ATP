library(XML)
library(plyr)
library(dplyr)
library(textreadr)
library(stringr)
library(rvest)
library(tibble)
library(tidyr)
library(purrr)
library(ggmap)
library(readr)
library(leaflet)

wiki_URL_ATP_World_Tour <- "https://www.atptour.com/en/tournaments"
wiki_URL_ATP_Challenger <- "https://www.atptour.com/en/atp-challenger-tour/calendar"

tournament_atp <- function(x) {
  tournament_atp <- read_html(wiki_URL_ATP_World_Tour) %>%
    html_nodes(xpath = '//*[@class="tourney-results-wrapper"]') %>%
    html_table(fill = TRUE) %>% .[[x]]

  tournament_atp <- tournament_atp[seq(1,nrow(tournament_atp),2),c(2,5,7)]
  names(tournament_atp) <- c("Details", "Surface", "Vainqueur")

  # Recuperer categorie tournoi (250, 500, etc)
  categorystamp <- read_html(wiki_URL_ATP_World_Tour) %>% #Back to Wikipedia we go
    html_nodes(xpath = '//*[@class="tourney-results-wrapper"]') %>% .[[x]] %>%
    html_nodes("td > img") %>%
    html_attr("src") #Grab its URL location

  df_select_categorystamp <- data.frame(deb = str_locate(categorystamp, "/assets/atpwt/images/tournament/badges/categorystamps_")[,2])
  categorystamp <- substr(categorystamp, df_select_categorystamp[,"deb"]+1, nchar(categorystamp)-4)

  # Recuperer lien joueur atp world tour + photo si possible
  tournament_atp <- tournament_atp %>% #Take the players data frame
    mutate(Categorie = categorystamp)

  # Supprimer les tournois spéciaux, type atp cup
  tournament_atp <- tournament_atp[tournament_atp$Categorie %in% c("250", "500", "1000", "grandslam"),]

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

  tournament_atp <- separate(tournament_atp, col="Details", into=c("Tournoi","Ville","Pays","Date"), sep=",")
  tournament_atp <- separate(tournament_atp, col="Date", into=c("Debut","Fin"), sep="-")

  #Now extracting the players' wiki pages so that I can look up their personal information (place of birth, height, playing position)
  player_links <- read_html(wiki_URL_ATP_World_Tour) %>%
    html_nodes(xpath = '//*[@class="tourney-results-wrapper"]') %>% .[[x]] %>%
    html_nodes("td > div > a") %>%
    html_attr("href") #Extract the redirect link for each player

  player_links <- player_links[seq(1,length(player_links),3)]

  # Recuperer lien joueur atp world tour + photo si possible
  tournament_atp <- tournament_atp %>% #Take the players data frame
    mutate(player_links = player_links)

  #Now grabbing the image in the infobox
  # player_image <- function (x) { #Creating a function that will...
  #   read_html(paste0("https://www.atptour.com",x)) %>%
  #     html_node(xpath = '//*[@id="playerProfileHero"]') %>%
  #     html_node("img") %>%
  #     html_attr("src") #Grab its URL location
  # }
  # tournament_atp <- mutate(tournament_atp, image = map(player_links, player_image)) #Apply the function created above for each player

  player_info <- function(x) { #Creating a function grabbing players' info that would be applied to every player.
    player_info <- read_html(paste0("https://www.atptour.com",x)) %>%
      html_node(xpath = '//*[@class="player-profile-hero-table"]') %>%
      html_table(fill = TRUE) #Turn that info into a table
    player_info <- data.frame(Age=player_info[1,1],Birthplace=player_info[2,1],TurnedPro=player_info[1,2],Play=player_info[2,2],Weight=player_info[1,3],Coach=player_info[2,3],Height=player_info[1,4])
    names(player_info) = c("Age","BirthPlace","TurnedPro","Play","Weight","Coach","Height")
    player_info$Age <- substr(player_info$Age,40,41)
    player_info$BirthPlace <- substr(player_info$BirthPlace,117,nchar(player_info$BirthPlace))
    player_info$TurnedPro <- substr(player_info$TurnedPro,nchar(player_info$TurnedPro)-3,nchar(player_info$TurnedPro))
    player_info$Play <- substr(player_info$Play,76,nchar(player_info$Play))
    player_info$Weight <- substr(player_info$Weight, str_locate(player_info$Weight, "\\(")[,1]+1, str_locate(player_info$Weight, "\\)")[,1]-1)
    player_info$Coach  <- str_replace(player_info$Coach, "Coach", "")
    player_info$Coach  <- str_replace_all(player_info$Coach, "\r\n", "")
    player_info$Coach  <- str_replace_all(player_info$Coach, "  ", "")
    player_info$Height <- substr(player_info$Height, str_locate(player_info$Height, "\\(")[,1]+1, str_locate(player_info$Height, "\\)")[,1]-1)
    return(player_info)
  }

  tournament_atp <- mutate(tournament_atp, player_data = map(player_links, player_info)) %>% unnest(player_data)

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

  # Renommer les catégories
  select_atp_250 <- (tournament_atp$Categorie == 250)
  select_atp_500 <- (tournament_atp$Categorie == 500)
  select_atp_1000 <- (tournament_atp$Categorie == 1000)
  select_atp_grand_chelem <- (tournament_atp$Categorie == "grandslam")
  tournament_atp[select_atp_250,8] <- "ATP 250"
  tournament_atp[select_atp_500,8] <- "ATP 500"
  tournament_atp[select_atp_1000,8] <- "Masters 1000"
  tournament_atp[select_atp_grand_chelem,8] <- "Grand Chelem"

return(tournament_atp)
}

# Données circuit secondaire : les challengers
tournament_atp_challenger <- function(x) {
  tournament_atp_challenger <- read_html(wiki_URL_ATP_Challenger) %>%
    html_nodes(xpath = '//*[@class="tourney-results-wrapper"]') %>%
    html_table(fill = TRUE) %>% .[[x]]

  tournament_atp_challenger <- tournament_atp_challenger[seq(1,nrow(tournament_atp_challenger),2),c(2,5,7,6)]
  names(tournament_atp_challenger) <- c("Details", "Surface", "Vainqueur", "Categorie")

  tournament_atp_challenger$Details <- str_replace_all(tournament_atp_challenger$Details,"  ","")
  tournament_atp_challenger$Details <- str_replace_all(tournament_atp_challenger$Details,"\r\n\r\n\r\n\r\n",", ")
  tournament_atp_challenger$Details <- str_replace_all(tournament_atp_challenger$Details,"\r\n","")

  for(j in 1:length(tournament_atp_challenger$Details)){
    if(length(unlist(str_split(tournament_atp_challenger$Details,",")[[j]]))==5) {
      tournament_atp_challenger$Details[j] <- paste0(unlist(str_split(tournament_atp_challenger$Details,",")[[j]])[1],",",unlist(str_split(tournament_atp_challenger$Details,",")[[j]])[2],",",unlist(str_split(tournament_atp_challenger$Details,",")[[j]])[4],",",unlist(str_split(tournament_atp_challenger$Details,",")[[j]])[5], sep=" ")
    }
  }

  tournament_atp_challenger$Surface <- str_replace_all(tournament_atp_challenger$Surface,"  ","")
  tournament_atp_challenger$Surface <- str_replace_all(tournament_atp_challenger$Surface,"\r\n\r\n",", ")

  tournament_atp_challenger$Vainqueur <- str_replace_all(tournament_atp_challenger$Vainqueur,"  ","")
  df_select <- data.frame(deb = str_locate(tournament_atp_challenger$Vainqueur, "SGL")[,1], fin = str_locate(tournament_atp_challenger$Vainqueur, "DBL")[,1])
  tournament_atp_challenger$Vainqueur <- substr(tournament_atp_challenger$Vainqueur, df_select[,"deb"]+3, df_select[,"fin"]-1)
  tournament_atp_challenger$Vainqueur <- str_replace_all(tournament_atp_challenger$Vainqueur,"\r\n","")

  tournament_atp_challenger$Categorie <- str_replace_all(tournament_atp_challenger$Categorie,"  ","")
  tournament_atp_challenger$Categorie <- str_replace_all(tournament_atp_challenger$Categorie,"\r\n"," ")

  tournament_atp_challenger <- separate(tournament_atp_challenger, col="Details", into=c("Tournoi","Ville","Pays","Date"), sep=",")
  tournament_atp_challenger <- separate(tournament_atp_challenger, col="Date", into=c("Debut","Fin"), sep="-")

  #Now extracting the players' wiki pages so that I can look up their personal information (place of birth, height, playing position)
  player_links <- read_html(wiki_URL_ATP_Challenger) %>%
    html_nodes(xpath = '//*[@class="tourney-results-wrapper"]') %>% .[[x]] %>%
    html_nodes("td > div > a") %>%
    html_attr("href") #Extract the redirect link for each player

  player_links <- player_links[seq(1,length(player_links),3)]

  # Recuperer lien joueur atp world tour + photo si possible
  tournament_atp_challenger <- tournament_atp_challenger %>% #Take the players data frame
    mutate(player_links = player_links)

  #Now grabbing the image in the infobox
  # player_image <- function (x) { #Creating a function that will...
  #   read_html(paste0("https://www.atptour.com",x)) %>%
  #     html_node(xpath = '//*[@id="playerProfileHero"]') %>%
  #     html_node("img") %>%
  #     html_attr("src") #Grab its URL location
  # }
  # tournament_atp <- mutate(tournament_atp, image = map(player_links, player_image)) #Apply the function created above for each player

  player_info <- function(x) { #Creating a function grabbing players' info that would be applied to every player.
    player_info <- read_html(paste0("https://www.atptour.com",x)) %>%
      html_node(xpath = '//*[@class="player-profile-hero-table"]') %>%
      html_table(fill = TRUE) #Turn that info into a table
    player_info <- data.frame(Age=player_info[1,1],Birthplace=player_info[2,1],TurnedPro=player_info[1,2],Play=player_info[2,2],Weight=player_info[1,3],Coach=player_info[2,3],Height=player_info[1,4])
    names(player_info) = c("Age","BirthPlace","TurnedPro","Play","Weight","Coach","Height")
    player_info$Age <- substr(player_info$Age,40,41)
    player_info$BirthPlace <- substr(player_info$BirthPlace,117,nchar(player_info$BirthPlace))
    player_info$TurnedPro <- substr(player_info$TurnedPro,nchar(player_info$TurnedPro)-3,nchar(player_info$TurnedPro))
    player_info$Play <- substr(player_info$Play,76,nchar(player_info$Play))
    player_info$Weight <- substr(player_info$Weight, str_locate(player_info$Weight, "\\(")[,1]+1, str_locate(player_info$Weight, "\\)")[,1]-1)
    player_info$Coach  <- str_replace(player_info$Coach, "Coach", "")
    player_info$Coach  <- str_replace_all(player_info$Coach, "\r\n", "")
    player_info$Coach  <- str_replace_all(player_info$Coach, "  ", "")
    player_info$Height <- substr(player_info$Height, str_locate(player_info$Height, "\\(")[,1]+1, str_locate(player_info$Height, "\\)")[,1]-1)
    return(player_info)
  }

  tournament_atp_challenger <- mutate(tournament_atp_challenger, player_data = map(player_links, player_info)) %>% unnest(player_data)

  # For each tournament, we add long and lat
  tournament_atp_challenger <- mutate_geocode(tournament_atp_challenger, Ville, source = "google", output = "more")
  tournament_atp_challenger <- tournament_atp_challenger %>%
    select(-type, -loctype, -address, -north, -south, -east, -west) #Remove unneeded columns

  #  Format date
  tournament_atp_challenger$Debut <- str_replace_all(tournament_atp_challenger$Debut, " ", "")
  tournament_atp_challenger$Debut <- str_replace_all(tournament_atp_challenger$Debut, "\\.", "/")
  tournament_atp_challenger$Debut <- as.Date(tournament_atp_challenger$Debut)

  tournament_atp_challenger$Fin <- str_replace_all(tournament_atp_challenger$Fin, " ", "")
  tournament_atp_challenger$Fin <- str_replace_all(tournament_atp_challenger$Fin, "\\.", "/")
  tournament_atp_challenger$Fin <- as.Date(tournament_atp_challenger$Fin)

  return(tournament_atp_challenger)
}

tournament_atp_final <- rbind(tournament_atp(1), tournament_atp(2), tournament_atp(3), tournament_atp(4), tournament_atp(5), tournament_atp(6), tournament_atp(7))
tournament_atp_challenger_final <- rbind(tournament_atp_challenger(1), tournament_atp_challenger(2), tournament_atp_challenger(3), tournament_atp_challenger(4), tournament_atp_challenger(5), tournament_atp_challenger(6), tournament_atp_challenger(7))

tournament_atp_final <- rbind(tournament_atp_final,tournament_atp_challenger_final)

# Renommer les surfaces
select_atp_outdoor_hard <- (tournament_atp_final$Surface == "Outdoor, Hard")
select_atp_outdoor_clay <- (tournament_atp_final$Surface == "Outdoor, Clay")
select_atp_outdoor_grass <- (tournament_atp_final$Surface == "Outdoor, Grass")
select_atp_indoor_hard <- (tournament_atp_final$Surface == "Indoor, Hard")
select_atp_indoor_clay <- (tournament_atp_final$Surface == "Indoor, Clay")
tournament_atp_final[select_atp_outdoor_hard,6] <- "Dur ext."
tournament_atp_final[select_atp_outdoor_clay,6] <- "TB ext."
tournament_atp_final[select_atp_outdoor_grass,6] <- "Gazon"
tournament_atp_final[select_atp_indoor_hard,6] <- "Dur int."
tournament_atp_final[select_atp_indoor_clay,6] <- "TB int."

# Renommer les types de jeu
select_atp_droitier_rev2m <- (tournament_atp_final$Play == "Right-Handed, Two-Handed Backhand")
select_atp_droitier_rev1m <- (tournament_atp_final$Play == "Right-Handed, One-Handed Backhand")
select_atp_gaucher_rev2m <- (tournament_atp_final$Play == "Left-Handed, Two-Handed Backhand")
select_atp_gaucher_rev1m <- (tournament_atp_final$Play == "Left-Handed, One-Handed Backhand")
tournament_atp_final[select_atp_droitier_rev2m,13] <- "Droitier, revers à 2 mains"
tournament_atp_final[select_atp_droitier_rev1m,13] <- "Droitier, revers à 1 main"
tournament_atp_final[select_atp_gaucher_rev2m,13] <- "Gaucher, revers à 2 mains"
tournament_atp_final[select_atp_gaucher_rev1m,13] <- "Gaucher, revers à 1 main"

icon_tournoi = case_when(
  tournament_atp_final$Tournoi == "Abierto Mexicano Telcel presentado por HSBC" ~ "www/tournaments/acapulco.png",
   tournament_atp_final$Tournoi == "ABN AMRO Open" ~ "www/tournaments/rotterdam.png",
   tournament_atp_final$Tournoi == "Adelaide International 1" ~ "www/tournaments/adelaide.png",
   tournament_atp_final$Tournoi == "Adelaide International 2" ~ "www/tournaments/adelaide.png",
   tournament_atp_final$Tournoi == "Argentina Open" ~ "www/tournaments/buenos_aires.png",
   tournament_atp_final$Tournoi == "Atlanta Open" ~ "www/tournaments/atlanta.png",
   tournament_atp_final$Tournoi == "Australian Open" ~ "www/tournaments/australian_open.png",
   tournament_atp_final$Tournoi == "Barcelona Open Banc Sabadell" ~ "www/tournaments/barcelone.png",
   tournament_atp_final$Tournoi == "BMW Open by American Express" ~ "www/tournaments/munich.png",
   tournament_atp_final$Tournoi == "BNP Paribas Open" ~ "www/tournaments/indian_wells.png",
   tournament_atp_final$Tournoi == "BOSS OPEN" ~ "www/tournaments/stuttgart.png",
   tournament_atp_final$Tournoi == "Chile Dove Men+Care Open" ~ "www/tournaments/santiago.png",
   tournament_atp_final$Tournoi == "Cinch Championships" ~ "www/tournaments/london.png",
   tournament_atp_final$Tournoi == "Cordoba Open" ~ "www/tournaments/cordoba.png",
   tournament_atp_final$Tournoi == "Dallas Open" ~ "www/tournaments/dallas.png",
   tournament_atp_final$Tournoi == "Delray Beach Open by VITACOST.com" ~ "www/tournaments/delray_beach.png",
   tournament_atp_final$Tournoi == "Dubai Duty Free Tennis Championships" ~ "www/tournaments/dubai.png",
   tournament_atp_final$Tournoi == "EFG Swiss Open Gstaad" ~ "www/tournaments/gstaad.png",
   tournament_atp_final$Tournoi == "Fayez Sarofim & Co. U.S. Men's Clay Court Championship" ~ "www/tournaments/houston.png",
   tournament_atp_final$Tournoi == "Generali Open" ~ "www/tournaments/kitzbuhel.png",
   tournament_atp_final$Tournoi == "Gonet Geneva Open" ~ "www/tournaments/geneve.png",
   tournament_atp_final$Tournoi == "Grand Prix Hassan II" ~ "www/tournaments/marrakech.png",
   tournament_atp_final$Tournoi == "Hamburg European Open" ~ "www/tournaments/hamburg.png",
   tournament_atp_final$Tournoi == "Infosys Hall of Fame Open" ~ "www/tournaments/newport.png",
   tournament_atp_final$Tournoi == "Internazionali BNL d'Italia" ~ "www/tournaments/rome.png",
   tournament_atp_final$Tournoi == "Libema Open" ~ "www/tournaments/hertogenbosch.png",
   tournament_atp_final$Tournoi == "Mallorca Championships" ~ "www/tournaments/mallorca.png",
   tournament_atp_final$Tournoi == "Melbourne Summer Set" ~ "www/tournaments/melbourne.png",
   tournament_atp_final$Tournoi == "Miami Open presented by Itau" ~ "www/tournaments/miami.png",
   tournament_atp_final$Tournoi == "Millennium Estoril Open" ~ "www/tournaments/estoril.png",
   tournament_atp_final$Tournoi == "Mutua Madrid Open" ~ "www/tournaments/madrid.png",
   tournament_atp_final$Tournoi == "Nordea Open" ~ "www/tournaments/bastad.png",
   tournament_atp_final$Tournoi == "Open 13 Provence" ~ "www/tournaments/marseille.png",
   tournament_atp_final$Tournoi == "Open Parc Auvergne-Rhone-Alpes Lyon" ~ "www/tournaments/lyon.png",
   tournament_atp_final$Tournoi == "Open Sud de France – Montpellier" ~ "www/tournaments/montpellier.png",
   tournament_atp_final$Tournoi == "Plava Laguna Croatia Open Umag" ~ "www/tournaments/umag.png",
   tournament_atp_final$Tournoi == "Qatar ExxonMobil Open" ~ "www/tournaments/doha.png",
   tournament_atp_final$Tournoi == "Rio Open presented by Claro" ~ "www/tournaments/rio.png",
   tournament_atp_final$Tournoi == "Roland Garros" ~ "www/tournaments/roland_garros.png",
   tournament_atp_final$Tournoi == "Rolex Monte-Carlo Masters" ~ "www/tournaments/monte_carlo.png",
   tournament_atp_final$Tournoi == "Rothesay International" ~ "www/tournaments/eastbourne.png",
   tournament_atp_final$Tournoi == "Serbia Open" ~ "www/tournaments/belgrade.png",
   tournament_atp_final$Tournoi == "Sydney Tennis Classic" ~ "www/tournaments/sydney.png",
   tournament_atp_final$Tournoi == "Tata Open Maharashtra" ~ "www/tournaments/pune.png",
   tournament_atp_final$Tournoi == "Terra Wortmann Open" ~ "www/tournaments/halle.png",
   tournament_atp_final$Tournoi == "Wimbledon" ~ "www/tournaments/wimbledon.png"
)

tournament_atp_final <- mutate(tournament_atp_final,
                               icon_tournoi = icon_tournoi)

icon_joueur = case_when(
  tournament_atp_final$Vainqueur == "Rafael Nadal" ~ "www/players/nadal.png",
  tournament_atp_final$Vainqueur == "Aslan Karatsev" ~ "www/players/karatsev.png",
  tournament_atp_final$Vainqueur == "Thanasi Kokkinakis" ~ "www/players/kokkinakis.png",
  tournament_atp_final$Vainqueur == "Joao Sousa" ~ "www/players/sousa.png",
  tournament_atp_final$Vainqueur == "Gael Monfils" ~ "www/players/monfils.png",
  tournament_atp_final$Vainqueur == "Albert Ramos-Vinolas" ~ "www/players/ramos-vinolas.png",
  tournament_atp_final$Vainqueur == "Alexander Bublik" ~ "www/players/bublik.png",
  tournament_atp_final$Vainqueur == "Carlos Alcaraz" ~ "www/players/alcaraz.png",
  tournament_atp_final$Vainqueur == "Felix Auger-Aliassime" ~ "www/players/auger-aliassime.png",
  tournament_atp_final$Vainqueur == "Sebastian Baez" ~ "www/players/baez.png",
  tournament_atp_final$Vainqueur == "Roberto Bautista Agut" ~ "www/players/bautista-agut.png",
  tournament_atp_final$Vainqueur == "Matteo Berrettini" ~ "www/players/berrettini.png",
  tournament_atp_final$Vainqueur == "Francisco Cerundolo" ~ "www/players/cerundolo_francisco.png",
  tournament_atp_final$Vainqueur == "Maxime Cressy" ~ "www/players/cressy.png",
  tournament_atp_final$Vainqueur == "Alex de Minaur" ~ "www/players/de_minaur.png",
  tournament_atp_final$Vainqueur == "Novak Djokovic" ~ "www/players/djokovic.png",
  tournament_atp_final$Vainqueur == "Taylor Fritz" ~ "www/players/fritz.png",
  tournament_atp_final$Vainqueur == "David Goffin" ~ "www/players/goffin.png",
  tournament_atp_final$Vainqueur == "Hubert Hurkacz" ~ "www/players/hurkacz.png",
  tournament_atp_final$Vainqueur == "Pedro Martinez" ~ "www/players/martinez.png",
  tournament_atp_final$Vainqueur == "Lorenzo Musetti" ~ "www/players/musetti.png",
  tournament_atp_final$Vainqueur == "Cameron Norrie" ~ "www/players/norrie.png",
  tournament_atp_final$Vainqueur == "Reilly Opelka" ~ "www/players/opelka.png",
  tournament_atp_final$Vainqueur == "Andrey Rublev" ~ "www/players/rublev.png",
  tournament_atp_final$Vainqueur == "Holger Rune" ~ "www/players/rune.png",
  tournament_atp_final$Vainqueur == "Casper Ruud" ~ "www/players/ruud.png",
  tournament_atp_final$Vainqueur == "Jannik Sinner" ~ "www/players/sinner.png",
  tournament_atp_final$Vainqueur == "Stefanos Tsitsipas" ~ "www/players/tsitsipas.png",
  tournament_atp_final$Vainqueur == "Tim Van Rijhtoven" ~ "www/players/van_rijthoven.png",
  tournament_atp_final$Vainqueur == "Ernesto Escobedo" ~ "www/players/escobedo.png",
  tournament_atp_final$Vainqueur == "Santiago Rodriguez Taverna" ~ "www/players/rodriguez_taverna.png",
  tournament_atp_final$Vainqueur == "Luca Nardi" ~ "www/players/nardi.png",
  tournament_atp_final$Vainqueur == "Jack Draper" ~ "www/players/draper.png",
  tournament_atp_final$Vainqueur == "Igor Marcondes" ~ "www/players/marcondes.png",
  tournament_atp_final$Vainqueur == "Daniel Elahi Galan" ~ "www/players/galan.png",
  tournament_atp_final$Vainqueur == "Pavel Kotov" ~ "www/players/kotov.png",
  tournament_atp_final$Vainqueur == "Yoshihito Nishioka" ~ "www/players/nishioka.png",
  tournament_atp_final$Vainqueur == "Vasek Pospisil" ~ "www/players/pospisil.png",
  tournament_atp_final$Vainqueur == "Dominic Stricker" ~ "www/players/stricker.png",
  tournament_atp_final$Vainqueur == "Chun-Hsin Tseng" ~ "www/players/tseng.png",
  tournament_atp_final$Vainqueur == "Benjamin Bonzi" ~ "www/players/bonzi.png",
  tournament_atp_final$Vainqueur == "Aleksandar Vukic" ~ "www/players/vukic.png",
  tournament_atp_final$Vainqueur == "Quentin Halys" ~ "www/players/halys.png",
  tournament_atp_final$Vainqueur == "Gianluca Mager" ~ "www/players/mager.png",
  tournament_atp_final$Vainqueur == "Mats Moraing" ~ "www/players/moraing.png",
  tournament_atp_final$Vainqueur == "Fernando Verdasco" ~ "www/players/verdasco.png",
  tournament_atp_final$Vainqueur == "Hugo Dellien" ~ "www/players/dellien.png",
  tournament_atp_final$Vainqueur == "Carlos Taberner" ~ "www/players/taberner.png",
  tournament_atp_final$Vainqueur == "Tomas Martin Etcheverry" ~ "www/players/etcheverry.png",
  tournament_atp_final$Vainqueur == "Manuel Guinard" ~ "www/players/guinard.png",
  tournament_atp_final$Vainqueur == "Denis Kudla" ~ "www/players/kudla.png",
  tournament_atp_final$Vainqueur == "Paul Jubb" ~ "www/players/jubb.png",
  tournament_atp_final$Vainqueur == "Flavio Cobolli" ~ "www/players/cobolli.png",
  tournament_atp_final$Vainqueur == "Jurij Rodionov" ~ "www/players/rodionov.png",
  tournament_atp_final$Vainqueur == "Jaume Munar" ~ "www/players/munar.png",
  tournament_atp_final$Vainqueur == "Facundo Bagnis" ~ "www/players/bagnis.png",
  tournament_atp_final$Vainqueur == "Gastao Elias" ~ "www/players/elias.png",
  tournament_atp_final$Vainqueur == "Marc-Andrea Huesler" ~ "www/players/huesler.png",
  tournament_atp_final$Vainqueur == "Emilio Gomez" ~ "www/players/gomez.png",
  tournament_atp_final$Vainqueur == "Antoine Bellier" ~ "www/players/bellier.png",
  tournament_atp_final$Vainqueur == "Nuno Borges" ~ "www/players/borges.png",
  tournament_atp_final$Vainqueur == "Pedro Cachin" ~ "www/players/cachin.png",
  tournament_atp_final$Vainqueur == "Tung-Lin Wu" ~ "www/players/wu_tunglin.png",
  tournament_atp_final$Vainqueur == "Christopher O'Connell" ~ "www/players/oconnell.png",
  tournament_atp_final$Vainqueur == "Sebastian Ofner" ~ "www/players/ofner.png",
  tournament_atp_final$Vainqueur == "Camilo Ugo Carabelli" ~ "www/players/carabelli.png",
  tournament_atp_final$Vainqueur == "Jay Clarke" ~ "www/players/clarke.png",
  tournament_atp_final$Vainqueur == "Jack Sock" ~ "www/players/sock.png",
  tournament_atp_final$Vainqueur == "Evan Furness" ~ "www/players/furness.png",
  tournament_atp_final$Vainqueur == "Franco Agamenone" ~ "www/players/agamenone.png",
  tournament_atp_final$Vainqueur == "Joao Domingues" ~ "www/players/domingues.png",
  tournament_atp_final$Vainqueur == "Alexei Popyrin" ~ "www/players/popyrin.png",
  tournament_atp_final$Vainqueur == "Daniel Altmaier" ~ "www/players/altmaier.png",
  tournament_atp_final$Vainqueur == "Facundo Diaz Acosta" ~ "www/players/diaz_acosta.png",
  tournament_atp_final$Vainqueur == "Emilio Nava" ~ "www/players/nava.png",
  tournament_atp_final$Vainqueur == "Filip Misolic" ~ "www/players/misolic.png",
  tournament_atp_final$Vainqueur == "Sergey Fomin" ~ "www/players/fomin.png",
  tournament_atp_final$Vainqueur == "Roberto Carballes Baena" ~ "www/players/carballes_baena.png",
  tournament_atp_final$Vainqueur == "Matteo Arnaldi" ~ "www/players/arnaldi.png",
  tournament_atp_final$Vainqueur == "Andrea Pellegrino" ~ "www/players/pellegrino.png",
  tournament_atp_final$Vainqueur == "Lukas Klein" ~ "www/players/klein.png",
  tournament_atp_final$Vainqueur == "Jordan Thompson" ~ "www/players/thompson.png",
  tournament_atp_final$Vainqueur == "Jason Kubler" ~ "www/players/kubler.png",
  tournament_atp_final$Vainqueur == "Vit Kopriva" ~ "www/players/kopriva.png",
  tournament_atp_final$Vainqueur == "Arthur Rinderknech" ~ "www/players/rinderknech.png",
  tournament_atp_final$Vainqueur == "Daniel Evans" ~ "www/players/evans.png",
  tournament_atp_final$Vainqueur == "Yibing Wu" ~ "www/players/wu_yibing.png",
  tournament_atp_final$Vainqueur == "Corentin Moutet" ~ "www/players/moutet.png",
  tournament_atp_final$Vainqueur == "Alexander Shevchenko" ~ "www/players/shevchenko.png",
  tournament_atp_final$Vainqueur == "Borna Coric" ~ "www/players/coric.png",
  tournament_atp_final$Vainqueur == "Zizou Bergs" ~ "www/players/bergs.png",
  tournament_atp_final$Vainqueur == "Alexandre Muller" ~ "www/players/muller.png",
  tournament_atp_final$Vainqueur == "Francisco Comesana" ~ "www/players/comesana.png",
  tournament_atp_final$Vainqueur == "Federico Coria" ~ "www/players/coria.png",
  tournament_atp_final$Vainqueur == "Kaichi Uchida" ~ "www/players/uchida.png",
  tournament_atp_final$Vainqueur == "Facundo Mena" ~ "www/players/mena.png",
  tournament_atp_final$Vainqueur == "Constant Lestienne" ~ "www/players/lestienne.png",
  tournament_atp_final$Vainqueur == "Juan Bautista Torres" ~ "www/players/bautista_torres.png",
  tournament_atp_final$Vainqueur == "Jan-Lennard Struff" ~ "www/players/struff.png",
  tournament_atp_final$Vainqueur == "Thiago Monteiro" ~ "www/players/monteiro.png",
  tournament_atp_final$Vainqueur == "Juan Pablo Ficovich" ~ "www/players/ficovich.png",
  tournament_atp_final$Vainqueur == "Altug Celikbilek" ~ "www/players/celikbilek.png",
  tournament_atp_final$Vainqueur == "Felipe Meligeni Alves" ~ "www/players/meligeni_avec.png",
  tournament_atp_final$Vainqueur == "Francesco Maestrelli" ~ "www/players/maestrelli.png",
  tournament_atp_final$Vainqueur == "Tallon Griekspoor" ~ "www/players/griekspoor.png",
  tournament_atp_final$Vainqueur == "Francesco Passaro" ~ "www/players/passaro.png",
  tournament_atp_final$Vainqueur == "Roman Safiullin" ~ "www/players/safiullin.png",
  tournament_atp_final$Vainqueur == "Zsombor Piros" ~ "www/players/piros.png",
  tournament_atp_final$Vainqueur == "Hugo Grenier" ~ "www/players/grenier.png",
  tournament_atp_final$Vainqueur == "Raul Brancaccio" ~ "www/players/brancaccio.png",
  tournament_atp_final$Vainqueur == "Tomas Machac" ~ "www/players/machac.png"
)

tournament_atp_final <- mutate(tournament_atp_final,
                               icon_joueur = icon_joueur)

icon_categorie = case_when(
  tournament_atp_final$Categorie == "ATP 250" ~ "inst/app/www/category/categorystamps_250.png",
  tournament_atp_final$Categorie == "ATP 500" ~ "inst/app/www/category/categorystamps_500.png",
  tournament_atp_final$Categorie == "Masters 1000" ~ "inst/app/www/category/categorystamps_1000.png",
  tournament_atp_final$Categorie == "Grand Chelem" ~ "inst/app/www/category/categorystamps_grandslam.png",
  tournament_atp_final$Categorie == "Challenger 50" ~ "inst/app/www/category/categorystamps_challenger.png",
  tournament_atp_final$Categorie == "Challenger 80" ~ "inst/app/www/category/categorystamps_challenger.png",
  tournament_atp_final$Categorie == "Challenger 90" ~ "inst/app/www/category/categorystamps_challenger.png",
  tournament_atp_final$Categorie == "Challenger 100" ~ "inst/app/www/category/categorystamps_challenger.png",
  tournament_atp_final$Categorie == "Challenger 110" ~ "inst/app/www/category/categorystamps_challenger.png",
  tournament_atp_final$Categorie == "Challenger 125" ~ "inst/app/www/category/categorystamps_challenger.png"
)


tournament_atp_final <- mutate(tournament_atp_final,
                               icon_categorie = icon_categorie)


tournament_atp_final <- mutate(tournament_atp_final,
                         links = paste0("https://www.atptour.com", player_links), #Bringing in player links
                         popup_text = paste0("<center>", #Setting up poopup info
                                             ifelse(!is.na(icon_joueur), paste0("<img src='",icon_joueur,"' width='100'>"), ""),
                                             "</br><b>Vainqueur : </b> ", Vainqueur,
                                             "</br><b>Age : </b> ", Age,
                                             "</br><b>Naissance : </b> ", BirthPlace,
                                             "</br><b>Prise de raquette :</b> ", Play,
                                             "</br><b>Taille :</b> ", Height,
                                             "</br><b>Poids :</b> ", Weight,
                                             "</br><b>Coach :</b> ", Coach,
                                             "</br><b>Pro depuis :</b> ", TurnedPro,
                                             "</br><a href='", links, "' target='_blank'>More info...</a></center>"))

tournament_atp_final <- mutate(tournament_atp_final,
                         label_text = paste0("<center>", #Setting up poopup info
                                             " <b>Tournoi : </b> ", Tournoi,",", Ville,",", Pays,
                                             "</br><b>Surface : </b> ", Surface,
                                             "</br><b> Date : </b> ", "Du ", format(Debut,"%d/%m/%Y"), " au ", format(Fin, "%d/%m/%Y"),
                                             "</br><b>Catégorie :</b> ", Categorie,
                                             "</br>", ifelse(!is.na(icon_tournoi), paste0("<img src='",icon_tournoi,"' width='100'>"), ""),"</center>") %>% lapply(htmltools::HTML))

#Saving for the app
write_rds(tournament_atp_final, "tournament_atp_final.rds")

# Manque lat, lon pour une ville
