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

#  Generalisons
noeuds <- read_html(wiki_URL_ATP_World_Tour) %>% html_nodes(xpath = '//*[@class="tourney-results-wrapper"]')

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

#Now grabbing the images in the infobox
player_image <- function (x) { #Creating a function that will...
  read_html(paste0("https://www.atptour.com",x)) %>%
    html_node(xpath = '//*[@id="playerProfileHero"]') %>%
    html_node("img") %>%
    html_attr("src") #Grab its URL location
}

tournament_atp <- mutate(tournament_atp, image = map(player_links, player_image)) #Apply the function created above for each player

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
tournament_atp <- mutate_geocode(tournament_atp, Ville, source = "google", output = "more") %>%
  mutate(
    country = str_extract(address, "(?<=, )[^,]*$") # everything after last comma
  )

tournament_atp <- tournament_atp %>%
  select(-type, -loctype, -address, -north, -south, -east, -west, -country) #Remove unneeded columns

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

tournament_atp <- mutate(tournament_atp,
                         links = paste0("https://www.atptour.com", player_links), #Bringing in player links
                         popup_text = paste0("<center>", #Setting up poopup info
                                             # ifelse(!is.na(image), paste0("<img src = https:", image, " width='100'>"), ""),
                                             "</br><b>Vainqueur : </b> ", Vainqueur,
                                             "</br><b>Age : </b> ", Age,
                                             "</br><b>Naissance : </b> ", BirthPlace,
                                             "</br><b>Prise de raquette :</b> ", Play,
                                             "</br><b>Taille :</b> ", Height,
                                             "</br><b>Poids :</b> ", Weight,
                                             "</br><b>Coach :</b> ", Coach,
                                             "</br><b>Pro depuis :</b> ", TurnedPro,
                                             "</br><a href='", links, "' target='_blank'>More info...</a></center>"))

tournament_atp <- mutate(tournament_atp,
                         label_text = paste0("<center>", #Setting up poopup info
                                             " <b>Tournoi : </b> ", tournament_atp$Tournoi,",", tournament_atp$Ville,",", tournament_atp$Pays,
                                             "</br><b>Surface : </b> ", tournament_atp$Surface,
                                             "</br><b> Date : </b> ", "Du ", format(tournament_atp$Debut,"%d/%m/%Y"), " au ", format(tournament_atp$Fin, "%d/%m/%Y"),
                                             "</br><b>Catégorie :</b> ", tournament_atp$Categorie, "</center>") %>% lapply(htmltools::HTML))
return(tournament_atp)
}

tournament_atp_final <- rbind(tournament_atp(1), tournament_atp(2), tournament_atp(3), tournament_atp(4), tournament_atp(5), tournament_atp(6), tournament_atp(7))

#Saving for the app
write_rds(tournament_atp_final, "tournament_atp_final.rds")
