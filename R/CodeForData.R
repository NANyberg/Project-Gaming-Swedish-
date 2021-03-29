library(tidyverse)
library(rvest)
library(gridExtra)

# Då hemsidorna uppdateras för detta år så måste vi skriva klart koden som krävs.
# Detta laddas sedan ned och förns då kan vi 


metaCritic1 <- # Första 100 spelen
  read_html("https://www.metacritic.com/browse/games/score/metascore/year/all/filtered?year_selected=2020&view=detailed&page=0") 
metaCritic2 <- # Nästa 200 osv...
  read_html("https://www.metacritic.com/browse/games/score/metascore/year/all/filtered?year_selected=2020&view=detailed&page=1")
metaCritic3 <- read_html("https://www.metacritic.com/browse/games/score/metascore/year/all/filtered?year_selected=2020&view=detailed&page=2")
metaCritic4 <- read_html("https://www.metacritic.com/browse/games/score/metascore/year/all/filtered?year_selected=2020&view=detailed&page=3")
metaCritic5 <- read_html("https://www.metacritic.com/browse/games/score/metascore/year/all/filtered?year_selected=2020&view=detailed&page=4")
metaCritic6 <- read_html("https://www.metacritic.com/browse/games/score/metascore/year/all/filtered?year_selected=2020&view=detailed&page=5")
metaCritic7 <- read_html("https://www.metacritic.com/browse/games/score/metascore/year/all/filtered?year_selected=2020&view=detailed&page=6")
metaCritic8 <- read_html("https://www.metacritic.com/browse/games/score/metascore/year/all/filtered?year_selected=2020&view=detailed&page=7")
metaCritic9 <- read_html("https://www.metacritic.com/browse/games/score/metascore/year/all/filtered?year_selected=2020&view=detailed&page=8")
metaCritic10 <- read_html("https://www.metacritic.com/browse/games/score/metascore/year/all/filtered?year_selected=2020&view=detailed&page=9")

obtainingValues <- function(readUrl) {
  # Huvudfunktionen kommer ta ut all väsentlig information från en
  # hemsida. 
  df_metaCritics <- data.frame(1:100)
  
  # Vi börjar med namn
  titleNames <- readUrl %>%
    html_nodes(css = "a h3") %>%
    html_text()
  
  # Här tas platform ut
  platform <- readUrl %>%
    html_nodes(css = ".platform .data") %>%
    html_text() %>% 
    str_replace_all(c("  " = "", "\\n" = ""))
  
  # Här tas Privatpersoners betygsättning ut
  userScore <- readUrl %>%
    html_nodes(css = ".user") %>%
    html_text()
  
######################################################
  # Här tas Metacritics betyg ut. Då jag fick dubbletter från skrapningen
  # av varje variabel så tar jag bort varannat värde med "duplicated".
  # Det behövdes också en funktion då attributet i MetaScore ändras bereonde
  # på färgen på hemsidan.
  duplicates <- seq(1, length(1:200), 2)
  metaScore <- readUrl %>%
    html_nodes(css = ".large.positive") %>%
    html_text() %>%
    as.data.frame() %>%
    rename("MetaScore" = ".") %>%
    mutate(MetaScore = as.integer(MetaScore)) %>%
    filter(MetaScore > 10)
  metaScore <- metaScore[-duplicates, ]
  # OM FÖRSTA METASCORE < 75
  if (length(metaScore) == 0) { #..då har vi bytt färg
    metaScore <- readUrl %>%
      html_nodes(css = ".clamp-score-wrap .mixed") %>%
      html_text() %>%
      as.integer()
  }
  # OM METASCORE AVBRYTS MITT I: (pga ny färg)
  if (length(metaScore) != 100) {
    if (metaScore[length(metaScore)] == 75) {
      metaScore2 <- readUrl %>%
        html_nodes(css = ".clamp-score-wrap .mixed") %>%
        html_text() %>%
        as.integer()
    }
    if (metaScore[length(metaScore)] == 51) {
      metaScore2 <- readUrl %>%
        html_nodes(css = ".clamp-score-wrap .negative") %>%
        html_text() %>%
        as.integer()
    }
  metaScore <- c(metaScore, metaScore2)
}
  
######################################################
  
  # Här tas datum för lansering ut
  releaseDate <- readUrl %>%
    html_nodes(css = ".clamp-details span:nth-child(2)") %>%
    html_text()
  releaseDate <- releaseDate[-duplicates]
  
  ############################################################
  
  # Jag hade hoppats på att få tag i utvecklare/lanserare via samma hemsida som information ovan...
  # Men dessvärre finns bara den informationen genom ett klick på varje spel. 
  # I och med det utför vi en funktion som ska kunna plocka ut dessa genom att gå
  # in på varje spels hemsida och ta den infromation. 
  
  ############################################################
  
  ####### --- OPERATION BIG GUNZ INFORMATION --- #######
  
  obtainGameUrl <- function(readUrlData) {
    # https://www.metacritic.com/game/nintendo-64/the-legend-of-zelda-ocarina-of-time
    
    # Ovan syns en url som krävs för att nå hemsidan för det specifika spelet. Vi måste bygga 
    # ..game/*PLATFORM*/*SPEL* med hjälp av strängmanipulation för att få tillgång
    # till hemsidan direkt via en funktion. Det enda som inte ändras är 
    # https://www.metacritic.com/game/, så det lägger vi till sist. 
    
    # Vi börjar med att skriva om alla spelnamn till godkända url namn för *SPEL*
    getUrlGameTitle <- readUrlData %>%
      html_nodes(css = "a h3") %>%
      html_text() %>%
      str_replace_all(pattern = c(" " = "-", "'" = "", ":" = "", "\\." = "", "\\&-" = "", "/-" = "",
                                  "game--" = "game-", "\\,-" = "-", "\\$!" = "!", "\\[" = "", "\\]" = "",
                                  "\\#" = "", "\\;" = "", "\\//" = "", "\\," = "")) %>%
      tolower()
    
    # UNDANTAG
    if (getUrlGameTitle[87] == "the-dark-eye--book-of-heroes") {
      getUrlGameTitle[87] <- "the-dark-eye-book-of-heroes"
    }
    
    # Sedan gör vi liknande för url platform *PLATFORM*
    getUrlPlatform <- readUrlData %>%
      html_nodes(css = ".platform .data") %>%
      html_text() %>% 
      str_replace_all(c("  " = "", "\\n" = "")) %>%
      str_replace_all(pattern = c(" " = "-", "'" = "", ":" = "", "\\." = "", "\\&-" = "")) %>%
      tolower()
    
    # Först lägger vi till *PLATFORM* genom att köra map med "https://www.metacritic.com/game/" 
    # sedan kör vi paste0 för att sätta ihop sista speltiteln *SPEL*
    legitUrl <- map(getUrlPlatform, .f = ~str_c("https://www.metacritic.com/game/", .x, "/", sep = ""))
    completeUrl <- paste0(legitUrl, getUrlGameTitle)
    
    return(completeUrl)
  } # Returnerar en 100 lång lista med url's. Kan användas med "map"
  
  # Big Gunz refererar till de stora utvecklarna. Vi ska alltså extrahera den 
  # informationen per spel.
  
  # Vi tar här ut publishers
  df_publishers <- data.frame()
  getPublisher <- map(obtainGameUrl(readUrl), .f = 
                        ~read_html(.x) %>%
                        html_nodes(css = ".publisher a:nth-child(1)") %>%
                        html_text() %>%
                        str_replace_all(c("  " = "", "\\n" = "")))
  publisherNames <- paste0(df_publishers, getPublisher)
  
  # Vi tar här ut developers
  df_developers <- data.frame()
  getDeveloper <- map(obtainGameUrl(readUrl), .f = 
                        ~read_html(.x) %>% 
                        html_nodes(css = ".developer .data") %>%
                        html_text() %>%
                        str_replace_all(c("  " = "", "\\n" = "")))
  developerNames <- paste0(df_developers, getDeveloper)
  
  ####### --- OPERATION BIG GUNZ INFORMATION --- #######
  
  ############################################################
  
  # Data frame sätts ihop för 100 spel per hemsida
  df_metaCritics$Titlar <- titleNames
  df_metaCritics$Platform <- platform
  df_metaCritics$MetaScore <- metaScore
  df_metaCritics$UserScore <- userScore
  df_metaCritics$ReleaseDate <- releaseDate
  df_metaCritics$Publishers <- publisherNames
  df_metaCritics$Developers <- developerNames
  
  return(df_metaCritics)
}

# Tanken av *Big Gunz*-operationen var att lyckas extrehera data från nya hemsidor - hemsidor för varje specifikt spel.
# Det som sker ovan är att nya URL's "byggs" ihop och länkar till hemsidan för ytterligare information för vardera titel.
# Vi tar alltså information från 1000 hemsidor, vilket jag ansåg vara det största hindret i projektet.
# Lyckligtsvis gick de att extrahera. Detta på grund av att hemsidans titelnamn samt platformsnamn (som krävs för vardera URL)
# var identiskt uppbyggt. Detta kan ses ovan i den första *str_replace_all*-funktionen där jag manupilerar om titelnamn
# för att matcha hemsidans del av URL:en. Som observerat så är det inte mycket ändringar som är gjorda, vilket underlättade arbetet väldigt mycket. 

# Sparar värden
metaPage1 <- obtainingValues(metaCritic1)

metaPage2 <- obtainingValues(metaCritic2)

metaPage3 <- obtainingValues(metaCritic3)

metaPage4 <- obtainingValues(metaCritic4)

metaPage5 <- obtainingValues(metaCritic5)

metaPage6 <- obtainingValues(metaCritic6)

metaPage7 <- obtainingValues(metaCritic7)

metaPage8 <- obtainingValues(metaCritic8)

metaPage9 <- obtainingValues(metaCritic9)

metaPage10 <- obtainingValues(metaCritic10)

gamesDF <- function() {
  # Då vi jobbar med 10 url's så används dessa var för sig kommer jag kopiera in var för sig.
  
  # Titlar läggs till
  allNames <- 
    c(metaPage1$Titlar, metaPage2$Titlar, metaPage3$Titlar, metaPage4$Titlar,
      metaPage5$Titlar, metaPage6$Titlar, metaPage7$Titlar, metaPage8$Titlar,
      metaPage9$Titlar, metaPage10$Titlar)
  
  # Platform läggs till
  allPlatforms <- 
    c(metaPage1$Platform, metaPage2$Platform, metaPage3$Platform, metaPage4$Platform, 
      metaPage5$Platform, metaPage6$Platform, metaPage7$Platform, metaPage8$Platform, 
      metaPage9$Platform, metaPage10$Platform)
  
  # MetaScore läggs till
  allMetacritics <- 
    c(metaPage1$MetaScore, metaPage2$MetaScore, metaPage3$MetaScore, metaPage4$MetaScore, 
      metaPage5$MetaScore, metaPage6$MetaScore, metaPage7$MetaScore, metaPage8$MetaScore, 
      metaPage9$MetaScore, metaPage10$MetaScore)
  
  # UserScore läggs till
  allUserscore <- 
    c(metaPage1$UserScore, metaPage2$UserScore, metaPage3$UserScore, metaPage4$UserScore, 
      metaPage5$UserScore, metaPage6$UserScore, metaPage7$UserScore, metaPage8$UserScore, 
      metaPage9$UserScore, metaPage10$UserScore)
  
  # Lanseringsdatum läggs till
  allReleasedate <- 
    c(metaPage1$ReleaseDate, metaPage2$ReleaseDate, metaPage3$ReleaseDate, 
      metaPage4$ReleaseDate, metaPage5$ReleaseDate, metaPage6$ReleaseDate, 
      metaPage7$ReleaseDate, metaPage8$ReleaseDate, metaPage9$ReleaseDate, 
      metaPage10$ReleaseDate)
  
  # Publishers läggs till
  allPublishers <- 
    c(metaPage1$Publishers, metaPage2$Publishers, metaPage3$Publishers, 
      metaPage4$Publishers, metaPage5$Publishers, metaPage6$Publishers, 
      metaPage7$Publishers, metaPage8$Publishers, metaPage9$Publishers, 
      metaPage10$Publishers)
  
  # Developers läggs till
  allDevelopers <- 
    c(metaPage1$Developers, metaPage2$Developers, metaPage3$Developers, 
      metaPage4$Developers, metaPage5$Developers, metaPage6$Developers, 
      metaPage7$Developers, metaPage8$Developers, metaPage9$Developers, 
      metaPage10$Developers)
  
  
  
  df_games <- data.frame(1:1000)
  colnames(df_games) <- "Topplista"
  
  # Fyller på med namn
  df_games$Titlar <- allNames
  df_games$Platform <- allPlatforms
  df_games$MetaScore <- allMetacritics
  df_games$UserScore <- allUserscore
  df_games$ReleaseDate <- allReleasedate
  df_games$Publishers <- allPublishers
  df_games$Developers <- allDevelopers
  
  return(df_games)
}

games_dataFrame <- gamesDF()

#write_csv(games_dataFrame, "GamesDF.csv")

