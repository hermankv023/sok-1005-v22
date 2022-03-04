  library(rvest)
  library(tidyverse)
  library(rlist)
  library(purrr)
  #Jobbet sammen med Amund Bech 
  #minefag
  fag1 <-"https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1005-1&week=1-20&View=list"
  fag2 <-"https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1006-1&week=1-20&View=list"
  fag3 <-"https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1016-1&week=1-20&View=list"
  fag <- list(fag1,fag2,fag3)
  
  #lager det om til en funksjon
  skrape <- function(fag) {
    page <- read_html(fag)
    
  #Lager liste og får 1 variabel  per liste
    table <- html_nodes(page, 'table') 
    table <- html_table(table, fill=TRUE) 
  #GJør om til dataframe
    dataframe <- list.stack(table)
    colnames(dataframe) <- dataframe[1,]
    dataframe <- dataframe %>% filter(!Dato=="Dato")
    dataframe <- dataframe %>% separate(Dato, 
                                into = c("Dag", "Dato"), 
                                sep = "(?<=[A-Za-z])(?=[0-9])")
    dataframe$Dato <- as.Date(dataframe$Dato, format="%d.%m.%Y")
    dataframe$Uke <- strftime(dataframe$Dato, format = "%V")
    #Legger til hvilke variabler vi vil fremstille
    dataframe <- dataframe %>% select(Dag,Dato,Uke,Tid,Rom,Emnekode,Lærer,Beskrivelse)
  return(dataframe)
  }
   map(fag, skrape)
  mintimeplan <- map(fag, skrape)
  mintimeplan <- bind_rows(mintimeplan)
  mintimeplan$Dag <- as.character(mintimeplan$Dag)
  # Setter inn manglende data i settet 
  mintimeplan <- mintimeplan %>% fill(c(Dag,Dato,Uke,Tid,Rom,Emnekode,Lærer,Beskrivelse))
#fjerner tomme koloner
  mintimeplan$Dag[mintimeplan$Dag ==""] <- NA
  


