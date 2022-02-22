  library(rvest)
  library(tidyverse)
  #Jobbet med Amund og
  #henter inn nettside 
  nettside <- read_html("https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132")
  tabell <- nettside %>% html_table(fill = TRUE)
  bil <- tabell[[1]]
  bil <-bil[-1,]
  colnames(bil) <- c("Modell (temp. varierte fra 0° til -10°)", "WLTP", "STOPP", "Avvik")
  #Gjør bil numeric 
  bil$STOPP <- substr(bil$STOPP, 0, 3) %>% 
    as.numeric(bil$STOPP) 
  bil$WLTP <- substr(bil$WLTP, 0, 3) %>% 
    as.numeric(bil$WLTP)
  # Lager plottet 
  car1 <-  bil %>%
    ggplot(aes(x =WLTP, y = STOPP)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1.03, col="red")+
    xlim(200,600)+ylim(200,600)+
  theme_bw()
   
 #OPpgave 2
  car <- lm(STOPP ~ WLTP, data = bil)
  
  summary(car)
  car1 + geom_smooth(method=lm, aes(x=WLTP, y=STOPP))
  
  #ut ifra dataene kan vi se at coefficienten er negativ på esimatet. Dette betyr at den rekeviden bilselvskapet har markedsført med er mindre en den faktiske rekeviden som tabellen viser 
 
  