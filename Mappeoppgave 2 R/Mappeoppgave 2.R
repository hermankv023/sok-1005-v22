#Jobbet sammen med Amund Bech og Knut Bakken
#Laster inn pakker
library(tidyverse)
library(data.table)
library(jsonlite)
library(ggeasy)
library(ggrepel)

#Oppgave 1, repliserer data til New York Times database
Covidstats <- fromJSON("https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json")

#endrer på datasettet for å få prosent og riktige statnavn, legger også inn DC
Covidstats <- Covidstats %>% 
  mutate(percent_vac = fully_vaccinated_pct_of_pop * 100)
Covidstats$name <- state.abb[match(Covidstats$name, state.name)]
Covidstats[is.na(Covidstats)] <- "DC"

#fremviser graf med navn på akser
Covidstats %>%
  ggplot(aes(x = fully_vaccinated_pct_of_pop, y = deaths_per_100k, label=name)) +
  geom_point(color="turquoise3", size=1.7) +
  labs("Covid-19 deaths since universal adult vaccine eligibility compared with vaccination rates",
       x= "Share of total population fully vaccinated",
       y= "20 avg. monthly deaths per 100,100") +
  scale_x_continuous(labels=scales::percent, 
                     breaks = seq(from = .45, to = 1, by = 0.05)) +
  geom_label_repel(label.size = 0.1,
                   label.padding =0.1,
                   label.r = 0,
  )  +
  theme_gray() +
  annotate(geom="text", x=0.73, y=8, 
           label="Higer Vaccination rate,
           Lower death rate") +
  annotate(geom="text", x=0.60, y=17, 
           label="Lower Vaccination rate,higher death rate") 

# Oppgave 2
# kjører lm og finner coefficients

lm(deaths_per_100k ~ fully_vaccinated_pct_of_pop, data = Covidstats)
#Fremviser graf med utgangspunkt i tabell med en linje som fremviser en klar sammenheng mellom døde og vaksinasjonsrate.
Covidstats %>%
  ggplot(aes(x = fully_vaccinated_pct_of_pop, y = deaths_per_100k, label=name)) +
  geom_point(color="turquoise3", size=0.5) +
  geom_smooth(method = lm, se=FALSE, fullrange=TRUE)+
  scale_x_continuous(labels=scales::percent, 
                     breaks = seq(from = .45, to = 1, by = 0.05)) +
  geom_label_repel(label.size = 0.0,
                   label.padding = 0.0,
                   label.r = 0,
  )  +
  geom_point(color="turquoise3", size=0.25) + 
  labs(title ="Covidstats for USA ") + 
  theme_bw() +
  annotate(geom="text", x=0.73, y=8, 
           label="Higer Vaccination rate,
           Lower death rate") +
  annotate(geom="text", x=0.60, y=17, 
           label="Lower Vaccination rate,higher death rate") +
  theme(panel.grid.minor = element_line(linetype="dashed")) +
  theme(panel.grid.major = element_line(linetype="dashed")) 

#Hva ser vi?
#Det vi kan få ut fra oppgave 2 er at vi ser det er en korrelasjon mellom vaksinasjon av populasjon og antall døde av Corona. 
#VI ser derimot at det er noen unntak som f.eks Florida som har en ganske så mye død, men også relativt høy vaksinasjon av befolkingen.
#Det kan kanskje være en sammenheng mellom hvor åpent de ulike statene har hatt det og død, som f.eks tx og fl har hatt det ganske åpent over en lengere tid mens DC har hatt det ganske strengt  
