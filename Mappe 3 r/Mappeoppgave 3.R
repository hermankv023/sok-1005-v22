library(data.table)
library(rjson)
library(tidyverse)
library(ggrepel)
library(ggeasy)

# Oppgave 1 

data <- fromJSON(file = "https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json")

covidnumbers <- do.call(rbind, data) %>% 
  as.data.frame

covidnumbers$fully_vaccinated_pct_of_pop = as.numeric(covidnumbers$fully_vaccinated_pct_of_pop)
covidnumbers$deaths_per_100k = as.numeric(covidnumbers$deaths_per_100k)
covidnumbers$name = as.character(covidnumbers$name)

covidnumbers$name <- state.abb[match(covidnumbers$name, state.name)]


covidnumbers <- covidnumbers %>% 
  mutate(percent_vac = fully_vaccinated_pct_of_pop * 100)

covidnumbers$name <- state.abb[match(covidnumbers$name, state.name)]


covidnumbers %>%
  ggplot(aes(x = fully_vaccinated_pct_of_pop, y = deaths_per_100k, label=name), col = "green") +
  geom_point() +
  labs("Covid-19 deaths since universal adult vaccine eligibility compared with \n vaccination rates",
       x= "Prosen fullvaksinert",
       y= "Dødsrate") +
  scale_x_continuous(labels=scales::percent, 
                     breaks = seq(from = .45, to = .81, by = 0.05)) +
  geom_label_repel(label.size = 0,
                   label.padding = 0,
                   label.r = 0,
  )  +
  theme_gray() +
  annotate(geom="text", x=0.54, y=17, 
           label=" 🢆Lower Vaccination rate,\n13 higher death rate") +
  annotate(geom="text", x=0.78, y=8, 
           label="🢆 Higer Vaccination rate,\n13 Lower death rate")

# Oppgave 2

lm(deaths_per_100k ~ fully_vaccinated_pct_of_pop, data = covidnumbers)


covidnumbers %>%
  ggplot(aes(x = fully_vaccinated_pct_of_pop, y = deaths_per_100k, label=name), col = "green") +
  geom_point() +
  geom_smooth(method = lm) +
  scale_x_continuous(labels=scales::percent, 
                     breaks = seq(from = .45, to = .81, by = 0.05)) +
  geom_label_repel(label.size = 0,
                   label.padding = 0,
                   label.r = 0,
  )  +
  geom_point() + 
  labs(title ="penis") + 
  theme_bw() +
  annotate(geom="text", x=0.54, y=17, 
           label=" 🢆Lower Vaccination rate,\n13 higher death rate") +
  annotate(geom="text", x=0.78, y=8, 
           label="🢆 Higer Vaccination rate,\n13 Lower death rate")