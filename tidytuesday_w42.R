
library(tidyverse)
library(gganimate)
library(ggflags)
library(countrycode)
library(extrafont)
windowsFonts("Bahnschrift" = windowsFont("Bahnschrift"))

df1 <- readr::read_csv("income_per_person_gdppercapita_ppp_inflation_adjusted.csv")
df1 <- as.data.frame(df1)

df1 <- df1 %>% 
  gather(key = "year", value = "income", 2:252) %>% 
  filter(country %in% c("Spain", "Italy", "Germany",
                        "Belgium", "Luxembourg", "Netherlands",
                        "United Kingdom", "Portugal", "Greece",
                        "Austria", "Ireland", "Denmark",
                        "Finland", "Sweden", "France",
                        "Norway", "Iceland"),
         year >1960 & year <2018) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(income = case_when(
    str_detect(income, "k") ~ as.numeric(str_extract(income, "[\\d\\.]+")) * 1000,
    TRUE ~ as.numeric(income)
  ))


captured <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fishery-production.csv')
consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-and-seafood-consumption-per-capita.csv')

df2 <- captured

df2 <- df2 %>% 
  filter(Entity %in% c("Spain", "Italy", "Germany",
                       "Belgium", "Luxembourg", "Netherlands",
                       "United Kingdom", "Portugal", "Greece",
                       "Austria", "Ireland", "Denmark",
                       "Finland", "Sweden", "France",
                       "Norway", "Iceland")) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  rename(country = Entity,
         year = Year)

df3 <- consumption %>% 
  filter(Entity %in% c("Spain", "Italy", "Germany",
                       "Belgium", "Luxembourg", "Netherlands",
                       "United Kingdom", "Portugal", "Greece",
                       "Austria", "Ireland", "Denmark",
                       "Finland", "Sweden", "France",
                       "Norway", "Iceland")) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  rename(country = Entity,
         year = Year)


df <- df1 %>% 
  left_join(df2, by = c("year", "country"))

df <- df %>% 
  left_join(df3, by = c("year", "country"))

df



df$country <- countrycode(df$country, "country.name", "iso2c")
df <- df  %>%
  mutate(country = str_to_lower(country)) 

my_theme <- theme(text =element_text(family = "Bahnschrift", face = "bold"),
                  plot.background = element_rect(fill = "gray95"),
                  panel.background = element_rect(fill = "gray95"),
                  panel.grid = element_line(color = "gray80"),
                  panel.spacing = unit(2.5, units = "cm"),
                  plot.title = element_text(family = "Bahnschrift", size = 10, hjust = 0.5, color = "black"),
                  plot.subtitle = element_text(family = "Bahnschrift", size = 10),
                  plot.caption = element_text(color = "black", size = 10),
                  legend.background = element_rect(fill = "gray95"),
                  legend.position = "top", 
                  axis.text.x = element_text(vjust = 0.5, hjust=1, size = 12, color = "black"),
                  axis.text.y = element_text(vjust = 0.5, hjust=1, size = 12, color = "black")) 




p1 <- df %>% 
  ggplot( 
    aes(x = `Capture fisheries production (metric tons)`, 
        y = `Fish, Seafood- Food supply quantity (kg/capita/yr) (FAO, 2020)`, 
        color = country, 
        size = income)) +
  geom_point(alpha = .6) +
  geom_flag(data = df,
            aes(country = country)) +
  labs(title = "Food supply in fish in kg/capita/year,\nby Income per person in each country,\nand Capture fishery production : {frame_time}",
       subtitle = "EU 15, Iceland and Norway",
       caption = "#Tidytuesday W42 | Source: Ourworldindata.org & gapminder.org | @dataR_amateur") +
  transition_time(as.integer(year)) +
  my_theme + 
  guides(fill = guide_legend(override.aes = list(color = NA)), 
         color = FALSE)  
p1  

anim_save("p1.gif", animation = last_animation())
