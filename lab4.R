#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("googlesheet2")
#install.packages('ggalt')
library("googlesheets4")
library("ggplot2")
library("tidyverse")
library("ggalt")
googlesheets4::gs4_auth()
auth_key_br = "1BxrtmkOzMCipu3_B7vpcIk5rFQtZ1ALRCuS5O1fZ0fk"
auth_key_it = "1Go0MtbrKwvzMOzGyN_WeUbBHSbJRxRKVen6J23VD4lI"
data_br = read_sheet(auth_key, sheet="source")

data_br = data_br %>%  mutate(education = case_when(
  education == "Podstawowe" ~ 1,
  education == "Srednie" ~ 2,
  education == "Wyzsze" ~ 3,
))

data_it = read_sheet("1Go0MtbrKwvzMOzGyN_WeUbBHSbJRxRKVen6J23VD4lI", sheet="source")

data_combined = bind_rows(data_it, data_br)

ggplot(data_combined) +
  aes(weight, height, color=country)+
  geom_point(alpha = .5)  

ggplot(data_combined) +
  aes(weight, height, color=country)+
  geom_boxplot()  

ggplot(data_combined) +
  aes(weight, height, color=country)+
  geom_col()  

ggplot(data_combined) +
  aes(year, education, color=country)+
  geom_count()  


data_avg_height = data_combined %>%
    group_by(country,year) %>%
    summarise(avg_height = mean(height, na.rm=TRUE), .groups = 'drop') %>%
  filter((country == "IT" & year %in% c(1925, 2015)) | 
           (country == "BR" & year %in% c(1925, 2025)))

data_wide = data_avg_height %>%
  pivot_wider(names_from = year, values_from = avg_height, names_prefix = 'year_')

ggplot(data_wide) +
geom_segment(aes(x = ifelse(country == "IT", year_2015, year_2025), 
                 xend = year_1925, 
                 y = country, 
                 yend = country), 
             color = "darkgrey", size = 1.5,
             arrow = arrow(length = unit(0.02, "npc"))) +  # Strzałka
  geom_point(aes(x = ifelse(country == "IT", year_2015, year_2025), y = country), 
             color = "blue", size = 4) +
  geom_point(aes(x = year_1925, y = country), color = "red", size = 4) +
  geom_text(aes(x = year_1925, y = country, label = round(year_1925, 1)), 
            hjust = 0.5, color = "red", size = 5, vjust= -1.5) +
  geom_text(aes(x = ifelse(country == "IT", year_2015, year_2025), y = country, 
                label = round(ifelse(country == "IT", year_2015, year_2025), 1)), 
            hjust = 0, color = "blue", size = 5, vjust= -1.5) +
labs(title = "Change in Average Height Over Time",
     subtitle = "1925 vs 2015 for Italy, 1925 vs 2025 for Brazil",
     x = "Average Height (cm)", 
     y = "Country") +
  theme_minimal()
