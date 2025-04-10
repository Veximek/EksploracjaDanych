#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("googlesheet2")
#install.packages('ggalt')
library("googlesheets4")
library("ggplot2")
library("tidyverse")
library("ggalt")
googlesheets4::gs4_auth()
auth_key_it = "1Go0MtbrKwvzMOzGyN_WeUbBHSbJRxRKVen6J23VD4lI"
auth_key_ng = "13Dd-p50FWtD1YR-sFeiudypReyy7gFNRwhIiTd7BktQ"
auth_key_us = "1_yeiClAqUlG_t5PgEPM0QBK99_AXi3qe_HAKrIQ_8Ic"
auth_key_ke = "1Lvk9ypLzSYUpFObdX7AArtNPqMxvQY2HgDp5Riqv6Mg"
auth_key_is = "1l9Cs5pYoi9xwjabYWgYHYpRLQAUbGIaY-6-rm-JwYs0"
auth_key_no = "1_4RmpWROCsIts-hehbBj-D1o94niebCQn7OVoRRXIzE"
auth_key_hp = "1aTanBJEmU6VakUkA-MeHTmlkfgz-N7MybWktwBwspI8"
auth_key_fr = "17lZqivv-ud0qA5qdvZjguvBge9f0IDWTywp0lOhX3zU"
auth_key_it = "1Go0MtbrKwvzMOzGyN_WeUbBHSbJRxRKVen6J23VD4lI"
auth_key_por = "1IV2afABGbXSIv2Kzjz1VrmR63JFzcLAesnxIuxdO9h8"
auth_key_br = "1BxrtmkOzMCipu3_B7vpcIk5rFQtZ1ALRCuS5O1fZ0fk"
auth_key_swe = "1y9c73WoFrzHy1ajPNoJI9ZWue7y20lLFmguZoFtpvns"
auth_key_tjk = "1O_GnUPEdCELxoiyczK_iYu5xxq0DZAZCQ4R4ZiLPYcs"
auth_key_nl = "162KyAYsaNpyhMl6P3qI6eF7gVVfrwklBlbFy4JBOOMc"
auth_key_by = "1oo8quoJGgIipGWuQdY5wJA45TErD_ucVvsBY9d56Yz8"
auth_key_fn = "1GodbkmR3YPiH5L2J3mQwV7Uey9vt39ipDQT_EpnINGs"
auth_key_ukr = "1HtT7TestjDUv4LRoi3JDX7fFoAqH33fxCRzNu_vMnDw"


data_it = read_sheet(auth_key_it, sheet="source")
data_ng = read_sheet(auth_key_ng, sheet="source")
data_us = read_sheet(auth_key_us, sheet="source")
data_ke = read_sheet(auth_key_ke, sheet="source")
data_is = read_sheet(auth_key_is, sheet="source")
data_no = read_sheet(auth_key_no, sheet="Norwegia")
data_hp = read_sheet(auth_key_hp, sheet="Arkusz1")
data_fr = read_sheet(auth_key_fr, sheet="source")
data_it = read_sheet(auth_key_it, sheet="source")
data_por = read_sheet(auth_key_por, sheet="source")
data_br= read_sheet(auth_key_br, sheet="source")
data_swe= read_sheet(auth_key_swe, sheet="source")
data_tjk = read_sheet(auth_key_tjk, sheet="source")
data_nl = read_sheet(auth_key_nl, sheet="source")
data_by = read_sheet(auth_key_by, sheet="source")
data_fn = read_sheet(auth_key_fn, sheet="source")
data_ukr = read_sheet(auth_key_ukr, sheet="source")



#auth_key = "1MQasKckL1ignNefiiRuvSpm2oKIyA2CbqtDAFf4yWhY"
#data_keys = read_sheet(auth_key, sheet="GR_1630")

#data_keys$`spreadsheet key`

#countrys_data = c()

#for(i in data_keys$`spreadsheet key`){
#  countrys_data = c(countrys_data, read_sheet(i, sheet="source"))
#}
#data_br = read_sheet(auth_key, sheet="source")

data_it = data_it %>% mutate(year = case_when(
  year == 2015 ~ 2025,
  year == 1925 ~1925,
))
data_br = data_br %>%  mutate(education = case_when(
  education == "Podstawowe" ~ 1,
  education == "Srednie" ~ 2,
  education == "Wyzsze" ~ 3,
))

data_by = data_by %>% rename(education = education_level)
data_by$country = "BY"
data_by = data_by %>% mutate(education = case_when(
  education == "Secondary" ~ 2,
  education == "Higher" ~ 3,
))

data_fn = data_fn %>% rename(education = education_level)

data_fn = data_fn %>% mutate(education = case_when(
  education == "Secondary" ~ 2,
  education == "Higher" ~ 3,
))

data_fr = data_fr %>% select(-iq)
data_fr = data_fr %>%  mutate(education = case_when(
  education == "Podstawowe" ~ 1,
  education == "Średnie" ~ 2,
  education == "Wyższe" ~ 3,
))

data_hp = data_hp %>%  mutate(education = case_when(
  education == "podstawowy" ~ 1,
  education == "sredni" ~ 2,
  education == "wyszy" ~ 3,
))
data_hp$year = sample(c(1925,2025), 1000,replace=TRUE)

data_is = data_is %>% mutate(education = case_when(
  education == "primary" ~ 1,
  education == "secondary" ~ 2,
  education == "higher" ~ 3,
))

data_ke = data_ke %>% select(-IQ)
data_ke = data_ke %>% rename(education = Wykształcenie)
data_ke = data_ke %>% rename(year = Rok)
data_ke = data_ke %>% rename(sex = Płeć)
data_ke = data_ke %>% rename(height = Wzrost)
data_ke = data_ke %>% rename(weight = Waga)

data_ke = data_ke %>% mutate(education = case_when(
  education == "Brak formalnego" ~0,
  education == "Podstawowe" ~ 1,
  education == "Średnie" ~ 2,
  education == "Wyższe" ~ 3,
))
data_ke = data_ke %>% mutate(sex = case_when(
  sex== "Mężczyzna" ~0,
  sex == "Kobieta" ~ 1,
))
data_ke$country = "KE"
data_ke$age = sample(18:60, 200, replace=TRUE)

data_ng = data_ng %>% select(-IQ)
data_ng = data_ng %>% rename(education = Wykształcenie)
data_ng = data_ng %>% rename(year = Rok)
data_ng = data_ng %>% rename(sex = Płeć)
data_ng = data_ng %>% rename(height = Wzrost)
data_ng = data_ng %>% rename(weight = Waga)
data_ng = data_ng %>% rename(age = Wiek)


data_ng = data_ng %>% mutate(education = case_when(
  education == "Brak formalnego" ~0,
  education == "Podstawowe" ~ 1,
  education == "Średnie" ~ 2,
  education == "Wyższe" ~ 3,
))
data_ng = data_ng %>% mutate(sex = case_when(
  sex== "Mężczyzna" ~0,
  sex == "Kobieta" ~ 1,
))

data_ng$country = "NG"

data_nl = data_nl %>% mutate(education = case_when(
  education == "Brak formalnego" ~0,
  education == "Podstawowe" ~ 1,
  education == "Srednie" ~ 2,
  education == "Wyzsze" ~ 3,
))

data_por = data_por %>% mutate(education = case_when(
  education == "Brak formalnego" ~0,
  education == "Podstawowy" ~ 1,
  education == "Średni" ~ 2,
  education == "Wyższy" ~ 3,
))

data_swe = data_swe %>% mutate(education = case_when(
  education == "Brak formalnego" ~0,
  education == "L" ~ 1,
  education == "M" ~ 2,
  education == "H" ~ 3,
))

data_tjk = data_tjk %>% rename(education = edu)

data_ukr = data_ukr %>% select(-ID)
data_ukr = data_ukr %>% rename(education = Education)
data_ukr$year = sample(c(1925,2025), 100, replace=TRUE)
data_ukr$sex = sample(c(0,1),100,replace=TRUE)
data_ukr = data_ukr %>% rename(height = Height)
data_ukr = data_ukr %>% rename(weight = Weight)
data_ukr = data_ukr %>% rename(age = Age)
data_ukr = data_ukr %>% rename(country = Country)

data_ukr = data_ukr %>% mutate(education = case_when(
  education == "Primary" ~ 1,
  education == "Secondary" ~ 2,
  education == "Higher" ~ 3,
))

data_combined = union(data_br, data_by)
data_combined = union(data_combined,data_fn)
data_combined = union(data_combined,data_fr)
data_combined = union(data_combined, data_hp)
data_combined = union(data_combined, data_is)
data_combined = union(data_combined, data_it)
data_combined = union(data_combined, data_ke)
data_combined = union(data_combined, data_ng)
data_combined = union(data_combined, data_nl)
data_combined = union(data_combined, data_no)
data_combined = union(data_combined, data_por)
data_combined = union(data_combined, data_swe)
data_combined = union(data_combined, data_tjk)
data_combined = union(data_combined, data_ukr)
data_combined = union(data_combined, data_us)


#data_combined = bind_rows(data_it, data_br)

#ggplot(data_combined) +
#  aes(weight, height, color=country)+
#  geom_point(alpha = .5)  

#ggplot(data_combined) +
#  aes(weight, height, color=country)+
#  geom_boxplot()  

#ggplot(data_combined) +
#  aes(weight, height, color=country)+
#  geom_col()  

#ggplot(data_combined) +
#  aes(year, education, color=country)+
#  geom_count()  


data_avg_height = data_combined %>%
    group_by(country,year) %>%
    summarise(avg_height = mean(height, na.rm=TRUE), .groups = 'drop') 


data_wide = data_avg_height %>%
  pivot_wider(names_from = year, values_from = avg_height, names_prefix = 'year_')

ggplot(data_wide) +
geom_segment(aes(x = year_2025, 
                 xend = year_1925, 
                 y = country, 
                 yend = country), 
             color = "darkgrey", size = 1.5,
             arrow = arrow(length = unit(0.03, "npc"))) +  # Strzałka
  geom_point(aes(x = year_2025, y = country), 
             color = "blue", size = 4) +
  geom_point(aes(x = year_1925, y = country), color = "red", size = 4) +
  geom_text(aes(x = year_1925, y = country, label = round(year_1925, 1)), 
            hjust = 1.2, color = "red", size = 4, vjust = -0.5) +
  geom_text(aes(x = year_2025, y = country, label = round(year_2025, 1)), 
            hjust = -0.2, color = "blue", size = 4, vjust = 0.5)
labs(title = "Change in Average Height Over Time",
     subtitle = "1925 vs 2025 for different countries over world",
     x = "Average Height (cm)", 
     y = "Country") +
  theme_minimal()+
theme(panel.grid.major.x = element_line(color = "gray", linetype = "dashed"))
