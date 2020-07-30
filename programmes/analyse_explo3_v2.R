# chargement des packages
library(dplyr)
if (!("tidyverse" %in% installed.packages()[,"Package"])) install.packages("tidyverse")
library(tidyverse)
if (!("lubridate" %in% installed.packages()[,"Package"])) install.packages("lubridate")
library(lubridate)
if (!("forecast" %in% installed.packages()[,"Package"])) install.packages("forecast")
library(forecast)
if (!("ggfortify" %in% installed.packages()[,"Package"])) install.packages("ggfortify")
library(ggfortify)
if (!("gtools" %in% installed.packages()[,"Package"])) install.packages("gtools")
library(gtools)

# fonction notin
"%notin%" <- Negate("%in%")

table_A17 <- read_csv2("donnees/logements/tables_passage/table_A17.csv")

### emploi dans la construction ###
emploi_FZ <- readxl::read_xls("donnees/logements/brut_dep_a17_2020t1.xls") %>% 
  gather(-REG2016,-REG,-DEP,-A5,-A17, key = "trimestre", value = "nb_emp") %>% 
  mutate(
    periode = str_sub(trimestre, 4,9),
    annee = str_sub(periode,1,4),
    trimestre = str_sub(trimestre, 9,9)
  ) %>% 
  select(-REG) %>% rename(REG = REG2016) %>% 
  arrange(desc(annee), REG, DEP) %>% 
  filter(A17 == "FZ")  %>% 
  filter(REG == "75") %>% 
  group_by(periode) %>% 
  summarise(nb_emp = as.numeric(sum(nb_emp, na.rm = T)))

### nbre de logements commencés dans la région ###
log_com <- read_csv2("donnees/logements/log_com.csv") %>% #https://www.insee.fr/fr/statistiques/serie/001739904
  select(-Codes, periode = Libellé, logcom = `Nombre de logements commencés - Cumul sur douze mois - Total - Nouvelle-Aquitaine - Estimations en date réelle`) %>% 
  slice(-c(1:2)) %>% 
  separate(periode, c("annee", "mois"), sep ="-") %>% 
  filter(mois %in% c("03","06","09","12")) %>% 
  mutate(
    periode = case_when(
      mois == "03" ~ paste0(annee, "t1"),
      mois == "06" ~ paste0(annee, "t2"),
      mois == "09" ~ paste0(annee, "t3"),
      mois == "12" ~ paste0(annee, "t4"),
      TRUE ~ "Erreur !"
    ),
    logcom = as.numeric(logcom)
  ) %>% select(periode, logcom)

# periodes communes #
per_list <- intersect(log_com$periode ,emploi_FZ$periode)
per_start <- c(per_list %>% last() %>% str_sub(.,1,4),per_list %>% last() %>% str_sub(.,6,6)) %>% as.numeric()
# series temporelles, ça commence au T4 2001 et c'est par trimestres #
ts_logcom <- log_com %>% 
  filter(periode %in% per_list) %>% 
  select(logcom) %>% 
  ts(frequency = 4, start = per_start) 
ts_emploi_FZ <- emploi_FZ %>% 
  filter(periode %in% per_list) %>% 
  select(nb_emp) %>% 
  ts(frequency = 4, start = per_start) 

# visualisation des deux séries #
forecast::autoplot(cbind(ts_logcom, ts_emploi_FZ), xlab = "Année", ylab = "Nombre d'emplois et de logements commencés") 

# Cross correlation entre ts_logcom et ts_emploi_FZ #
forecast::Ccf(as.numeric(ts_logcom), as.numeric(ts_emploi_FZ))

# Correlation glissante, fenetre de 4 trimestres #
corr_FZ <- function(width){
  gtools::running(ts_logcom, ts_emploi_FZ, fun=cor, width = width) 
}

corr_FZ(4) %>% 
  enframe() %>% summarise(moyenne = mean(value, na.rm = T))

autoplot(cbind(ts_logcom, stats::lag(ts_emploi_FZ, -18)))

# Cross correlation entre ts_logcom et ts_emploi_FZ #
forecast::Ccf(as.numeric(ts_logcom), as.numeric(ts_emploi_FZ), type = "correlation", xlab = "toto", main = "tutu")


ts_emploi_FZ_decal <- stats::lag(ts_emploi_FZ, -18)
# periodes communes #
per_list <- intersect(log_com$periode ,ts_emploi_FZ_decal )

ts_emploi_FZ_decal <- ts(ts_emploi_FZ_decal,frequency = 4, start = c(2006,2) )

ts_emploi_FZ_decal$periode



### emploi dans la'hébergement-restauration ###
emploi_IZ <- readxl::read_xls("donnees/logements/brut_dep_a17_2020t1.xls") %>% 
  gather(-REG2016,-REG,-DEP,-A5,-A17, key = "trimestre", value = "nb_emp") %>% 
  mutate(
    periode = str_sub(trimestre, 4,9),
    annee = str_sub(periode,1,4),
    trimestre = str_sub(trimestre, 9,9)
  ) %>% 
  select(-REG) %>% rename(REG = REG2016) %>% 
  arrange(desc(annee), REG, DEP) %>% 
  filter(A17 == "IZ")  %>% 
  filter(REG == "75") %>% 
  group_by(periode) %>% 
  summarise(nb_emp = as.numeric(sum(nb_emp, na.rm = T)))

nuitees <- read_csv2("donnees/logements/nuitees.csv") %>% 
  select(1,hotels = 2, campings = 4) %>% 
  slice(-c(1:2)) %>%
  separate(Libellé, c("annee", "mois"), "-") %>% mutate_all(as.numeric) %>% 
  rowwise %>% 
  mutate(nuitees =  sum(hotels , campings, na.rm = T)) %>% 
  mutate(trimestre = case_when(
    mois < 4 ~ "1",
    mois < 7 ~ "2",
    mois < 10 ~ "3",
    TRUE ~ "4",
  )) %>% unite("periode", c(annee,trimestre), sep = "t") %>% 
  group_by(periode) %>% summarise(nuitees = sum(nuitees, na.rm = T)) %>% arrange(desc(periode))

# periodes communes #
per_list <- intersect(nuitees$periode ,emploi_IZ$periode)
per_start <- c(per_list %>% last() %>% str_sub(.,1,4),per_list %>% last() %>% str_sub(.,6,6)) %>% as.numeric()
# series temporelles, ça commence au T4 2001 et c'est par trimestres #
ts_nuitees <- nuitees %>% 
  filter(periode %in% per_list) %>% 
  select(nuitees) %>% 
  ts(frequency = 4, start = per_start)
ts_emploi_IZ <- emploi_IZ %>% 
  filter(periode %in% per_list) %>% 
  select(nb_emp) %>% 
  ts(frequency = 4, start = per_start) 

autoplot(cbind(ts_nuitees, ts_emploi_IZ))


## on decompose les series pour travailler sur le trend et sur les residus
ts_nuitees.dec <- decompose(ts_nuitees, type="add") # decompose the TS
autoplot(ts_nuitees.dec)  # view composantes

ts_emploi_IZ.dec <- decompose(ts_emploi_IZ, type="add") # decompose the TS
autoplot(ts_emploi_IZ.dec)  # view composantes

# les deux series
autoplot(list(ts_nuitees.dec, ts_emploi_IZ.dec))

# visualisation des residus
autoplot(cbind(ts_nuitees.dec$random, ts_emploi_IZ.dec$random), xlab = "Années", ylab = "Nombre d'emplois et nuitées")
plot(ts_nuitees.dec$random)
plot(ts_emploi_IZ.dec$random)

forecast::Ccf(as.numeric(ts_nuitees.dec$random), as.numeric(ts_emploi_IZ.dec$random), 
              type = "correlation", xlab = "Déphasage trimestriel", main = "Diagramme de correlation croisée",
              sub = "Nuitées hôtel+campings et l'emploi dans l'hébergement-restauration")
