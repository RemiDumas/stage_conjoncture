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

# series temporelles, ça commence au T4 2001 et c'est par trimestres #
ts_logcom <- log_com %>% 
  filter(periode %in% per_list) %>% 
  select(logcom) %>% 
  ts(frequency = 4, start = c(2001,4)) 
ts_emploi_FZ <- emploi_FZ %>% 
  filter(periode %in% per_list) %>% 
  select(nb_emp) %>% 
  ts(frequency = 4, start = c(2001,4)) 

# visualisation des deux séries #
forecast::autoplot(cbind(ts_logcom, ts_emploi_FZ)) 

# Cross correlation entre ts_logcom et ts_emploi_FZ #
forecast::Ccf(as.numeric(ts_logcom), as.numeric(ts_emploi_FZ))

# Correlation glissante, fenetre de 4 trimestres #
gtools::running(ts_logcom, ts_emploi_FZ, fun=cor, width=4, by=1, allow.fewer=TRUE, align=c("right"), simplify=TRUE) %>% 
  enframe() %>% 
  knitr::kable()


cor_gliss <- function(k){
  tab <- gtools::running(ts_logcom, ts_emploi_FZ, fun=cor, width=k, by=1, allow.fewer=TRUE, align=c("right"), simplify=TRUE) %>% 
    enframe() %>% as.data.frame
  return(tab[1:nrow(tab),2])
}

cor_gliss_mean <- function(k){
  return(mean(cor_gliss(k), na.rm=T))
}

X <- 2:18
Y <- sapply(X, cor_gliss_mean)





