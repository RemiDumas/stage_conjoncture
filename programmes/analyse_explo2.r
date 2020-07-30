rm(list=ls())
library(dplyr)
source("programmes/fonctions.r", encoding = "UTF-8")
library(stringr)
library(tidyverse)
install.packages("corrplot")
library(corrplot)


# tables de passage
table_dep <- readr::read_csv("tables_passage/departement2019.csv")
table_reg <- readr::read_csv("tables_passage/region2019.csv")
table_secteur <- readr::read_csv2("tables_passage/table_a17.csv")

# chargement des ETE

data <- readxl::read_xls("donnees/brut_dep_a17_2020t1.xls") %>% 
  gather("trimestre", "nb_emp",starts_with("eff")) %>% 
  select(-REG) %>% 
  rename(REG = REG2016) %>% 
  mutate(
    periode = str_sub(trimestre,4,9),
    annee = as.numeric(str_sub(periode,1,4)),
    trimestre = str_to_upper(str_sub(periode,5,6))
  )

data75 <- data %>% filter(REG == "75", annee > 2010) %>% group_by(periode) %>% 
  summarise(nb_emp = sum(nb_emp, na.rm = T))
plot(data75$nb_emp)

result_acf  <-  acf(data75$nb_emp)










# chargement des communes (ancienne version)
communes <- read.csv("donnees/communes/communes_france.csv", header=FALSE) 
colnames(communes) <- c("id","dept","nom_ville_minusc_tirets","nom_ville_majusc_tirets","nom_ville_minusc_sans_tirets","nom_ville_normal","nom_soundex","nom_metaphone","code_postal","num_commune","code_insee","arrondissement","canton","ville_amdi","population_2010","population_1999","population_2012_approx_centaine","densite_hab_par_km2","surface_km2","longitude_deg","latitude_deg","longitude_grd","latitude_grd","longitude_dms","latitude_dms","altitude_min","altitude_max")
communes <- communes %>% mutate(code_postal = as.character(code_postal)) %>% mutate(code_postal = as.numeric(code_postal))
communes <- communes %>% group_by(dept) %>% mutate(popul_relative = population_2010/sum(population_2010))

# Chargement de la base ville
villes <- readxl::read_xls("donnees/ville.xls")

# chargement des communes (nouvelle version)
communes_v2 <- read.csv("donnees/communes/communes_v2.csv", sep = ";", header=TRUE)
communes_v2 <- communes_v2 %>% mutate(code_insee = DEPCOM, nom = COM, population = PTOT) %>% select(-PMUN, -PCAP, -DEPCOM, -COM, -PTOT)
communes_v2 <- communes_v2 %>% mutate(code_insee = code_insee %>% as.character() %>% as.numeric())
communes_v2 <- communes_v2 %>% mutate(dept = as.character(floor(as.numeric(communes_v2$code_insee)/1000)))
communes_v2 <- communes_v2 %>% group_by(dept) %>% mutate(popul_relative = population/sum(population))
communes_v2 <- communes_v2 %>% mutate(longitude = villes$Longitude, latitude = villes$Latitude)


#######
df <- tibble(a = c("3", "4", "5"))
b <- 2

data2 <- data %>% 
  group_by(Région) %>% 
  mutate(Max2019 = max(eff2019t4)) %>% 
  select(Région, Département, eff2019t4, Max2019)

#######

ete <- data %>% 
  gather(key = "periode", value = "nb_emplois", -REG, -DEP, -A5, -A17) %>% 
  mutate(
    Annee = str_sub(periode, 4, 7),
    Trimestre = periode %>% str_sub(8,9) %>% str_to_upper
  ) %>% 
  select(-periode) %>% mutate(Periode = paste(Annee, Trimestre, sep = "-"))

# On restreint le champ car l'emploi public n'est pris en compte qu'à partir de 2010

ete_restreint <- ete %>% filter(A17!="OQ") %>%  mutate(Annee = as.numeric(Annee))

ete_restreint_ap_2010 <- ete_restreint %>% filter(Annee > 2010, REG == "75") %>%  mutate(Annee = as.character(Annee))

# secteur MN0 dans chaque département

ete_restreint_ap_2010_MN0_16 <- ete_restreint_ap_2010 %>% filter(A17=='MN0', DEP=='16')
ete_restreint_ap_2010_MN0_17 <- ete_restreint_ap_2010 %>% filter(A17=='MN0', DEP=='17')
ete_restreint_ap_2010_MN0_19 <- ete_restreint_ap_2010 %>% filter(A17=='MN0', DEP=='19')
ete_restreint_ap_2010_MN0_23 <- ete_restreint_ap_2010 %>% filter(A17=='MN0', DEP=='23')
ete_restreint_ap_2010_MN0_24 <- ete_restreint_ap_2010 %>% filter(A17=='MN0', DEP=='24')
ete_restreint_ap_2010_MN0_33 <- ete_restreint_ap_2010 %>% filter(A17=='MN0', DEP=='33')
ete_restreint_ap_2010_MN0_40 <- ete_restreint_ap_2010 %>% filter(A17=='MN0', DEP=='40')
ete_restreint_ap_2010_MN0_47 <- ete_restreint_ap_2010 %>% filter(A17=='MN0', DEP=='47')
ete_restreint_ap_2010_MN0_64 <- ete_restreint_ap_2010 %>% filter(A17=='MN0', DEP=='64')
ete_restreint_ap_2010_MN0_79 <- ete_restreint_ap_2010 %>% filter(A17=='MN0', DEP=='79')
ete_restreint_ap_2010_MN0_86 <- ete_restreint_ap_2010 %>% filter(A17=='MN0', DEP=='86')
ete_restreint_ap_2010_MN0_87 <- ete_restreint_ap_2010 %>% filter(A17=='MN0', DEP=='87')

emp_MN0_16 <- as.list(ete_restreint_ap_2010_MN0_16)$nb_emplois
emp_MN0_17 <- as.list(ete_restreint_ap_2010_MN0_17)$nb_emplois
emp_MN0_19 <- as.list(ete_restreint_ap_2010_MN0_19)$nb_emplois
emp_MN0_23 <- as.list(ete_restreint_ap_2010_MN0_23)$nb_emplois
emp_MN0_24 <- as.list(ete_restreint_ap_2010_MN0_24)$nb_emplois
emp_MN0_33 <- as.list(ete_restreint_ap_2010_MN0_33)$nb_emplois
emp_MN0_40 <- as.list(ete_restreint_ap_2010_MN0_40)$nb_emplois
emp_MN0_47 <- as.list(ete_restreint_ap_2010_MN0_47)$nb_emplois
emp_MN0_64 <- as.list(ete_restreint_ap_2010_MN0_64)$nb_emplois
emp_MN0_79 <- as.list(ete_restreint_ap_2010_MN0_79)$nb_emplois
emp_MN0_86 <- as.list(ete_restreint_ap_2010_MN0_86)$nb_emplois
emp_MN0_87 <- as.list(ete_restreint_ap_2010_MN0_87)$nb_emplois



communes_du_16 <- communes[which(floor(communes$code_postal/1000)==16),]
communes_du_17 <- communes[which(floor(communes$code_postal/1000)==17),]
communes_du_19 <- communes[which(floor(communes$code_postal/1000)==19),]
communes_du_23 <- communes[which(floor(communes$code_postal/1000)==23),]
communes_du_24 <- communes[which(floor(communes$code_postal/1000)==24),]
communes_du_33 <- communes[which(floor(communes$code_postal/1000)==33),]
communes_du_40 <- communes[which(floor(communes$code_postal/1000)==40),]
communes_du_47 <- communes[which(floor(communes$code_postal/1000)==47),]
communes_du_64 <- communes[which(floor(communes$code_postal/1000)==64),]
communes_du_79 <- communes[which(floor(communes$code_postal/1000)==79),]
communes_du_86 <- communes[which(floor(communes$code_postal/1000)==86),]
communes_du_87 <- communes[which(floor(communes$code_postal/1000)==87),]


ete_75_C1_ann <- ete %>% filter(REG == "75", A17 == "C1", Trimestre == "T4") %>% 
  group_by(DEP) %>% 
  mutate(
    evol_nb = nb_emplois - lag(nb_emplois),
    evol_tx = round(100 * evol_nb / lag(nb_emplois) , 1)
  )

ete_75_C1_ann %>% 
  select(DEP, Annee, evol_tx) %>% 
  spread(Annee, evol_tx)


ete2 <- ete %>% 
  mutate(Periode = paste(Annee, Trimestre, sep = "-"))


#########################  TRACES GRAPHIQUES  #################################

# Emploi en Nouvelle-Aquitaine entre 2011 et 2019
# Une répartition hétérogène de l'emploi par secteur d'activité 
ggplot(ete_restreint_ap_2010) +
  aes(x = A17, y = nb_emplois) +
  geom_boxplot(fill = "#0c4c8a") +
  labs(x = "Secteur", y = "Nombre d'emplois en fin de chaque trimestre") +
  theme_minimal()




#########################  EXPLOITATION DES DONNEES  #################################

### Recherche de retards et d'avance entre 2 courbes


# Ecart quadratique moyen entre 2 séries de données

d <- function(x,y){
  if (length(x)!=length(y)){warning("Longueurs différentes entre 2 séries dont on calcule l'écart quadratique moyen")}
  return(mean((x-y)^2))
}

# Décalage des séries l'une par rapport à l'autre d'au maximum une demi longueur dans les 2 sens

ajust_decal_series <- function(x,y){
  if (length(x)!=length(y)){warning("Longueurs différentes entre 2 séries qu'on cherche à comparer")}
  n <- length(x)-1
  tab <- (-n):n
  vect <- c()
  for (k in n:0){
    vect <- append(vect, d(y[(k+1):length(y)], x[1:(length(x)-k)]))
  }
  for (k in 1:n){
    vect <- append(vect, d(y[1:(length(y)-k)], x[(k+1): length(x)]))
  }
  cbind(tab,vect)
  m <- min(vect)
  return(c(tab[which(vect == m)],log(m/10000)))
}

d_communes <- function(nom1,nom2){
  com1 <- communes[which(communes$nom_ville_minusc_tirets == nom1),]
  com2 <- communes[which(communes$nom_ville_minusc_tirets == nom2),]
  tmp <- (com1$longitude_deg*cos(pi*com1$latitude_deg/180)-com2$longitude_deg*cos(pi*com2$latitude_deg/180))^2+(com1$latitude_deg-com2$latitude_deg)^2
  return(pi*6371.009*sqrt(tmp)/180)
}

d_communes_v2 <- function(nom1,nom2){
  com1 <- communes_v2[which(communes_v2$nom_ville_minusc_tirets == nom1),]
  com2 <- communes_v2[which(communes_v2$nom_ville_minusc_tirets == nom2),]
  tmp <- (com1$longitude_deg*cos(pi*com1$latitude_deg/180)-com2$longitude_deg*cos(pi*com2$latitude_deg/180))^2+(com1$latitude_deg-com2$latitude_deg)^2
  return(pi*6371.009*sqrt(tmp)/180)
}

d_dep <- function(num1, num2){
  dep1 <- communes %>% filter(dept == num1)
  dep2 <- communes %>% filter(dept == num2)
  longitude1 <- sum(dep1$longitude_deg*dep1$popul_relative)
  latitude1 <- sum(dep1$latitude_deg*dep1$popul_relative)
  longitude2 <- sum(dep2$longitude_deg*dep2$popul_relative)
  latitude2 <- sum(dep2$latitude_deg*dep2$popul_relative)
  tmp <- (longitude1*cos(pi*latitude1/180)-longitude2*cos(pi*latitude2/180))^2+(latitude1-latitude2)^2
  return(pi*6371.009*sqrt(tmp)/180)
}

d_dep_v2 <- function(num1, num2){
  dep1 <- communes_v2 %>% filter(dept == num1)
  dep2 <- communes_v2 %>% filter(dept == num2)
  longitude1 <- sum(dep1$longitude*dep1$popul_relative)
  latitude1 <- sum(dep1$latitude*dep1$popul_relative)
  longitude2 <- sum(dep2$longitude*dep2$popul_relative)
  latitude2 <- sum(dep2$latitude*dep2$popul_relative)
  tmp <- (longitude1*cos(pi*latitude1/180)-longitude2*cos(pi*latitude2/180))^2+(latitude1-latitude2)^2
  return(pi*6371.009*sqrt(tmp)/180)
}

dist <- c()
vect <- c("16", "17", "19", "23", "24", "33", "40", "47", "64", "79", "86", "87")
for (i in 1:11){
  for (j in (i+1):12){
    dist <- dist %>% append(d_dep(vect[i], vect[j]))
  }
}

dist_v2 <- c()
for (i in 1:11){
  for (j in (i+1):12){
    dist_v2 <- dist_v2 %>% append(d_dep_v2(vect[i], vect[j]))
  }
}

prox <- c(8.32,2.27,7.3,2.44,12.39,1.17,2.38,9.62,5.78,7.98,4.48,8.47,9.57,8.52,11.98,8.57,8.21,6.98,6.87,5.87,7.39,7.45,0.28,12.42,0.69,1.91,9.73,6.28,7.97,5.36,7.51,12.61,7.43,7.53,10.37,8.61,9.23,8.37,12.42,1.02,0.28,9.74,6.47,8.03,5.66,12.42,12.37,11.62,12.25,12.2,12.29,0.22,9.74,6.49,8.11,5.68,9.55,5.25,7.85,3.3,8.99,8.7,9.2,6.08,1.53,6.8)



table_emplois_distance <- 
  data.frame(dep=c("16_17", "16_19","16_23", "16_24", "16_33", "16_40", "16_47", "16_64", "16_79", "16_86", "16_87", "17_19", "17_23", "17_24", "17_33", "17_40", "17_47", "17_64", "17_79", "17_86", "17_87", "19_23", "19_24", "19_33", "19_40", "19_47", "19_64", "19_79", "19_86", "19_87", "23_24", "23_33", "23_40", "23_47", "23_64", "23_79", "23_86", "23_87", "24_33", "24_40", "24_47", "24_64", "24_79", "24_86", "24_87", "33_40", "33_47", "33_64", "33_79", "33_86", "33_87", "40_47", "40_64", "40_79", "40_86", "40_87", "47_64", "47_79", "47_86", "47_87", "64_79","64_86", "64_87", "79_86", "79_87", "86_87"), ecart_temp=c(33, 8, 32, 3, 35, 8, 16, 35, 35, 33, 35, 30, 31, 32, 35, 35, 35, 35, 32, 31, 32, 35, 11, 35, 21, 7, 35, 26, 20, 25, 30, 35, 27, 34, 35, 31, 20, 35, 35, 8, 18, 35, 34, 20, 35, 35, 35, 35, 35, 35, 35, 25, 35, 35, 21, 35, 35, 35, 33, 35, 35, 35, 35, 32, 21, 30), dist, dist_v2, prox)


rho_ecart <- cor(table_emplois_distance$dist, table_emplois_distance$ecart_temp)
T_ecart <- 8*rho_ecart/sqrt(1-rho_ecart^2)
p_val_ecart <- 2*(1-pt(abs(T_ecart), 65))

rho_prox <- cor(table_emplois_distance$dist, table_emplois_distance$prox)
T_prox <- 8*rho_prox/sqrt(1-rho_prox^2)
p_val_prox <- 2*(1-pt(abs(T_prox), 65))



rho_ecart_v2 <- cor(table_emplois_distance$dist_v2, table_emplois_distance$ecart_temp)
T_ecart_v2 <- 8*rho_ecart/sqrt(1-rho_ecart_v2^2)
p_val_ecart_v2 <- 2*(1-pt(abs(T_ecart_v2), 65))

rho_prox_v2 <- cor(table_emplois_distance$dist_v2, table_emplois_distance$prox)
T_prox_v2 <- 8*rho_prox_v2/sqrt(1-rho_prox_v2^2)
p_val_prox_v2 <- 2*(1-pt(abs(T_prox_v2), 65))


# Corrélogramme par secteur 

emplois_secteur <- readxl::read_xls("donnees/brut_dep_a17_2020t1.xls") %>% 
  select(A17, REG2016, eff2011t1, eff2011t2, eff2011t3, eff2011t4, eff2012t1, eff2012t2, eff2012t3, eff2012t4, eff2013t1, eff2013t2, eff2013t3, eff2013t4, eff2014t1, eff2014t2, eff2014t3, eff2014t4, eff2015t1, eff2015t2, eff2015t3, eff2015t4, eff2016t1, eff2016t2, eff2016t3, eff2016t4, eff2017t1, eff2017t2, eff2017t3, eff2017t4, eff2018t1, eff2018t2, eff2018t3, eff2018t4, eff2019t1, eff2019t2, eff2019t3, eff2019t4, eff2020t1) %>% 
  rename(REG=REG2016) %>% 
  filter(REG == "75") %>% 
  select(-REG) %>% 
  group_by(A17) %>% 
  mutate(eff2011t1 = sum(eff2011t1), eff2011t2 = sum(eff2011t2), eff2011t3 = sum(eff2011t3), eff2011t4 = sum(eff2011t4), eff2012t1 = sum(eff2012t1), eff2012t2 = sum(eff2012t2), eff2012t3 = sum(eff2012t3), eff2012t4 = sum(eff2012t4), eff2013t1 = sum(eff2013t1), eff2013t2 = sum(eff2013t2), eff2013t3 = sum(eff2013t3), eff2013t4 = sum(eff2013t4), eff2014t1 = sum(eff2014t1), eff2014t2 = sum(eff2014t2), eff2014t3 = sum(eff2014t3), eff2014t4 = sum(eff2014t4), eff2015t1 = sum(eff2015t1), eff2015t2 = sum(eff2015t2), eff2015t3 = sum(eff2015t3), eff2015t4 = sum(eff2015t4), eff2016t1 = sum(eff2016t1), eff2016t2 = sum(eff2016t2), eff2016t3 = sum(eff2016t3), eff2016t4 = sum(eff2016t4), eff2017t1 = sum(eff2017t1), eff2017t2 = sum(eff2017t2), eff2017t3 = sum(eff2017t3), eff2017t4 = sum(eff2017t4), eff2018t1 = sum(eff2018t1), eff2018t2 = sum(eff2018t2), eff2018t3 = sum(eff2018t3), eff2018t4 = sum(eff2018t4), eff2019t1 = sum(eff2019t1), eff2019t2 = sum(eff2019t2), eff2019t3 = sum(eff2019t3), eff2019t4 = sum(eff2019t4), eff2020t1 = sum(eff2020t1))

emplois_secteur <- emplois_secteur[1:17,]

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))


M <- t(emplois_secteur[,2:length(emplois_secteur[1,])])
secteurs <- c()
for (i in 1:17){
  secteurs <- secteurs %>% append(as.character(emplois_secteur[i,1]))
}
colnames(M) <- secteurs


corrplot(cor(M), method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = NULL, sig.level = 0.01, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
)

# Corrélogramme de décalage temporel par secteur 

tab <- c()

ACF <- function(L){
  tmp <- c()
  for (i in 1:(length(L)-3)){tmp <- append(tmp, cor(L[1:(length(L)-i)], L[(i+1):length(L)]))}
  return(tmp)
}

for (i in 1:length(M[1,])){tab <- rbind(tab, ACF(M[,i]))}

row.names(tab) <- secteurs
colnames(tab) <- dec_temp

corrplot(tab, tl.srt=45)

# Corrélogramme de décalage temporel par département

emplois_dep <- readxl::read_xls("donnees/brut_dep_a17_2020t1.xls") %>% 
  select(A17, DEP, REG2016, eff2011t1, eff2011t2, eff2011t3, eff2011t4, eff2012t1, eff2012t2, eff2012t3, eff2012t4, eff2013t1, eff2013t2, eff2013t3, eff2013t4, eff2014t1, eff2014t2, eff2014t3, eff2014t4, eff2015t1, eff2015t2, eff2015t3, eff2015t4, eff2016t1, eff2016t2, eff2016t3, eff2016t4, eff2017t1, eff2017t2, eff2017t3, eff2017t4, eff2018t1, eff2018t2, eff2018t3, eff2018t4, eff2019t1, eff2019t2, eff2019t3, eff2019t4, eff2020t1) %>% 
  rename(REG=REG2016) %>% 
  filter(REG == "75") %>% 
  select(-REG) %>% 
  group_by(DEP) %>% 
  mutate(eff2011t1 = sum(eff2011t1), eff2011t2 = sum(eff2011t2), eff2011t3 = sum(eff2011t3), eff2011t4 = sum(eff2011t4), eff2012t1 = sum(eff2012t1), eff2012t2 = sum(eff2012t2), eff2012t3 = sum(eff2012t3), eff2012t4 = sum(eff2012t4), eff2013t1 = sum(eff2013t1), eff2013t2 = sum(eff2013t2), eff2013t3 = sum(eff2013t3), eff2013t4 = sum(eff2013t4), eff2014t1 = sum(eff2014t1), eff2014t2 = sum(eff2014t2), eff2014t3 = sum(eff2014t3), eff2014t4 = sum(eff2014t4), eff2015t1 = sum(eff2015t1), eff2015t2 = sum(eff2015t2), eff2015t3 = sum(eff2015t3), eff2015t4 = sum(eff2015t4), eff2016t1 = sum(eff2016t1), eff2016t2 = sum(eff2016t2), eff2016t3 = sum(eff2016t3), eff2016t4 = sum(eff2016t4), eff2017t1 = sum(eff2017t1), eff2017t2 = sum(eff2017t2), eff2017t3 = sum(eff2017t3), eff2017t4 = sum(eff2017t4), eff2018t1 = sum(eff2018t1), eff2018t2 = sum(eff2018t2), eff2018t3 = sum(eff2018t3), eff2018t4 = sum(eff2018t4), eff2019t1 = sum(eff2019t1), eff2019t2 = sum(eff2019t2), eff2019t3 = sum(eff2019t3), eff2019t4 = sum(eff2019t4), eff2020t1 = sum(eff2020t1))

tmp <- c()
dep <- c("16", "17", "19", "23", "24", "33", "40", "47", "64", "79", "86", "87")
for (x in dep){
  tmp_dep <- emplois_dep[which(emplois_dep$DEP == x),]
  tmp <- tmp %>% rbind(tmp_dep[1,] %>% select(-A17))
}

tmp_2 <- tmp[,2:length(tmp[1,])]
rownames(tmp_2) <- as.character(tmp[,1])

tmp_3 <- c()
for (i in 1:12){
  tmp_3 <- tmp_3 %>% append(tmp[i,1]$DEP)
}

rownames(tmp_2) <- tmp_3

emplois_dep <- t(tmp_2)

tab <- c()


for (i in 1:length(emplois_dep[1,])){tab <- rbind(tab, ACF(emplois_dep[,i]))}

dec_temp <- c()
for (i in 1:34){
  dec_temp <- dec_temp %>% append(str_c("T+",as.character(i)))
}


row.names(tab) <- dep
colnames(tab) <- dec_temp

corrplot(tab, tl.srt=45)

# Corrélogramme de décalage temporel par département*secteur

emplois_dep_sec <- readxl::read_xls("donnees/brut_dep_a17_2020t1.xls") %>% 
  select(A17, DEP, REG2016, eff2011t1, eff2011t2, eff2011t3, eff2011t4, eff2012t1, eff2012t2, eff2012t3, eff2012t4, eff2013t1, eff2013t2, eff2013t3, eff2013t4, eff2014t1, eff2014t2, eff2014t3, eff2014t4, eff2015t1, eff2015t2, eff2015t3, eff2015t4, eff2016t1, eff2016t2, eff2016t3, eff2016t4, eff2017t1, eff2017t2, eff2017t3, eff2017t4, eff2018t1, eff2018t2, eff2018t3, eff2018t4, eff2019t1, eff2019t2, eff2019t3, eff2019t4, eff2020t1) %>% 
  rename(REG=REG2016) %>% 
  filter(REG == "75") %>% 
  select(-REG) 

tmp_2 <- c()
for (i in 1:nrow(emplois_dep_sec)){
  tmp_2 <- tmp_2 %>% rbind(emplois_dep_sec[i,] %>% select(-A17, -DEP))
}

tmp_2 <- as.data.frame(tmp_2)

tmp_3 <- c()
for (i in 1:nrow(tmp_2)){
  tmp_3 <- tmp_3 %>% rbind(as.numeric(tmp_2[i,]))
}

tab <- c()

for (i in 1:nrow(tmp_3)){tab <- rbind(tab, ACF(tmp_3[i,]))}


dep_sec <- c()
for (x in dep){
  for (y in secteurs){
    dep_sec <- dep_sec %>% append(str_c("dep", x, ", ", y))
  }
}


row.names(tab) <- dep_sec
colnames(tab) <- dec_temp

ind <- 17*(0:11)+1

rch_ind <- function(L, x){
  i <- 1
  while (x != L[i] & i <= length(L)){
    i <- i+1
  }
  if (i == length(L)+1){return(NA)}
  return(i)
}

departement <- readline(prompt="Département : ")

i <- rch_ind(dep, departement)
corrplot(tab[ind[i]:(ind[i+1]-1),], tl.srt=45)










