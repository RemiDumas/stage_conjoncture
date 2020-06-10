source("programmes/fonctions.r", encoding = "UTF-8")

# tables de passage
table_dep <- readr::read_csv("tables_passage/departement2019.csv")
table_reg <- readr::read_csv("tables_passage/region2019.csv")
table_secteur <- readr::read_csv2("tables_passage/table_a17.csv")

# chargement des ETE
data <- readxl::read_xls("donnees/ete_dep_a17.xls") %>% 
  select(REG = REG2016, DEP, A5, A17, everything(), -REG)
  
#######
df <- tibble(a = c("3", "4", "5"))
b <- 2

data2 <- data %>% 
  group_by(Région) %>% 
  mutate(Max2019 = max(eff2019t4)) %>% 
  select(Région, Département, eff2019t4, Max2019)

data3 <- data %>% 
  group_by(Région) %>% 
  summarise(Max2019 = max(eff2019t4)) 
#######

ete <- data %>% 
  gather(key = "periode", value = "nb_emplois", -REG, -DEP, -A5, -A17) %>% 
  mutate(
    Annee = str_sub(periode, 4, 7),
    Trimestre = periode %>% str_sub(8,9) %>% str_to_upper
  ) %>% 
  select(-periode)

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
