## Emploi salarié en fin de trimestre - Industries manufacturières, industries extractives et autres (A5-BE) - France hors Mayotte

X1 <- read.csv("~/Desktop/Etudes/Stage 1A/donnes/valeur2020_06_03/valeurs_trimestrielles.csv", header=TRUE, sep=";")
X1 <- as.data.frame(X1)
for (i in 1:length(X1)){
  for (j in 1:length(X1[i,]))
    X1[i,j] <- as.numeric(X1[i,j])
}
X1 <- X1[4:length(X1[,1]),]
colnames(X1)[2] <- "donnee"



## Emploi salarié en fin de trimestre - Fabrication de denrées alimentaires, de boissons et de produits à base de tabac (A17-C1) - France hors Mayotte

X2 <- read.csv("~/Desktop/Etudes/Stage 1A/donnes/valeur2020_06_03-2/valeurs_trimestrielles.csv", header=TRUE, sep=";")
X2 <- as.data.frame(X2)
for (i in 1:length(X2)){
  for (j in 1:length(X2[i,]))
    X2[i,j] <- as.numeric(X2[i,j])
}
X2 <- X2[4:length(X2[,1]),]
colnames(X2)[2] <- "donnee"


## Emploi salarié en fin de trimestre - Cokéfaction et raffinage ; industries extractives, énergie, eau, gestion des déchets et dépollution (A17-C2DE) - France hors Mayotte

X3 <- read.csv("~/Desktop/Etudes/Stage 1A/donnes/valeur2020_06_03-3/valeurs_trimestrielles.csv", header=TRUE, sep=";")
X3 <- as.data.frame(X3)
for (i in 1:length(X3)){
  for (j in 1:length(X3[i,]))
    X3[i,j] <- as.numeric(X3[i,j])
}
X3 <- X3[4:length(X3[,1]),]
colnames(X3)[2] <- "donnee"



## Emploi salarié en fin de trimestre - Fabrication de matériels de transport (A17-C4) - France hors Mayotte

X4 <- read.csv("~/Desktop/Etudes/Stage 1A/donnes/valeur2020_06_03-4/valeurs_trimestrielles.csv", header=TRUE, sep=";")
X4 <- as.data.frame(X4)
for (i in 1:length(X4)){
  for (j in 1:length(X4[i,]))
    X4[i,j] <- as.numeric(X4[i,j])
}
X4 <- X4[4:length(X4[,1]),]
colnames(X4)[2] <- "donnee"


plot(X1$donnee, type="l")
plot(X2$donnee, type="l")
plot(X3$donnee, type="l")
plot(X4$donnee, type="l")

ks.test(X1$donnee, "pnorm", mean(X1$donnee), sd(X1$donnee))


## Distribution empirique de la loi de X1$donnee

x <- X1$donnee
?rep
n <- 30
m <- min(x)
M <- max(x)
f <- c()
for (i in 1:(n-1)){
  f <- append(f,length(x[which(x >= m+i*(M-m)/n & x < m+(i+1)*(M-m)/n)]))
}
plot(f, type="l")





install.packages("stringr", dependencies=TRUE)
library(stringr)
install.packages("FactoMineR")
library(FactoMineR)
library(missMDA)



## Emploi salarié en fin de trimestre - Hébergement et restauration (A17-IZ) - Nouvelle-Aquitaine


X1 <- read.csv("~/Desktop/Etudes/Stage 1A/donnees/Emplois hébergement et restauration/valeurs_trimestrielles.csv", header=TRUE, sep=";")
X1 <- as.data.frame(X1)
for (i in 1:length(X1)){
  for (j in 1:length(X1[i,]))
    X1[i,j] <- as.numeric(X1[i,j])
}
X1 <- X1[4:length(X1[,1]),]
colnames(X1)[2] <- "donnee"

## Fréquentation touristique (nuitées, arrivées)

X2 <- read.csv("~/Desktop/Etudes/Stage 1A/donnees/Fréquentation touristique (nuitées, arrivées)/valeurs_mensuelles.csv", header=TRUE, sep=";")
X2$Libellé <- as.character(X2$Libellé)
X2 <- X2[which(str_detect(X2$Libellé, "Nouvelle-Aquitaine")),]
T <- c()
for (i in 2010:2019){
  T <- append(T, paste(as.character(i),"-T1"))
  T <- append(T, paste(as.character(i),"-T2"))
  T <- append(T, paste(as.character(i),"-T3"))
  T <- append(T, paste(as.character(i),"-T4"))
}
X <- X2[,4:length(X2[1,])]
X <- as.data.frame(X)

X <- estim_ncpPCA(X)
L <- seq(1,120,3)
for (i in 1:40){
  X[,i] <- X[,L[i]]+X[,L[i]+1]+X[,L[i]+2]
}
X <- X[,1:40]

#################################################################################