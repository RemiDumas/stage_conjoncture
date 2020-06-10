## Emploi salarié en fin de trimestre - Industries manufacturières, industries extractives et autres (A5-BE) - France hors Mayotte

X1 <- read.csv("./données/Emplois et chômage/emplois_industrie_manufacturière.csv", header=TRUE, sep=";")
X1 <- as.data.frame(X1)
for (i in 1:length(X1)){
  for (j in 1:length(X1[i,]))
    X1[i,j] <- as.numeric(X1[i,j])
}
X1 <- X1[4:length(X1[,1]),]
colnames(X1)[2] <- "donnee"



## Emploi salarié en fin de trimestre - Fabrication de denrées alimentaires, de boissons et de produits à base de tabac (A17-C1) - France hors Mayotte

X2 <- read.csv("./données/Emplois et chômage/emplois_denrées_alimentaires.csv", header=TRUE, sep=";")
X2 <- as.data.frame(X2)
for (i in 1:length(X2)){
  for (j in 1:length(X2[i,]))
    X2[i,j] <- as.numeric(X2[i,j])
}
X2 <- X2[4:length(X2[,1]),]
colnames(X2)[2] <- "donnee"


## Emploi salarié en fin de trimestre - Cokéfaction et raffinage ; industries extractives, énergie, eau, gestion des déchets et dépollution (A17-C2DE) - France hors Mayotte

X3 <- read.csv("./données/Emplois et chômage/emplois_energie.csv", header=TRUE, sep=";")
X3 <- as.data.frame(X3)
for (i in 1:length(X3)){
  for (j in 1:length(X3[i,]))
    X3[i,j] <- as.numeric(X3[i,j])
}
X3 <- X3[4:length(X3[,1]),]
colnames(X3)[2] <- "donnee"



## Emploi salarié en fin de trimestre - Fabrication de matériels de transport (A17-C4) - France hors Mayotte

X4 <- read.csv("./données/Emplois et chômage/emplois_matériel_de_transport.csv", header=TRUE, sep=";")
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

X1 <- read.csv("./données/Emplois et chômage/emplois_hébergement_et_restauration.csv", header=TRUE, sep=";")
X1 <- as.data.frame(X1)
for (i in 1:length(X1)){
  for (j in 1:length(X1[i,]))
    X1[i,j] <- as.numeric(X1[i,j])
}
X1 <- X1[4:length(X1[,1]),]
colnames(X1)[2] <- "donnee"



## Fréquentation touristique (nuitées, arrivées)

X2 <- read.csv("./données/Tourisme/fréquentation_touristique.csv", header=TRUE, sep=";")
X2$Libellé <- as.character(X2$Libellé)
X2 <- X2[which(str_detect(X2$Libellé, "Nouvelle-Aquitaine")),]
X <- X2[,4:length(X2[1,])]


X <- X2[,4:length(X2[1,])]
X <- as.data.frame(X)
tmp <- data.frame(matrix(rep(0,length(X[1,])*length(X[,1])), nrow = length(X[,1])))
for (i in 1:length(X[,1])){
  for (j in 1:length(X[1,])){
    tmp[i,j] <- as.numeric(as.character(X[i,j]))
  }
}
X <- tmp

## Taux de chômage localisé

X3 <- read.csv("./données/Emplois et chômage/emplois_industrie_manufacturière.csv", header=TRUE, sep=";")
X3 <- X3[which(str_detect(X3$Libellé, "Nouvelle-Aquitaine")),]
X3 <- X3[,84:155]
tmp <- c()
for (i in 1:length(X3[1,])){
  tmp <- append(tmp,as.numeric(as.character(X3[1,i])))
}
X3 <- tmp	

## Taux de chômage en Nouvelle-Aquitaine et Emplois dans le secteur de l'Hébergement-Restauration en Nouvelle-Aquitaine

plot.new() 
par(mar=c(4,4,3,5)) 
plot(X3,col="blue",axes=F,xlab="",ylab="") 
axis(2, ylim=c(0,10),col="blue",col.axis="blue") #,at=seq(0, 10, by=2)) 
mtext("Taux de chômage en Nouvelle-Aquitaine",side=2,line=2.5,col="blue") 
par(new = T) 
plot(X1$donnee,col="red",axes=F,xlab="",ylab="") #,ylim=c(20,40))
axis( 4 ,col="red",col.axis="red") #,at=seq(20, 40, by=5)) 
mtext("Emplois dans le secteur de l'Hébergement-Restauration en Nouvelle-Aquitaine",side=4,line=2.5,col="red") 
axis( 1 , ylim=c(20,40),col="black",col.axis="black") #,at=seq(0, 40, by=1)) 
mtext("Trimestres",side=1,line=2.5,col="black") 


L1 <- c()
L2 <- c()
for (i in 2:length(X3)){
  L1 <- append(L1,X3[i]-X3[i-1])
  L2 <- append(L2,X1$donnee[i]-X1$donnee[i-1])
}

plot.new() 
par(mar=c(4,4,3,5)) 
plot(-L1,col="blue",axes=F,xlab="",ylab="", type="l") 
axis(2, ylim=c(0,10),col="blue",col.axis="blue") #,at=seq(0, 10, by=2)) 
mtext("Opposé de l'évolution du taux de chômage en Nouvelle-Aquitaine",side=2,line=2.5,col="blue") 
par(new = T) 
plot(L2,col="red",axes=F,xlab="",ylab="", type="l") #,ylim=c(20,40))
axis( 4 ,col="red",col.axis="red") #,at=seq(20, 40, by=5)) 
mtext("Evolution de l'emplois dans le secteur de l'Hébergement-Restauration en Nouvelle-Aquitaine",side=4,line=2.5,col="red") 
axis( 1 , ylim=c(20,40),col="black",col.axis="black") #,at=seq(0, 40, by=1)) 
mtext("Trimestres",side=1,line=2.5,col="black") 

## Test de corrélation entre le taux de chômage et l'emplois dans l'hôtellerie-restauration

rho <- sum((X3-mean(X3))*(X1$donnee-mean(X1$donnee)))/sqrt(sum((X3-mean(X3))^2)*sum((X1$donnee-mean(X1$donnee))^2))

T <- sqrt(length(X3)-2)*rho/sqrt(1-rho^2)

  # On trouve une p-valeur du test à 10^-4, ON PEUT CONSIDERER LES ECHANTILLONS DECORRELES









