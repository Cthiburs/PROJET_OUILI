library(tidyverse)
library(DataLoader)
library(questionr)
library(MASS)
# Importation base
# read_lines("base partie 1.xlsx", n_max=6)
# OUILI_1 <- read_excel("BASE_NON_PFC.xlsx")
# OUILI_2 <- read_excel("BASE_PFC.xlsx")
# glimpse(OUILI_1)
# glimpse(OUILI_2)
# OUILI <- rbind(OUILI_1, OUILI_2)
# glimpse(OUILI)
# OUILI$PFC <- fct_collapse(OUILI$CODE_ACTE, "Oui"=c("PFC", "PFE", "SPR", "SPR R", "SPRV"),"Non"="autre")
# OUILI <- OUILI %>% select(-c("LIBELLE ACTE","FAMILLE_ACTES", "SURVENANCE", "CODE_ACTE", "QUANTITE"))
# glimpse(OUILI)
# summary(OUILI)

OUILI <- read_excel("LABASE - Copie.xlsx")
glimpse(OUILI)
OUILI[c("MOIS_SOINS","SEXE","PFC","TRIMESTRE","COUVERTURE_INDIVIDUELLE","NIVEAU_DE_GARANTIE","REGION")]<-lapply(OUILI[c("MOIS_SOINS","SEXE","PFC","TRIMESTRE","COUVERTURE_INDIVIDUELLE","NIVEAU_DE_GARANTIE","REGION")], as.factor)

# STAT DESCRITPTIVE
# PFC
freq(OUILI$PFC, total=T)   # Expliquer comment PFC est obtenue

# nous avons extrait les trimestres de consommation de soins ‡ partir des dates de soins

freq(OUILI$TRIMESTRE) # on observe une forte consommation d'actes sur le premier trimestre de l'annÈe et une consommation faible au cours du troisiËme trimestre
cprop(table(OUILI$TRIMESTRE,OUILI$PFC))
# Que ce soit au niveau des personnes ayant consommÈ de pfc ou non, on observe une forte consommation d'acte au premier trimestre une faible au troisiËme. De plus il y a une dÈcroissance de la consommation d'acte sur les trois premiers trimestres plus puis une nouvelle augmentation de la consommation en fin d'annÈe
chisq.test(table(OUILI$TRIMESTRE,OUILI$PFC))
# avec une proba (0.006948) < 5%, on rejette Ho, donc il existerait une une probable relation entre les deux variables
mosaicplot(table(OUILI$TRIMESTRE,OUILI$PFC), las = 3,shade = T)

# MOIS_SOINS
freq(OUILI$MOIS_SOINS, total=T)
class(OUILI$MOIS_SOINS)
OUILI$MOIS_SOINS <- as.factor(OUILI$MOIS_SOINS)
cprop(table(OUILI$MOIS_SOINS, OUILI$PFC))
chisq.test(table(OUILI$MOIS_SOINS, OUILI$PFC))
mosaicplot(table(OUILI$MOIS_SOINS, OUILI$PFC), las=3, shade = T)

# AGE
# l'age
hist(OUILI$AGE)
ggplot(OUILI,aes(x=AGE,fill=PFC,color=PFC))+
  geom_histogram(position="identity")
ggplot(OUILI, aes(PFC, AGE, color=PFC)) + geom_boxplot()
dim(OUILI)
OUILI <- OUILI %>% filter(AGE > 20 & AGE < 80 )
OUILI$AGE2 <- cut(OUILI$AGE,c(20,35,45,55,65,80))
freq(OUILI$AGE2, total=T)
prop(table(OUILI$AGE2,OUILI$PFC))
chisq.test(table(OUILI$AGE2,OUILI$PFC))
mosaicplot(table(OUILI$AGE2,OUILI$PFC), las = 3,shade = T)
freq(OUILI$AGE2)
#le sexe
freq(OUILI$SEXE)
cprop(table(OUILI$SEXE,OUILI$PFC))
chisq.test(table(OUILI$SEXE,OUILI$PFC))
mosaicplot(table(OUILI$SEXE,OUILI$PFC), las = 3,shade = T)

# niveau de couverture
freq(OUILI$NIVEAU_DE_GARANTIE)
cprop(table(OUILI$NIVEAU_DE_GARANTIE,OUILI$PFC))
chisq.test(table(OUILI$NIVEAU_DE_GARANTIE,OUILI$PFC))
mosaicplot(table(OUILI$NIVEAU_DE_GARANTIE,OUILI$PFC), las = 3,shade = T)

# rÈgion
freq(OUILI$REGION)
cprop(table(OUILI$REGION,OUILI$PFC))
chisq.test(table(OUILI$REGION,OUILI$PFC))
mosaicplot(table(OUILI$REGION,OUILI$PFC), las = 3,shade = T)

# Couverture individuelle
freq(OUILI$COUVERTURE_INDIVIDUELLE)
cprop(table(OUILI$COUVERTURE_INDIVIDUELLE,OUILI$PFC))
chisq.test(table(OUILI$COUVERTURE_INDIVIDUELLE,OUILI$PFC))
mosaicplot(table(OUILI$COUVERTURE_INDIVIDUELLE,OUILI$PFC), las = 3,shade = T)

glimpse(OUILI)
# # FRAIS_REELS   ---- Supprimer
# hist(OUILI$FRAIS_REELS)
# ggplot(OUILI,aes(x=FRAIS_REELS,fill=PFC,color=PFC))+
#   geom_histogram(position="identity")
# ggplot(OUILI, aes(PFC, FRAIS_REELS, color=PFC)) + geom_boxplot()
# dim(OUILI)
# 
# # RAC --- Supprimer
# hist(OUILI$RAC)
# ggplot(OUILI,aes(x=RAC,fill=PFC,color=PFC))+
#   geom_histogram(position="identity")
# ggplot(OUILI, aes(PFC, RAC, color=PFC)) + geom_boxplot()
# dim(OUILI)
# 

################################################################
################## REGRESSION LOGISTIQUE ######################
library(MASS)
library(caret)
OUILI2 <- OUILI %>% dplyr:: select("MOIS_SOINS", "SEXE", "REGION", "COUVERTURE_INDIVIDUELLE", 
                           "NIVEAU_DE_GARANTIE", "PFC", "AGE2")
# Dichomisation
OUILI3 <- dummyVars(~.,data = OUILI2)
OUILI3 <- predict(OUILI3,newdata = OUILI)
OUILI3 <- as.data.frame(OUILI3)
head(OUILI3)
# Recréer la variable catégorielle à prédire Y
OUILI3$PFC <- ifelse(OUILI3$PFC.Non == 1, 0,1)
#Supprimer les variables y.no et y.yes
OUILI3$PFC.Non <- NULL
OUILI3$PFC.Oui <- NULL
glimpse(OUILI3)

# Conversion en factor
names(OUILI3)

V <- "c("
for (i in 1:length(OUILI3)) {
  V <- paste(V,names(OUILI3[i]),",") 
}
V
V <- paste(substr(V,1,nchar(V)-2),")")

OUILI3[c( "MOIS_SOINS.1" , "MOIS_SOINS.2" , "MOIS_SOINS.3" , "MOIS_SOINS.4", "MOIS_SOINS.5" , 
         "MOIS_SOINS.6" , "MOIS_SOINS.7" , "MOIS_SOINS.8" , "MOIS_SOINS.9" , "MOIS_SOINS.10" , 
         "MOIS_SOINS.11" , "MOIS_SOINS.12" , "SEXE.F" , "SEXE.H" , "REGION.IDF" , "REGION.Province" , 
         "COUVERTURE_INDIVIDUELLE.BASE" , "COUVERTURE_INDIVIDUELLE.OPT1" , 
          "COUVERTURE_INDIVIDUELLE.OPT2" , "NIVEAU_DE_GARANTIE.elevé" , "NIVEAU_DE_GARANTIE.faible" ,
         "NIVEAU_DE_GARANTIE.moyen" , "AGE2.(20,35]" , "AGE2.(35,45]", "AGE2.(45,55]" , "AGE2.(55,65]" , 
         "AGE2.(65,80]" , "PFC" )]<-lapply(OUILI3[c( "MOIS_SOINS.1" , "MOIS_SOINS.2" , "MOIS_SOINS.3" , "MOIS_SOINS.4", "MOIS_SOINS.5" , 
                                                     "MOIS_SOINS.6" , "MOIS_SOINS.7" , "MOIS_SOINS.8" , "MOIS_SOINS.9" , "MOIS_SOINS.10" , 
                                                     "MOIS_SOINS.11" , "MOIS_SOINS.12" , "SEXE.F" , "SEXE.H" , "REGION.IDF" , "REGION.Province" , 
                                                     "COUVERTURE_INDIVIDUELLE.BASE" , "COUVERTURE_INDIVIDUELLE.OPT1" , 
                                                     "COUVERTURE_INDIVIDUELLE.OPT2" , "NIVEAU_DE_GARANTIE.elevé" , "NIVEAU_DE_GARANTIE.faible" ,
                                                     "NIVEAU_DE_GARANTIE.moyen" , "AGE2.(20,35]" , "AGE2.(35,45]", "AGE2.(45,55]" , "AGE2.(55,65]" , 
                                                     "AGE2.(65,80]" , "PFC" )], as.factor)

glimpse(OUILI3)

V <- "PFC ~ "
for (i in 1:length(OUILI3)-1) {
  V <- paste(V,names(OUILI3[i]),"+") 
}
V
V <- substr(V,1,nchar(V)-2)

# REF : + MOIS_SOINS.3 + SEXE.F + REGION.Province  + COUVERTURE_INDIVIDUELLE.BASE + NIVEAU_DE_GARANTIE.faible  + AGE2.(20,35]
JORDAN <- glm(PFC ~   + MOIS_SOINS.1 + MOIS_SOINS.2 + MOIS_SOINS.4 + MOIS_SOINS.5 + 
                MOIS_SOINS.6 + MOIS_SOINS.7 + MOIS_SOINS.8 + MOIS_SOINS.9 + MOIS_SOINS.10 + 
                MOIS_SOINS.11 + MOIS_SOINS.12 + SEXE.H + REGION.IDF + COUVERTURE_INDIVIDUELLE.OPT1 + 
                COUVERTURE_INDIVIDUELLE.OPT2 + NIVEAU_DE_GARANTIE.elevé + NIVEAU_DE_GARANTIE.moyen + 
                `AGE2.(35,45]` + `AGE2.(45,55]` + `AGE2.(55,65]` + `AGE2.(65,80]`, OUILI3, family=binomial(logit)) 
summary(JORDAN)
stepAIC(JORDAN)   # Meilleur modèle : Residual Deviance: 19850 	AIC: 19880
JORDAN <- glm(PFC ~ MOIS_SOINS.6 + MOIS_SOINS.7 + MOIS_SOINS.8 + 
                MOIS_SOINS.10 + MOIS_SOINS.12 + SEXE.H + REGION.IDF + COUVERTURE_INDIVIDUELLE.OPT1 + 
                NIVEAU_DE_GARANTIE.elevé + NIVEAU_DE_GARANTIE.moyen + `AGE2.(35,45]` + 
                `AGE2.(45,55]` + `AGE2.(55,65]` + `AGE2.(65,80]`, family = binomial(logit), 
              data = OUILI3) 
summary(JORDAN)




# freq(OUILI$AGE)   
#    class(OUILI$AGE)
# ggplot(OUILI, aes(x=AGE, fill=PFC, color=PFC)) +
#   geom_histogram(position="identity")
# 
#  sante_FR$age_r <- if_else()
# chisq.test(table(sante_FR$age_r,sante_FR$etat_sante))
# mosaicplot(table(sante_FR$age_r,sante_FR$etat_sante),las=3,shade=TRUE)                                  
# freq(sante_FR$age_r)

# Supprimer : CLIENT, SECTEUR, DATE DE SOINS, DATE DE REGLEMENT, SURVENANCE, DATE DE NAISSANCE, RO PAR ACTE
# , RC PAR ACTE , AUTRES MUTUELLES PAR ACTE, RESEAU DE SOINS, CODE ACTE, LIBELLE ACTE, FAMILLE ACTES,
# COUVERTURE INDIVIDUELLE, COUVERTURE ENTREPRISE
# Extraire le mois de la date de soins
# Calculer le délai de règlement si la consommation de PFC est plusieurs
# Créer l'age du bénéficiaire à partir de la date de naissance
# Recoder le code postal avec l'aide de tableau
# capter 'idée de ce que la personne a une autre mutuelle ou pas
# keep : NIVEAU DE GARANTIE, FRAIS REELS PAR ACTE (recoder en classe), QUANTITE(recoder en tenant compte de la PFC)
# 

write.csv2(OUILI2, "OUILI.csv")

# ANALYSE FACTORIELLE
OUILI <- read_csv2("OUILI.csv")
glimpse(OUILI)
OUILI <- OUILI %>% dplyr:: select("MOIS_SOINS", "AGE2", "SEXE", "REGION", "COUVERTURE_INDIVIDUELLE", "NIVEAU_DE_GARANTIE", "PFC")
library(FactoMineR)
OUILI[c("MOIS_SOINS","SEXE","PFC","COUVERTURE_INDIVIDUELLE","NIVEAU_DE_GARANTIE","REGION", "AGE2")]<-lapply(OUILI[c("MOIS_SOINS","SEXE","PFC","COUVERTURE_INDIVIDUELLE","NIVEAU_DE_GARANTIE","REGION", "AGE2")], as.factor)

ACM_OUILI <- MCA(OUILI, ncp = 19, quali.sup = 7, graph=F)
ACM_OUILI
dim(ACM_OUILI$ind$contrib)
OUILI_IND <- as.data.frame(ACM_OUILI$ind$contrib)
glimpse(OUILI_IND)
OUILI_PFC <- OUILI %>%dplyr::  select("PFC")
glimpse(OUILI_PFC)
OUILI_N <- cbind(OUILI_PFC, OUILI_IND)
glimpse(OUILI_N)
summary(OUILI_N)
# Visualiser les valeurs propres
val_propre <- ACM_OUILI$eig
barplot(val_propre[, 2], 
        names.arg = 1:nrow(val_propre), 
        main = "Variances Explained by Dimensions (%)",
        xlab = "Principal Dimensions",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(val_propre), val_propre[, 2], 
      type = "b", pch = 19, col = "red")

# Nuage des Variables
plot(ACM_OUILI,
     invisible = c("ind", "quali.sup"),
     cex = 0.8,                                    
     autoLab = "yes")

# Nuage des Variables
plot(ACM_OUILI,
     invisible = c("var", "quali.sup"),
     cex = 0.8,                                    
     autoLab = "yes")
glimpse(OUILI_N)

p <- round(16393*0.7)

echa <- sample(1:16393,p)
base_train <- OUILI_N[echa,] # selection de p OUILI_N
base_test <- OUILI_N[-echa,] # tableau des 100 autres 
espsup <- iris[-echa,5] # nom de l'esp`ece des 100 autres 
lda2 <- lda(as.matrix(base_train[,2:20]),base_train$PFC)
lda2
summary(lda2)
lda2$scaling
# Calcul de projeté des individus sur D1
xy2 <- as.vector(as.matrix(base_train[,2:20])%*%lda2$scaling[,1]) 
xy2

pred_lda2 <- predict(lda2,base_train[,2:20]) # fonction ge ́ne ́rique utilise predict.lda table(espestim,espsup)
class(pred_lda2$posterior)
pred1_lda2 <- pred_lda2$posterior[,2]
PFC_lda2 <- if_else(base_train$PFC=="Non",0,1)
pred2_lda2 <- as.data.frame(cbind(pred1_lda2, PFC_lda2))
#table(OUILI$PFC,pred)
library(ROCR)
pred3_lda2 <- prediction(pred2_lda2$pred1,pred2_lda2$PFC) 
perf_lda2 <- performance(pred3_lda2, "auc")m
perf_lda2@y.values[[1]]
perf_ROC_lda2 <- performance(pred3_lda2, "tpr", "fpr")
plot(perf_ROC_lda2)





# OUILI_lda <- lda(as.matrix(OUILI_N[,2:20]),OUILI_N$PFC)
# OUILI_lda
# summary(OUILI_lda)
# OUILI_lda$scaling
# # Calcul de projeté des individus sur D1
# xy <- as.vector(as.matrix(OUILI_N[,2:20])%*%OUILI_lda$scaling[,1]) 
# xy
# 
# pred <- predict(OUILI_lda,OUILI_N[,2:20]) # fonction ge ́ne ́rique utilise predict.lda table(espestim,espsup)
# class(pred$posterior)
# pred1 <- pred$posterior[,2]
# PFC <- if_else(OUILI_N$PFC=="Non",0,1)
# pred2 <- as.data.frame(cbind(pred1, PFC))
# #table(OUILI$PFC,pred)
# library(ROCR)
# pred3 <- prediction(pred2$pred1,pred2$PFC) 
# perf <- performance(pred3, "auc")
# perf@y.values[[1]]
# perf_ROC <- performance(pred3, "tpr", "fpr")
# plot(perf_ROC)