#########################
#                       #
#     OBJECTS ISATIS    #
#                       #
#########################



#si gilles : Attention il faudra reenregistrer les tables sous csv
#filenameinternet<-"/Users/gilles_travail/Documents/Travauxscientifiques/SarahFeldman_Isatis/patients_Internet_150202_0955_Isatis.xlsx"
#filenametelephone<-"/Users/gilles_travail/Documents/Travauxscientifiques/SarahFeldman_Isatis/patients_telephone_150202_0955_Isatis.xlsx"

#si Sarah
filenameinternet<-"/Users/Sarah/Documents/2016 ete et 2015 hiver/2015 12 SarahFeldman_Isatis/questionnaires ISATIS/patients_Internet_150202_0955_complet_Isatis.xlsx"
filenametelephone<-"/Users/Sarah/Documents/2016 ete et 2015 hiver/2015 12 SarahFeldman_Isatis/questionnaires ISATIS/patients_telephone_150202_0955_Isatis_original.xlsx"

filenameinternet<-"data/patients_Internet_150202_0955_complet_Isatis.xlsx"
filenametelephone<-"data/patients_telephone_150202_0955_Isatis_original.xlsx"
# d <- read.table (filenameinternet, header=T,sep=";",dec=".",na.strings=" ")
# g <- read.table (filenametelephone, header=T,sep=";",dec=".",na.strings=" ")

#marche
# d <- read.xlsx (filenameinternet, sheetName="patients_internet_150202_0952.c")
# g <- read.xlsx(filenametelephone, sheetName="patients_telephone_150202_0955.")


d <- readRDS("data/d.rds")
g <- readRDS("data/g.rds")

f <- g[g$Groupe%in%"T",]
NE <- g[g$Groupe%in% "NE", ]
rm(g)

#pour reponse aux reviewers 

#comparaison NE et tel : internet est remplace par NE
#d <- NE



####################################################################
#VARIABLES INTERNET

d$nitems <- rowSums(!is.na(d[,paste0("ISatis_Q",1:32)])) ;table(d$nitems)

a <- as.Date(d$Date_Entree, format = "%j/%m/%y")
b<- as.Date(d$Date_Sortie, format = "%j/%m/%y");
d$Duree <- b-a
d$Duree <- as.numeric(d$Duree)
d$Date_Sortie2 <- as.Date(d$Date_Sortie, format = "%j/%m/%y")
d$Isatis_Date2 <- as.Date(d$ISatis_Date, format = "%j/%m/%y")
d$groupe <- rep("int",nrow(d))

#Repondant = a repondu à 16 Questions ou plus. non repondant = reponse a moins de 16 questions  
#d$repondant <- ifelse(d$ISATIS%in%"Complet","oui","non")
d$repondant <- ifelse (rowSums(!is.na(d[,paste0("ISatis_Q",1:32)]))>15,"oui",NA)
d$repondant <- ifelse (rowSums(!is.na(d[,paste0("ISatis_Q",1:32)]))<=15,"non",d$repondant)

d$nonHDJ <- ifelse(d$Duree>=2 & d$repondant%in%"oui","repondant",NA) #les repondant sont non HDJ et complet
d$nonHDJ <- ifelse(d$Duree>=2 & d$repondant%in%"non","non_repondant",d$nonHDJ)#les non repondant sont non HDJ et incomplet

d$service.Recode <- d$Service
d$service.Recode<- Recode(d$service.Recode,"'1_Chir_Dig'='CHIRURGIE DIGESTIVE';'2_Gastro'='GASTRO';'3_Hepato'='HEPATO';'4_Maladies_Inf'='MALADIES INFECTIEUSES';'5_Med_Int'='MEDECINE INTERNE'")

#calcul des scores
for(i in 1:32) d[,paste0("decQ",i)] <- decode_answer2(d[,paste0("ISatis_Q",i)]) #decode...2 = as num
for(i in 1:32) d[ ,paste0("val.decQ",i)] <- Recode(d[ ,paste0("decQ",i)],"1=0;2=25;3=50;4=75;5=100;6:11=NA;0=NA")
vI1 <- d[ ,paste0("val.decQ",c(1,2,4,13:15))]
vI2 <- d[ ,paste0("val.decQ",c(16,18,27:30))]
vI3 <- d[ ,paste0("val.decQ",c(3,5,6,17,20))]
vI4 <- d[ ,paste0("val.decQ",c(7:11))]
vI5 <- d[ ,paste0("val.decQ",c(21:24))]
vI6 <- d[ ,paste0("val.decQ",c(25,26))]

for (x in 1:6) assign (paste0 ("nb.quest", x), rowSums(!is.na(get(paste0("vI",x))))) #On ne peut créer plusieurs pbjets qu'avec une boucle

#THEME CALCULE UNIQUEMENT SI DUREE >=2, ET SI PLUS DE 15 QUESTIONS REPONDUES
#Un patient avec que des NA pour res.theme est soit un HDJ soit un non repondant (moins de 16 questions repondues)
d[ ,paste0("theme.valid", 1:4)] <- sapply (1:4, function (x) ifelse(get(paste0("nb.quest",x))>=3 & d$Duree>=2 & d$repondant %in% "oui", "valide", "non"))
d[ ,paste0("theme.valid", 5:6)] <- sapply (5:6, function (x) ifelse(get(paste0("nb.quest",x))>=2 & d$Duree>=2 & d$repondant %in% "oui", "valide", "non"))

d[ ,paste0("res.theme", 1:6)] <- sapply (1:6, function(x)ifelse( d[,paste0("theme.valid",x)]%in%"valide",rowMeans(get(paste0("vI",x)),na.rm=TRUE),NA))
vItot <- d[,paste0("res.theme", 1:6)]


# d$score.valid <- ifelse(d$theme.valid1%in%"valide" & d$theme.valid2%in%"valide" & d$theme.valid3%in%"valide"
#                         & d$theme.valid4%in%"valide" & d$theme.valid5%in%"valide" & d$theme.valid6%in%"valide","valide","non")
# d$res.score.final<- ifelse (d$score.valid%in%"valide",rowMeans(d[,paste0("val.decQ",c(1,2,4,13:15,16,18,27:30,3,5,6,17,20,7:11,21:24,25:26))],na.rm=T),NA )
d$score.valid <- rowSums (d[ ,paste0("theme.valid", 1:6)] == "valide") == 6 #TRUE si aucune reponse non a "theme valide"
d$res.score.final<- ifelse (d$score.valid,rowMeans(d[,paste0("val.decQ",c(1,2,4,13:15,16,18,27:30,3,5,6,17,20,7:11,21:24,25:26))],na.rm=T),NA )

#découpage en quantile 
d$quant_del <- NA
d$quant_del <- ifelse(d$Isatis_Date2-d$Date_Sortie2<3,1, d$quant_del)
d$quant_del <- ifelse(d$Isatis_Date2-d$Date_Sortie2 >= 3,2, d$quant_del)
d$quant_del <- ifelse(d$Isatis_Date2-d$Date_Sortie2>=6,3, d$quant_del)
d$quant_del <- ifelse(d$Isatis_Date2-d$Date_Sortie2 >= 15.75,4,d$quant_del)

#early responder : délai de réponse strict inferieur à 7 jours
d$early_resp <- NA
d$early_resp <- ifelse(d$Isatis_Date2-d$Date_Sortie2<7, 1, d$early_resp)
d$early_resp <- ifelse(d$Isatis_Date2-d$Date_Sortie2>=7, 0, d$early_resp)

####################################################################################
#VARIABLES TELEPHONE
f$nitems <- rowSums(!is.na(f[,paste0("ISatis_Q",1:32)]))

a.tel <- as.Date(f$Date_Entree, format = "%j/%m/%y")
b.tel<- as.Date(f$Date_Sortie, format = "%j/%m/%y")
f$Duree <- as.numeric(b.tel-a.tel)
f$Date_Sortie2 <- as.Date(f$Date_Sortie, format = "%j/%m/%y")
f$Isatis_Date2 <- as.Date(f$ISatis_Date, format = "%j/%m/%y")
f$groupe <- rep("tel",nrow(f))

#Repondant = a repondu à 16 Questions ou plus. non repondant = reponse a moins de 16 questions 
#f$repondant <- ifelse(f$ISATIS%in%"Complet","oui","non") #code en complet/incomplet/non sans savoir def de chaque codage.
f$repondant <- ifelse (rowSums(!is.na(f[,paste0("ISatis_Q",1:32)]))>15,"oui",NA)
f$repondant <- ifelse (rowSums(!is.na(f[,paste0("ISatis_Q",1:32)]))<=15,"non",f$repondant)

f$nonHDJ <- ifelse(f$Duree>=2 & f$repondant%in%"oui","repondant",NA)
f$nonHDJ <- ifelse(f$Duree>=2 & f$repondant%in%"non","non_repondant",f$nonHDJ)
f$service.Recode <- f$Service
f$service.Recode<- Recode(f$service.Recode,"'1_Chir_Dig'='CHIRURGIE DIGESTIVE';'2_Gastro'='GASTRO';'3_Hepato'='HEPATO';'4_Maladies_Inf'='MALADIES INFECTIEUSES';'5_Med_Int'='MEDECINE INTERNE'")

#Calcul score
for(i in 1:32) f[,paste0("decQ",i)] <- decode_answer2(f[,paste0("ISatis_Q",i)])
for(i in 1:32) f[ ,paste0("val.decQ",i)] <- Recode(f[ ,paste0("decQ",i)],"1=0;2=25;3=50;4=75;5=100;6:11=NA;0=NA")
vT1 <- f[ ,paste0("val.decQ",c(1,2,4,13:15))]
vT2 <- f[ ,paste0("val.decQ",c(16,18,27:30))]
vT3 <- f[,paste0("val.decQ",c(3,5,6,17,20))]
vT4 <- f[ ,paste0("val.decQ",c(7:11))]
vT5 <- f[ ,paste0("val.decQ",c(21:24))]
vT6 <- f[,paste0("val.decQ",c(25,26))]

for (x in 1:6) assign (paste0 ("nb.quest.tel", x), rowSums(!is.na(get(paste0("vT",x))))) 

#THEME CALCULE UNIQUEMENT SI DUREE >=2, ET SI PLUS DE 15 QUESTIONS REPONDUES
f[ ,paste0("theme.valid", 1:4)] <- sapply (1:4, function (x) ifelse(get(paste0("nb.quest.tel",x))>=3 & f$Duree>=2 & f$repondant %in% "oui", "valide", "non"))
f[ ,paste0("theme.valid", 5:6)] <- sapply (5:6, function (x) ifelse(get(paste0("nb.quest.tel",x))>=2 & f$Duree>=2 & f$repondant %in% "oui", "valide", "non"))
f[ ,paste0("res.theme", 1:6)] <- sapply (1:6, function(x)ifelse( f[,paste0("theme.valid",x)]%in%"valide",rowMeans(get(paste0("vT",x)),na.rm=TRUE),NA))
vTtot <- f[,paste0("res.theme", 1:6)]

f$score.valid <- rowSums (f[ ,paste0("theme.valid", 1:6)] == "valide") == 6 #TRUE si aucune reponse non a "theme valide"
f$res.score.final<- ifelse (f$score.valid,rowMeans(f[,paste0("val.decQ",c(1,2,4,13:15,16,18,27:30,3,5,6,17,20,7:11,21:24,25:26))],na.rm=T),NA )


#############################################################################
#VARIABLES SOCIOLOGIQUES

#INTERNET
d$socio1 <- d$Age
d$socio2 <- d$Sexe
d$socio3 <- decode_answer2(d$Education) ; d$socio3 <- Recode(d$socio3,"c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4;else=NA")
#d$socio3 <- Recode(d$socio3,"c(1,2,3)='->1er cycle secondaire';c(4,5)='->second cycle secondaire';c(6,7)='->superieur court ou post secondaire';c(8,9,10)='enseignement superieur';else=NA")
d$socio4 <- decode_answer2(d$Statut_Marital) ; d$socio4 <- Recode(d$socio4,"c(1,4,5,6)='seul';c(2,3)='en couple';else=NA")
d$socio5 <- decode_answer2(d$Emploi) ; d$socio5 <- Recode(d$socio5,"c(1,4)='emploi ou etude';c(2,3,5,6,7)='sans activite';else=NA")
d$socio7 <- decode_answer_prof(d$Profession) # colonne avec juste niveau 1 INSEE
d$socio8 <- decode_answer2(d$Assurance) ; d$socio8 <- Recode(d$socio8,"c(2,4,5,6,7)=1;1=2;3=3;else=NA")
#d$socio8 <- Recode(d$socio8,"c(2,4,5,6,7)='precaire';1='secu';3='secu et mutuelle';else=NA")

#TELEPHONE
f$socio1 <- f$Age
f$socio2 <- f$Sexe
f$socio3 <- decode_answer2(f$Education) ;f$socio3 <- Recode(f$socio3,"c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4;else=NA")
#f$socio3 <- Recode(f$socio3,"c(1,2,3)='->1er cycle secondaire';c(4,5)='->second cycle secondaire';c(6,7)='->superieur court ou post secondaire';c(8,9,10)='enseignement superieur';else=NA")
f$socio4 <- decode_answer2(f$Statut_Marital) ; f$socio4 <- Recode(f$socio4,"c(1,4,5,6)='seul';c(2,3)='en couple';else=NA")
f$socio5 <- decode_answer2(f$Emploi); f$socio5 <- Recode(f$socio5,"c(1,4)='emploi ou etude';c(2,3,5,6,7)='sans activite';else=NA")
f$socio7 <- decode_answer_prof(f$Profession) #colonne avec juste niveau 1 INSEE
f$socio8 <- decode_answer2(f$Assurance); f$socio8 <- Recode(f$socio8,"c(2,4,5,6,7)=1;1=2;3=3;else=NA")
#f$socio8 <- Recode(f$socio8,"c(2,4,5,6,7)='precaire';1='secu';3='secu et mutuelle';else=NA")

#Revenu INT et TEL: attribuer a  8 et 9 la moyenne des revenus i et t
d$socio6 <- decode_answer2(d$Revenu)
f$socio6 <- decode_answer2(f$Revenu)
drev <- d[!is.na(d$nonHDJ) & d$socio6 != 8 & d$socio6 !=9, "socio6"]
frev<- f[!is.na(f$nonHDJ) & f$socio6 != 8 & f$socio6 != 9, "socio6"]
mean(c(drev,frev))
d[d$socio6%in%c(8,9),"socio6"] <- mean(c(drev,frev)) 
f[f$socio6%in%c(8,9),"socio6"] <- mean(c(drev,frev))

r1<-DesN (2)    #2 = nombre de nuits
r8 <- DesDuree(2)
r2<- DesSexe(2)
r3<- DesAge(2)
r4<- DesMarital(2)
r5<- DesEduc(2)
r6 <- DesEmploi(2)
r7 <- DesAssurance (2)

