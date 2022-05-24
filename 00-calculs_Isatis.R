#########################
#                       #
#     CALCULS ISATIS    #
#                       #
#########################



#charger fonctions et librairies
source("src/01-fonctions_ISATIS.R") 
#charger objects
source("src/02-objects_Isatis.R")

# comparaison internet rep avant 1 sem et internet rep apres 1 semaine
# d : rep internet <  7 jours (sera note "internet" dans le tableau de resultat de score)
# f : rep internet >= 7 jours (sera note "telephone" dans le tableau de resultat de score)


########### TABLE 1 : CARACTERISTIQUES DES PATIENTS ############
num_col <- paste0("socio",c(1,3,6,8,2,4,5,7))
myname_col <- c("Age","Education","Revenu","Assurance","Sexe","Statut marital","Emploi","Profession")

cbind(num_col[order(num_col)],myname_col[order(num_col)])
# "Age"            "socio1"
# "Sexe"           "socio2"
# "Education"      "socio3"
# "Statut marital" "socio4"
# "Emploi"         "socio5"
# "Revenu"         "socio6"
# "Profession"     "socio7"
# "Assurance"      "socio8"

#TABLE 1:
#cf fonctions Des (pour description) dans fonction_ISATIS.R et variables r1 : 8 dans objects_Isatis.R
write.table(print(rbind(r1,r8,r2,r3,r4,r5,r6,r7)),file="clipboard",sep="\t",dec=",",row.names=TRUE)

#verif age tot:
summary (c(d$socio1[d$Duree>=2],f$socio1[f$Duree>=2])) 
#verif age non repondant int
summary (d[d$Duree>=2 & d$repondant%in%"non", "socio1"])

#verif des legendes du tableau (est-ce que resultat correspond bien a la legende) : prise de l'exemple repondant internet
table (d[d$Duree>=2,"socio2"],d[d$Duree>=2, "repondant"]) #remplace socio pour chaque caracteristique et verif avec tableau du dessus
table (d[d$Duree>=2,"socio3"],d[d$Duree>=2, "repondant"]) #Pour savoir a quoi correspondent les numeros, regarder object_isatis.R
table (d[d$Duree>=2,"socio4"],d[d$Duree>=2, "repondant"])
table (d[d$Duree>=2,"socio5"],d[d$Duree>=2, "repondant"])
table (d[d$Duree>=2,"socio6"],d[d$Duree>=2, "repondant"])
table (d[d$Duree>=2,"socio7"],d[d$Duree>=2, "repondant"])
table (d[d$Duree>=2,"socio8"],d[d$Duree>=2, "repondant"])

######### AUTRES RESULTATS SOCIO DANS RESULTS ######################

# periode d'inclusion
summary(d$Date_Inclusion)
summary(f$Date_Inclusion)

#ATTENTION : missing values pour baselines:
table(d$socio3[d$Duree>=2],useNA = "a")
table(d$socio5[d$Duree>=2],useNA = "a")
table(f$socio3[f$Duree>=2],useNA = "a")

d %>% filter(is.na(socio3) & Duree>=2) %>% select(id)
f %>% filter(is.na(socio3) & Duree>=2) %>% select(id)
d %>% filter(is.na(socio5) & Duree>=2) %>% select(id)

#Median LOS
summary(rbind(d %>% filter (Duree>=2) %>% select (Duree),f %>% filter (Duree>=2) %>% select (Duree)))

# pourcentage de repondeurs
table(d$Duree>=2, d$repondant); prop.table(table(d$Duree>=2, d$repondant),1)
table(f$Duree>=2, f$repondant);prop.table(table(f$Duree>=2, f$repondant),1)

#comparaison taux de reponse
chisq.test(matrix(c(154,238,344,45),2))
afs<-fisher.test(rbind(table(d[d$Duree>=2,"repondant"]),table(f[f$Duree>=2,"repondant"]))) 

#temps entre sortie et ISATIS
del_int <- d %>% filter (Duree>=2 & repondant %in% "oui")
del_int <- as.numeric(del_int$Isatis_Date2 - del_int$Date_Sortie2)
hist(del_int,breaks = 40, main="internet : delay between release and ISATIS")
quart_int <- summary(del_int) ;quart_int
quantile(del_int) #3,6,15.75,115 (med=6)
#
del_tel <- f %>% filter (Duree>=2 & repondant %in% "oui")
del_tel <- as.numeric(del_tel$Isatis_Date2 - del_tel$Date_Sortie2)
quart_tel <- summary(del_tel) ;quart_tel
#
wilcox.test(del_int,del_tel)

#Difference de satisfaction selon delai
sat_del_gr1<-d %>% filter(Duree>=2 & repondant %in% "oui") %>% group_by(quant_del) %>% summarise(mean(res.theme1,na.rm=T),sd(res.theme1,na.rm=T))

sat_del_gr2<-d %>% filter(Duree>=2 & repondant %in% "oui") %>% group_by(quant_del) %>% summarise(mean(res.theme2,na.rm=T),sd(res.theme2,na.rm=T))

sat_del_gr3<-d %>% filter(Duree>=2 & repondant %in% "oui") %>% group_by(quant_del) %>% summarise(mean(res.theme3,na.rm=T),sd(res.theme3,na.rm=T))

sat_del_gr4<-d %>% filter(Duree>=2 & repondant %in% "oui") %>% group_by(quant_del) %>% summarise(mean(res.theme4,na.rm=T),sd(res.theme4,na.rm=T))

sat_del_gr5<-d %>% filter(Duree>=2 & repondant %in% "oui") %>% group_by(quant_del) %>% summarise(mean(res.theme5,na.rm=T),sd(res.theme5,na.rm=T))

sat_del_gr6<-d %>% filter(Duree>=2 & repondant %in% "oui") %>% group_by(quant_del) %>% summarise(mean(res.theme6,na.rm=T),sd(res.theme6,na.rm=T))

sat_tot <- d %>% filter(Duree>=2 & repondant %in% "oui") %>% group_by(quant_del) %>% summarise(mean(res.score.final,na.rm=T),sd(res.score.final,na.rm=T))
for (i in 1:6){
  #data.frame(t(sat_del_gr6))[2,]
  .res <- round(cbind(get(paste0("sat_del_gr",i))[,2],get(paste0("sat_del_gr",i))[,3]),2)
  .res<- paste0(.res[,1],"(",.res[,2],")")
  .res <- data.frame(t(.res))
  if(i==1) quant_sat<-.res else quant_sat <- rbind(quant_sat,.res)
}
.res <- round(cbind(sat_tot[,2],sat_tot[,3]),2)
.res<- paste0(.res[,1],"(",.res[,2],")")
.res <- data.frame(t(.res))
quant_sat <- rbind(quant_sat,.res)
rownames(quant_sat) <- c(paste0("mean(sd)_res.theme",1:6),"res.global")


#pour verifier si NA pour temps entre sortie et ISATIS et savoir identite
.DF <-data.frame(date=as.numeric(d$Isatis_Date2-d$Date_Sortie2), critere= d$Duree>=2 & d$nitems>=15, index = d$id)
.DF%>% filter(is.na(date) & critere==TRUE) %>% select (index)


#DESCRIPTION DES MISSING DATA/ NON CONCERNE

listeQ<-c(1,2,4,13:15,16,18,27:30,3,5,6,17,20,7:11,21:24,25,26) #Questions dans l'ordre des themes
listtheme<-rep(c(1,2,3,4,5,6), c(6,6,5,5,4,2)) #numero de theme repete x fois: x etant le nombre de questions qu'il contient

#colonne decQ_class qui Recode en ok, non conerne ou missing
for(i in 1:32) d[ ,paste0("decQ_class",i)] <- Recode(d[ ,paste0("decQ",i)],"1:5='ok';6:11='nonconc';0='nonconc'")
for(i in 1:32) f[ ,paste0("decQ_class",i)] <- Recode(f[ ,paste0("decQ",i)],"1:5='ok';6:11='nonconc';0='nonconc'")

#tableaux qui donne pour chaque patient son id, son groupe, le resultat pour chaque question (ok, NC ou na)
#J'inclus dans le tableau tous les patients restes 2 nuits ou plus, repondants et non repondants
d_pat <-d[d$Duree>=2 ,c("id","groupe","repondant",paste0("decQ_class",listeQ))] #met les colonnes decQ_classe dans l'ordre de listeQ
f_pat <-f[f$Duree>=2 ,c("id","groupe","repondant",paste0("decQ_class",listeQ))]

names(d_pat)[c(-1,-2,-3)]<-paste0("Q",listeQ,"_theme",listtheme)
names(f_pat)[c(-1,-2,-3)]<-paste0("Q",listeQ,"_theme",listtheme)

ITpat<-rbind(d_pat,f_pat)

# listtheme<-rep(c("id","groupe",1,2,3,4,5,6), c(1,1,6,6,5,5,4,2))
# names(d_pat)[c(-1,-2)]<-paste0("Q",substr(names(d_pat),11,length(names(d_pat))),"_theme",listtheme)[c(-1,-2)]

#J'ajoute les question 12 et 19 au tableau ITpat
ITpat$decQ12<- c(d[d$Duree>=2,"decQ12"],f[f$Duree>=2,"decQ12"]) ; names(ITpat)[names(ITpat) == 'decQ12'] <- 'Q12'
ITpat$decQ19<- c(d[d$Duree>=2,"decQ19"],f[f$Duree>=2,"decQ19"]) ; names(ITpat)[names(ITpat) == 'decQ19'] <- 'Q19'
#changer ordre des colonnes (mettre 12 et 19 avant 13 et 20):
ITpat<-ITpat[,c(1:6,32,7:19,33,20:31)]

#rajouter les colonnes resultat theme et resultat global
for(i in 1:6) ITpat[ ,paste0("result_theme",i)] <- c(d[d$Duree>=2,paste0("res.theme",i)],f[f$Duree>=2,paste0("res.theme",i)])
ITpat$global_score<-c(d[d$Duree>=2,"res.score.final"],f[f$Duree>=2,"res.score.final"])

write.xlsx(ITpat, file=paste(getwd(), "/patientflow20162107.xlsx", sep=""), sheetName="Sheet1",
           col.names=TRUE, row.names=F, append=FALSE, showNA=F)


#COMPTE DSE NA OK ET NC : attention ne correspond pas a ce qui est dans le manuscrit : recuperer scirpt de gilles!!

count_NA<- function (x){
  .row<-x
  .row_count<-ifelse(is.na(.row),1,0)
  sumNA<-rowSums(.row_count)
  return(sumNA)
}
count_ok<- function (x){
     .row<-x
    .row_count<-ifelse(.row=="ok",1,0)
     sumok<-rowSums(.row_count,na.rm=T)
     return(sumok)
}
count_NC<-function (x){
  .row<-x
  .row_count<-ifelse(.row=="nonconc",1,0)
  sumok<-rowSums(.row_count,na.rm=T)
  return(sumok)
}

ITpat$sumNA<-sapply(1:nrow(ITpat),function (x)count_NA(ITpat[x,]))
ITpat$sumok<-sapply(1:nrow(ITpat),function (x)count_ok(ITpat[x,]))
ITpat$sumNC<-sapply(1:nrow(ITpat),function (x)count_NC(ITpat[x,]))


#Calcul du nombre de non concernes parmis les repondants : ??
count_rep<-ITpat%>% filter (repondant=="oui" & groupe=="int") %>% select(one_of(paste0("Q",listeQ,"_theme",listtheme)))
count_rep<-ITpat%>% filter (repondant=="oui" & groupe=="int") 
count_rep %>% filter (sumNC==0)





######### CALCUL TABLE 2 : COEFFICIENTS DE CROHNBACH ############

#CALCUL DES CRONBACH et 95%CI

set.seed(12345)
crontab<- data.frame(t(sapply(c(1:6,"tot"), function(i)BootCronCi(i,20000)))) #nb : les alpha sont calcules de la meme facon que dans le tableau du test de permutation ci dessous
write.table(print(crontab),file="clipboard",sep="\t",dec=".",row.names=FALSE )
#nb :warning si le nb de permutation est faible, "NA introduce" a cause de !is.na(as.numeric(x)), pas de csq sur le resultat

#P VALUE CRONBACH : TEST DE PERMUTATION
#1/GENERE les 10E6 permutations cronbach, calcul (ni,alphai,nt,alphat,diff,pval,n_perm), et enregistre les 10E6 perm ([[1]]) et (ni,alphai,nt,alphat,diff,pval,n_perm) [[2]]

.set<-"results/"
set.seed(1234)
#i=1
for (i in c(1:6,"tot")) {
  #for (i in c(2,"tot")){#NB for "tot" : "NA's introduced by coercion" car as.numeric("tot")=NA
  #res.perm<-perm.cronbach(x=i,1000)
  res.perm<-perm.cronbach(x=i,1000000)#le resultat est une liste :[[1]] 10E6 valeur de permutation [[2]]tableau de resultat
  saveRDS(res.perm,file=paste0(.set,"test perm/perm10E6.pval.cronbach",i,".Rdata"))
}

#2/lecture des Pvalue : assigne le tableau de cronbach a res.perm1:6 et res.permfin

for (i in c(1:6,"tot")){
  assign((paste0("res.perm",i)),readRDS(paste0(.set,"test perm/perm10E6.pval.cronbach",i,".Rdata"))[[2]]) # rappel :[[2]]= (ni,alphai,nt,alphat,diff,pval,n_perm)
}



#3/cree un seul tableau cronbach avec ni,alphai,nt,alphat,diff,pval,n_perm (seul pval est utilise dans le manuscrit)

for (i in c(1:6,"tot")){
  tab<-get(paste0("res.perm",i))
  if (i == 1)tabcron<-tab else tabcron<-rbind(tabcron,tab)
}

tabcron<-cbind(crontab,tabcron$p_value)
write.table( print(tabcron),file="clipboard",sep="\t",dec=".",row.names=FALSE )



#######CALCULS TABLE 3 : MEAN, diffMEAN, PVAL, ES ############

#MEAN IC DES THEMES ET SCORES FINAUX PAR BOOTSTRAP
set.seed(123) 
#N,moyenne(IC) des themes
MCi.theme <- t(sapply(1:6,function(x)BootMCi (paste0("res.theme",x),20000)))
MCi.theme<- data.frame(rbind(MCi.theme,BootMCi("res.score.final",20000)))

#Effect size
ES.theme <- sapply(1:6,function(x)BootCi(.theme=paste0("res.theme",x),R=20000))
ES.theme <- data.frame(c(ES.theme,BootCi(.theme="res.score.final",R=20000))) ;names(ES.theme)<- "Effect size"

#Calcul difference de moyenne et CI
difMCi<- sapply(1:6,function(x)boot.diff.ci(paste0("res.theme",x),R=20000))
difMCi <- data.frame(c(difMCi,boot.diff.ci("res.score.final",R=20000))) ;names(difMCi)<- "Difference telephone-internet"

#COMPARAISON DES THEMES PAR TEST DE PERMUTATION
#cf fonction dans script "test de permutation 2016 07 12.R"
r16<- sapply(c(1:6),function(.x)perm.moyenne.th(x=.x,d,f,1000000)$p_value)
rf<-perm.moyenne.fin(1000000)$p_value 

pval.tot <- data.frame(c(r16,rf)) ; pval.tot<- round(pval.tot,3) ; names(pval.tot) <- "p value"

#TABLEAU SCORE, ES ET P
tab.theme <- cbind(MCi.theme,difMCi,ES.theme,pval.tot)
write.table(print(tab.theme),file="clipboard",sep="\t",dec=".",row.names=FALSE)


######### FIGURE 1 : study profile ############


#nombre de patients
#nombre total
length(d$id)
length (f$Groupe)
#nombre de plus de 2 nuits
table(d$Duree>=2)
table(f$Duree>=2)
#repondeurs par groupe
prop.table(table(d$Duree>=2, d$repondant),1)
prop.table(table(f$Duree>=2, f$repondant),1)
#N par service
#eligible
table(d$Service) + table(f$Service)
#par groupe
table(d$Service)
table(f$Service)
#par responders et par groupe pour duree >=2 nuits
.grpfc <- d %>% filter (Duree>=2) %>% filter(repondant =="oui") %>% group_by(Service) %>% summarise(n())
colSums(.grpfc[,2]) #verif : la somme vaut bien le nb de responders internet
.intfc <- as.numeric(unlist(.grpfc[2]))
.grpfc <- f %>% filter (Duree>=2) %>% filter(repondant =="oui") %>% group_by(Service) %>% summarise(n())
colSums(.grpfc[,2]) #verif : la somme vaut bien le nb de responders internet
.telfc <- as.numeric(unlist(.grpfc[2]))
.intfc+.telfc
#type d'hospit
.a<-table(d$Duree>=2, d$Type_Hospi)
.b<-table(f$Duree>=2, f$Type_Hospi)
.a+.b
table(f[f$Duree>=2 & f$Type_Hospi=="HC","repondant"]) + table(d[d$Duree>=2 & d$Type_Hospi=="HC","repondant"])
table(f[f$Duree>=2 & f$Type_Hospi=="HDS","repondant"]) + table(d[d$Duree>=2 & d$Type_Hospi=="HDS","repondant"])
#nb d'item completes
table(rowSums(!is.na(f[ f$Duree>=2 & f$Type_Hospi=="HC", paste0("ISatis_Q",1:32)])))
table(rowSums(!is.na(d[ d$Duree>=2 & d$Type_Hospi=="HC", paste0("ISatis_Q",1:32)])))
table(rowSums(!is.na(f[ f$Duree>=2 & f$Type_Hospi=="HDS", paste0("ISatis_Q",1:32)]))) + table(rowSums(!is.na(d[ d$Duree>=2 & d$Type_Hospi=="HDS", paste0("ISatis_Q",1:32)])))
table(rowSums(!is.na(f[ f$Duree>=2, paste0("ISatis_Q",1:32)])))
table(rowSums(!is.na(d[ d$Duree>=2, paste0("ISatis_Q",1:32)])))
#nombre de patients par service selon type hospit (pour duree >= 2)
HCtot <- data.frame(table(f[f$Duree>=2 & f$Type_Hospi=="HC","Service"]) + table(d[d$Duree>=2 & d$Type_Hospi=="HC" ,"Service"]))
HCresp <- data.frame(table(f[f$Duree>=2 & f$Type_Hospi=="HC" & f$repondant=="oui","Service"]) + table(d[d$Duree>=2 & d$Type_Hospi=="HC" & d$repondant=="oui","Service"]))
HDStot <-data.frame(table(f[f$Duree>=2 & f$Type_Hospi=="HDS","Service"]) + table(d[d$Duree>=2 & d$Type_Hospi=="HDS","Service"]))
HDSresp <-data.frame(table(f[f$Duree>=2 & f$Type_Hospi=="HDS"& f$repondant=="oui","Service"]) + table(d[d$Duree>=2 & d$Type_Hospi=="HDS"& d$repondant=="oui","Service"]))
serv.names<- c("Gastrointestinal surgery","Gastroentorology","Hepatology","Infectious diseases","internal medicine")

resHC<-paste0(HCresp[,2]," ",serv.names,"(",round(HCresp[,2]/HCtot[,2]*100,1),"%, ",HCresp[,2],"/",HCtot[,2],")")
cat(paste(resHC,collapse="\n"))
resHDS <- paste0(HDSresp[,2]," ",serv.names,"(",round(HDSresp[,2]/HCtot[,2]*100,1),"%, ",HDSresp[,2],"/",HDStot[,2],")")
cat(paste(resHDS,collapse="\n"))

nrow(f[f$repondant=="oui" & f$Duree>=2 & f$Type_Hospi=="HC",])#equivalent a table(f$nonHDJ) et table(f$repondant,f$Duree>=2)

nrow(d[d$repondant=="oui" & d$Duree>=2,]) #equivalent a table(d$nonHDJ) et table(d$repondant,d$Duree>=2)








#CB DE PATIENTS REPONDENT A COMBIEN D'ITEMS

#Liste des modalites de reponse par question (liste 1 = question 1)
listQrep<-sapply(1:32,function(i)print(names(table(d[,paste0("ISatis_Q",i)]))))
grep("1_",listQrep)#dit dans quelle liste se trouve le terme
# listrep<-sapply(c(1,2,4,13:15,16,18,27:30,3,5,6,17,20,7:11,21:24,25,26),function(x)levels(d[,paste0("ISatis_Q",x)]))
# table(unlist(listrep))

#Pour chaque question, combien de patient ont repondu
class_fun <- function (.dat){
  classement_rep<- function (x) {
    Q <-as.factor(.dat[, paste0("decQ",x)])
    Q <-Q[.dat$Duree>=2]
    Q<-table(Q,useNA = "a")
    #Q <- table(.dat[.dat$Duree>=2,paste0("decQ",x)],useNA="a")
    nbNA <- sum(tail(Q,1))
    nbrep <- sum(Q)-sum(tail(Q,1))
    nbconcerne <- sum(Q[1:5])
    nbnonconcerne <- sum(Q[-c(1:5)])-sum(tail(Q,1))
    dfQ<- data.frame(theme = "t", question = x, missing = nbNA, non_concerned = nbnonconcerne, concerned = nbconcerne, non_missing = nbrep,N_calculated_theme = "N")
    return(dfQ)
  }
  listeQ<-c(1,2,4,13:15,16,18,27:30,3,5,6,17,20,7:11,21:24,25,26)
  nbQrep<-data.frame(t(sapply( listeQ, function(x)classement_rep(x))))
  nbQrep$theme<-rep(c(1,2,3,4,5,6), c(6,6,5,5,4,2))
  nbQrep$N_calculated_theme <- rep(apply(.dat[,paste0("res.theme",1:6)],2,function(x)sum(!is.na(x))), c(6,6,5,5,4,2))
  for (.c in colnames (nbQrep)) nbQrep[, .c] <- unlist (nbQrep[, .c])
  write.table(print(nbQrep),file="clipboard",sep="\t",dec=",",row.names=F)
}

class_fun(d)






#FLOW CHART

#Les repondants ont-ils bien repondu a toutes les questions? A combien d'item ont repondu les non repondants?
table(rowSums(!is.na(d[,paste0("ISatis_Q",1:32)][d$Duree>=2,])),d$repondant[d$Duree>=2])
table(rowSums(!is.na(f[,paste0("ISatis_Q",1:32)][f$Duree>=2,])),f$repondant[f$Duree>=2])
#sortir les 2 telephones qui n'ont repondu qu'a 2 et 11 questions
f[rowSums(!is.na(f[,paste0("ISatis_Q",1:32)]))==2 & f$Duree>=2,"id"]
f[f$id=="46706z",]
f[rowSums(!is.na(f[,paste0("ISatis_Q",1:32)]))==11 & f$Duree>=2,"id"]
f[f$id=="48154h",]





#EXPORTER LES RESULTATS : nouveau tableau de donnees excel
d$Groupe <- rep("I",length(d$Duree>=2))
myexporti<- d[d$Duree>=2, c("Groupe","id","NIP","Duree",paste0("res.theme", 1:6),"res.score.final")]
myexportt<-f[f$Duree>=2, c("Groupe","id","NIP","Duree",paste0("res.theme", 1:6),"res.score.final")]
write.table(print(myexporti),file="clipboard",sep="\t",dec=".",row.names=FALSE)
write.table(print(myexportt),file="clipboard",sep="\t",dec=".",row.names=FALSE)


#TEST DE DISTRIBUTION NORMALE
distrib_th_I<- data.frame(sapply(1:6,function(x)ks.test(d[ ,paste0("res.theme",x)],"pnorm",
                                                        mean(d[, paste0("res.theme",x)],na.rm=TRUE),
                                                        sd(d[, paste0("res.theme",x)],na.rm=TRUE))$p.value))
distrib_fin_I<-ks.test(d$res.score.final,"pnorm",mean(d$res.score.final,na.rm=TRUE),sd(d$res.score.final,na.rm=TRUE))$p.value
#ks.test(na.omit(d$res.score.final),"pnorm",mean = mean(na.omit(d$res.score.final)), sd= sd(na.omit(d$res.score.final)))
theme_I <- c(paste0("theme",1:6),"score final")
tabdisI<-rbind(distrib_th_I,distrib_fin_I)
tabdistribI<- cbind(theme_I,tabdisI)
colnames(tabdistribI) <- c("theme_I","KS_test")
write.table(print(tabdistribI),file="clipboard",sep="\t",dec=".",row.names=FALSE)



distrib_th_T<- data.frame(sapply(1:6,function(x)ks.test(f[, paste0("res.theme",x)],"pnorm",
                                                        mean(f[, paste0("res.theme",x)],na.rm=TRUE),
                                                        sd(f[, paste0("res.theme",x)],na.rm=TRUE))$p.value))
distrib_fin_T<-ks.test(f$res.score.final,"pnorm",mean(f$res.score.final,na.rm=TRUE),sd(f$res.score.final,na.rm=TRUE))$p.value
theme_T <- c(paste0("theme",1:6),"score final")
tabdisT<-rbind(distrib_th_T,distrib_fin_T)
tabdistribT<- cbind(theme_T,tabdisT)
colnames(tabdistribT) <- c("theme_T","KS_test")
write.table(print(tabdistribT),file="clipboard",sep="\t",dec=".",row.names=FALSE)



#NB : moyenne ou mediane pour le nombre de nuit?
DesDureebis(2) #Fonction pour avoir la moyenne du nombre de nuit
#mais Duree n'est pas symetrique donc mediane plus appropriee
summary(c(d[d$Duree>=2, "Duree"],f[f$Duree>=2, "Duree"]))
hist(c(d[d$Duree>=2, "Duree"],f[f$Duree>=2, "Duree"]), breaks = c(1:20,138))
ks.test(c(d[d$Duree>=2, "Duree"],f[f$Duree>=2, "Duree"]),"pnorm",mean=mean(c(d[d$Duree>=2, "Duree"],f[f$Duree>=2, "Duree"]),na.rm=T),sd= sd(c(d[d$Duree>=2, "Duree"],f[f$Duree>=2, "Duree"]),na.rm=T))
.dd<-d %>% filter (Duree>=duree) %>% select(Duree)
#.dd<-d %>% filter (Duree>=duree & (nitems==0 | nitems>=15 )) %>% select(Duree)
table(.dd)
#non symetrique donc j'exprime en median(IQR)




