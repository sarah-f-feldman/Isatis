#########################
#                       #
#   FONCTIONS ISATIS    #
#                       #
#########################

library (xlsx)
library(rJava)
library(car)
library(RMySQL)
library(DBI)
library(ggplot2)
library(Hmisc)
#library(gtools) #smartbind
library(psy)
library (dplyr)
library (boot)
#library(compute.es)
#library(MBESS)
#library(cocron)


###### NB : méthodes bootstrap utilisées dans le manuscrit ######
#Exemple de calcul d'estimateur par median : BootCi (mais finalement on a utilisé l'estimateur observé (res$t0))
#exemple de calcul percentile et Bca : BootCi (finalement on a utilisé la méthode percentile pour tous les bootstraps)
#Valeurs absolues ou non? oui pour les tests de permutation, non pour les différences de moyennes et effect size
  #difference de moyenne (est et IC) : pas de valeur absolue 
  #effect size (est et IC) : pas de valeur absolue (on en avait fait au début, ce qui donnait des valeurs aberrantes)
  #permutation différence de moyenne : valeur absolue (pour faire test unilatéral (ou le contraire?))
  #permutation difference de cronbach : valeur absolue ( pour faire test unilatéral (ou le contraire?))
#set.seed sont dans le script calcul_Isatis et en feuille 2 des fichiers : 
        #2016 08 05 calculs table 2.xlsx
        #2016 08 05 calculs table 3.xlsx
#pour permutation : R= 1000000 (10E6)
#pour bootstrap : R=20000 (2*10E4)

#############FONCTION POUR LE CALCUL DES SCORES###############

decode_answer2 <- function(answer_ori)
  return(as.numeric (ifelse (substr (answer_ori, 2, 2)=="_", substr (answer_ori, 1, 1), substr (answer_ori, 1, 2))))
#Fonction sans return : decode_answer2 <- function(answer_ori) as.numeric (ifelse (substr (answer_ori, 2, 2)=="_", substr (answer_ori, 1, 1), substr (answer_ori, 1, 2)))



###############FONCTION POUR LA TABLE 1 SOCIODEMO#########################
duree<-2#pour tester les fonctions

decode_answer_prof <- function(answer_prof) as.numeric (substr (answer_prof, 1, 1))

#Pour chaque fonction, résultat en 5 groupes: .int : repondant et non repondant internet séparemment, .tel: idem pour tel, .som : description int et tel ensemble

#Effectifs 
DesN <- function (duree) {
  .d <- d %>% filter (Duree>=duree) %>% group_by(repondant) %>% summarise(n=n())
  .int <- data.frame(rbind(.d[2,],.d[1,]))
  
  .d <- f %>% filter (Duree>=duree) %>% group_by(repondant) %>% summarise(n=n())
  .tel <- data.frame(rbind(.d[2,],.d[1,]))
  
  .som <- sum(d %>% filter (Duree>=duree) %>% summarise(n=n()),f %>% filter (Duree>=duree) %>% summarise(n=n()))
  .som <- data.frame(repondant="total", n=.som) 
 
  .res <- rbind(total=.som,internet=.int,telephone=.tel)[,2]
  res <- data.frame(.res)
  rownames(res)<-c("total","rep_int","nonrep_int","rep_tel","nonrep_tel")
  colnames(res)<- "Effectif"
 
  return (t(res))
}

#Age 
DesAge <- function (duree) {
  .d <- d %>% filter (Duree>=duree) %>% group_by(repondant) %>% summarise(med=median(Age), q1=quantile(Age, .25), q3=quantile(Age, .75))
  .int <- data.frame(rbind(.d[2,],.d[1,]))
  .d <- f %>% filter (Duree>=duree) %>% group_by(repondant) %>% summarise(med=median(Age), q1=quantile(Age, .25), q3=quantile(Age, .75))
  .tel <- data.frame(rbind(.d[2,],.d[1, ]))      
  .som<-unlist(c(d %>% filter (Duree>=duree ) %>% select(Age),f %>% filter (Duree>=duree ) %>% select(Age)))
  .som <- c(totmed=median(.som),totq1=as.numeric(quantile(.som,.25)),totq3=as.numeric(quantile(.som,.75)))
  .som<-c(repondant="total",med=as.numeric(.som[1]),q1=as.numeric(.som[2]),q3=as.numeric(.som[3]))
  .res <- rbind(total=.som,internet=.int,telephone=.tel)
  #.fin <- for (i in 1:5) {
  for (i in 1:5) {
    resint<- paste0(.res[i,2]," (",.res[i,3],"-",.res[i,4],")")
    if (i==1) res <- resint else res <- rbind (res, resint)
  }
  rownames(res)<-c("total","rep_int","nonrep_int","rep_tel","nonrep_tel")
  colnames(res)<- "Age"
  return(t(res))
}

DesSexe <- function (duree){  
  .d <- d %>% filter (Duree>=duree )  %>% group_by(repondant) %>%  select(Sexe)
  .int <-table (.d)
  .d <- f %>% filter (Duree>=duree )  %>% group_by(repondant) %>%  select(Sexe)
  .tel <-table (.d)
  .d<-unlist(c(d %>% filter (Duree>=duree) %>% select(Sexe),f %>% filter (Duree>=duree) %>% select(Sexe)))
  .som <-table (.d)
  .fin <- as.vector(rbind(.som,.int[c(2,1),],.tel[c(2,1),])[,2])
  .int <- prop.table(.int,1)
  .tel <- prop.table(.tel,1)
  .som <- prop.table(.som)
  .pfin<- sapply(as.vector(rbind(.som,.int[c(2,1),],.tel[c(2,1),])[,2]),function(x)paste0(round(x*100,2),"%"))
  .res<-t(data.frame(paste0(.fin," (",.pfin,")"))); rownames(.res)<-"masc" 
  colnames(.res)<-c("total","rep_int","nonrep_int","rep_tel","nonrep_tel")
  return(.res)
}

#nb de nuits by responders med IQR
DesDuree <- function(duree){
  .d <- d %>% filter (Duree>=duree ) %>% group_by(repondant) %>% summarise(med=median(Duree), q1=quantile(Duree, .25), q3=quantile(Duree, .75))
  .int <- data.frame(rbind(.d[2,],.d[1,]))  
  .d <- f %>% filter (Duree>=duree ) %>% group_by(repondant) %>% summarise(med=median(Duree), q1=quantile(Duree, .25), q3=quantile(Duree, .75))
  .tel <- data.frame(rbind(.d[2,],.d[1,]))  
  .som<-unlist(c(d %>% filter (Duree>=duree ) %>% select(Duree),f %>% filter (Duree>=duree) %>% select(Duree)))
  .som <- c(totmed=median(.som),totq1=as.numeric(quantile(.som,.25)),totq3=as.numeric(quantile(.som,.75)))
  .som<-c(repondant="total",med=as.numeric(.som[1]),q1=as.numeric(.som[2]),q3=as.numeric(.som[3]))
  .res <- rbind(total=.som,internet=.int,telephone=.tel)
  for (i in 1:5) {
    resint<- paste0(.res[i,2]," (",.res[i,3],"-",.res[i,4],")")
    if (i==1) res <- resint else res <- rbind (res, resint)
  }
  rownames(res)<-c("total","rep_int","nonrep_int","rep_tel","nonrep_tel")
  colnames(res)<- "Duree d'hospitalisation"
  return(t(res))
}
#nb de nuits by responders mean sd
DesDureebis <- function(duree){
  .d <- d %>% filter (Duree>=duree) %>% group_by(repondant) %>% summarise(mean=mean(Duree), sd=sd(Duree))
  .int <- data.frame(rbind(.d[2,],.d[1,]))  
  .d <- f %>% filter (Duree>=duree) %>% group_by(repondant) %>% summarise(mean=mean(Duree), sd=sd(Duree))
  .tel <- data.frame(rbind(.d[2,],.d[1,]))  
  .som<-unlist(c(d %>% filter (Duree>=duree) %>% select(Duree),f %>% filter (Duree>=duree)%>% select(Duree)))
  #.som <-c(repondant="total",c(totmean=mean(.som),totsd=sd(.som)))
  .som <-c(totmean=mean(.som),totsd=sd(.som))
  .res <- round(rbind(total=.som,internet=.int[,2:3],telephone=.tel[,2:3]),2)
  for (i in 1:5) {
    resint<- paste0(.res[i,1]," (",.res[i,2],")")
    if (i==1) res <- resint else res <- rbind (res, resint)
  }
  rownames(res)<-c("total","rep_int","nonrep_int","rep_tel","nonrep_tel")
  colnames(res)<- "Duree d'hospitalisation"
  return(t(res))
}

DesEduc <- function(duree){
  for (i in 1:4){
    .d <- d %>% filter (Duree>=duree )  %>% group_by(repondant) %>%  select(socio3)
    .int <-table (.d)
    .d <- f %>% filter (Duree>=duree )  %>% group_by(repondant) %>%  select(socio3)
    .tel <-table (.d)
    .d<-unlist(c(d %>% filter (Duree>=duree ) %>% select(socio3),f %>% filter (Duree>=duree ) %>% select(socio3)))
    .som <-table (.d)
    .fin <- as.vector(rbind(.som,.int[c(2,1),],.tel[c(2,1),])[,i])
    .int <- prop.table(.int,1)
    .tel <- prop.table(.tel,1)
    .som <- prop.table(.som)
    .pfin<- sapply(as.vector(rbind(.som,.int[c(2,1),],.tel[c(2,1),])[,i]),function(x)paste0(round(x*100,2),"%"))
    .res<-t(data.frame(paste0(.fin," (",.pfin,")"))); rownames(.res)<- paste0("Education",0+i)  
    if (i==1) res <- .res else res<- rbind(res,.res)
  }
  colnames(res)<-c("total","rep_int","nonrep_int","rep_tel","nonrep_tel")
  return(res)
} 

DesMarital <- function (duree) {
  for (i in 1:2){
    .d <- d %>% filter (Duree>=duree )  %>% group_by(repondant) %>%  select(socio4)
    .int <-table (.d)
    .d <- f %>% filter (Duree>=duree )  %>% group_by(repondant) %>%  select(socio4)
    .tel <-table (.d)
    .d<-unlist(c(d %>% filter (Duree>=duree ) %>% select(socio4),f %>% filter (Duree>=duree) %>% select(socio4)))
    .som <-table (.d)
    .fin <- as.vector(rbind(.som,.int[c(2,1),],.tel[c(2,1),])[,i])
    .int <- prop.table(.int,1)
    .tel <- prop.table(.tel,1)
    .som <- prop.table(.som)
    .pfin<- sapply(as.vector(rbind(.som,.int[c(2,1),],.tel[c(2,1),])[,i]),function(x)paste0(round(x*100,2),"%"))
    .res<-t(data.frame(paste0(.fin," (",.pfin,")"))); rownames(.res)<- paste0("Statut Marital",0+i)  
    if (i==1) res <- .res else res<- rbind(res,.res)
  }
  colnames(res)<-c("total","rep_int","nonrep_int","rep_tel","nonrep_tel")
  return(res)
}

DesEmploi <- function(duree) {
  for (i in 1:2){
    .d <- d %>% filter (Duree>=duree)  %>% group_by(repondant) %>%  select(socio5)
    .int <-table (.d)
    .d <- f %>% filter (Duree>=duree)  %>% group_by(repondant) %>%  select(socio5)
    .tel <-table (.d)
    .d<-unlist(c(d %>% filter (Duree>=duree ) %>% select(socio5),f %>% filter (Duree>=duree ) %>% select(socio5)))
    .som <-table (.d)
    .fin <- as.vector(rbind(.som,.int[c(2,1),],.tel[c(2,1),])[,i])
    .int <- prop.table(.int,1)
    .tel <- prop.table(.tel,1)
    .som <- prop.table(.som)
    .pfin<- sapply(as.vector(rbind(.som,.int[c(2,1),],.tel[c(2,1),])[,i]),function(x)paste0(round(x*100,2),"%"))
    .res<-t(data.frame(paste0(.fin," (",.pfin,")"))); rownames(.res)<- paste0("Emploi",0+i)  
    if (i==1) res <- .res else res<- rbind(res,.res)
  }
  colnames(res)<-c("total","rep_int","nonrep_int","rep_tel","nonrep_tel")
  return(res)
}  

DesAssurance <- function(duree) {
  for (i in 1:3){
    .d <- d %>% filter (Duree>=duree)  %>% group_by(repondant) %>%  select(socio8)
    .int <-table (.d)
    .d <- f %>% filter (Duree>=duree)  %>% group_by(repondant) %>%  select(socio8)
    .tel <-table (.d)
    
    .d<-unlist(c(d %>% filter (Duree>=duree ) %>% select(socio8),f %>% filter (Duree>=duree ) %>% select(socio8)))
    .som <-table (.d)
    
    .fin <- as.vector(rbind(.som,.int[c(2,1),],.tel[c(2,1),])[,i])
    .int <- prop.table(.int,1)
    .tel <- prop.table(.tel,1)
    .som <- prop.table(.som)
    .pfin<- sapply(as.vector(rbind(.som,.int[c(2,1),],.tel[c(2,1),])[,i]),function(x)paste0(round(x*100,2),"%"))
    .res<-t(data.frame(paste0(.fin," (",.pfin,")"))); rownames(.res)<- paste0("Assurance",0+i)  
    if (i==1) res <- .res else res<- rbind(res,.res)
  }
  colnames(res)<-c("total","rep_int","nonrep_int","rep_tel","nonrep_tel")
  return(res)
}  


#######CRONBACH#########

#cronbach2 utilisé avant que je cré la fonction BootCronCi, qui élimine les lignes pour lesquelles restheme est manquant
# c'est à dire si nv1<= 2 si 4Q ou moins ou si nv1<=3 si plus de 4Q:
    # cronbach2 <- function (v1, duree)
    # {
    #   nv1 <- ncol(v1)
    #   keep <- rowSums (!is.na (v1)) >= ifelse (nv1<=4, 2, 3) & duree >= 2
    #   v1 <- v1[keep, ]
    #   pv1 <- nrow(v1)
    #   alpha <- (nv1/(nv1 - 1)) * (1 - sum(apply(v1, 2, var, na.rm=T))/var(apply(v1, 1, sum, na.rm=T)))
    #   resu <- list(sample.size = pv1, number.of.items = nv1, alpha = alpha)
    #   resu
    # }

#calcul CRONBACH et intervalle de confiance
cronbach.no.omit<- function(v1){
  nv1<-ncol(v1)
  pv1 <- nrow(v1)
  alpha <- (nv1/(nv1 - 1)) * (1 - sum(apply(v1, 2, var, na.rm=T))/var(apply(v1, 1, sum, na.rm=T)))
  return(alpha)
}

cronbach3.boot<- function(data,indices){
  .dat<- data[indices,]
  #.int<- .dat[.dat$group=="int",][-1]
  #.int<- cronbach3(.int)
  #.tel<- .dat[.dat$group=="tel",][-1]
  #.tel<- cronbach3(.tel)
  cron<-by(.dat[-1],.dat$group,cronbach.no.omit)
  #return(list(.int,.tel))
  return(cron)
}

# bootcron<- function (x , R)  {    #x = "1":6 ou x="tot"
#   
#   xI<-get(paste0("vI",x))
#   xT<-get(paste0("vT",x))
#   if(!is.na(as.numeric(x))){
#   keepI <- xI[!is.na(d[,paste0("res.theme",x)]),]
#   keepT <- xT[!is.na(f[,paste0("res.theme",x)]),]
#   }
#   else {
#     keepI <- na.omit(xI)
#     keepT <- na.omit(xT)
#   }
#   .df <- data.frame(
#               group=rep(c("int","tel"),c(nrow(keepI),nrow(keepT))),
#               items=rbind(keepI,keepT)
#   )
#   .res<-boot(data=.df,statistic = cronbach3.boot ,R=R)
# }
bootcron<- function (x , R)  {    #x = "1":6 ou x="tot"
  if(!is.na(as.numeric(x))){
    xI<-get(paste0("vI",x))
    xT<-get(paste0("vT",x))
    keepI <- xI[!is.na(d[,paste0("res.theme",x)]),] #Si le résultat du thème est NA, c'est que le thème n'est pas valide
    keepT <- xT[!is.na(f[,paste0("res.theme",x)]),]
  }
  else {
    xI <- d[,paste0("val.decQ",c(1,2,4,13:15,16,18,27:30,3,5,6,17,20,7:11,21:24,25:26))]
    xT <- f[,paste0("val.decQ",c(1,2,4,13:15,16,18,27:30,3,5,6,17,20,7:11,21:24,25:26))]
    keepI <- xI[d$score.valid,] #Je ne garde que les lignes qui sont considérées comme valide pour le calcul du score 
    keepT <- xT[f$score.valid,]
  }
  .df <- data.frame(
    group=rep(c("int","tel"),c(nrow(keepI),nrow(keepT))),
    items=rbind(keepI,keepT)
  )
  .res<-boot(data=.df,statistic = cronbach3.boot ,R=R)
}



BootCronCi <- function(x,R)  {
  .bootres <- bootcron (x=x, R=R)
  #browser()
  .n <- length (.bootres$t0) #donne le nombre de resultat boot realise : 1 pour internet, 1 pour telephone
  .list.ci <- lapply(1:.n, function(x) boot.ci(.bootres,index=x,type="perc")) #fct boot.ci : intervalle de confiance pour chaque boot
  .res <- data.frame (t (sapply (.list.ci, function (x) x[["percent"]][4:5]))) #selectionne les valeur de IC
  rownames (.res) <- names (.bootres$t0)
  colnames (.res) <- c ("CI_L", "CI_U")
  .res$est <- as.numeric (.bootres$t0)
  if(!is.na(as.numeric(x))).res$n<-c(sum(!is.na(d[,paste0("res.theme",x)])),sum(!is.na(f[,paste0("res.theme",x)])))
  else .res$n <- c(sum(!is.na(d$res.score.final)),sum(!is.na(f$res.score.final)))
  .res <- .res[, c (4,3, 1, 2)]
  .ans <- round (.res, 2) #fait un arrondi sur chaque valeur
  .ans <- data.frame (N=.res$n, alpha_CI=paste0 (.ans$est, " [", .ans$CI_L, "-", .ans$CI_U, "]")) #met en forme les valeurs
  .ans <- sapply  (c (internet=.ans[1, ], telephone=.ans[2, ]), as.vector )#c(int= , tel=)donne un titre, mais met en liste, 
  #sapply(as.vector) realigne en chaine de caracteres
  return (.ans)
}

#pvalue : test de permutation:

perm.cronbach <-function (x,N){
  
  if(!is.na(as.numeric(x))){
    xI<-get(paste0("vI",x))
    xT<-get(paste0("vT",x))
    keepI <- xI[!is.na(d[,paste0("res.theme",x)]),] #Si le résultat du thème est NA, c'est que le thème n'est pas valide
    keepT <- xT[!is.na(f[,paste0("res.theme",x)]),]
  }
  else {
    xI <- d[,paste0("val.decQ",c(1,2,4,13:15,16,18,27:30,3,5,6,17,20,7:11,21:24,25:26))]
    xT <- f[,paste0("val.decQ",c(1,2,4,13:15,16,18,27:30,3,5,6,17,20,7:11,21:24,25:26))]
    keepI <- xI[d$score.valid,] #Je ne garde que les lignes qui sont considérées comme valide pour le calcul du score 
    keepT <- xT[f$score.valid,]
  }
  
  .df <- data.frame(
    group=rep(c("int","tel"),c(nrow(keepI),nrow(keepT))),
    items=rbind(keepI,keepT)
  )
  
  calc_cron <- function(.df){
    #browser()
    cron<-by(.df[-1],.df$group,cronbach.no.omit)
    cronI<- as.numeric(cron)[1]
    cronT<- as.numeric(cron)[2]
    diffcron <- abs(cronI - cronT)
    return(c(cronI,cronT,diffcron))
  }
  
  perm.test<- function(.df=.df){
    mixgpe <- sample(.df$group,replace = FALSE)
    .df$group <- mixgpe
    diff<- calc_cron(.df)[3] #la 3e valeur de calc_cron est la différence
    return(diff)
  }
  
  cron.obs <- calc_cron(.df)
  diff.obs <- cron.obs[3]
  many.samp <- replicate (N, perm.test(.df))
  
  p.val <- length(many.samp[many.samp>= diff.obs]) / length(many.samp)
  hist(many.samp,main=paste0("Difference cronbach theme",x))
  abline(v=diff.obs,lwd=2,col=2)
  abline(v=quantile(many.samp,prob=0.95,type=3),col=8)
  
  #return(data.frame(ni=nrow(keepI),alphai=obs.cronI,nt=nrow(keepT),alphat=obs.cronT, diff_obs=diff.obs, p_value=p.val,nb_perm=N))
  return(list(many.samp, 
              data.frame(ni=nrow(keepI),alphai=cron.obs[1],nt=nrow(keepT),
                         alphat=cron.obs[2], diff_obs=diff.obs, p_value=p.val,nb_perm=N)))
}


perm.cronbach.theme<- function (x,N){
  # x<-1
  # x<-"tot"
  xI<-get(paste0("vI",x))
  xT<-get(paste0("vT",x))
  keepI <- xI[!is.na(d[,paste0("res.theme",x)]),]
  keepT <- xT[!is.na(f[,paste0("res.theme",x)]),]
  keepIT <- rbind(keepI,keepT)
  obs.cronI<- cronbach3(keepI)
  obs.cronT<- cronbach3(keepT)
  diff.obs <- abs(obs.cronI - obs.cronT)
  #diff.obs <- obs.cronI - obs.cronT
  
  .gpe <- rep(c("int","tel"),c(nrow(keepI),nrow(keepT)))
  
  perm.test<- function(keepIT=keepIT,.gpe=.gpe){
    mixgpe <- sample(.gpe,replace = FALSE)
    int <- keepIT[mixgpe=="int",]
    tel <- keepIT[mixgpe=="tel",]
    cri<-cronbach3(int)
    crt<-cronbach3(tel)
    diff<-abs(cri-crt)
    #diff<-cri-crt
    return(diff)
  }
  many.samp<- replicate (N, perm.test(keepIT,.gpe))
  
  #p.val <- 1-(length(many.samp[many.samp<= diff.obs])) / (length(many.samp))
  p.val <- length(many.samp[many.samp>= diff.obs]) / length(many.samp)
  hist(many.samp,main=paste0("Difference cronbach theme",x))
  abline(v=diff.obs,lwd=2,col=2)
  abline(v=quantile(many.samp,prob=0.95,type=3),col=8)
  
  #return(data.frame(ni=nrow(keepI),alphai=obs.cronI,nt=nrow(keepT),alphat=obs.cronT, diff_obs=diff.obs, p_value=p.val,nb_perm=N))
  return(list(many.samp, 
              data.frame(ni=nrow(keepI),alphai=obs.cronI,nt=nrow(keepT),
                         alphat=obs.cronT, diff_obs=diff.obs, p_value=p.val,nb_perm=N)))
  
}
perm.cronbach.fin<- function (N){  
  # xI<-vItot
  # xT<-vTtot
  # keepI <- xI[!is.na(d$res.score.final),]
  # keepT <- xT[!is.na(f$res.score.final),]
  xI <- d[,paste0("val.decQ",c(1,2,4,13:15,16,18,27:30,3,5,6,17,20,7:11,21:24,25:26))]
  xT <- f[,paste0("val.decQ",c(1,2,4,13:15,16,18,27:30,3,5,6,17,20,7:11,21:24,25:26))]
  keepI <- xI[d$score.valid,] #Je ne garde que les lignes qui sont considérées comme valide pour le calcul du score 
  keepT <- xT[f$score.valid,]
  
  keepIT <- rbind(keepI,keepT)
  obs.cronI<- cronbach(keepI)$alpha
  obs.cronT<- cronbach(keepT)$alpha
  diff.obs <- abs(obs.cronI - obs.cronT)
  #diff.obs <- obs.cronI - obs.cronT
  
  .gpe <- rep(c("int","tel"),c(nrow(keepI),nrow(keepT)))
  
  perm.test<- function(keepIT=keepIT,.gpe=.gpe){
    mixgpe <- sample(.gpe,replace = FALSE)
    int <- keepIT[mixgpe=="int",]
    tel <- keepIT[mixgpe=="tel",]
    cri<-cronbach(int)$alpha
    crt<-cronbach(tel)$alpha
    diff<-abs(cri-crt)
    #diff<-cri-crt
    return(diff)
  }
  
  many.samp <- replicate (N, perm.test(keepIT,.gpe))
  
  #p.val <- 1-(length(many.samp[many.samp<= diff.obs])) / (length(many.samp))
  p.val <- length(many.samp[many.samp>= diff.obs]) / length(many.samp)
  hist(many.samp,main=paste0("Difference cronbach theme tot"))
  abline(v=diff.obs,lwd=2,col=2)
  abline(v=quantile(many.samp,prob=0.95,type=3),col=8)
  
  #return(data.frame(ni=nrow(keepI),alphai=obs.cronI,nt=nrow(keepT),alphat=obs.cronT, diff_obs=diff.obs, p_value=p.val,nb_perm=N))
  return(list(many.samp, 
              data.frame(ni=nrow(keepI),alphai=obs.cronI,nt=nrow(keepT),
                         alphat=obs.cronT, diff_obs=diff.obs, p_value=p.val,nb_perm=N)))
  
}

#1 fonction perm cronbach pour tous les themes et score global (finalement pas utilisée, mais elle marche)
# perm.cronbach<- function (x,N){
#   # x<-1
#   # x<-"tot"
#   xI<-get(paste0("vI",x))
#   xT<-get(paste0("vT",x))
#   
#   #if (is.numeric(x)){   
#   if (!is.na(as.numeric(x))){
#     keepI <- xI[!is.na(d[,paste0("res.theme",x)]),]
#     keepT <- xT[!is.na(f[,paste0("res.theme",x)]),]
#     keepIT <- rbind(keepI,keepT)
#     obs.cronI<- cronbach3(keepI)
#     obs.cronT<- cronbach3(keepT)
#     diff.obs <- abs(obs.cronI - obs.cronT)
#     
#     .gpe <- rep(c("int","tel"),c(nrow(keepI),nrow(keepT)))
#     
#     perm.test<- function(keepIT=keepIT,.gpe=.gpe){
#       mixgpe <- sample(.gpe,replace = FALSE)
#       int <- keepIT[mixgpe=="int",]
#       tel <- keepIT[mixgpe=="tel",]
#       cri<-cronbach3(int)
#       crt<-cronbach3(tel)
#       abs(cri-crt)
#     }
#   }
#   
#   else {
#     keepI <- xI[!is.na(d$res.score.final),]
#     keepT <- xT[!is.na(f$res.score.final),]
#     keepIT <- rbind(keepI,keepT)
#     obs.cronI<- cronbach(keepI)$alpha
#     obs.cronT<- cronbach(keepT)$alpha
#     diff.obs <- abs(obs.cronI - obs.cronT)
#     
#     .gpe <- rep(c("int","tel"),c(nrow(keepI),nrow(keepT)))
#     
#     perm.test<- function(keepIT=keepIT,.gpe=.gpe){
#       mixgpe <- sample(.gpe,replace = FALSE)
#       int <- keepIT[mixgpe=="int",]
#       tel <- keepIT[mixgpe=="tel",]
#       cri<-cronbach(int)$alpha
#       crt<-cronbach(tel)$alpha
#       abs(cri-crt)
#     }
#   }
#   many.samp<- replicate (N, perm.test(keepIT,.gpe))
#   
#   p.val <- length(many.samp[many.samp>= diff.obs]) / length(many.samp)
#   
#   #quantile(x = many.samp,probs = 0.95)
#   hist(many.samp,main=paste0("Difference cronbach theme",x))
#   abline(v=diff.obs,lwd=2,col=2)
#   
#   #mean(abs(many.samp)>abs(diff.obs))??
#   
#   return(data.frame(ni=nrow(keepI),alphai=obs.cronI,nt=nrow(keepT),alphat=obs.cronT, diff_obs=diff.obs, p_value=p.val,nb_perm=N))
# } 


############CALCUL DES ESTIMATEURS ET IC PAR BOOTSTRAP#############

#Pour tester les fonctions lignes à lignes : 
#1/ .theme <- "res.theme1" ou .theme<-"res.score.final"
        #=>lancer la ligne.df
#2/ .dat<-.df
        #=>lancer les lignes de la fonction stat àpartir de .dat<- na.omit(.dat) pour tester la fonction stat (indices ne marche pas sans boot)
        # si ça marche, lancer la fonction stat en entier
#3/ R<-100 pour tester la fonction boot
        # lancer .res<-boot(data=.df, statistic=fun.mean, R=R)
        # on peut rentrer dans .res pour voir l'échantillon bootstrap 
              #.res$t0 : estimateur
              #.res$t : les R valeurs
        #si ça marche, lancer la fonction boot.theme
#4/Pour tester la fonction BootMCi: lancer la ligne .bootres et les lignes suivantes.


#MOYENNE DES SCORES

#1. Je cree mon tableau/vecteur de data que j'appelle .df. Ce sera ensuite dans fonction boot
#2. Je peux modifier mon tableau/vecteur et appliquer une fonction a ce tableau/vecteur. ce sera fonction stat
#3. Je cree une fonction ci qui integre la fonction avec boot.

#Je cree la fonction stat pour avoir la moyenne
#data va prendre la valeur de .df, comme defini dans la fonction suivante
fun.mean <- function(data,indices){
  .dat <- data[indices,]
  .dat <- na.omit (.dat)
  #.int <- .dat[.dat$group=="int", "score"]
  #.tel <- .dat[.dat$group=="tel", "score"]
  mymean<-by(.dat$score,.dat$group,mean)
  return(mymean)
}

#Je cree la fonction boot. 
#le tableau .df defini l'argument data de la fonction fun mean. 
#boot repete la fonction fun.mean R fois : il genere un nouveau .df[indices,] et fait la moyenne. 
boot.theme <- function (.theme,R){
  .df <- data.frame (
    score=c (d[, .theme], f[, .theme]),
    group=c (rep ("int", nrow (d)), rep ("tel", nrow (f)))
  )
  .res<- boot(data=.df,statistic=fun.mean,R=R)
  return(.res)
}

#Je cree une fonction BootMCi qui reintegre resultat de boot.theme et ajoute les IC et met en forme
BootMCi <- function (.theme, R) {
  #  browser()
  .bootres <- boot.theme (.theme=.theme, R=R)
  .n <- length (.bootres$t0) #donne le nombre de resultat boot realise : 1 pour internet, 1 pour telephone
  .list.ci <- lapply(1:.n, function(x) boot.ci(.bootres,index=x,type="perc")) #fct boot.ci : intervalle de confiance pour chaque boot
  .res <- data.frame (t (sapply (.list.ci, function (x) x[["percent"]][4:5]))) #selectionne les valeur de IC
  rownames (.res) <- names (.bootres$t0) #appelle les lignes int et tel mais ca sera perdu ensuite
  colnames (.res) <- c ("CI_L", "CI_U")
  #.res$est <- apply (.bootres$t, 2, median) #pour faire la mediane des échantillons
  .res$est <- as.numeric (.bootres$t0) #selectionne l'estimateur et le rajoute au tableau .res : ici moyenne
  .res$n<-c(sum(!is.na(d[,.theme])),sum(!is.na(f[,.theme]))) #true(non manquant) vaut 1, donc nb de non NA
  .res <- .res[, c (4,3, 1, 2)] #remet les colonnes dans l'ordre
  .ans <- round (.res, 2) #fait un arrondi sur chaque valeur
  .ans <- data.frame (N=.res$n, meanCI=paste0 (.ans$est, " [", .ans$CI_L, "-", .ans$CI_U, "]")) #met en forme les valeurs
  .ans <- sapply  (c (internet=.ans[1, ], telephone=.ans[2, ]), as.vector )#c(int= , tel=)donne un titre, mais met en liste, 
  #sapply(as.vector) realigne en chaine de caracteres
  return (.ans)
}


#DIFFERENCE DES MOYENNES
boot.stat.diffM<-function(data,indices){
  .dat<-data[indices,]  
  .dat <- na.omit (.dat)
  .int <- .dat[.dat$group=="int", "score"]
  .tel <- .dat[.dat$group=="tel", "score"]
  .dif <- mean (.tel)-mean (.int)
  return(.dif)
}

boot.diff <- function (.theme,R){
  .df <- data.frame (
    score=c (d[, .theme], f[, .theme]),
    group=c (rep ("int", nrow (d)), rep ("tel", nrow (f)))
  )
  .res <- boot(data=.df,statistic=boot.stat.diffM, R=R)
  return(.res)
}

boot.diff.ci<- function (.theme,R){
  .bootres <- boot.diff (.theme=.theme,R=R)
  .n <- length (.bootres$t0) #donne le nombre de resultat boot realise : 1 pour internet, 1 pour telephone
  .list.ci <- lapply(1:.n, function(x) boot.ci(.bootres,index=x,type="perc")) #fct boot.ci : intervalle de confiance pour chaque boot
  .res <- data.frame (t (sapply (.list.ci, function (x) x[["percent"]][4:5]))) #selectionne les valeur de IC
  rownames (.res) <- names (.bootres$t0)
  colnames (.res) <- c ("CI_L", "CI_U")
  #.res$est <- apply (.bootres$t, 2, median)
  .res$est <- as.numeric (.bootres$t0) #selectionne l'estimateur et le rajoute au tableau .res
  .res <- .res[, c (3, 1, 2)] #remet les colonnes dans l'ordre
  .ans <- round (.res,2) #fait un arrondi sur chaque valeur
  .ans <-data.frame (diff_tel_min_int=paste0 (.ans$est, " [", .ans$CI_L, "-", .ans$CI_U, "]")) #met en forme les valeurs
  .ans <- as.vector(.ans[1,])
  return (.ans)
}

#EFFECT SIZE DES SCORES

#0. choisir un theme pour faire les essais : .theme<- "res.score.final"
#1. creer .df = tableau sur lequel on va travailler, qui comprend .theme
#2. faire .dat<-.df et creer une fonction de stat qui travaille sur .dat, 
#organiser le tout en fonction stat et fonction boot : rajouter .dat <-data[indices,] au debut de fonction stat et boot() en fin de fonction boot
#Resume des variables pour test de bootci: .theme<- "res.score.final" R<-100 puis faire tourner premiere ligne de bootci
boot.stat.es <- function (data, indices) {  #data=.df
  .dat <- data[indices,]
  .dat <- na.omit (.dat)
  .int <- .dat[.dat$group=="int", "score"]
  .tel <- .dat[.dat$group=="tel", "score"]
  .sd <- sd (c (.int, .tel))
  #.dif <- abs(mean (.int)-mean (.tel))
  .dif <- mean (.int)-mean (.tel)
  .es1 <- .dif / .sd
  return (es=.es1)
}

ES.boot <-  function (.theme, R) {
  .df <- data.frame (
    score=c (d[, .theme], f[, .theme]),
    group=c (rep ("int", nrow (d)), rep ("tel", nrow (f)))
  )
  .res <- boot (data=.df, statistic=boot.stat.es, R=R)
  return (.res)
}

BootCi <- function (.theme, R) {
  #  browser()
  .bootres <<- ES.boot (.theme=.theme,R=R)
  .n <- length (.bootres$t0) #donne le nombre de resultat boot realise : 1 pour internet, 1 pour telephone
  .list.ci <- lapply(1:.n, function(x) boot.ci(.bootres,index=x,type="perc")) #fct boot.ci : intervalle de confiance pour chaque boot
  #.list.ci <- lapply(1:.n, function(x) boot.ci(.bootres,index=x,type="bca"))
  .res <- data.frame (t (sapply (.list.ci, function (x) x[["percent"]][4:5]))) #selectionne les valeur de IC
  #.res <- data.frame (t (sapply (.list.ci, function (x) x[["bca"]][4:5]))) 
  rownames (.res) <- names (.bootres$t0)
  colnames (.res) <- c ("CI_L", "CI_U")
  #.res$est <- apply (.bootres$t, 2, median)
  .res$est <- as.numeric (.bootres$t0) #selectionne l'estimateur et le rajoute au tableau .res
  .res <- .res[, c (3, 1, 2)] #remet les colonnes dans l'ordre
  .ans <- round (.res,4) #fait un arrondi sur chaque valeur
  .ans <-data.frame (Effect_size=paste0 (.ans$est, " [", .ans$CI_L, "-", .ans$CI_U, "]")) #met en forme les valeurs
  .ans <- as.vector(.ans[1,])
  return (.ans)
}



############ TEST DE PERMUTATION DIFFERENCE MOYENNE ##########

# data1<-d
# data2<-f
# x<-1
# N<-100

perm.moyenne.th <- function (x,data1,data2,N){
  xI<-data1[ , paste0("res.theme",x)]
  xT<-data2[ , paste0("res.theme",x)]
  
  keepI <- na.omit(xI)
  keepT <- na.omit(xT)
  keepIT <- c(keepI,keepT)
  obsI<- mean(keepI)
  obsT<- mean(keepT)
  diff.obs <- abs(obsI - obsT)
  #diff.obs <- obsI - obsT
  
  .gpe <- rep(c("int","tel"),c(length(keepI),length(keepT)))
  
  perm.test<- function(keepIT=keepIT,.gpe=.gpe){
    mixgpe <- sample(.gpe,replace = FALSE)
    int <- keepIT[mixgpe=="int"]
    tel <- keepIT[mixgpe=="tel"]
    mi<-mean(int)
    mt<-mean(tel)
    diff<-abs(mi-mt)
    #diff<-mi-mt
    return(diff)
  }
  
  many.samp<- replicate (N, perm.test(keepIT,.gpe))
  
  #p.val <- 1-(length(many.samp[many.samp<= diff.obs])) / (length(many.samp))
  p.val <-length(many.samp[many.samp>= diff.obs]) / length(many.samp)
  hist(many.samp,main=paste0("Difference mean theme",x))
  abline(v=diff.obs,lwd=2,col=2)
  
  return(data.frame(ni=length(keepI),meani=obsI,nt=length(keepT),meant=obsT, diff_obs=diff.obs, p_value=p.val,nb_perm=N))
}

perm.moyenne.fin <- function (N){
  xI<-d$res.score.final
  xT<-f$res.score.final
  
  keepI <- na.omit(xI)
  keepT <- na.omit(xT)
  keepIT <- c(keepI,keepT)
  obsI<- mean(keepI)
  obsT<- mean(keepT)
  diff.obs <- abs(obsI - obsT)
  #diff.obs <- obsI - obsT
  
  .gpe <- rep(c("int","tel"),c(length(keepI),length(keepT)))
  
  perm.test<- function(keepIT=keepIT,.gpe=.gpe){
    mixgpe <- sample(.gpe,replace = FALSE)
    int <- keepIT[mixgpe=="int"]
    tel <- keepIT[mixgpe=="tel"]
    mi<-mean(int)
    mt<-mean(tel)
    diff<-abs(mi-mt)
    #diff<-mi-mt
    return(diff)
  }
  
  many.samp<- replicate (N, perm.test(keepIT,.gpe))
  
  #p.val <- 1-(length(many.samp[many.samp<= diff.obs])) / (length(many.samp))
  p.val <-length(many.samp[many.samp>= diff.obs]) / length(many.samp)
  hist(many.samp,main=paste0("Difference mean global score"))
  abline(v=diff.obs,lwd=2,col=2)
  
  return(data.frame(ni=length(keepI),meani=obsI,nt=length(keepT),meant=obsT, diff_obs=diff.obs, p_value=p.val,nb_perm=N))
}



############CALCUL DES ESTIMATEURS ET IC SANS BOOTSTRAP : PAS UTILISE POUR LE MANUSCRIT #############

#MOYENNE DES SCORES
#.theme<- "res.theme2"
NMCI<- function (.theme) {
  .df<-data.frame(score=c (d[, .theme], f[, .theme]), group=c (rep ("int", nrow (d)), rep ("tel", nrow (f))))
  .df<- na.omit(.df)
  listN <- by(.df$score,.df$group,length)
  res1<- data.frame(sapply(listN, function(x)x[[1]])) ;colnames (res1)="N"
  n.1 <- res1[1,]
  n.2 <- res1[2,]
  
  level = 95 ; alpha <- (100 - level)/100
  ddl <- (n.1 + n.2) - 2
  crit <- qt(alpha/2, ddl, lower.tail = FALSE) #t
  # crit <- qnorm(0.975,0,1) #z
  
  res1$mymean<- by(.df$score,.df$group,mean)
  res1$ICinf <- by(.df$score,.df$group, function(x)mean(x)-crit*sqrt(var(x)/length(x)))
  res1$ICsup <-by(.df$score,.df$group, function(x)mean(x)+crit*sqrt(var(x)/length(x)))
  
  anc1 <- round(res1,2) 
  anc1 <- data.frame(N= anc1$N, ICmean=paste0(anc1$mymean," [", anc1$ICinf,"–",anc1$ICsup,"]" ))
  anc1 <- sapply  (c (internet=anc1[1, ], telephone=anc1[2, ]), as.vector )#c(int= , tel=)donne un titre, mais met en liste, 
  
  return(anc1)
}

#DIFFERENCES DES MOYENNES
diffmfun<- function (.theme){
  .df<-data.frame(score=c (d[, .theme], f[, .theme]), group=c (rep ("int", nrow (d)), rep ("tel", nrow (f))))
  .df <- na.omit (.df)
  .int <- .df[.df$group=="int", "score"]
  .tel <- .df[.df$group=="tel", "score"]
  .dm <- mean(.tel)- mean(.int)
  .var1<- var(.int) ; .var2<- var(.tel)
  n.1 <-length(.int) ;  n.2 <- length (.tel)
  vardm <-(.var1/n.1)+(.var2/n.2)
  
  level = 95 ; alpha <- (100 - level)/100
  ddl <- (n.1 + n.2) - 2
  crit <- qt(alpha/2, ddl, lower.tail = FALSE) #t
  #crit <- qnorm(0.975,0,1) #z
  
  l.d <- .dm - crit * sqrt(vardm)
  u.d <- .dm + crit * sqrt(vardm)
  
  dmf <- round(data.frame(.dm,l.d,u.d),2)
  dmtab<- data.frame(ICmean= paste0(est=dmf$.dm," [",UCL=dmf$l.d,"-", UCU=dmf$u.d,"]"))
  dmtab <- as.vector(dmtab[1,])
  #EStab<- as.vector(data.frame(ICmean= paste0(est=ESf$d," [",UCL=ESf$l.d,"-", UCU=ESf$u.d,"]")))
  return (dmtab)
} 

#EFFECT SIZE DES SCORES
ESfun <- function (.theme) {  #.dat=.df
  .df<-data.frame(score=c (d[, .theme], f[, .theme]), group=c (rep ("int", nrow (d)), rep ("tel", nrow (f))))
  .df <- na.omit (.df)
  .int <- .df[.df$group=="int", "score"]
  .tel <- .df[.df$group=="tel", "score"]
  .sd <- sd (c (.int, .tel))
  .dif <- abs(mean (.int)-mean (.tel))
  .es1 <- .dif / .sd
  
  n.1 <-length(.int) ;  n.2 <- length (.tel)
  level = 95 ; alpha <- (100 - level)/100
  ddl <- (n.1 + n.2) - 2
  crit <- qt(alpha/2, ddl, lower.tail = FALSE) #t
  
  var.d <- (n.1 + n.2)/(n.1 * n.2) + (.es1^2)/(2 * (n.1 + n.2))
  l.d <- .es1 - crit * sqrt(var.d)
  u.d <- .es1 + crit * sqrt(var.d)
  ESf <- round(data.frame(d=.es1,l.d,u.d),3)
  #ESf <- data.frame(des(d=.es1,n.1=length(.int),n.2=length(.tel)))
  EStab<- data.frame(ICmean= paste0(est=ESf$d," [",UCL=ESf$l.d,"-", UCU=ESf$u.d,"]"))
  EStab <- as.vector(EStab[1,])
  #EStab<- as.vector(data.frame(ICmean= paste0(est=ESf$d," [",UCL=ESf$l.d,"-", UCU=ESf$u.d,"]")))
  return (EStab)
}





###############FONCTIONS POUR LES RAPPORTS PAR SERVICE####################

#pour test:
#.theme<-
#.dat<-.df
#.service<-"CHIRURGIE DIGESTIVE"

fun.mean <- function(data,indices){
  .dat <- data[indices,]
  .dat <- na.omit (.dat)
  #.int <- .dat[.dat$group=="int", "score"]
  #.tel <- .dat[.dat$group=="tel", "score"]
  mymean<-by(.dat$score,.dat$group,mean)
  return(mymean)
}

fun.meantot <- function(data,indices){
  .dat <- data[indices,]
  .dat <- na.omit (.dat)
  mymeantot <- mean(.dat$score)
  return(mymeantot)
}

#Je cree la fonction boot. 
#le tableau .df defini l'argument data de la fonction fun mean. 
#boot repete la fonction fun.mean R fois : il genere un nouveau .df[indices,] et fait la moyenne. 
boot.theme.rap <- function (.service,.theme,R){
  .df <- data.frame (
    score=c (d[d$service.Recode==.service, .theme], f[f$service.Recode==.service, .theme]),
    group=c (rep ("int", nrow (d[d$service.Recode==.service,])), rep ("tel", nrow (f[f$service.Recode==.service,])))
  )
  .res <- boot(data=.df,statistic=fun.mean,R=R)
  #meantot <- mean(.df$score, na.rm=T)
  .restot <- boot(data=.df, statistic=fun.meantot,R=R)
  #return(list(.res,meantot))
  return(list(.res,.restot))
}

#Je cree une fonction BootMCi qui reintegre resultat de boot.theme et ajoute les IC et met en forme
BootMCi.rap <- function (.service,.theme, R) {
  #  browser()
  .boottemp <- boot.theme.rap (.service=.service,.theme=.theme, R=R)
  .bootres <-.boottemp[[1]]
  .restot <-.boottemp[[2]]
  ############
  .n <- length (.bootres$t0) #donne le nombre de resultat boot realise : 1 pour internet, 1 pour telephone
  .list.ci <- lapply(1:.n, function(x) boot.ci(.bootres,index=x,type="perc")) #fct boot.ci : intervalle de confiance pour chaque boot
  .res <- data.frame (t (sapply (.list.ci, function (x) x[["percent"]][4:5]))) #selectionne les valeur de IC
  rownames (.res) <- names (.bootres$t0) #appelle les lignes int et tel mais ca sera perdu ensuite
  colnames (.res) <- c ("CI_L", "CI_U")
  #.res$est <- apply (.bootres$t, 2, median) #pour faire la mediane des échantillons
  .res$est <- as.numeric (.bootres$t0) #selectionne l'estimateur et le rajoute au tableau .res : ici moyenne
  .res$n<-c(sum(!is.na(d[d$service.Recode==.service,.theme])),sum(!is.na(f[f$service.Recode==.service,.theme]))) #true(non manquant) vaut 1, donc nb de non NA
  ###########
  .n <- length (.restot$t0) #donne le nombre de resultat boot realise : 1 pour internet, 1 pour telephone
  .list.ci <- lapply(1:.n, function(x) boot.ci(.restot,index=x,type="perc")) #fct boot.ci : intervalle de confiance pour chaque boot
  .tot <- data.frame (t (sapply (.list.ci, function (x) x[["percent"]][4:5]))) #selectionne les valeur de IC
  rownames (.tot) <- "tot" #appelle les lignes int et tel mais ca sera perdu ensuite
  colnames (.tot) <- c ("CI_L", "CI_U")
  #.res$est <- apply (.bootres$t, 2, median) #pour faire la mediane des échantillons
  .tot$est <- as.numeric (.restot$t0) #selectionne l'estimateur et le rajoute au tableau .res : ici moyenne
  .tot$n<-sum(!is.na(d[d$service.Recode==.service,.theme]))+ sum(!is.na(f[f$service.Recode==.service,.theme])) #true(non manquant) vaut 1, donc nb de non NA
  ############
  .res <- rbind(.res,.tot)
  .res <- .res[, c (4,3, 1, 2)] #remet les colonnes dans l'ordre
  .ans <- round (.res, 2) #fait un arrondi sur chaque valeur
  .ans <- data.frame (N=.res$n, meanCI=paste0 (.ans$est, " [", .ans$CI_L, "-", .ans$CI_U, "]")) #met en forme les valeurs
  .ans <- sapply  (c (internet=.ans[1, ], telephone=.ans[2, ], total=.ans[3,]), as.vector )#c(int= , tel=)donne un titre, mais met en liste, 
  #.ans <- c(service=.service,.ans) #pour avoir nom du service mais lourd
  #sapply(as.vector) realigne en chaine de caracteres
  return (.ans)
}





#calcul n, mean, sd, median interquartile

calIC.bootstrap<-function(thedata, nrep) {
  mymeans<-rep(NA,nrep);
  for (i in 1:nrep) {
    mymeans[i]<-mean(sample(x=thedata,size= length(thedata), replace = TRUE));
  }
  IC <- quantile(mymeans, prob=c(0.025,0.975));
  names(IC) <- c("2.5%", "97.5%")
  return(meanIC)
}

scoreestim <- function (x,myname=NULL) {
  x <- x[!is.na (x)]
  n <- length(x)
  mymean <- mean(x)
  myIC<-calIC.bootstrap(x,10000)
  # ab<-calsd.bootstrap(x,10000)   #la fonction n'existe plus mais inutile de calculer sd (qui plus est par bootstrap) quand on a déjà IC.
  #b<-median(x)
  #c<-quantile(x, probs = c(0.25, 0.75))
  if (is.null (myname)) {
    abc<-c(n,mymean,myIC)
    names(abc)<- c("n","mean","2.5%","97.5%") 
  } else {
    v<-myname
    abc<-c(v,n,mymean,myIC)
    names(abc)<- c("result","n","mean","2.5%","97.5%")  
  }
  return(abc)
}

#RESULTAT THEME SELON SERVICE
fun.score <- function(service,data) {
  #  browser()
  theme <- data.frame (t (sapply (1:6, function (x) scoreestim (data[data$service.Recode==service , paste0("res.theme",x)], myname=paste0("theme groupe",x)))))
  final <- data.frame(t(scoreestim(data[data$service.Recode==service,"res.score.final"],"score final")))
  output <- rbind(theme,final)
  #  write.table(print(output),file="clipboard",sep="\t",dec=",",row.names=FALSE) 
  output
}

fun.score.IT <- function(service,data_int,data_tel) {
  d <- data_int
  f <- data_tel
  tot <- rbind(d[d$service.Recode==service , ],f[f$service.Recode==service , ])
  themei <- data.frame (t (sapply (1:6, function (x) scoreestim (d[d$service.Recode==service , paste0("res.theme",x)], myname=paste0("theme groupe",x)))))
  finali <- data.frame(t(scoreestim(d[d$service.Recode==service,"res.score.final"],"score final")))
  themet <- data.frame (t (sapply (1:6, function (x) scoreestim (f[f$service.Recode==service , paste0("res.theme",x)], myname=paste0("theme groupe",x)))))
  finalt <- data.frame(t(scoreestim(f[f$service.Recode==service,"res.score.final"],"score final")))
  themeit<- data.frame (t (sapply (1:6, function (x) scoreestim (tot[tot$service.Recode==service , paste0("res.theme",x)], myname=paste0("theme groupe",x)))))
  finalit<- data.frame(t(scoreestim(tot[tot$service.Recode==service,"res.score.final"],"score final")))
  
  output <- rbind(theme,final)
  output
}


#FREQUENCE SOCIO SELON SERVICE
#internet et telephone separe
fun.socio3 <- function (service, data){
  soc1<-summary(data[data$service.Recode==service,"Age"])
  soc2<-table(data[data$service.Recode==service,"Sexe"])
  soc3<-table(data[data$service.Recode==service,"socio3"])
  soc4<-table(data[data$service.Recode==service,"socio4"])
  soc5<-table(data[data$service.Recode==service,"socio5"])
  soc8<-table(data[data$service.Recode==service,"socio8"])
  res1 <- paste0 ("Age:\n range: ", as.numeric (soc1)[1], ",", as.numeric (soc1)[6], "\n median [IQR]: ", as.numeric (soc1)[3], " [", as.numeric (soc1)[2], ", ", as.numeric (soc1)[5], "]")
  res2 <- paste0 ("\n\nSexe=M: ", soc2[2], " (", round (soc2[2]/sum (soc2)*100, 1), "%)")
  res3 <- paste0(c ("jusqu'au 1er cycle secondaire : ", " jusqu'au second cycle secondaire : ", " jusqu'au superieur court ou post secondaire : ", " enseignement superieur : "), soc3," (", round(prop.table(soc3),3)*100,"%)")
  res4 <- paste0(c("seul : "," en couple : "),soc4," (",round(prop.table(soc4),3)*100,"%)")
  res5 <- paste0(c("emploi ou etude : "," sans activite : "),soc5," (",round(prop.table(soc5),3)*100,"%)")
  res8 <- paste0(c("precaire : "," secu : "," secu et mutuelle : "),soc8," (",round(prop.table(soc8),3)*100,"%)")
  cat (service, ":\n\n", res1, res2, "\n\nEducation :\n", paste (res3, collapse="\n"),"\n\nStatut marital :\n",paste (res4, collapse="\n"),"\n\nActivité :\n",paste(res5,collapse="\n"),"\n\nRegime :\n",paste(res8,collapse="\n"), "\n\n********************\n\n")
  return("OK")
}

#internet et telephone dans meme feuille
#data1=telephone, data2 = internet
fun.socio4 <- function (service, data1, data2){
  soc1<-summary(data1[data1$service.Recode==service,"Age"])
  soc2<-table(data1[data1$service.Recode==service,"Sexe"])
  soc3<-table(data1[data1$service.Recode==service,"socio3"])
  soc4<-table(data1[data1$service.Recode==service,"socio4"])
  soc5<-table(data1[data1$service.Recode==service,"socio5"])
  soc8<-table(data1[data1$service.Recode==service,"socio8"])
  soc1b<-summary(data2[data2$service.Recode==service,"Age"])
  soc2b<-table(data2[data2$service.Recode==service,"Sexe"])
  soc3b<-table(data2[data2$service.Recode==service,"socio3"])
  soc4b<-table(data2[data2$service.Recode==service,"socio4"])
  soc5b<-table(data2[data2$service.Recode==service,"socio5"])
  soc8b<-table(data2[data2$service.Recode==service,"socio8"])
  res1 <- paste0 ("Age:\n range: ", as.numeric (soc1)[1], ",", as.numeric (soc1)[6]," / ", as.numeric(soc1b)[1],",",as.numeric (soc1b)[6], 
                  "\n median [IQR]: ", as.numeric (soc1)[3], " [", as.numeric (soc1)[2], ", ", as.numeric (soc1)[5], "]", 
                  " / ",as.numeric (soc1b)[3], " [", as.numeric (soc1b)[2], ", ", as.numeric (soc1b)[5], "]")
  res2 <- paste0 ("\n\nSexe=M: ", soc2[2], " (", round (soc2[2]/sum (soc2)*100, 1), "%)", " / ",soc2b[2], " (", round (soc2b[2]/sum (soc2b)*100, 1), "%)" )
  res3 <- paste0(c ("jusqu'au 1er cycle secondaire : ", " jusqu'au second cycle secondaire : ", " jusqu'au superieur court ou post secondaire : ", " enseignement superieur : "),
                 soc3," (", round(prop.table(soc3),3)*100,"%)"," / ",soc3b," (", round(prop.table(soc3b),3)*100,"%)")
  res4 <- paste0(c("seul : "," en couple : "),soc4,"(",round(prop.table(soc4),3)*100,"%)"," / ",soc4b,"(",round(prop.table(soc4b),3)*100,"%)")
  res5 <- paste0(c("emploi ou etude : "," sans activite : "),soc5," (",round(prop.table(soc5),3)*100,"%)"," / ",soc5b," (",round(prop.table(soc5b),3)*100,"%)")
  res8 <- paste0(c("precaire : "," secu : "," secu et mutuelle : "),soc8," (",round(prop.table(soc8),3)*100,"%)"," / ",soc8b," (",round(prop.table(soc8b),3)*100,"%)")
  cat (service, ": telephone/internet\n\n", res1, res2, "\n\nEducation :\n", paste (res3, collapse="\n"),"\n\nStatut marital :\n",paste (res4, collapse="\n"),"\n\nActivité :\n",paste(res5,collapse="\n"),"\n\nRegime :\n",paste(res8,collapse="\n"), "\n\n********************\n\n")
  return("OK")
}
#service<-"CHIRURGIE DIGESTIVE"
#data1<-d
#data2<-f

fun.socio5 <- function (service, data1, data2){
  soc1<-summary(data1[data1$service.Recode==service,"Age"])
  soc2<-table(data1[data1$service.Recode==service,"Sexe"])
  soc3<-table(data1[data1$service.Recode==service,"socio3"])
  soc4<-table(data1[data1$service.Recode==service,"socio4"])
  soc5<-table(data1[data1$service.Recode==service,"socio5"])
  soc8<-table(data1[data1$service.Recode==service,"socio8"])
  soc1b<-summary(data2[data2$service.Recode==service,"Age"])
  soc2b<-table(data2[data2$service.Recode==service,"Sexe"])
  soc3b<-table(data2[data2$service.Recode==service,"socio3"])
  soc4b<-table(data2[data2$service.Recode==service,"socio4"])
  soc5b<-table(data2[data2$service.Recode==service,"socio5"])
  soc8b<-table(data2[data2$service.Recode==service,"socio8"])
  res1 <- paste0 ("Age:\n range: ", as.numeric (soc1)[1], ",", as.numeric (soc1)[6]," / ", as.numeric(soc1b)[1],",",as.numeric (soc1b)[6], 
                  "\n median [IQR]: ", as.numeric (soc1)[3], " [", as.numeric (soc1)[2], ", ", as.numeric (soc1)[5], "]", 
                  " / ",as.numeric (soc1b)[3], " [", as.numeric (soc1b)[2], ", ", as.numeric (soc1b)[5], "]")
  res2 <- paste0 ("\n\nSexe=M: ", soc2[2], " (", round (soc2[2]/sum (soc2)*100, 1), "%)", " / ",soc2b[2], " (", round (soc2b[2]/sum (soc2b)*100, 1), "%)" )
  res3 <- paste0(c ("jusqu'au 1er cycle secondaire : ", " jusqu'au second cycle secondaire : ", " jusqu'au superieur court ou post secondaire : ", " enseignement superieur : "),
                 soc3," (", round(prop.table(soc3),3)*100,"%)"," / ",soc3b," (", round(prop.table(soc3b),3)*100,"%)")
  res4 <- paste0(c("seul : "," en couple : "),soc4,"(",round(prop.table(soc4),3)*100,"%)"," / ",soc4b,"(",round(prop.table(soc4b),3)*100,"%)")
  res5 <- paste0(c("emploi ou etude : "," sans activite : "),soc5," (",round(prop.table(soc5),3)*100,"%)"," / ",soc5b," (",round(prop.table(soc5b),3)*100,"%)")
  res8 <- paste0(c("precaire : "," secu : "," secu et mutuelle : "),soc8," (",round(prop.table(soc8),3)*100,"%)"," / ",soc8b," (",round(prop.table(soc8b),3)*100,"%)")
  cat (res1, res2, "\n\nEducation :\n", paste (res3, collapse="\n"),"\n\nStatut marital :\n",paste (res4, collapse="\n"),"\n\nActivité :\n",paste(res5,collapse="\n"),"\n\nRegime :\n",paste(res8,collapse="\n"), "\n\n********************\n\n")
  return("OK")
}








