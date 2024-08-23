library(dplyr)
library(gtsummary)
library(xlsx)

##############################################################
### Statistical analysis of a cohort of patients who
### developed chronical urticaria (CU) after COVID-19 
### mRNA vaccination
### Tables 1+2 and Figure 3A and 3J of the related publication
##############################################################


#set dir
datapath='/MYPATHDIR/UC/'
outdir='/MYPATHDIR/'

########## survey 1 (Table 1) ###########

db=read.csv(file=paste0(datapath,'UC_db.csv'))
nrow(db)
db=db[db$Survey.1 ==1,]
nrow(db) #89 patients

survey1=db %>% select(Sexe,Age,Jours.entre.vaccin.et.urticaire...0.calculable.,
                      Quelle.marque.de.vaccin.avez.vous.reçu.,
                      L.urticaire.est.elle.toujours.active.aujourd.hui.,
                      Votre.urticaire.peut.elle.être.déclenchée.par.des.facteurs.extérieurs.,
                      Lesquels...choice.grattage.,Lesquels...choice.soleil.,Lesquels...choice.eau.,
                      Lesquels...choice.froid.,Lesquels...choice.effort.physique.,
                      X..12.first.month,X.12.present,
                      La.première.semaine.des.symptômes,La.dernière.semaine.des.symptômes, #mean number of lesion
                      La.première.semaine.des.symptômes.1,La.dernière.semaine.des.symptômes.1, #pruritus severity
                      A.quelle.fréquence.prenez.vous...avez.vous.pris.les.antihistaminiques.,
                      Jusqu.à.combien.de.comprimés.d.antihistaminiques.par.jour.prenez.vous...avez.vous.pris.,
                      Avez.vous.déjà.fait.de.l.urticaire.par.le.passé.,En.moyenne..combien.de.temps.l.urticaire.avait.elle.duré.,
                      Est.ce.que.le.paracétamol..Dafalgan..et.ou.les.anti.inflammatoires..Irfen..Voltaren.....péjorent.les.symptômes.de.votre.urticaire.,
                      Avez.vous.déjà.eu.une.infection.par.COVID.19..,Est.ce.que.l.infection.par.COVID.19.a.péjoré.vos.syptômes.d.urticaire.,
                      Avez.vous.de.l.asthme.,Avez.vous.un.rhume.des.foins.,Avez.vous.des.allergies.médicamenteuses.
                      )

df=survey1 %>%  tbl_summary(statistic = list(
  all_continuous() ~ "{mean} ({sd})",
  all_categorical() ~ "{n} ({p}%)"
),)

df=as.data.frame(df)
file=paste0(outdir,"survey1.xlsx")
write.xlsx(df, file, sheetName ="survey1",   col.names = T, row.names = F, append = FALSE)




########## survey 2 (Table 2)  ###########

db=read.csv(file=paste0(datapath,'UC_db.csv'))
nrow(db)
db=db[db$Survey.2 >0,]
nrow(db) #61 patients

survey2=db %>% select(Sexe,Age,UC.Booster,
                      Quelle.marque.de.vaccin.avez.vous.reçu.,
                      Souffrez.vous.encore.d.une.urticaire..,
                      ucsuffer_type.0..inductible..1.spontanée..2.les.2,
                      Qu.est.ce.qui.provoque..votre.urticaire.....choice.grattage.,
                      Qu.est.ce.qui.provoque..votre.urticaire.....choice.soleil.,
                      Qu.est.ce.qui.provoque..votre.urticaire.....choice.eau.,
                      Qu.est.ce.qui.provoque..votre.urticaire.....choice.froid.,
                      Qu.est.ce.qui.provoque..votre.urticaire.....choice.effort.,
                      Qu.est.ce.qui.provoque..votre.urticaire.....choice.vibration.,
                      Total...12,
                      En.moyenne.durant.la.dernière.semaine..combien.de.lésions.d.urticaire.étaient.présentes....,
                      Comment.qualifiez.vous.vos.démangeaisons.durant.la.dernière.semaine..,
                      Actuellement.combien.de.comprimés..cp..antihistaminique.devez.vous.prendre.par.jour.en.moyenne..,
                      Avez.vous.pris.ou.devez.vous.encore.recevoir.des.injections.d.omalizumab..xolair..pour.calmer.l.urticaire..,
                      Prenez.vous.toujours.un.traitement.d.omalizumab..xolair...,
                      Avez.vous.pris.de.la.prednisone...cortisone..par.la.bouche.pour.calmer.les.lésions..,
                      Les.anti.inflammatoires..par.exemple.dafalgan..irfen..aspirine...peuvent.ils.péjorer.vos.symptômes.de.l.urticaire....,
                      Avez.vous.reçu.un.nouveau.rappel.avec.le.vaccin.COVID.19.après.le.début.de.l.urticaire..,
                      Avez.vous.noter.une.péjoration.des.symptômes.de.l.urticaire.chronique..,
                      Quel.vaccin.COVID.19.avez.vous.reçu.en.rappel.après.le.début.de.l.urticaire..,
                      Avez.vous.fait.le.COVID.après.le.début.de.l.urticaire...,
                      Est.ce.que.le.COVID.a.péjoré.les.symptômes.de.l.urticaire..)

df=survey2 %>%  tbl_summary(statistic = list(
  all_continuous() ~ "{mean} ({sd})",
  all_categorical() ~ "{n} ({p}%)"
),)

df=as.data.frame(df)
file=paste0(outdir,"survey2.xlsx")
write.xlsx(df, file, sheetName ="survey2",   col.names = T, row.names = F, append = FALSE)



############ Fig 3A ################
db2=read.csv(file=paste0(datapath,'blood_test.csv'))
nrow(db2)
db2=db2[!is.na(db2$X10..POS),]
db2=db2[db2$valide==1,]
db2=db2[!is.na(db2$N.dose),]
nrow(db2)
db2$subgroup=ifelse(db2$'X10..POS'==1,'BATpos','BATneg')



runFisher=function(nr.bat.neg,nr.bat.pos,tot1,tot2){
  print("Observed")
  yes=c(nr.bat.neg,nr.bat.pos)
  no=c(tot2-nr.bat.neg,tot1-nr.bat.pos)
  df2=data.frame(yes,no)
  df=as.data.frame(t(df2))
  colnames(df)=c("BATneg","BATpos")
  print(df)
  chi <- chisq.test(df)
  print("Expected")
  print(chi$expected)
  fi= fisher.test(df)
  out=c(yes,fi$p.value)
  df.out=data.frame(out)
  dfout=as.data.frame(t(df.out))
  colnames(dfout)=c("BATneg","BATpos","pvalue")
  dfout
}

runFisherMultiple=function(nr.bat.neg.1,nr.bat.pos.1,nr.bat.neg.2,nr.bat.pos.2,nr.bat.neg.3,nr.bat.pos.3){
  print("Observed")
  var1=c(nr.bat.neg.1,nr.bat.pos.1)
  var2=c(nr.bat.neg.2,nr.bat.pos.2)
  var3=c(nr.bat.neg.3,nr.bat.pos.3)
  df2=data.frame(var1,var2,var3)
  df=as.data.frame(t(df2))
  colnames(df)=c("BATneg","BATpos")
  print(df)
  chi <- chisq.test(df)
  print("Expected")
  print(chi$expected)
  fi= fisher.test(df)
  print(paste0("pvalue:", fi$p.value))
}

runFisherMultiple4=function(nr.bat.neg.1,nr.bat.pos.1,nr.bat.neg.2,nr.bat.pos.2,nr.bat.neg.3,nr.bat.pos.3,nr.bat.neg.4,nr.bat.pos.4){
  print("Observed")
  var1=c(nr.bat.neg.1,nr.bat.pos.1)
  var2=c(nr.bat.neg.2,nr.bat.pos.2)
  var3=c(nr.bat.neg.3,nr.bat.pos.3)
  var4=c(nr.bat.neg.4,nr.bat.pos.4)
  df2=data.frame(var1,var2,var3,var4)
  df=as.data.frame(t(df2))
  colnames(df)=c("BATneg","BATpos")
  print(df)
  chi <- chisq.test(df)
  print("Expected")
  print(chi$expected)
  fi= fisher.test(df)
  print(paste0("pvalue:", fi$p.value))
}

tot1=nrow(db2[db2$subgroup=='BATpos',])
tot2=nrow(db2[db2$subgroup=='BATneg',])
print(paste0("BATpos ",tot1))
print(paste0("BATneg ",tot2))

#unvaccinated
nr.bat.pos=nrow(db2[db2$N.dose==0 & db2$subgroup =='BATpos',])
nr.bat.neg=nrow(db2[db2$N.dose==0 & db2$subgroup =='BATneg',])
runFisher(nr.bat.neg,nr.bat.pos,tot1,tot2)

tot1=nrow(db2[db2$subgroup=='BATpos' & db2$Vaccin.0.non..=='Pfizer' ,])
tot2=nrow(db2[db2$subgroup=='BATneg'& db2$Vaccin.0.non..=='Pfizer' ,])
print(paste0("BATpos ",tot1))
print(paste0("BATneg ",tot2))


#Vaccine BNT 162b2
nr.bat.pos=nrow(db2[db2$N.dose==1 & db2$Vaccin.0.non..=='Pfizer' & db2$subgroup =='BATpos',])
nr.bat.neg=nrow(db2[db2$N.dose==1 & db2$Vaccin.0.non..=='Pfizer'& db2$subgroup =='BATneg',])
#we use as 'reference' the unvaccinated patients
tot1=nrow(db2[db2$N.dose==0 & db2$subgroup =='BATpos',]) + nr.bat.pos
tot2=nrow(db2[db2$N.dose==0 & db2$subgroup =='BATneg',])+nr.bat.neg
print(paste0("BATpos ",tot1))
print(paste0("BATneg ",tot2))
runFisher(nr.bat.neg,nr.bat.pos,tot1,tot2)

#Vaccine BNT 162b2
nr.bat.pos=nrow(db2[db2$N.dose==2 & db2$Vaccin.0.non..=='Pfizer' & db2$subgroup =='BATpos',])
nr.bat.neg=nrow(db2[db2$N.dose==2 & db2$Vaccin.0.non..=='Pfizer'& db2$subgroup =='BATneg',])
#we use as 'reference' the unvaccinated patients
tot1=nrow(db2[db2$N.dose==0 & db2$subgroup =='BATpos',]) + nr.bat.pos
tot2=nrow(db2[db2$N.dose==0 & db2$subgroup =='BATneg',])+nr.bat.neg
print(paste0("BATpos ",tot1))
print(paste0("BATneg ",tot2))
runFisher(nr.bat.neg,nr.bat.pos,tot1,tot2)

#Vaccine BNT 162b2
nr.bat.pos=nrow(db2[db2$N.dose==3 & db2$Vaccin.0.non..=='Pfizer' & db2$subgroup =='BATpos',])
nr.bat.neg=nrow(db2[db2$N.dose==3 & db2$Vaccin.0.non..=='Pfizer'& db2$subgroup =='BATneg',])
#we use as 'reference' the unvaccinated patients
tot1=nrow(db2[db2$N.dose==0 & db2$subgroup =='BATpos',]) + nr.bat.pos
tot2=nrow(db2[db2$N.dose==0 & db2$subgroup =='BATneg',])+nr.bat.neg
print(paste0("BATpos ",tot1))
print(paste0("BATneg ",tot2))
runFisher(nr.bat.neg,nr.bat.pos,tot1,tot2)


tot1=nrow(db2[db2$subgroup=='BATpos' & db2$Vaccin.0.non..=='Moderna' ,])
tot2=nrow(db2[db2$subgroup=='BATneg'& db2$Vaccin.0.non..=='Moderna' ,])
print(paste0("BATpos ",tot1))
print(paste0("BATneg ",tot2))
#Vaccine mRNA-1273
nr.bat.pos=nrow(db2[db2$N.dose==1 & db2$Vaccin.0.non..=='Moderna' & db2$subgroup =='BATpos',])
nr.bat.neg=nrow(db2[db2$N.dose==1 & db2$Vaccin.0.non..=='Moderna'& db2$subgroup =='BATneg',])
#we use as 'reference' the unvaccinated patients
tot1=nrow(db2[db2$N.dose==0 & db2$subgroup =='BATpos',]) + nr.bat.pos
tot2=nrow(db2[db2$N.dose==0 & db2$subgroup =='BATneg',])+nr.bat.neg
print(paste0("BATpos ",tot1))
print(paste0("BATneg ",tot2))
runFisher(nr.bat.neg,nr.bat.pos,tot1,tot2)

#Vaccine mRNA-1273
nr.bat.pos=nrow(db2[db2$N.dose==2 & db2$Vaccin.0.non..=='Moderna' & db2$subgroup =='BATpos',])
nr.bat.neg=nrow(db2[db2$N.dose==2 & db2$Vaccin.0.non..=='Moderna'& db2$subgroup =='BATneg',])
#we use as 'reference' the unvaccinated patients
tot1=nrow(db2[db2$N.dose==0 & db2$subgroup =='BATpos',]) + nr.bat.pos
tot2=nrow(db2[db2$N.dose==0 & db2$subgroup =='BATneg',])+nr.bat.neg
print(paste0("BATpos ",tot1))
print(paste0("BATneg ",tot2))
runFisher(nr.bat.neg,nr.bat.pos,tot1,tot2)

#Vaccine mRNA-1273
nr.bat.pos=nrow(db2[db2$N.dose==3 & db2$Vaccin.0.non..=='Moderna' & db2$subgroup =='BATpos',])
nr.bat.neg=nrow(db2[db2$N.dose==3 & db2$Vaccin.0.non..=='Moderna'& db2$subgroup =='BATneg',])
#we use as 'reference' the unvaccinated patients
tot1=nrow(db2[db2$N.dose==0 & db2$subgroup =='BATpos',]) + nr.bat.pos
tot2=nrow(db2[db2$N.dose==0 & db2$subgroup =='BATneg',])+nr.bat.neg
print(paste0("BATpos ",tot1))
print(paste0("BATneg ",tot2))
runFisher(nr.bat.neg,nr.bat.pos,tot1,tot2)


tot1=nrow(db2[db2$subgroup=='BATpos' & db2$Vaccin.0.non..=='Moderna' & db2$N.dose==3,])
tot2=nrow(db2[db2$subgroup=='BATneg'& db2$Vaccin.0.non..=='Moderna' & db2$N.dose==3,])
print(paste0("BATpos ",tot1))
print(paste0("BATneg ",tot2))

#Vaccine mRNA-1273 booster CU
nr.bat.pos.1=nrow(db2[db2$N.dose==3 & db2$Vaccin.0.non..=='Moderna' & db2$Group..CSED.0..COVID.LONG.1...UC.2..UCcont.3..HC.4..==2 & db2$subgroup =='BATpos',])
nr.bat.neg.1=nrow(db2[db2$N.dose==3 & db2$Vaccin.0.non..=='Moderna'& db2$Group..CSED.0..COVID.LONG.1...UC.2..UCcont.3..HC.4..==2 & db2$subgroup =='BATneg',])
#Vaccine mRNA-1273 booster long-covid
nr.bat.pos.2=nrow(db2[db2$N.dose==3 & db2$Vaccin.0.non..=='Moderna' & (db2$Group..CSED.0..COVID.LONG.1...UC.2..UCcont.3..HC.4..==1 |
                                                                       db2$Group..CSED.0..COVID.LONG.1...UC.2..UCcont.3..HC.4..==0 )& db2$subgroup =='BATpos',])
nr.bat.neg.2=nrow(db2[db2$N.dose==3 & db2$Vaccin.0.non..=='Moderna'&(db2$Group..CSED.0..COVID.LONG.1...UC.2..UCcont.3..HC.4..==1 |
                                                                     db2$Group..CSED.0..COVID.LONG.1...UC.2..UCcont.3..HC.4..==0 ) & db2$subgroup =='BATneg',])
#Vaccine mRNA-1273 booster immunovax
nr.bat.pos.3=nrow(db2[db2$N.dose==3 & db2$Vaccin.0.non..=='Moderna' & db2$Group..CSED.0..COVID.LONG.1...UC.2..UCcont.3..HC.4..==4 & db2$subgroup =='BATpos',])
nr.bat.neg.3=nrow(db2[db2$N.dose==3 & db2$Vaccin.0.non..=='Moderna'& db2$Group..CSED.0..COVID.LONG.1...UC.2..UCcont.3..HC.4..==4 & db2$subgroup =='BATneg',])
#Run multiple for the 3 cohorts
runFisherMultiple(nr.bat.neg.1,nr.bat.pos.1,nr.bat.neg.2,nr.bat.pos.2,nr.bat.neg.3,nr.bat.pos.3)

#Vaccine mRNA-1273 booster female
nr.bat.pos=nrow(db2[db2$N.dose==3 & db2$Vaccin.0.non..=='Moderna' & db2$Gender=='F' & db2$subgroup =='BATpos',])
nr.bat.neg=nrow(db2[db2$N.dose==3 & db2$Vaccin.0.non..=='Moderna'& db2$Gender=='F' & db2$subgroup =='BATneg',])
runFisher(nr.bat.neg,nr.bat.pos,tot1,tot2)
#Vaccine mRNA-1273 booster male
nr.bat.pos=nrow(db2[db2$N.dose==3 & db2$Vaccin.0.non..=='Moderna' & db2$Gender=='M' & db2$subgroup =='BATpos',])
nr.bat.neg=nrow(db2[db2$N.dose==3 & db2$Vaccin.0.non..=='Moderna'& db2$Gender=='M' & db2$subgroup =='BATneg',])
runFisher(nr.bat.neg,nr.bat.pos,tot1,tot2)




############ Fig 3J ################
db2=read.csv(file=paste0(datapath,'blood_test.csv'))
nrow(db2)
db2=db2[!is.na(db2$Phadiatop..0.neg..1.pos.),]
db2=db2[db2$valide==1,]
db2=db2[!is.na(db2$N.dose),]
nrow(db2)
db2$subgroup=ifelse(db2$'Phadiatop..0.neg..1.pos.'==1,'PhadiatopPos','PhadiatopNeg')

print_percentage=function(nr.phad.neg,nr.phad.pos){
  per.neg=round(((nr.phad.neg/(nr.phad.neg+nr.phad.pos))*100),digit=0)
  per.pos=round(((nr.phad.pos/(nr.phad.neg+nr.phad.pos))*100),digit=0)
  print(paste0(nr.phad.neg," (",per.neg,"%)    ",nr.phad.pos," (",per.pos,"%)"))
}

tot1=nrow(db2[db2$subgroup=='PhadiatopPos',])
tot2=nrow(db2[db2$subgroup=='PhadiatopNeg',])
print(paste0("PhadiatopPos ",tot1))
print(paste0("PhadiatopNeg ",tot2))

#CU
nr.phad.pos.1=nrow(db2[db2$Group..CSED.0..COVID.LONG.1...UC.2..UCcont.3..HC.4..==2 & db2$subgroup =='PhadiatopPos',])
nr.phad.neg.1=nrow(db2[db2$Group..CSED.0..COVID.LONG.1...UC.2..UCcont.3..HC.4..==2 & db2$subgroup =='PhadiatopNeg',])
runFisher(nr.phad.neg.1,nr.phad.pos.1,tot1,tot2)
print_percentage(nr.phad.neg.1,nr.phad.pos.1)

#LongCOV
nr.phad.pos.2=nrow(db2[(db2$Group..CSED.0..COVID.LONG.1...UC.2..UCcont.3..HC.4..==1 |
                       db2$Group..CSED.0..COVID.LONG.1...UC.2..UCcont.3..HC.4..==0)  & db2$subgroup =='PhadiatopPos',])
nr.phad.neg.2=nrow(db2[(db2$Group..CSED.0..COVID.LONG.1...UC.2..UCcont.3..HC.4..==1 |
                       db2$Group..CSED.0..COVID.LONG.1...UC.2..UCcont.3..HC.4..==0) & db2$subgroup =='PhadiatopNeg',])
runFisher(nr.phad.neg.2,nr.phad.pos.2,tot1,tot2)
print_percentage(nr.phad.neg.2,nr.phad.pos.2)

#Immunovax
nr.phad.pos.3=nrow(db2[db2$Group..CSED.0..COVID.LONG.1...UC.2..UCcont.3..HC.4..==4 & db2$subgroup =='PhadiatopPos',])
nr.phad.neg.3=nrow(db2[db2$Group..CSED.0..COVID.LONG.1...UC.2..UCcont.3..HC.4..==4 & db2$subgroup =='PhadiatopNeg',])
runFisher(nr.phad.neg.3,nr.phad.pos.3,tot1,tot2)
print_percentage(nr.phad.neg.3,nr.phad.pos.3)


#unvaccinated
tot1=nrow(db2[db2$subgroup=='PhadiatopPos',])
tot2=nrow(db2[db2$subgroup=='PhadiatopNeg' ,])
print(paste0("PhadiatopPos ",tot1))
print(paste0("PhadiatopNeg ",tot2))
nr.phad.pos=nrow(db2[db2$N.dose==0 & db2$subgroup =='PhadiatopPos',])
nr.phad.neg=nrow(db2[db2$N.dose==0 & db2$subgroup =='PhadiatopNeg',])
runFisher(nr.phad.neg,nr.phad.pos,tot1,tot2)
print_percentage(nr.phad.neg,nr.phad.pos)



#vaccination
tot1=nrow(db2[db2$subgroup=='PhadiatopPos'& db2$N.dose>0,])
tot2=nrow(db2[db2$subgroup=='PhadiatopNeg' & db2$N.dose>0,])
print(paste0("PhadiatopPos ",tot1))
print(paste0("PhadiatopNeg ",tot2))
nr.phad.pos.1=nrow(db2[db2$Vaccin.0.non..=='Moderna' & db2$subgroup =='PhadiatopPos',])
nr.phad.neg.1=nrow(db2[db2$Vaccin.0.non..=='Moderna' & db2$subgroup =='PhadiatopNeg',])
nr.phad.pos.2=nrow(db2[db2$Vaccin.0.non..=='Pfizer' & db2$subgroup =='PhadiatopPos',])
nr.phad.neg.2=nrow(db2[db2$Vaccin.0.non..=='Pfizer' & db2$subgroup =='PhadiatopNeg',])
nr.phad.pos.3=nrow(db2[db2$Vaccin.0.non..=='Janssen' & db2$subgroup =='PhadiatopPos',])
nr.phad.neg.3=nrow(db2[db2$Vaccin.0.non..=='Janssen' & db2$subgroup =='PhadiatopNeg',])
runFisherMultiple(nr.phad.neg.1,nr.phad.pos.1,nr.phad.neg.2,nr.phad.pos.2,nr.phad.neg.3,nr.phad.pos.3)



#vaccinated by dose
#3dose+unvaccinated
nr.phad.pos.1=nrow(db2[db2$N.dose==1 & db2$subgroup =='PhadiatopPos',])
nr.phad.neg.1=nrow(db2[db2$N.dose==1 & db2$subgroup =='PhadiatopNeg',])
nr.phad.pos.2=nrow(db2[db2$N.dose==2 & db2$subgroup =='PhadiatopPos',])
nr.phad.neg.2=nrow(db2[db2$N.dose==2 & db2$subgroup =='PhadiatopNeg',])
nr.phad.pos.3=nrow(db2[db2$N.dose==3 & db2$subgroup =='PhadiatopPos',])
nr.phad.neg.3=nrow(db2[db2$N.dose==3 & db2$subgroup =='PhadiatopNeg',])
nr.phad.pos.4=nrow(db2[db2$N.dose==0 & db2$subgroup =='PhadiatopPos',])
nr.phad.neg.4=nrow(db2[db2$N.dose==0 & db2$subgroup =='PhadiatopNeg',])
runFisherMultiple4(nr.phad.neg.1,nr.phad.pos.1,nr.phad.neg.2,nr.phad.pos.2,nr.phad.neg.3,nr.phad.pos.3,nr.phad.neg.4,nr.phad.pos.4)

#BATpos
nr.phad.pos=nrow(db2[db2$'X10..POS'==1 & db2$subgroup =='PhadiatopPos',])
nr.phad.neg=nrow(db2[db2$'X10..POS'==1 & db2$subgroup =='PhadiatopNeg',])
runFisher(nr.phad.neg,nr.phad.pos,tot1,tot2)
print_percentage(nr.phad.neg,nr.phad.pos)
#BATneg
nr.phad.pos=nrow(db2[db2$'X10..POS'==0 & db2$subgroup =='PhadiatopPos',])
nr.phad.neg=nrow(db2[db2$'X10..POS'==0 & db2$subgroup =='PhadiatopNeg',])
runFisher(nr.phad.neg,nr.phad.pos,tot1,tot2)
print_percentage(nr.phad.neg,nr.phad.pos)




