berger<-read.csv2("C:/Users/adupont/Documents/berger/berger.csv",na.strings=c("","NC"),dec=".")
berger<-berger[!is.na(berger$centre),]
berger_noms<-read.csv2("C:/Users/adupont/Documents/berger/berger_noms.csv",na.strings=c("","NC"),dec=".",stringsAsFactors=FALSE)
berger_noms<-berger_noms[!is.na(berger_noms$variable),]


berger$crp<-as.numeric(as.character(berger$crp))
berger$hb<-as.numeric(as.character(berger$hb))
berger$sex<-as.factor(berger$sex)
levels(berger$sex)<- c("F","H")

berger$eth<-as.factor(berger$eth)
levels(berger$eth)<-c("Caucasien","Maghreb","Noir","Turc","autre")

berger$eth2<-berger$eth
levels(berger$eth2)<-c("Caucasien","No-Caucasien","No-Caucasien","No-Caucasien","No-Caucasien")


berger$sym<-as.factor(berger$sym)

levels(berger$sym)<-c("TVS","TVP","Claudication MI","Claudication MS","Raynaud",
"Necrose MS","Necrose MI","Ischémie MS","Ischémie MI","Arthralgies"
,"Raynaud+Claudi MS","Claudication MI+MS","Autres")


berger$sym2<-berger$sym

levels(berger$sym2)<-c("TV","TV","Claudication","Claudication","Raynaud",
                      "Necrose","Necrose","Ischémie","Ischémie","Autres"
                      ,"Autres","Claudication","Autres")

berger$sym3<-berger$sym

levels(berger$sym3)<-c("TV","TV","Claudication","Claudication","Raynaud ou Autres",
                       "Necrose ou Ischémie","Necrose ou Ischémie","Necrose ou Ischémie","Necrose ou Ischémie","Raynaud ou Autres"
                       ,"Raynaud ou Autres","Claudication","Raynaud ou Autres")



berger$dcd<-as.numeric(as.character(berger$dcd))
berger$dcd<-ifelse(is.na(berger$dcd), 0, berger$dcd)
table(berger$dcd)

berger$pm_diag<-as.factor(berger$pm_diag)
levels(berger$pm_diag)<-c("<50","50-100","100-250","250-500","500-1000",">1000")

berger$pm_diag2<-berger$pm_diag
levels(berger$pm_diag2)<-c("<100","<100",">=100",">=100",">=100",">=100")




berger$sub<-as.factor(berger$sub)
levels(berger$sub)<-c("0 sub","Substitunicotine","Pharmaco")

berger$coag<-as.factor(berger$coag)
levels(berger$coag)<-c("Pas de tt","HBPM","AVK")

berger$typ_amput1<-as.factor(berger$typ_amput1)
levels(berger$typ_amput1)<-c("mineure","majeure")
berger$typ_amput2<-as.factor(berger$typ_amput2)
levels(berger$typ_amput2)<-c("mineure","majeure","majeure")
berger$typ_amput3<-as.factor(berger$typ_amput3)
levels(berger$typ_amput3)<-c("mineure","majeure")
#berger$typ_amput4<-as.factor(berger$typ_amput4)
# levels(berger$typ_amput4)<-c("mineure","majeure")



berger$loc_amput1<-as.factor(berger$loc_amput1)
levels(berger$loc_amput1)<-c("MID","MIG","MSD","MSG","MS")
berger$loc_amput2<-as.factor(berger$loc_amput2)
levels(berger$loc_amput2)<-c("MID","MIG","MSD","MSG")
berger$loc_amput3<-as.factor(berger$loc_amput3)
levels(berger$loc_amput3)<-c("MID","MIG","MSD","MSG")
# berger$loc_amput4<-as.factor(berger$loc_amput4)
# levels(berger$loc_amput4)<-c("MID","MIG","MSD","MSG")


berger$typ_gv1<-as.factor(berger$typ_gv1)
levels(berger$typ_gv1)<-c("endovascu", "pontage","sympathectomie")
berger$typ_gv2<-as.factor(berger$typ_gv2)
levels(berger$typ_gv2)<-c("endovascu", "pontage","sympathectomie")
berger$typ_gv3<-as.factor(berger$typ_gv3)
levels(berger$typ_gv3)<-c("endovascu", "pontage","sympathectomie")


berger$typ_sym_fu<-as.factor(berger$typ_sym_fu) 
levels(berger$typ_sym_fu)<-c("TVS","TVP","claudication MI","claudication MS","Raynaud","Necrose MS",
"Necrose MI","Ischémie MS","Ischémie MI","arthralgies")

berger$typ_evt1<-as.factor(berger$typ_evt1) 
c("Claudication","Ischémie critique","Tr Trophique","TVP","TVS","Osteite","IDM","Autres")






for(i in 
c("ms_claudi", "ms_isch", "ms_necro", "ms_inf", "mi_claudi", 
"mi_isch", "mi_necro")){
  berger[,i]<-as.factor(berger[,i])
  levels(berger[,i])<-c("0","Unilateral","Bilateral")

  
}

for(i in 
    c("ms_claudi", "ms_isch", "ms_necro", "ms_inf", "mi_claudi", 
      "mi_isch", "mi_necro")){
  
  j<-paste0(i,2)
  berger[,j]<-berger[,i]
  levels(berger[,j])<-c("0","1","1")
  
  
}

berger$mi_inf<-as.factor(berger$mi_inf)




for ( i in 
c( "atcd_fam_cv", 
"hta", "diabete", "dys", "tabac", "tabac_passif", "cannabis", 
"oh", "cocaine", "hero", "ray_diag", "arthradiag", "pouls_diag", 
"tvp","tvs",
"colchicine", "aspirine",  
"plavix", 
"statin",  "inh",  "vas", "iec",  "tab_1", "tab_3", "tab_5",
 "can_fu", "sym_fu",
"dcd", "grossese")){
  berger[,i]<-as.factor(berger[,i])
  #levels(berger[,i])<-c("Non","Oui")
  }
  

### DATES###
berger$ddn<-as.Date(as.character(berger$ddn),format="%d/%m/%Y")

for(i in 
c( "date_sym", 
  "date_diag",  "date_colchi", 
  "date_fin_colchi", "date_asp", "date_fin_asp", 
   "date_pla", "date_fin_pla",  "date_coag", "date_fin_coag", 
  "date_statin", "date_fin_statin", "date_inh", 
  "date_fin_inh",  "date_vas", "date_fin_vas",  "date_iec", 
  "date_fin_iec", "date_amput1",  
  "date_amput2",  "date_amput3",  "date_evt1",  "date_evt2","date_evt3", 
   "date_evt4",  
  "date_evt5",  "date_evt6", "date_evt7", "date_gv1", 
   "date_gv2", "date_gv3", "date_gv4", 
   "date_gv5",
  "date_fu")){
berger[,i]<-as.Date(as.character(berger[,i]),format="%d/%m/%Y")
}


for(i in 
    c( "iloprost_1", "iloprost_2", "iloprost_3", 
       "iloprost_4", "iloprost_5", "iloprost_6", "iloprost_7", "iloprost_8")){
  berger[,i]<-as.Date(as.character(berger[,i]),format="%d/%m/%Y")
}








### Amputation###

berger$amputt<-as.factor(ifelse(!is.na(berger$typ_amput1)|!is.na(berger$typ_amput2)|!is.na(berger$typ_amput3),1,0))
table(berger$amputt)





berger$amputtm<-as.factor(ifelse((!is.na(berger$typ_amput1) & berger$typ_amput1=="majeure")
                                 
                                 |(!is.na(berger$typ_amput2) & berger$typ_amput2=="majeure")|
                                   
                                   (!is.na(berger$typ_amput3) & berger$typ_amput3=="majeure"),1,0))
table(berger$amputtm)
a1<-as.numeric(!is.na(berger$typ_amput1))
a2<-as.numeric(!is.na(berger$typ_amput2))
a3<-as.numeric(!is.na(berger$typ_amput3))

berger$amputtot<-a1+a2+a3
sum(berger$amputtot)

a1<-as.numeric(!is.na(berger$typ_amput1) & berger$typ_amput1=="majeure")
a2<-as.numeric(!is.na(berger$typ_amput2) & berger$typ_amput2=="majeure")
a3<-as.numeric(!is.na(berger$typ_amput3) & berger$typ_amput3=="majeure")

berger$amputtotm<-a1+a2+a3
sum(berger$amputtotm)

# 1 amput, 2 deces
berger$amput_deces<-ifelse(!is.na(berger$date_amput1) & difftime(berger$date_diag, berger$date_amput1) <=0 ,1,0)
berger$amput_deces<-ifelse(difftime(berger$date_diag, berger$date_amput1) >0 & !is.na(berger$date_amput1) 
                           & difftime(berger$date_diag, berger$date_amput2) <=0 & !is.na(berger$date_amput2),
 1,berger$amput_deces)
berger$amput_deces<-ifelse(difftime(berger$date_diag, berger$date_amput2) >0 & !is.na(berger$date_amput2) 
                           & difftime(berger$date_diag, berger$date_amput3) <=0 & !is.na(berger$date_amput3),
                           1,berger$amput_deces)
berger$amput_deces<-ifelse(berger$amput_deces==0 & berger$dcd==1,2,berger$amput_deces)


table(berger$amput_deces,exclude=NULL)

berger$date_amput_deces<-ifelse(berger$amput_deces==1 & !is.na(berger$date_amput1) & 
                                  difftime(berger$date_diag, berger$date_amput1) <=0
 ,berger$date_amput1,NA)
berger$date_amput_deces<-ifelse(berger$amput_deces==1 & difftime(berger$date_diag, berger$date_amput1) >0 & !is.na(berger$date_amput1) 
                                & difftime(berger$date_diag, berger$date_amput2) <=0 & !is.na(berger$date_amput2),
                                berger$date_amput2,berger$date_amput_deces)                              
berger$date_amput_deces<-ifelse(berger$amput_deces==1 &difftime(berger$date_diag, berger$date_amput2) >0 & !is.na(berger$date_amput2) 
                                & difftime(berger$date_diag, berger$date_amput3) <=0 & !is.na(berger$date_amput3),
                                berger$date_amput3,berger$date_amput_deces)       
berger$date_amput_deces<-ifelse(is.na(berger$date_amput_deces),berger$date_fu,berger$date_amput_deces)  
berger$date_amput_deces<-as.Date(berger$date_amput_deces,origin="1970-01-01")  


table(difftime(berger$date_diag, berger$date_amput_deces))

berger$delai_amput_deces<-difftime( berger$date_amput_deces,berger$date_diag,units=c("days"))/365

### Survie sans amputation majeures ###

# 1 amput majeure ,2 deces

berger$amputm_deces<-ifelse(!is.na(berger$date_amput1) & difftime(berger$date_diag, berger$date_amput1) <=0 
                             & berger$typ_amput1=="majeure",1,0)
berger$amputm_deces<-ifelse(difftime(berger$date_diag, berger$date_amput1) >0 & !is.na(berger$date_amput1) 
                           & difftime(berger$date_diag, berger$date_amput2) <=0 & !is.na(berger$date_amput2)
                           & berger$typ_amput2=="majeure",
                           1,berger$amputm_deces)
berger$amputm_deces<-ifelse(difftime(berger$date_diag, berger$date_amput2) >0 & !is.na(berger$date_amput2) 
                           & difftime(berger$date_diag, berger$date_amput3) <=0 & !is.na(berger$date_amput3)
                           & berger$typ_amput3=="majeure",
                           1,berger$amputm_deces)
berger$amputm_deces<-ifelse(berger$amputm_deces==0 & berger$dcd==1,2,berger$amputm_deces)

table(berger$amputm_deces,exclude=NULL)
table(berger$amput_deces,exclude=NULL)


berger$amputmoudeces<-ifelse(berger$amputm_deces==1|berger$amputm_deces==2,1,0)

berger$date_amputm_deces<-ifelse(berger$amputm_deces==1 & !is.na(berger$date_amput1) & 
                                  difftime(berger$date_diag, berger$date_amput1) <=0 & 
                                   berger$typ_amput1=="majeure"
                                ,berger$date_amput1,NA)
berger$date_amputm_deces<-ifelse(berger$amputm_deces==1 & difftime(berger$date_diag, berger$date_amput1) >0 
                                 & !is.na(berger$date_amput1) 
                                & difftime(berger$date_diag, berger$date_amput2) <=0 & 
                                  !is.na(berger$date_amput2) & 
                                  berger$typ_amput2=="majeure" ,
                                berger$date_amput2,berger$date_amputm_deces)                              
berger$date_amputm_deces<-ifelse(berger$amputm_deces==1 &difftime(berger$date_diag, berger$date_amput2) >0 & !is.na(berger$date_amput2) 
                                & difftime(berger$date_diag, berger$date_amput3) <=0 & !is.na(berger$date_amput3)
                                &  berger$typ_amput3=="majeure",
                                berger$date_amput3,berger$date_amputm_deces)       
berger$date_amputm_deces<-ifelse(is.na(berger$date_amputm_deces),berger$date_fu,berger$date_amputm_deces)  
berger$date_amputm_deces<-as.Date(berger$date_amputm_deces,origin="1970-01-01")  


table(difftime(berger$date_diag, berger$date_amput_deces))

berger$delai_amputm_deces<-difftime( berger$date_amputm_deces,berger$date_diag,units=c("days"))/365








### Survie###
str(berger$dcd)

berger$dcd_s<-as.numeric(as.character(berger$dcd))
berger$delai_dcd<-difftime(berger$date_fu,berger$date_diag,units=c("days"))/365

### Evenements###

berger$claudit<-as.factor(ifelse((!is.na(berger$typ_evt1) & berger$typ_evt1==1)
                                 
                                 |(!is.na(berger$typ_evt2) & berger$typ_evt2==1)|
                                   
                                   (!is.na(berger$typ_evt3) & berger$typ_evt3==1)|
                                   (!is.na(berger$typ_evt4) & berger$typ_evt4==1)|
                                   (!is.na(berger$typ_evt5) & berger$typ_evt5==1)|
                                   (!is.na(berger$typ_evt6) & berger$typ_evt6==1)|
                                   (!is.na(berger$typ_evt7) & berger$typ_evt7==1)
                               ,1,0))
berger$clauditot<-as.numeric(!is.na(berger$typ_evt1) & berger$typ_evt1==1)+
  as.numeric((!is.na(berger$typ_evt2) & berger$typ_evt2==1))+
  as.numeric((!is.na(berger$typ_evt3) & berger$typ_evt3==1))+
  as.numeric((!is.na(berger$typ_evt4) & berger$typ_evt4==1))+
  as.numeric((!is.na(berger$typ_evt5) & berger$typ_evt5==1))+
  as.numeric((!is.na(berger$typ_evt6) & berger$typ_evt6==1))+
  as.numeric((!is.na(berger$typ_evt7) & berger$typ_evt7==1))




evenement<-function(evt, nom){
berger[,paste0(nom,"t")]<-as.factor(ifelse((!is.na(berger$typ_evt1) & berger$typ_evt1==evt)
                                 
                                 |(!is.na(berger$typ_evt2) & berger$typ_evt2==evt)|
                                   
                                   (!is.na(berger$typ_evt3) & berger$typ_evt3==evt)|
                                   (!is.na(berger$typ_evt4) & berger$typ_evt4==evt)|
                                   (!is.na(berger$typ_evt5) & berger$typ_evt5==evt)|
                                   (!is.na(berger$typ_evt6) & berger$typ_evt6==evt)|
                                   (!is.na(berger$typ_evt7) & berger$typ_evt7==evt)
       ,1,0))}

evenement2<-function(evt, nom){
berger[,paste0(nom,"tot")]<-as.numeric(!is.na(berger$typ_evt1) & berger$typ_evt1==evt)+
  as.numeric((!is.na(berger$typ_evt2) & berger$typ_evt2==evt))+
  as.numeric((!is.na(berger$typ_evt3) & berger$typ_evt3==evt))+
  as.numeric((!is.na(berger$typ_evt4) & berger$typ_evt4==evt))+
  as.numeric((!is.na(berger$typ_evt5) & berger$typ_evt5==evt))+
  as.numeric((!is.na(berger$typ_evt6) & berger$typ_evt6==evt))+
  as.numeric((!is.na(berger$typ_evt7) & berger$typ_evt7==evt))
return(berger[,paste0(nom,"tot")])}


berger$ischemiet<-evenement(evt = 2,nom="ischemie")
berger$ischemietot<-evenement2(evt = 2,nom="ischemie")

berger$tropht<-evenement(evt = 3,nom="troph")
berger$trophtot<-evenement2(evt = 3,nom="troph")

berger$tvpt<-evenement(evt = 4,nom="tvp")
berger$tvptot<-evenement2(evt = 4,nom="tvp")

berger$tvst<-evenement(evt = 5,nom="tvs")
berger$tvstot<-evenement2(evt = 5,nom="tvs")

berger$idmt<-evenement(evt = 7,nom="idm")
berger$idmtot<-evenement2(evt = 7,nom="idm")


### Gestes vaculaires###


vasculaire<-function(evt, nom){
  berger[,paste0(nom,"t")]<-as.factor(ifelse((!is.na(berger$typ_gv1) & berger$typ_gv1==evt)
                                             
                                             |(!is.na(berger$typ_gv2) & berger$typ_gv2==evt)|
                                               
                                               (!is.na(berger$typ_gv3) & berger$typ_gv3==evt)

                                             ,1,0))}



berger$endovascut<-vasculaire(evt = "endovascu",nom="endovascu")
berger$pontaget<-vasculaire(evt = "pontage",nom="pontage")
berger$sympathet<-vasculaire(evt = "sympathectomie",nom="sympathectomie")


vasculaire2<-function(evt, nom){
  berger[,paste0(nom,"tot")]<-as.numeric(!is.na(berger$typ_gv1) & berger$typ_gv1==evt)+
    as.numeric((!is.na(berger$typ_gv2) & berger$typ_gv2==evt))+
    as.numeric((!is.na(berger$typ_gv3) & berger$typ_gv3==evt))
  
  return(berger[,paste0(nom,"tot")])}


berger$endovascutot<-vasculaire2(evt = "endovascu",nom="endovascu")
berger$pontagetot<-vasculaire2(evt = "pontage",nom="pontage")
berger$sympathetot<-vasculaire2(evt = "sympathectomie",nom="sympathectomie")


### délai 1er sympt délai dia ###

berger$delai_s<-difftime(berger$date_diag,berger$date_sym, units=c("days"))/30.25
berger$delai_s<-as.numeric(berger$delai_s)


controledatedia<-berger[berger$delai_s<0 & !is.na(berger$delai_s),c("nip","ddn")]

write.csv2(controledatedia,file="date1symdatedia.csv")



### en attendant on vire le patient avec sa date de dernière nouvelle bizare ###


#berger<-berger[! berger$nip %in% c(140),]
berger$age_dia<-difftime(berger$date_diag,berger$ddn,units=c("days"))/365
berger$age_dia<-as.numeric(as.character(berger$age_dia))

### Survie sans évenements ###

berger$eve_deces<-ifelse(!is.na(berger$date_evt1) & difftime(berger$date_diag, berger$date_evt1) <=0 ,1,0)
berger$eve_deces<-ifelse(difftime(berger$date_diag, berger$date_evt1) >0 & !is.na(berger$date_evt1) 
                           & difftime(berger$date_diag, berger$date_evt2) <=0 & !is.na(berger$date_evt2),
                           1,berger$eve_deces)
berger$eve_deces<-ifelse(difftime(berger$date_diag, berger$date_evt2) >0 & !is.na(berger$date_evt2) 
                           & difftime(berger$date_diag, berger$date_evt3) <=0 & !is.na(berger$date_evt3),
                           1,berger$eve_deces)
berger$eve_deces<-ifelse(difftime(berger$date_diag, berger$date_evt3) >0 & !is.na(berger$date_evt3) 
                         & difftime(berger$date_diag, berger$date_evt4) <=0 & !is.na(berger$date_evt4),
                         1,berger$eve_deces)

berger$eve_deces<-ifelse(difftime(berger$date_diag, berger$date_evt4) >0 & !is.na(berger$date_evt4) 
                         & difftime(berger$date_diag, berger$date_evt5) <=0 & !is.na(berger$date_evt5),
                         1,berger$eve_deces)
berger$eve_deces<-ifelse(difftime(berger$date_diag, berger$date_evt5) >0 & !is.na(berger$date_evt5) 
                         & difftime(berger$date_diag, berger$date_evt6) <=0 & !is.na(berger$date_evt6),
                         1,berger$eve_deces)
berger$eve_deces<-ifelse(difftime(berger$date_diag, berger$date_evt6) >0 & !is.na(berger$date_evt6) 
                         & difftime(berger$date_diag, berger$date_evt7) <=0 & !is.na(berger$date_evt7),
                         1,berger$eve_deces)



berger$eve_deces<-ifelse(berger$eve_deces==0 & berger$dcd==1,2,berger$eve_deces)


table(berger$eve_deces,exclude=NULL)


berger$eveoudeces<-ifelse(berger$eve_deces==1 |berger$eve_deces==2,1,0)

table(berger$eveoudeces,exclude=NULL)

berger$date_eve_deces<-ifelse(berger$eve_deces==1 & !is.na(berger$date_evt1) & 
                                  difftime(berger$date_diag, berger$date_evt1) <=0
                                ,berger$date_evt1,NA)
berger$date_eve_deces<-ifelse(berger$eve_deces==1 & difftime(berger$date_diag, berger$date_evt1) >0 & 
                                !is.na(berger$date_evt1) 
                                & difftime(berger$date_diag, berger$date_evt2) <=0 & !is.na(berger$date_evt2),
                                berger$date_evt2,berger$date_eve_deces)                              
berger$date_eve_deces<-ifelse(berger$eve_deces==1 &difftime(berger$date_diag, berger$date_evt2) >0 & 
                                !is.na(berger$date_evt2) 
                                & difftime(berger$date_diag, berger$date_evt3) <=0 & 
                                !is.na(berger$date_evt3),
                                berger$date_evt3,berger$date_eve_deces)   
berger$date_eve_deces<-ifelse(berger$eve_deces==1 &difftime(berger$date_diag, berger$date_evt3) >0 & 
                                !is.na(berger$date_evt3) 
                              & difftime(berger$date_diag, berger$date_evt4) <=0 & 
                                !is.na(berger$date_evt4),
                              berger$date_evt4,berger$date_eve_deces)    

berger$date_eve_deces<-ifelse(berger$eve_deces==1 &difftime(berger$date_diag, berger$date_evt4) >0 & 
                                !is.na(berger$date_evt4) 
                              & difftime(berger$date_diag, berger$date_evt5) <=0 & 
                                !is.na(berger$date_evt5),
                              berger$date_evt5,berger$date_eve_deces)    
berger$date_eve_deces<-ifelse(berger$eve_deces==1 &difftime(berger$date_diag, berger$date_evt5) >0 & 
                                !is.na(berger$date_evt5) 
                              & difftime(berger$date_diag, berger$date_evt6) <=0 & 
                                !is.na(berger$date_evt6),
                              berger$date_evt6,berger$date_eve_deces)    
berger$date_eve_deces<-ifelse(berger$eve_deces==1 &difftime(berger$date_diag, berger$date_evt6) >0 & 
                                !is.na(berger$date_evt6) 
                              & difftime(berger$date_diag, berger$date_evt7) <=0 & 
                                !is.na(berger$date_evt7),
                              berger$date_evt7,berger$date_eve_deces)    


berger$date_eve_deces<-ifelse(is.na(berger$date_eve_deces),berger$date_fu,berger$date_eve_deces)  
berger$date_eve_deces<-as.Date(berger$date_eve_deces,origin="1970-01-01")  


table(difftime(berger$date_diag, berger$date_eve_deces))

berger$delai_eve_deces<-difftime( berger$date_eve_deces,berger$date_diag,units=c("days"))/365
