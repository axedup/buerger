#natvar<-



patientb<-TABKRIS(baz=berger,vect.var = c("sex", "eth", "atcd_fam_cv", 
                                          "hta", "diabete", "dys",
                                          "taille", "poids", "imc", 
                                          "tabac", "tabac_pa", 
                                          "tabac_aged", "tabac_passif", "cannabis", 
                                          "cannabis_jointsem", 
                                          "oh", "cocaine", "hero", "sym","delai_s","age_dia"),
                  vect.quali = c(1,1,1,1,1,1,0,0,0,1,0,0,1,1,0,1,1,1,1,0,0),
                  varint=NULL,valvarint = NULL,
                  nomvarint = NULL,
                  test=NULL,
                  vecnoms=c("Sexe", "Ethnie", "Atcd fam CV", 
                            "HTA", "Diabete", "Dyslipidemies", "Taille", "Poids", "IMC", 
                            "Tabac", "Tabac en pa", "Age debut tabagisme", "Tabagisme passif", 
                            "Cannabis", "Cannabis en joint par sem", "Alcool", "Cocaine", "Heroine", 
                             "1ers symp","Delai 1er sym-dia (mois)","Age au dia"),valeurs=NULL,
                  vecrefs=NULL,varassoc=NULL, codassoc=NULL,pres=
                    c("","","","","","","med_mm","med_mm","med_mm","","med_mm","med_mm","","","med_mm","","","","","med_mm","med_mm")
                    ,langue="en",digits=2)

colnames(patientb)<-c("Parameters","Parameters","N","n(\\%) med[Q1;Q3](min,max)")

caracb<-TABKRIS(baz=berger,vect.var = c("leuco", "hb", "plq", "crp", "vs", "fg", "creat", 
                                        "homo", "tvp", "tvs", "ray_diag", "arthradiag", "pouls_diag", 
                                        "ms_claudi", "ms_isch", "ms_necro", "ms_inf", "mi_claudi", "pm_diag", 
                                        "mi_isch", "mi_necro", "mi_inf"),
                  vect.quali = c(0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                  varint=NULL,valvarint = NULL,
                  nomvarint = NULL,
                  test=NULL,
                  vecnoms=c("Leucocytes", "Hb", "Plaquettes", "CRP ", "VS", "Fg", "Créat", 
                            "Homocystéine", "TVP", "TVS", "Raynaud", "Arthradiag", "Poulsdiag", 
                            "Claudication MS", "Ischémie MS", "Nécrose MS", "Infection MS", 
                            "MI Claudi Diag", "PM diag", "Ischémie MI", "Nécrose MI", "Infection MI"),valeurs=NULL,
                  vecrefs=NULL,varassoc=NULL, codassoc=NULL,pres=c("med_mm","med_mm","med_mm","med_mm","med_mm","med_mm","med_mm","med_mm","","","","","","","","","","","","","","","","","")
                   
                  ,langue="en",digits=2)

colnames(caracb)<-c("Parameters","Parameters","N","n(\\%) med[Q1;Q3](min,max)")

ttb<-TABKRIS(baz=berger,vect.var = c("sub","colchicine","aspirine","plavix","coag","statin","inh"
                                     ,"vas","iec"),
                vect.quali = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                varint=NULL,valvarint = NULL,
                nomvarint = NULL,
                test=NULL,
                vecnoms=c("Substitut","Colchicine","Aspirine","Plavix","TT anti coagulant","Statine",
                          "Inh","VAS","IEC"),valeurs=NULL,
                vecrefs=NULL,varassoc=NULL, codassoc=NULL,pres=NULL
                
                ,langue="en",digits=2)

colnames(ttb)<-c("Parameters","Parameters","N","n(\\%)")

vascb<-TABKRIS(baz=berger,vect.var = c("endovascut","endovascutot","pontaget","pontagetot","sympathet",
                                       "sympathetot"),
              vect.quali = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
              varint=NULL,valvarint = NULL,
              nomvarint = NULL,
              test=NULL,
              vecnoms=c("Au moins un geste endovasculaire","Gestes endovasculaire par patient",
                        "Au moins un pontage",
                        "Pontages par patient",
                        "Au moins une sympathectomie",
                        "sympathectomies par patient"),valeurs=NULL,
              vecrefs=NULL,varassoc=NULL, codassoc=NULL,pres=NULL
              
              ,langue="en",digits=2)

colnames(vascb)<-c("Parameters","Parameters","N","n(\\%)")


outb<-TABKRIS(baz=berger,vect.var = c("amputt","amputtot","amputtm","amputtotm","claudit",
                                      "clauditot","ischemiet","ischemietot","tropht",
                                      "trophtot","tvpt","tvptot","tvst","tvstot",
                                      "idmt","idmtot"),
             vect.quali = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
             varint=NULL,valvarint = NULL,
             nomvarint = NULL,
             test=NULL,
             vecnoms=c("Au moins une amputation","Amputations par patient","Au moins une amputation majeure",
                       "Amputations majeures par patient",
                       "Au moins une claudication",
                       "claudications par patient","Au moins une ischemie aigue","Ischemie aigue par patient","Au moins un trouble trophique",
                       "Trouble trophique","Au moins un TVP","TVP par patient","Au moins un TVS","TVS par patient",
                       "Au moins un IDM","IDM par patient"),valeurs=NULL,
             vecrefs=NULL,varassoc=NULL, codassoc=NULL,pres=NULL
             
             ,langue="en",digits=2)

colnames(outb)<-c("Parameters","Parameters","N","n(\\%)")





### Survie globale ###

os<-Surv(event = berger$dcd_s,time=as.numeric(berger$delai_dcd))
oss <- survfit( os ~ 1)
plot(oss,xlab="")

re<-summary(oss,censored = TRUE)

censure<-as.data.frame(cbind(re$time[re$n.event==0],re$surv[re$n.event==0] ))
colnames(censure)<-c("time","ce")
evenement<-as.data.frame(cbind(re$time,re$surv ))
colnames(evenement)<-c("time","ev")
debut<-data.frame(time=0,ev=1)
evenement<-rbind(debut,evenement)



intervalle<-as.data.frame(cbind(re$time,re$upper
                                ,re$lower ))
colnames(intervalle)<-c("time","haut","bas")

# pfs<-Surv(event = berger$pfst,time=berger$delai_pfs)
# pfss <- survfit( pfs ~ 1)
# plot(pfss,xlab="Délai depuis la date de diagnostic stade IV (mois)")

#ref<-summary(pfss,censored = TRUE)

# censurpf<-as.data.frame(cbind(ref$time[ref$n.event==0],ref$surv[ref$n.event==0] ))
# colnames(censurpf)<-c("time","ce")
# evenementpf<-as.data.frame(cbind(ref$time,ref$surv ))
# colnames(evenementpf)<-c("time","ev")
# debut<-data.frame(time=0,ev=1)
# evenementpf<-rbind(debut,evenementpf)
# 
# 
# intervallepf<-as.data.frame(cbind(ref$time,ref$upper
#                                   ,ref$lower ))
#colnames(intervallepf)<-c("time","haut","bas")


km_os<-ggplot()+ geom_step(data=evenement,aes(x=time, y=ev),color="black", direction="hv")  +
  geom_ribbon(data=intervalle, aes(x=time, ymin=bas, ymax=haut),linetype="dashed",fill="grey",alpha="0.4")+
  geom_step(data=intervalle,aes(x=time, y=haut),color="black" ,direction="hv",linetype="dashed")+
  geom_step(data=intervalle,aes(x=time, y=bas),color="black", direction="hv",linetype="dashed")+
  scale_x_continuous(breaks=c(0,5,10,15,20,25,30),expand = c(0, 0),limits=c(0,max(re$time)))+
  
  
  #geom_point(data=censure, aes(x=time, y=ce),shape=3,size=1 )+
  #ggtitle("DurÃ©e de vie des implants") +
  xlab("Delay from diagnostic (an)")+
  ylab("Probability")+
  #geom_step(data=gri,aes(x=time, y=ics), direction="hv",color="gray10",linetype="dashed" )+
  #  geom_step(data=gri,aes(x=time, y=ici), direction="hv",color="gray10",linetype="dashed" )+
  #  geom_step(data=gci,aes(x=time, y=ics), direction="hv",color="black",linetype="dashed" )+
  #  geom_step(data=gci,aes(x=time, y=ici), direction="hv",color="black",linetype="dashed" )+
  #
  #scale_colour_manual("",values = c("Rupture"="blue", "Autres causes"="black"))+annotate(geom="text", x=52, y=0.91, label="Tous les parcours",color="black", size=4)+coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),expand = c(0, 0),limits = c(0.7, 1))+
  #coord_cartesian(ylim=c(0,1))+
  theme_classic()+
  theme(legend.position="bottom",
        legend.title=element_blank())+
  ggtitle("")




test= data.frame(

  x = c(3.5,1,3,5,10,20),
  y = c(33,25,27,36,23,25),
  n=c(230,re$n.risk[which.min((re$time-1)<0)-1],re$n.risk[which.min((re$time-3)<0)-1],
      re$n.risk[which.min((re$time-5)<0)-1],
      re$n.risk[which.min((re$time-10)<0)-1],
      re$n.risk[which.min((re$time-20)<0)-1]),
  ypos=c(0.04,0.04,0.04,0.04,0.04,0.04)

)

for (ii in 1:nrow(test))
{
  #display numbers at each visit
  km_os=km_os+ annotation_custom(grob = textGrob(test$n[ii]),  
                                 xmin = test$x[ii], 
                                 xmax = test$x[ii], 
                                 ymin = test$ypos[ii], 
                                 ymax = test$ypos[ii])
  
}


km_os=km_os+annotation_custom(grob = textGrob("N at risk"),  
                              xmin = 6.5, 
                              xmax = 6.5, 
                              ymin = 0.1, 
                              ymax = 0.1)



gt <- ggplot_gtable(ggplot_build(km_os))
gt$layout$clip[gt$layout$name=="panel"] <- "off"

### survie amputations ###

ampu<-Surv(event = berger$amput_deces,time=as.numeric(berger$delai_amput_deces))
ampuss <- survfit( ampu ~ 1)
plot(ampuss,xlab="")

reamp<-summary(ampuss,censored = TRUE)

censurea<-as.data.frame(cbind(reamp$time[reamp$n.event==0],reamp$surv[reamp$n.event==0] ))
colnames(censurea)<-c("time","ce")
evenementa<-as.data.frame(cbind(reamp$time,reamp$surv ))
colnames(evenementa)<-c("time","ev")
debuta<-data.frame(time=0,ev=1)
evenementa<-rbind(debuta,evenementa)



intervallea<-as.data.frame(cbind(reamp$time,reamp$upper
                                ,reamp$lower ))
colnames(intervallea)<-c("time","haut","bas")

# pfs<-Surv(event = berger$pfst,time=berger$delai_pfs)
# pfss <- survfit( pfs ~ 1)
# plot(pfss,xlab="Délai depuis la date de diagnostic stade IV (mois)")

#ref<-summary(pfss,censored = TRUE)

# censurpf<-as.data.frame(cbind(ref$time[ref$n.event==0],ref$surv[ref$n.event==0] ))
# colnames(censurpf)<-c("time","ce")
# evenementpf<-as.data.frame(cbind(ref$time,ref$surv ))
# colnames(evenementpf)<-c("time","ev")
# debut<-data.frame(time=0,ev=1)
# evenementpf<-rbind(debut,evenementpf)
# 
# 
# intervallepf<-as.data.frame(cbind(ref$time,ref$upper
#                                   ,ref$lower ))
#colnames(intervallepf)<-c("time","haut","bas")


km_amp<-ggplot()+ geom_step(data=evenementa,aes(x=time, y=ev),color="black", direction="hv")  +
  geom_ribbon(data=intervallea, aes(x=time, ymin=bas, ymax=haut),linetype="dashed",fill="grey",alpha="0.4")+
  geom_step(data=intervallea,aes(x=time, y=haut),color="black" ,direction="hv",linetype="dashed")+
geom_step(data=intervallea,aes(x=time, y=bas),color="black", direction="hv",linetype="dashed")+
  scale_x_continuous(breaks=c(0,5,10,15,20,25,30),expand = c(0, 0),limits=c(0,max(reamp$time)))+
  
  
  #geom_point(data=censure, aes(x=time, y=ce),shape=3,size=1 )+
  #ggtitle("DurÃ©e de vie des implants") +
  xlab("Delay from diagnosis (years)")+
  ylab("Probability")+
  #geom_step(data=gri,aes(x=time, y=ics), direction="hv",color="gray10",linetype="dashed" )+
  #  geom_step(data=gri,aes(x=time, y=ici), direction="hv",color="gray10",linetype="dashed" )+
  #  geom_step(data=gci,aes(x=time, y=ics), direction="hv",color="black",linetype="dashed" )+
  #  geom_step(data=gci,aes(x=time, y=ici), direction="hv",color="black",linetype="dashed" )+
  #
  #scale_colour_manual("",values = c("Rupture"="blue", "Autres causes"="black"))+annotate(geom="text", x=52, y=0.91, label="Tous les parcours",color="black", size=4)+coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),expand = c(0, 0),limits = c(0, 1))+
  #coord_cartesian(ylim=c(0,1))+
  theme_classic()+
  theme(legend.position="bottom",
        legend.title=element_blank())+
  ggtitle("")




test= data.frame(
  
  x = c(0.5,1,3,5,10,20),
  y = c(33,25,27,36,23,25),
  n=c("",reamp$n.risk[which.min((reamp$time-1)<0)-1],reamp$n.risk[which.min((reamp$time-3)<0)-1],
      reamp$n.risk[which.min((reamp$time-5)<0)-1],
      reamp$n.risk[which.min((reamp$time-10)<0)-1],
      reamp$n.risk[which.min((reamp$time-20)<0)-1]),
  ypos=c(0.04,0.04,0.04,0.04,0.04,0.04)
  
)



for (ii in 1:nrow(test))
{
  #display numbers at each visit
  km_amp=km_amp+ annotation_custom(grob = textGrob(test$n[ii]),  
                                 xmin = test$x[ii], 
                                 xmax = test$x[ii], 
                                 ymin = test$ypos[ii], 
                                 ymax = test$ypos[ii])
  
}


km_amp=km_amp+annotation_custom(grob = textGrob("N at risk"),  
                              xmin = 2.0, 
                              xmax = 2.0, 
                              ymin = 0.1, 
                              ymax = 0.1)



gta <- ggplot_gtable(ggplot_build(km_amp))
gta$layout$clip[gta$layout$name=="panel"] <- "off"


### survie sans amputations majeures ###

ampum<-Surv(event = berger$amputmoudeces,time=as.numeric(berger$delai_amputm_deces))
ampussm <- survfit( ampum ~ 1)
plot(ampussm,xlab="")

reampm<-summary(ampussm,censored = TRUE)

censuream<-as.data.frame(cbind(reampm$time[reampm$n.event==0],reampm$surv[reampm$n.event==0] ))
colnames(censuream)<-c("time","ce")
evenementam<-as.data.frame(cbind(reampm$time,reampm$surv ))
colnames(evenementam)<-c("time","ev")
debutam<-data.frame(time=0,ev=1)
evenementam<-rbind(debuta,evenementam)



intervalleam<-as.data.frame(cbind(reampm$time,reampm$upper
                                 ,reampm$lower ))
colnames(intervalleam)<-c("time","haut","bas")

# pfs<-Surv(event = berger$pfst,time=berger$delai_pfs)
# pfss <- survfit( pfs ~ 1)
# plot(pfss,xlab="Délai depuis la date de diagnostic stade IV (mois)")

#ref<-summary(pfss,censored = TRUE)

# censurpf<-as.data.frame(cbind(ref$time[ref$n.event==0],ref$surv[ref$n.event==0] ))
# colnames(censurpf)<-c("time","ce")
# evenementpf<-as.data.frame(cbind(ref$time,ref$surv ))
# colnames(evenementpf)<-c("time","ev")
# debut<-data.frame(time=0,ev=1)
# evenementpf<-rbind(debut,evenementpf)
# 
# 
# intervallepf<-as.data.frame(cbind(ref$time,ref$upper
#                                   ,ref$lower ))
#colnames(intervallepf)<-c("time","haut","bas")


km_ampm<-ggplot()+ geom_step(data=evenementam,aes(x=time, y=ev),color="black", direction="hv")  +
  geom_ribbon(data=intervalleam, aes(x=time, ymin=bas, ymax=haut),linetype="dashed",fill="grey",alpha="0.4")+
  geom_step(data=intervalleam,aes(x=time, y=haut),color="black" ,direction="hv",linetype="dashed")+
  geom_step(data=intervalleam,aes(x=time, y=bas),color="black", direction="hv",linetype="dashed")+
  scale_x_continuous(breaks=c(0,5,10,15,20,25,30),expand = c(0, 0),limits=c(0,max(reampm$time)))+
  
  
  #geom_point(data=censure, aes(x=time, y=ce),shape=3,size=1 )+
  #ggtitle("DurÃ©e de vie des implants") +
  xlab("Delay from diagnosis (years)")+
  ylab("Probability")+
  #geom_step(data=gri,aes(x=time, y=ics), direction="hv",color="gray10",linetype="dashed" )+
  #  geom_step(data=gri,aes(x=time, y=ici), direction="hv",color="gray10",linetype="dashed" )+
  #  geom_step(data=gci,aes(x=time, y=ics), direction="hv",color="black",linetype="dashed" )+
  #  geom_step(data=gci,aes(x=time, y=ici), direction="hv",color="black",linetype="dashed" )+
  #
  #scale_colour_manual("",values = c("Rupture"="blue", "Autres causes"="black"))+annotate(geom="text", x=52, y=0.91, label="Tous les parcours",color="black", size=4)+coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),expand = c(0, 0),limits = c(0, 1))+
  #coord_cartesian(ylim=c(0,1))+
  theme_classic()+
  theme(legend.position="bottom",
        legend.title=element_blank())+
  ggtitle("")




test= data.frame(
  
  x = c(0.5,1,3,5,10,20),
  y = c(33,25,27,36,23,25),
  n=c("",reamp$n.risk[which.min((reamp$time-1)<0)-1],reamp$n.risk[which.min((reamp$time-3)<0)-1],
      reamp$n.risk[which.min((reamp$time-5)<0)-1],
      reamp$n.risk[which.min((reamp$time-10)<0)-1],
      reamp$n.risk[which.min((reamp$time-20)<0)-1]),
  ypos=c(0.04,0.04,0.04,0.04,0.04,0.04)
  
)



for (ii in 1:nrow(test))
{
  #display numbers at each visit
  km_amp=km_amp+ annotation_custom(grob = textGrob(test$n[ii]),  
                                   xmin = test$x[ii], 
                                   xmax = test$x[ii], 
                                   ymin = test$ypos[ii], 
                                   ymax = test$ypos[ii])
  
}


km_amp=km_amp+annotation_custom(grob = textGrob("N at risk"),  
                                xmin = 2.0, 
                                xmax = 2.0, 
                                ymin = 0.1, 
                                ymax = 0.1)



gta <- ggplot_gtable(ggplot_build(km_amp))
gta$layout$clip[gta$layout$name=="panel"] <- "off"

### Survie sans événements###

eves<-Surv(event = berger$eveoudeces,time=as.numeric(berger$delai_eve_deces))
evesu <- survfit( eves ~ 1)
plot(evesu ,xlab="")

revesu<-summary(evesu,censored = TRUE)

censurev<-as.data.frame(cbind(revesu$time[revesu$n.event==0],revesu$surv[revesu$n.event==0] ))
colnames(censurev)<-c("time","ce")
evenementev<-as.data.frame(cbind(revesu$time,revesu$surv ))
colnames(evenementev)<-c("time","ev")
debutev<-data.frame(time=0,ev=1)
evenementev<-rbind(debutev,evenementev)



intervalleev<-as.data.frame(cbind(revesu$time,revesu$upper
                                  ,revesu$lower ))
colnames(intervalleev)<-c("time","haut","bas")

# pfs<-Surv(event = berger$pfst,time=berger$delai_pfs)
# pfss <- survfit( pfs ~ 1)
# plot(pfss,xlab="Délai depuis la date de diagnostic stade IV (mois)")

#ref<-summary(pfss,censored = TRUE)

# censurpf<-as.data.frame(cbind(ref$time[ref$n.event==0],ref$surv[ref$n.event==0] ))
# colnames(censurpf)<-c("time","ce")
# evenementpf<-as.data.frame(cbind(ref$time,ref$surv ))
# colnames(evenementpf)<-c("time","ev")
# debut<-data.frame(time=0,ev=1)
# evenementpf<-rbind(debut,evenementpf)
# 
# 
# intervallepf<-as.data.frame(cbind(ref$time,ref$upper
#                                   ,ref$lower ))
#colnames(intervallepf)<-c("time","haut","bas")


km_ev<-ggplot()+ geom_step(data=evenementev,aes(x=time, y=ev),color="black", direction="hv")  +
  geom_ribbon(data=intervalleev, aes(x=time, ymin=bas, ymax=haut),linetype="dashed",fill="grey",alpha="0.4")+
  geom_step(data=intervalleev,aes(x=time, y=haut),color="black" ,direction="hv",linetype="dashed")+
  geom_step(data=intervalleev,aes(x=time, y=bas),color="black", direction="hv",linetype="dashed")+
  scale_x_continuous(breaks=c(0,5,10,15,20,25,30),expand = c(0, 0),limits=c(0,max(revesu$time)))+
  
  
  #geom_point(data=censure, aes(x=time, y=ce),shape=3,size=1 )+
  #ggtitle("DurÃ©e de vie des implants") +
  xlab("Delay from diagnosis (years)")+
  ylab("Probability")+
  #geom_step(data=gri,aes(x=time, y=ics), direction="hv",color="gray10",linetype="dashed" )+
  #  geom_step(data=gri,aes(x=time, y=ici), direction="hv",color="gray10",linetype="dashed" )+
  #  geom_step(data=gci,aes(x=time, y=ics), direction="hv",color="black",linetype="dashed" )+
  #  geom_step(data=gci,aes(x=time, y=ici), direction="hv",color="black",linetype="dashed" )+
  #
  #scale_colour_manual("",values = c("Rupture"="blue", "Autres causes"="black"))+annotate(geom="text", x=52, y=0.91, label="Tous les parcours",color="black", size=4)+coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks=c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),expand = c(0, 0),limits = c(0, 1))+
  #coord_cartesian(ylim=c(0,1))+
  theme_classic()+
  theme(legend.position="bottom",
        legend.title=element_blank())+
  ggtitle("")


