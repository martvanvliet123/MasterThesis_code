library(ggplot2)
library(ggpubr)
#Elke week hebben we 10 patiënten, allemaal voor de ORT.
#Ze moeten allemaal van App's gelijk naar Surg en van Surg gelijk naar Hosp.

# HET MODEL -----------------------------------------------------------------

#Het model duurt
model_duur <- 12 #weken
#Nu in de wachtrij:
Start_app_wachtrij <- 50
Start_surg_wachtrij <- 10
Start_hosp <- 12
#Hoe lang liggen mensen gemiddeld op de hosp?
Ligtijd <- 2 #weken
#Inladen conversierates:
source("C:/Users/m.vanvliet/OneDrive - Performation Healthcare Intelligence BV/Documenten/Testen/conversierates.R")

#Het aantal patiënten dat we elke week kunnen helpen:
Nieuwe_pat_aankomst <- data.frame(week=seq(1, model_duur, by=1), nieuwe=rep(c(6), times=model_duur))
Max_app_p_week <- data.frame(week=seq(1, model_duur, by=1), nr_app=c(8,8,8,8,8,8,8,8,4,4,8,8))
Max_surg_p_week <- data.frame(week=seq(1, model_duur, by=1), nr_surg=c(6,6,8,8,8,4,4,8,8,6,6,6))

#APPS 
#week 0 komen er 10 patiënten bij voor de ORT wachtrij voor apps, en er zijn nog geen apps geweest
App_wachtrij_p_week <- data.frame(week = c(0), nr_pat = Start_app_wachtrij, wachttijd = c(NA))
App_week <- data.frame(week = c(0), nr_app = c(0))

#SURG

#Laten we voor simpelheid afspreken dat we 10 surgs p week kunnen doen.
#week 0 zijn er nog geen surgs, maar 10 wachtenden op een surg
surg_wachtrij_p_week <- data.frame(week = c(0), nr_pat = Start_surg_wachtrij, wachttijd = c(NA))
#Het aantal surgs op week 0 zijn er nog 0
surg_week <- data.frame(week = c(0), nr_surg = c(0))

#HOSP
#Laten we afspreken dat er 1 ontslagmoment is, en dat moment is voordat er nieuwe surg gedaan worden.
#Dus snachts is de piek van het aantal mensen.
#Laten we voor simpelheid afspreken dat elke patiënt er 1 week moet liggen.
#Dus ze komen op week 1 aan en gaan week 2 weer weg.
#Hier komt in te staan hoeveel er die week ontslagen gaan worden, dit wordt vooruit berekend.
nr_hosp=rep(c(0), times=model_duur)
#nr_hosp[c(5,8,12)]=4
ontslag_p_week <- data.frame(week=seq(1, model_duur, by=1), nr_hosp=nr_hosp)
#Hoeveel er dan nog liggen zit in
hosp_liggen_p_week <- data.frame(week = c(0), nr_pat_ochtend = c(Start_hosp), nr_pat_avond = c(Start_hosp))


#In een for_loop rekenen we voor elke week uit:
#1.Hoeveel patiënten er geholpen worden per week
#2.De werkvoorraad voor app voor de volgende week
#3.Hoeveel patiënten er geopereerd worden deze week
#4.De werkvoorraad voor surg voor de volgende week (volgt uit apps)
#5.Het aantal bedden in gebruik aan het einde van de week
#6.Het aantal patiënten dat die week ontslagen is
for (i in 1:model_duur){
  # HOSP ---
  
  #Eerst worden de mensen om 11.00 ontslagen, we nemen nu even aan dat dat vòòr alle surg's en app's is.
  #Er zijn oneindig veel bedden beschikbaar, elke patient wordt na ':Ligtijd' weken verwijderd.
  #Ze komen niet alleen van de OK, maar ook van de poli gelijk (zoals bv KIND)
  #Hoeveel mensen ontslaan moeten er deze week ontslagen worden?
  ontslag_deze_week <- ontslag_p_week[ontslag_p_week$week==i, "nr_hosp"]
  #Er liggen dan nog
  hosp_liggen_p_week <- rbind(hosp_liggen_p_week, c(i, hosp_liggen_p_week[hosp_liggen_p_week$week==(i-1), "nr_pat_avond"] - ontslag_deze_week, NA))
  
  #APPS ---
  
  #App_deze_week is het aantal appointments dat die week maximaal doorgelopen kan worden, dat zijn er voor nu even 10
  Max_app_deze_week <- Max_app_p_week[Max_app_p_week$week==i, "nr_app"]
  #Hoeveel appointments hebben we dan kunnen draaien deze week?
  App_deze_week <- min(Max_app_deze_week, App_wachtrij_p_week[nrow(App_wachtrij_p_week), "nr_pat"])
  App_week <- rbind(App_week, list(i, App_deze_week))
  #Hoeveel patiënten zijn er niet geholpen?
  Niet_geholpen <- max(App_wachtrij_p_week[nrow(App_wachtrij_p_week),"nr_pat"] - App_deze_week)
  #Hoeveel patiënten zijn er nog over?
  #Dat zijn de niet geholpen patiënten van gisteren + alle nieuwe patiënten (elke week 10) die erbij komen
  Nieuwe_pat <- Niet_geholpen + Nieuwe_pat_aankomst[Nieuwe_pat_aankomst$week==i, "nieuwe"]
  #De nieuwe wachttijd is dan dit aantal patiënten modulo hoeveel patiënten we de komende weken kunnen helpen.
  wachttijd=0 #weken
  leftover=Nieuwe_pat
  for (j in i:model_duur){
    leftover <- leftover - Max_app_p_week[j+1,"nr_app"]
    if (leftover<=0 | is.na(leftover)){
      break
    }
    wachttijd <- wachttijd+1
  }
  App_wachtrij_p_week <- rbind(App_wachtrij_p_week, list(i, Nieuwe_pat, wachttijd))
  
  #SURG ---
  
  #Max_surg_deze_week is het maximaal aantal operaties dat die week uitgevoerd kan worden.
  Max_surg_deze_week <- Max_surg_p_week[Max_surg_p_week$week==i, "nr_surg"]
  #Hoeveel surg zijn er dan uitgevoerd deze week?
  surg_deze_week <- min(c(Max_surg_deze_week, surg_wachtrij_p_week$nr_pat[surg_wachtrij_p_week$week==(i-1)]))
  surg_week <- rbind(surg_week, list(i, surg_deze_week))
  #Hoeveel patiënten zijn er niet geholpen?
  Niet_geholpen <-  max(c(surg_wachtrij_p_week[nrow(surg_wachtrij_p_week),"nr_pat"] - surg_deze_week, 0))
  #Wat is de werkvoorraad?
  #Dat zijn de niet geopereerde patiënten van gisteren + alle nieuwe patiënten die van apps komen (met conversierate)
  Nieuwe_pat <- Niet_geholpen + ceiling((1-dropout-conversierate_poli)*App_week[nrow(App_week), "nr_app"])
  wachttijd=0 #weken
  leftover=Nieuwe_pat
  for (j in i:model_duur){
    leftover <- leftover - Max_surg_p_week[j+1,"nr_surg"]
    if (leftover<=0 | is.na(leftover)){
      break
    }
    wachttijd <- wachttijd+1
  }
  surg_wachtrij_p_week <- rbind(surg_wachtrij_p_week, list(i, Nieuwe_pat, wachttijd))
  
  # HOSP ---
  
  #Sla op wanneer deze mensen ontslagen worden van hosp:
  ontslag_p_week[ontslag_p_week$week==(i+Ligtijd), "nr_hosp"] <- ontslag_p_week[ontslag_p_week$week==(i+Ligtijd), "nr_hosp"] + surg_deze_week
  
  #Het is nu het einde van de week, hoeveel bedden zijn er nu in gebruik?
  #Het nieuwe aantal bedden in gebruik na een week werken is alles wat nog niet ontslagen was, plus iedereen die van de poli kwam, plus iedereen die van de OK kwam
  Nieuwe_hosp <- hosp_liggen_p_week[nrow(hosp_liggen_p_week), "nr_pat_ochtend"] + ceiling(conversierate_poli*App_deze_week) + surg_deze_week
  hosp_liggen_p_week[nrow(hosp_liggen_p_week),"nr_pat_avond"] <- Nieuwe_hosp
} 

#Na de for_loop plotten we de werkvoorraden en de geholpen patiënten p afdeling.
par(mfrow=c(1,2))
p1 <- ggplot(App_week, aes(x=week, y=nr_app),pch=1) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust=0.4)) + ggtitle("Plotted #appointments per work day")
p2 <- ggplot(App_wachtrij_p_week, aes(x=week, y=nr_pat)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust=0.4)) + ggtitle("Plotted queue appointments")
p3 <- ggplot(surg_week, aes(x=week, y=nr_surg)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust=0.4)) + ggtitle("Plotted #surgeries per work day")
p4 <- ggplot(surg_wachtrij_p_week, aes(x=week, y=nr_pat)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust=0.4)) + ggtitle("Plotted queue surgeries")
p5 <- ggplot(ontslag_p_week, aes(x=week, y=nr_hosp)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust=0.4)) + ggtitle("Plotted #ontslagen per work day")
p6 <- ggplot(hosp_liggen_p_week, aes(x=week, y=nr_pat_ochtend)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust=0.4)) + ggtitle("Plotted #hospitalized per work day")
ggarrange(p1, p2, p3, p4, p5, p6, ncol = 3, nrow = 2)

