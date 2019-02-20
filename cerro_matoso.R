

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(ggplot2)

##----------------------------loading dataset----------------------------
cerro_matoso<-read_excel(path = 'C:/Users/ASUS/DownloadS/cerro_matoso.xlsx', sheet='DISPONIBILIDAD')

##primera limpieza

##-------------------------archivo principal------------------------------------------------------------
cerro_matoso_1 <- cerro_matoso %>% filter(Flota=="Camiones 777")%>%select(fecha,Turno,Operativo, Equipo,`Mant Preventiva`,`Mant Planeada`,`Mant Correctiva`,`Over Haul`, `Demora Prog `, `Demora Noprog `, DemoraStandBy)
#---------------------------- First summary by date and shift no Equipment.--------------------------------------
cerro_matoso_group<-cerro_matoso_1%>%group_by(fecha, Turno)%>%summarize(Operativo=sum(Operativo),

                                                                        preventiva=sum(`Mant Preventiva`),
                                                                        planeada=sum(`Mant Planeada`),
                                                                        correctiva=sum(`Mant Correctiva`),
                                                                        overall=sum(`Over Haul`),
                                                                        programada=sum(`Demora Prog `),
                                                                        noprogramada=sum(`Demora Noprog `),
                                                                        stanby=sum(DemoraStandBy))%>%mutate(Tcalendario=(Operativo+preventiva+planeada+correctiva+overall+programada+noprogramada),disponibilidad=(Tcalendario-preventiva-correctiva-planeada-overall)/Tcalendario,utilizacion=Operativo/(Tcalendario-preventiva-correctiva-planeada-overall))
##-------------------------second summary by only date, either equipment or shift.---------------------------
##-------------------------Aditionally, a gather by date and categorical var and value aside.--------------- 
cerro_matoso_gather<-cerro_matoso_1%>%
  group_by(fecha)%>%
  summarize(
    Operativo=sum(Operativo),
    preventiva=sum(`Mant Preventiva`),
    planeada=sum(`Mant Planeada`),
    correctiva=sum(`Mant Correctiva`),
    overall=sum(`Over Haul`),
    programada=sum(`Demora Prog `),
    noprogramada=sum(`Demora Noprog `),
    stanby=sum(DemoraStandBy))%>%
  mutate(
    Tcalendario=(Operativo+preventiva+planeada+correctiva+overall+programada+noprogramada),
    disponibilidad=(Tcalendario-preventiva-correctiva-planeada-overall)/Tcalendario,
    utilizacion=Operativo/(Tcalendario-preventiva-correctiva-planeada-overall))%>%
  select(fecha,disponibilidad, utilizacion) %>%gather(key = categoria,value=value,-fecha)
  
##---------------------------thirth summary by equipment and date-------------------------
cerro_matoso_equipo <- cerro_matoso_1%>%group_by(fecha,Equipo)%>%
  summarize(
    Operativo=sum(Operativo),
    preventiva=sum(`Mant Preventiva`),
    planeada=sum(`Mant Planeada`),
    correctiva=sum(`Mant Correctiva`),
    overall=sum(`Over Haul`),
    programada=sum(`Demora Prog `),
    noprogramada=sum(`Demora Noprog `),
    stanby=sum(DemoraStandBy))%>%
  mutate(
    Tcalendario=(Operativo+preventiva+planeada+correctiva+overall+programada+noprogramada),
    disponibilidad=(Tcalendario-preventiva-correctiva-planeada-overall)/Tcalendario,
    utilizacion=Operativo/(Tcalendario-preventiva-correctiva-planeada-overall))                                                                                                         
                                                                                                            
##cross variable

##----------------------------lays out utilization vs disponibilidad coloured by shift.
##----------------------------furthermore,method  simple linear regresion line for more interpretability.
ggplot(cerro_matoso_group, aes(x=disponibilidad, y=utilizacion, col=Turno))+
  geom_point()+geom_smooth(se = F,method = lm)
##----------------------------lays out utilization vs standby coloured by shift.
##----------------------------notice the  standby's effect over utilization regard to shift. 
ggplot(cerro_matoso_group, aes(x=stanby, y=utilizacion,col=Turno))+
  geom_point( )+geom_smooth(se = F,method = lm)

##----------------------------lays out a time serie showing the disponibilidad and utilization along with date.------- 

ggplot(cerro_matoso_gather,aes(x=fecha, y=value, col=categoria))+geom_line()
##---------------------------lays out  a boxplot as a proof of different operativo's time.
ggplot(cerro_matoso_equipo,aes(x=Equipo,y=Operativo, col=Equipo))+geom_boxplot()
##-------------------------corrplot-----------------------------------------------
cor_cerro_matoso <- cor(cerro_matoso_group[,3:13])
cor_cerro_matoso_equipo<-cerro_matoso_equipo[,3:13] %>% filter(!is.na(utilizacion))%>%cor()


corrplot::corrplot(cor_cerro_matoso,type = "upper",order = 'AOE',addCoef.col = T,tl.col = 'black')
corrplot::corrplot(cor_cerro_matoso_equipo)


corrplot::corrplot.mixed(cor_cerro_matoso,tl.pos = "lt")



                                                                        
  
                                                                        
                      






