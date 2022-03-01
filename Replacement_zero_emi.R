
library(tidyverse)
library(mgcv)
M<- 100
k1 <- 0.6
k2<- 0.3
k3<- 0.4545 # (dP/dt)/P specific change rate BEV
P0 <- 0.66
C<- (100-0.66)/0.66 # 150  M-P0/P0
P <- tibble(t = c(2020:2050),
            p1 =  M/(1+C*exp(-k1*(t-2020))),
            p2 =  M/(1+C*exp(-k2*(t-2020))),
            p3 =  M/(1+C*exp(-k3*(t-2020)))
                    )
ggplot(P)+
  geom_point(aes(x= t, y= p1),col = "green")+
  geom_point(aes(x= t, y= p2),col= "blue")+
  geom_point(aes(x= t, y= p3),col= "red")+
  labs(x="",y="%BEV")+
  ggtitle(" Anteil BEV an Gesamtbestand PkW ",
          subtitle = "Prognose rot: Förderung wie 2020/
          grün : 100% Neufahrzeuge BEV")
  
library(readxl)
uba_url<-"https://www.umweltbundesamt.de/sites/default/files/medien/361/dokumente/2021_03_10_trendtabellen_thg_nach_sektoren_v1.0.xlsx"
emi_path<-"/Users/alfloeffler/Desktop/Klima_Energiewende/Verkehr/Emissionen/CO2_Emissionen_FlottenBestand.xls"

Flt_emi <- read_xls(emi_path)
names(Flt_emi)<- c("year","CO2_Flt_emi","ICCT_flt_emi")
milg_flt <- tibble(year = 2021:2050,
                   p =  M/(1+C*exp(-k3*(year-2021))),
                   mlg = (M-p)*630,
                   CO2 = mlg * Flt_emi$ICCT_flt_emi)
ggplot(milg_flt,aes(x=year,y=mlg))+
  geom_point(aes(x=year,y= CO2/1000))+
  labs(y= " tons CO2/ year", x="")+
  ggtitle("CO2 emissions of passenger -cars",
          subtitle = "all replaced cars with zero emissions")
# road freight transport
 SNF<-read_excel(file.path("~/desktop/Klima_Energiewende/Verkehr/Emissionen","SNF_emissionen.xls"))
colnames(SNF)<-colnames(SNF)%>% str_replace_all(" ","_")
SNF$`Prognos_Szenario_[tkm/10^9]`
head(SNF)
SNF$Value_Dimension%>% head(1)#CO2-emission"g(direct)"
SNF$Traffic_Dimension#"Tons-km"
SNF%>% ggplot(aes(x = Year))+
  geom_point(aes(y= Traffic/10^9))+
  geom_point(aes(y = `Prognos_Szenario_[tkm/10^9]`),col = "red")+
  geom_smooth(method = "gam",mapping= aes(x= Year, y= Traffic/10^9),formula = y~ s(x, k=3))+
  ggtitle(" Freight trafic Germany in tkm
  1995-2017 recorded (black)
  2020 to 2050 estimates(red)")+
   labs( x="", y = "Mrd. tkm")
ggsave(filename = "freight_traffic_trend.png",path = "./figs")
SNF%>% ggplot(aes(x = Year))+
  geom_point(aes(y = Value/10^12))+
  geom_smooth(method = "gam",mapping= aes(x= Year, y= Value/10^12),formula = y~ s(x, k=3))+
  ggtitle("CO2 Emission
  Freight (road transport)/ year")+
  labs(x="", y = "CO2 [Mio. t]")
ggsave(filename= "CO2_Road_Transport.png",path = "./figs")
SNF%>% ggplot(aes(x= Year))+
  geom_point(aes(y = Specific_Value))+
  geom_smooth(method = "gam",mapping= aes(x= Year, y=Specific_Value),formula = y~ s(x, k=3))+
  ggtitle("Freight road-traffic:
Specific CO2-emission  [g/tkm]")+
  labs(x= "",y = "g/tkm)")
ggsave(filename = "Specific_CO2_emissions.png", path="./figs")
  

SNF<-SNF%>% dplyr::select(Year,Traffic,Specific=`Specific Value`)%>% 
  mutate(TotalCO2= Traffic*Specific/10^12)
SNF%>% ggplot(aes(x= Year))+
  geom_point(aes(y= TotalCO2))+
  geom_smooth(method = "gam",mapping= aes(x= Year, y= TotalCO2),formula = y~ s(x, k=3))+
  ggtitle ("German Freight 
Road Traffic CO2-emissions")+
  labs( x="", y="CO2 [Mio. t] ")
ggsave(filename = "Freight_traffic_CO2_emissions.png", path="./figs")
