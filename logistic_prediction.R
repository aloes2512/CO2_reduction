library(tidyverse)
library(mgcv)
library(readxl)
# passenger cars
emi_path<-"/Users/alfloeffler/Desktop/Klima_Energiewende/Verkehr/Emissionen/CO2_Emissionen_FlottenBestand.xls"
Flt_emi <- read_xls(emi_path)
summary(Flt_emi)
names(Flt_emi)<- c("year","CO2_Flt_emi","ICCT_flt_emi")
# prediction with logistic equation
# initial conditions
M<- 100 # upper limit percent fleet
k1 <- 0.6 # specific rate of growth equal to replacement of all vehicles by zero emission vehicles
k2<- 0.3 # half of k1 
k3<- 0.4545 # (dP/dt)/P specific change rate BEV
P0 <- 0.66 # percentage of zero emission vehicles
C<- (100-0.66)/0.66 # 150 integration constant to match initial condition
Tot_mileage_cars<- 626* 10^9 #Mrd.km
milg_flt <- tibble(year = 2021:2050,
                   p =  M/(1+C*exp(-k3*(year-2021))),# percentage of zero emi vehicles
                   mlg = (M-p)*Tot_mileage_cars/100,# mileage of existing vehicles 
                   CO2 = mlg * Flt_emi$ICCT_flt_emi/10^6)# total CO2- emissions based on ICCT specfic anual values
head(milg_flt)
milg_flt%>% ggplot(aes(x= year))+
  geom_point(aes(y= CO2/10^6))+
  ggtitle("Passenger Cars Emission [Mio.t]",
          subtitle= "Existing fleet reduced 6%/a")+
  labs(x="", y= "CO2 [Mio.t]")
ggsave(filename = "Emissions_Cars_existg_fleet.png", path = "./figs")
# freight road transport
SNF<-read_excel(file.path("~/desktop/Klima_Energiewende/Verkehr/Emissionen","SNF_emissionen.xls"))
colnames(SNF)<-colnames(SNF)%>% str_replace_all(" ","_")
SNF$`Prognos_Szenario_[tkm/10^9]`
head(SNF)
SNF$Traffic_Dimension# tkm
SNF$Value_Dimension # g(direkt)
SNF$CO2<- SNF$`CO2_[Mio.t]`

colnames(SNF)                  
SNF%>% subset(Year<=2021)%>% 
  ggplot(aes(x=Year))+
  geom_point(aes(y= CO2 ))+
  geom_smooth(method = "lm", mapping = aes(x=Year,y= CO2),linetype =1,formula = y~ x, col = "red")+
  geom_smooth(method = "gam", mapping = aes(x=Year,y= CO2),linetype =4,formula = y~ s(x, k=5), col = "red")+
  labs( x=" ",y= "CO2 [Mio.t/a]")+
  ggtitle("Road Freight Emissions Mio.t per Year",
          subtitle = "linear trend (red line)
smoothed average (dashed line)")

# Built  traffic Scenarios for 2050
## Prognos Scenario 2007
Freight_trfc<- SNF%>% dplyr::select(Year,Prognos_Szenario=`Prognos_Szenario_[tkm/10^9]`,Traffic)
Freight_trfc%>% 
  ggplot(aes(x=Year))+geom_point(aes(y= Prognos_Szenario),col = "red")+
  geom_point(aes(y= Traffic/10^9))
# Predict using gam (generalized additive model)
sample_data<- tibble (x= Freight_trfc$Year,
                      y = Freight_trfc$Traffic/10^9)%>% subset(x<= 2019)
ggplot(sample_data,aes(x,y))+
  geom_point(shape =1)
gam_y <- mgcv::gam(y ~ s(x), method = "REML", data = sample_data)
x_new <- seq(1995, 2050, length.out = 56)
y_pred <- predict(gam_y, data.frame(x = x_new))
pred_2050 <- tibble(x= x_new,y_pred)
str(pred_2050)
Freight_2050<-pred_2050%>% 
  left_join(Freight_trfc, by = c("x"= "Year"))%>% 
  mutate(Traffic= Traffic/10^9)
Freight_2050%>% ggplot(aes(x=x))+
  geom_point(aes(y= Traffic))+
  geom_smooth(method= "gam",aes(y= y_pred),col = "green", linetype= 2)+
  geom_point(aes(y= Prognos_Szenario),col = "red")
# assuming  limited growth 
Freight_2050[1,]# 280 starting value in 1995
Freight_2050[6,]# 346 actual 2000
avrg_chg <- (Freight_2050[6,4]-Freight_2050[1,4])/5#13.32826
spec_chg <- avrg_chg/Freight_2050[1,4]#0.04765904
#assumed upper limit
Trfc_mx <- 650 # Mrd. tkm asymptotic maximum
Trfc_0 <- Freight_2050[1,4]# starting in 1995
k<- spec_chg%>% as.numeric() #0.04765904
C<- (Trfc_mx-Trfc_0)/Trfc_0 # 1.324262
C<-1.324262
#Prediction with logistic function
Freight_2050<-Freight_2050%>% mutate(logstc_pred= Trfc_mx/(1+C*exp(-k*(x-1995)))) 
Freight_2050%>% ggplot(aes(x= x))+
  geom_smooth(aes(y=logstc_pred),col ="green")+
  geom_smooth(mapping = aes(x,y=Prognos_Szenario),
              col = "red",data = Freight_2050)+geom_point(aes(y = Traffic))+
  labs(x="",y= "Mrd. tkm")+
  ggtitle(" Trend of Annual Road Freight
  German Transport")
ggsave(filename = "Scenario_Roadfreight_Germany.png", path = "./figs")
# Specific change tkm/year == Traffic

Freight_growth<-Freight_trfc %>% mutate (Traffic = Traffic/10^9)
Freight_growth<- Freight_growth%>% mutate(specfc_chg= (lead(Traffic)-Traffic)/Traffic)
chg_modl<- lm(specfc_chg ~ Year, data = Freight_growth)
Freight_growth%>% ggplot(aes(x=Year))+
  geom_point(aes(y= specfc_chg))+geom_smooth(method = "lm",mapping = aes(x= Year, y=specfc_chg), data = Freight_growth)+
  geom_abline(mapping=aes(x= Year, y=specfc_chg), slope = chg_modl$coefficients[2],intercept = chg_modl$coefficients[2], data= Freight_growth)
Grwth <- tibble(t = Freight_growth$Year,
                chg = Freight_growth$specfc_chg) 
ggplot(Grwth,aes(x=t, y= chg))+
  geom_point()+
  geom_abline(slope = -0.001914582,intercept = 3.864873)
mean(Grwth$chg,na.rm = TRUE)
chg_modl$coefficients[1]/chg_modl$coefficients[2]
3.864873/0.001914582
chg_modl$model%>% ggplot(aes(x= Year,y= specfc_chg))+
  geom_point()+geom_abline(slope = -0.001914582,intercept = 3.864873,col ="red")
# Logistic model 
tonns_km_0 <- Freight_growth[1,]# 280
tonns_km_1 <- Freight_growth[23,]#476
Dt <- 2017-1995 #22
k<-0.045282775
k*22
exp(-k*22)#0.9964484
#tonns_km_1=M/(1+ C*exp(-k*22))
(280-476)/(280-476*0.3678794)
exp(2,e)
log(exp(2))
exp(1)
