# SNF_CO2
library(tidyverse)
library(lubridate)
library(readxl)
library(xts)
library(mgcv)
library(broom)
yr_dateformat <- function( dt){
  date <- str_glue("'","{dt}","{'-07-01'}","'")%>% ymd(.)
  return(date)
}
url_spez_emi<-"https://www.umweltbundesamt.de/bild/spezifische-emissionen-lkw-emissionen-lkw"
browseURL(url_spez_emi)
url<-Trend_emi_UBA<-"https://www.umweltbundesamt.de/themen/klima-energie/treibhausgas-emissionen"
dat_path<- "~/desktop/Klima_Energiewende/Verkehr/Emissionen"
SNF_emi_proz<-read_xls(file.path(dat_path,"SNF_emissionen.xls"),sheet =2)
names(SNF_emi_proz)
SNF_emi_proz<-rename(SNF_emi_proz,"Year"= "...1")%>% mutate(CO2_miot=CO2_Value_g/10^12)%>% 
  dplyr::select(Year,Proz_CO2,Traffic_Proz,Specf_CO2,CO2_miot)
NROW(SNF_emi_proz)#23
tail(SNF_emi_proz)# 1995 100 % bis 2017 70.5%
summary(SNF_emi_proz)
SNF_emi_proz<-SNF_emi_proz%>% dplyr::select(Year,Freight_tns=Traffic_Proz,CO2_tns=Proz_CO2,"CO2/tnskm"=Specf_CO2)
SNF_emi_proz%>% pivot_longer(-Year,names_to= "perc",values_to = "Value")%>%
  ggplot(aes(x=Year,y =Value,col = perc))+
  geom_point()+
  geom_smooth(aes(x=Year,y=Value),size = 0.5,se =T)+
  ggtitle("CO2-Emissions Road Freight %(1995=100)
          Germany",
          subtitle = "relative change 1995 = 100%
Freight-data updated KBA 2010: 2017  ")
#==========================
# logistic model for CO2 growth
names(SNF_emi_proz)#"Year"        "Freight"     "Traffic_CO2" "Specf_CO2"  
SNF_CO2_mdl<-SNF_emi_proz %>% as.data.frame()%>% mgcv::gam(data= .,formula = Traffic_CO2 ~s(Year, k=4),method = "REML")
SNF_CO2_mdl_data<- SNF_CO2_mdl%>%augment()%>% 
  dplyr::select(Year,CO2_fit=.fitted,CO2= Traffic_CO2)%>% 
  mutate(Freight=SNF_emi_proz%>% pull(Freight))
# visualizing trends
CO2_emission_trend<- function(dfr){
  plt <- dfr %>% ggplot(aes(x= Year, y= CO2_fit))+
    
    geom_point(col = "red",size= 0.9)+
    geom_line(aes(y=CO2_fit))+
    geom_point(aes(y= CO2),size = 0.5)+
    ggtitle("Road-Freight CO2 Emission Trend
          Germany",
            subtitle = "Relative change 1995 = 100%
Interpolation with gaussian functions (red dots)")+
    labs (x ="", y = "CO2 [%]")
  return(plt)
}
CO2_emission_trend (SNF_CO2_mdl_data) 
# Change rate of fitted values shows two distinct phases
SNF_CO2_fitted<-SNF_CO2_mdl_data%>% 
  mutate(CO2_chg = lead(CO2_fit)-CO2_fit,specfc_CO2_chg=CO2_chg/CO2_fit)
SNF_CO2_fitted%>%  ggplot(aes(x= CO2_fit, y=specfc_CO2_chg))+
  geom_point()
min(SNF_CO2_fitted%>% na.omit())# 0.00210113
SNF_CO2_fitted%>%subset(specfc_CO2_chg < 0.00211)# Year == 2006
# subset of data < "2007" and data > "2006"
dfrm1<- SNF_CO2_mdl_data%>% subset(Year < 2007)
CO2_emission_trend (dfrm1)
dfrm2<-  SNF_CO2_mdl_data%>% subset(Year >= 2006)
CO2_emission_trend (dfrm2)
#======
log_prms<-SNF_CO2_fitted%>% subset(Year < 2007) %>% lm(formula=specfc_CO2_chg~CO2_fit)%>% .$coefficients
k_co2.1 <- log_prms[[2]]#-0.001331706
CO2_max <- -log_prms[[1]]/log_prms[[2]]#114.333
CO2.0<-SNF_CO2_fitted%>% filter(Year== 2000)%>% .[,2]%>% as.numeric()# 107.7935
C_co2.1 <- CO2_max/CO2.0-1#0.06066724
SNF_CO2_fitted.xts<- xts(order.by = yr_dateformat(SNF_CO2_fitted$Year),as_tibble(SNF_CO2_fitted$CO2))
CO2_growth_Trend<- xts(order.by =  yr_dateformat(1995:2050),tibble(Jahr= 1995:2050))
CO2_Trend<-merge(CO2_growth_Trend,SNF_CO2_fitted.xts)#
CO2_Trend<-CO2_Trend%>%  coredata()%>% 
  as_tibble()%>%mutate(CO2_pred=CO2_max/(1+C_co2.1*exp(k_co2.1*CO2_max*(Jahr-2000))))

CO2_Trend%>% ggplot(aes(x=Jahr))+
  geom_line(aes(y= CO2_pred),col ="red")+
  geom_point(aes(y= value))+
  ggtitle("Predicted CO2 Mio. tonns",
          subtitle= "logistic model basis 1995 to 2006 data")+
  labs(x = "", y= " CO2 [Mio.t]")+
  coord_cartesian(xlim= c(1990,2030))
#===================
SNF_CO2_fitted%>% 
  subset(Year >= 2007) %>%ggplot(aes(x=Year,y=CO2_fit))+
  geom_point()
log_prms.2<-SNF_CO2_fitted%>% 
  subset(Year >= 2007) %>% 
  na.omit()%>%lm(formula=specfc_CO2_chg~CO2_fit)%>% .$coefficients
k_co2.2 <- log_prms.2[[2]]#0.001227872
CO2_max.2 <- -log_prms.2[[1]]/log_prms.2[[2]]#109.3676
CO2.2<-SNF_CO2_fitted%>% filter(Year== 2000)%>% .[,2]%>% as.numeric()# 107.7935
C_co2.2 <- CO2_max.2/CO2.2-1#0.06066724#0.01460271
CO2_Trend2<-CO2_Trend%>%  coredata()%>% 
  as_tibble()%>%mutate(CO2_pred2=CO2_max.2/(1+C_co2.2*exp(k_co2.2*CO2_max.2*(Jahr-2000))))
CO2_Trend2%>% ggplot(aes(x=Jahr))+
  geom_point(aes(y= CO2_pred2))

 # old code
#=========
SNF_CO2_UBA<- read_xls(file.path("~/Documents/Luftqualitaet/Daten/BRD","Spez_CO2_Emi_SNF.xls"))
SNF_CO2_specific <-SNF_CO2_UBA%>% dplyr::select(Year,CO2_g= Value,Spec_CO2="Specific Value")
SNF_CO2_specific<-SNF_CO2_specific%>% mutate(CO2_proz= CO2_g/first(CO2_g)*100 )
SNF_CO2_specific%>% ggplot(aes(x= Year,y=Spec_CO2))+
  geom_point()+
  geom_smooth()+
  labs(x="",y= SNF_CO2_UBA$`Specific Value Dimension`[[1]])+
  ggtitle("Specific CO2-Emissions [g/tkm]
Heavy Duty Road-Freight  Germany",
          subtitle = "source UBA")
SNF_CO2_specific%>% summary()

# smooth data
names(SNF_CO2_specific)#"Year"     "CO2_g"    "Spec_CO2"
SNF_spcf.CO2_mdl<-SNF_CO2_specific %>% as.data.frame()%>% mgcv::gam(data= .,formula = Spec_CO2 ~s(Year, k=4),method = "REML")
SNF_spcf.CO2_mdl%>% augment()%>% 
  dplyr::select(Year,chg_fit=.fitted,Spec_CO2)%>% 
  ggplot(aes(x=Year, y= chg_fit))+
  geom_point(aes(x= Year,y= Spec_CO2))+
  geom_line(aes(x=Year,y= chg_fit), col = "blue")
# Model for total CO2 SNF
SNF_CO2_UBA%>% names()
SNF_CO2_UBA$`Value Dimension`# "g(direct)"
SNF_CO2_UBA<-SNF_CO2_UBA%>%dplyr::select(Year,tkm=Traffic,CO2_g=Value)%>%
  mutate(CO2_specf_chg=(lead(CO2_g)-CO2_g)/CO2_g)
SNF_CO2_UBA%>% ggplot(aes(x=Year))+
  geom_point(aes(y= CO2_g))
SNF_CO2_UBA%>%  ggplot(aes(x= CO2_g,y= CO2_specf_chg))+
  geom_point()+
  geom_smooth(method = "lm")
SNF_CO2_mdl<-SNF_CO2_specific %>% as.data.frame()%>% mgcv::gam(data= .,formula = Spec_CO2 ~s(Year, k=4),method = "REML")

# calculate specific change of change
SNF_CO2_mdl%>% augment()%>% 
  dplyr::select(Year,chg_fit=.fitted,Spec_CO2)%>%
  mutate(specific_chg =  (lead(chg_fit)-chg_fit)/chg_fit)%>%
  ggplot(aes(x=chg_fit,y=specific_chg))+
  geom_point()+
  labs(x="Specific CO2-Emission [g/tkm]", y= "Change rate of specific emission")
# freight road transport recorded data & scenario Prognos
SNF<-read_excel(file.path("~/desktop/Klima_Energiewende/Verkehr/Emissionen","SNF_emissionen.xlsx"))
colnames(SNF)<-colnames(SNF)%>% str_replace_all(" ","_")
SNF$Value_Dimension# CO2 [g]
SNF$Value%>% head(3)# 3.420468e+13 3.448555e+13 3.549085e+13
SNF$Traffic_Dimension# Tons.km
SNF_CO2 <- SNF %>% dplyr::select(Year,Traffic,CO2= Value)%>% 
  mutate(Traffic = round(Traffic/10^9,0), CO2_kg= CO2/10^3,kg.t_km=CO2_kg/(Traffic*10^9) )
SNF_CO2%>% subset(Year< 2020)%>% 
  ggplot(aes(x=Year, y=kg.t_km))+
  geom_point()+
  geom_smooth()+
  ggtitle("CO2 Emissions [kg/tkm]
  Heavyduty Freight Traffic")+
  labs(y= "[kg/tkm]")
# change of specific CO2-emissions
SNF_CO2_trend<-SNF_CO2%>%subset(Year< 2020) %>% 
  dplyr::select(Year,kg.t_km)%>% mutate(chg_emi = lead(kg.t_km)-kg.t_km,spec_chg=chg_emi/kg.t_km)
 spec_chg_plt<-SNF_CO2_trend%>% ggplot(aes(x=kg.t_km,y= spec_chg))+
  geom_point()+
   coord_cartesian(xlim = c(0.01,0.15))
CO2_trend_mdl<-SNF_CO2_trend%>%subset(kg.t_km< 0.10)%>% lm(.,formula=spec_chg ~ kg.t_km)
spec_chg_plt+geom_abline(intercept = CO2_trend_mdl$coefficients[1],slope= CO2_trend_mdl$coefficients[2])
