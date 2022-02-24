# Freight Traffic Growth Limit Germany
library(tidyverse)
library(lubridate)
library(readxl)
library(xts)
library(mgcv)
library(broom)
# freight road transport UBA recorded data & scenario Prognos
SNF<-read_excel(file.path("~/desktop/Klima_Energiewende/Verkehr/Emissionen","SNF_emissionen.xls"))
colnames(SNF)<-colnames(SNF)%>% str_replace_all(" ","_")
SNF_data <- SNF %>% dplyr::select(Year,Traffic,Prognos= "Prognos_Szenario_[tkm/10^9]")%>% 
  mutate(Traffic = round(Traffic/10^9,0) )

yr_dateformat <- function( dt){
  date <- str_glue("'","{dt}","{'-07-01'}","'")%>% ymd(.)
 return(date)
  }
idx_SNF_data<- yr_dateformat(SNF$Year)%>% ymd()
SNF_data.xts<-xts(order.by=idx_SNF_data,SNF_data)# all old data as xts irregular timeseries
dim(SNF_data.xts)#29 3
#=====================
# make regular timeseries 
#==========================
indx<-yr_dateformat(1995:2050)
SNF_xts<- xts(,order.by = indx)# just index timeseries
SNF_progn<-merge(SNF_xts,SNF_data.xts[,2:3])
SNF_progn%>%dim()#56 2 # is regular timeseries 
SNF_progn%>% ggplot(aes(x= index(SNF_progn)))+
                      geom_point(aes(y = Traffic))+
  geom_point(aes(y=Prognos),col="red",shape = 2)
#=========================

# update with KBA data available from 2019
url_KBA_statistik<- "https://www.kba.de/DE/Statistik/Produktkatalog/produkte/Kraftverkehr/ve5_uebersicht.html?nn=3514348"
browseURL(url_KBA_statistik)
#====================
# Datei (Timeseries) named: "ve5_2020.xlsx"
KBA_freight_path<- "~/desktop/Klima_Energiewende/Verkehr/Fahrleistungen/Verkehrsleistung_LKW_KBA.xls"
KBA_dfr <- read_xls(file.path(KBA_freight_path),sheet = 1)[-1,]
names(KBA_dfr)<- c( "Land",2010:2020)
KBA_dfr<-KBA_dfr%>% pivot_longer(col =-"Land",names_to = "Jahr",  values_to = "Miotkm")%>% 
  mutate(Jahr= as.numeric(Jahr), Traffic= round(Miotkm/10^3,0))
KBA_dfr<- KBA_dfr%>% 
  subset(Land== "Insgesamt")%>% 
  dplyr::select(Jahr,Traffic)
KBA_21 <- c(2021,1.04*531)
names(KBA_21)<-names(KBA_dfr)
KBA_dfr<-bind_rows(KBA_dfr,KBA_21)
idx_KBA_dfr<- yr_dateformat(KBA_dfr$Jahr)
# select Prognos for times where KBA data are available
KBA_dfr$Prognos<-SNF_progn[idx_KBA_dfr][,2]
KBA_xts<- xts(KBA_dfr, order.by= idx_KBA_dfr)%>% .[,2:3]
KBA_xts%>% head(12)
# old data up to 2009 + replace  data if KBA data exist
SNF_prognose<-rbind(SNF_progn["/2009-07-01"],KBA_xts)%>% rbind(.,SNF_progn["2022-07-01/"])
#save updated data
saveRDS(SNF_prognose, file = "updated_road_freight.rds")

prog_plt<- SNF_prognose%>% ggplot(aes(x=index(SNF_prognose),y= Traffic))+
  geom_point()+
  geom_point(mapping = aes(x=index(SNF_prognose),y=Prognos),col = "red")+
  labs(x="", y= "Freight [Mrd.tkm]")+
  ggtitle("Freight(road-transport) Germany ",
          subtitle = "Data: reported(black dots)  extrapolated (red dots)")
ggsave(prog_plt,filename = "./figs/German_Road_Freight.png")
# GAM Model with limited growth
# Format xts to tibble
y<- coredata(SNF_prognose)%>% as_tibble()%>%pull(Traffic)
SNF_tibble <- tibble (date= index(SNF_prognose),
                      Jahr = 1995:2050,
                      Traffic = y)
names(SNF_tibble)
saveRDS(SNF_tibble, file = "updated_road_freight_21.rds")
# Interpolating recorded data with GAM "Gaussian"
SNF_GAM_mdl<- gam(Traffic ~ s(Jahr,k=4) , data =SNF_tibble, method = "REML")
SNF_mdl_Data <-SNF_GAM_mdl$model %>% as_tibble()
NROW(SNF_mdl_Data)#27
SNF_mdl_Data$fit <- SNF_GAM_mdl$fitted.values
SNF_mdl_Data$date <-yr_dateformat(1995:2021)
SNF_mdl_Data$Jahr
SNF_mdl_Data<-SNF_mdl_Data%>% dplyr::select(date,Jahr,Mrdtkm=Traffic,fit)
SNF_mdl_Data%>% head() # 1995 up to 2021
saveRDS(SNF_mdl_Data,file = "updated_road_freight_21.rds")
SNF_mdl_xts<- xts(order.by = SNF_mdl_Data$date,dplyr::select(SNF_mdl_Data,Jahr,Mrdtkm,fit))
prog_plt+ geom_line(data= SNF_mdl_Data,aes(x= date,y= fit),col= "blue")
#construct logistic model from fitted data
chng_p_tkm<-SNF_mdl_Data%>% mutate(chg.yr = lead(fit)-fit,spec_chg= chg.yr/fit)
lin_mdl<-lm(chng_p_tkm,formula = spec_chg ~ fit)
chng_p_tkm%>% ggplot(aes(x= fit, y= spec_chg))+
  geom_point()+
  geom_smooth(method = "lm",
              formula =y~x, 
              mapping = aes(x= fit, y= spec_chg),
              data =chng_p_tkm)+
  geom_abline(intercept = lin_mdl$coefficients[1], slope = lin_mdl$coefficients[2] )+
  coord_cartesian(xlim = c(0,700), ylim= c(0,0.1))
Traffic_spec.growth <- tibble ( Mrdtkm = 0:620,
                           grwth_rate= Mrdtkm*(lin_mdl$coefficients[1]+Mrdtkm*lin_mdl$coefficients[2]))
Traffic_spec.growth%>% ggplot(aes(x=Mrdtkm))+
  geom_line(aes(y= grwth_rate), size= 0.5, col = "blue")+
  ggtitle("Road Freight- Traffic Germany
          Heavy Duty Vehicles",
          subtitle = "Growth-Rate (logistic model)")
Traffic_max <- as.numeric(- lin_mdl$coefficients[1]/lin_mdl$coefficients[2])%>% round(0)
k <- lin_mdl$coefficients[2]%>% as.numeric()
Traffic_2000<- coredata(SNF_mdl_xts["2000"])%>% as_tibble %>% .$fit
C<-Traffic_max/Traffic_2000-1
Traffic_growth_Trend<- xts(order.by =  yr_dateformat(1995:2050),tibble(Jahr= 1995:2050))
Traffic_growth_Trend<-merge(Traffic_growth_Trend,SNF_mdl_xts)[,c("Jahr","Mrdtkm","fit")]
Traffic_predict<-coredata(Traffic_growth_Trend)%>% 
  as_tibble%>% 
  mutate(Traffic_pred=Traffic_max/(1+C*exp(k*Traffic_max*(Jahr-2000))))
  
names(Traffic_predict)
Freight_plt<-Traffic_predict%>% as_tibble %>% ggplot(aes(x=  Jahr))+
  geom_line(aes(y= Traffic_pred),col = "blue")+
  geom_point(aes(y= Mrdtkm))+
  ggtitle("Predicted Road Freight 
          Germany",
          subtitle = "Logistic model (data 1995:2020 (reported UBA & KBA)  ,2021 preliminary) ")+
  labs(y = "Mrd.tkm",x = "Year")
ggsave(Freight_plt,filename ="./figs/predicted_road_freight.png")
