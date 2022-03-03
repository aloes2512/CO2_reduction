# Forecast with mgcViz
library(mgcViz)
library(tidyverse)
#==========================
# example code
n  <- 10e2
dat <- data.frame("x1" = rnorm(n), "x2" = rnorm(n), "x3" = rnorm(n))
dat$Y <- with(dat, sin(x1) + 0.5*x2^2 + 0.2*x3 + pmax(x2, 0.2) * rnorm(n))
range(dat$x1)
dat$t<- seq(min(dat$x1),max(dat$x1),length.out =n)
dat<-dat%>% mutate(z= sin(t))
 dat%>% ggplot(aes(x= x1,y=Y))+
  geom_point(col= "red",size = 0.5)+
  geom_point(aes(x=x2,y=Y),col = "green",size=0.5)+
   geom_line(data =dat,mapping =aes(x=t,y=z))
b <- gam(Y ~ s(x1) + s(x2) + x3, data = dat, method = "REML")
b <- getViz(b)
# first smooth function
sm(b,1)
o <- plot( sm(b, 1) )
o<-o+geom_line(data = dat,mapping=(aes(x=t,y=z)),linetype= 1,col ="red")
o + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 3) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()
#========================================
# data from "Freight_forecast.R" 
my_dat <-readRDS("updated_road_freight.rds")
head(my_dat)# Land       Jahr Miotkm
summary(my_dat)
library(readxl)
BIP<-read_excel(path = "~/desktop/Klima_Energiewende/Verkehr/Emissionen/Bruttoinlandsprodukt.xls")
Jahr<- index(my_dat)%>%str_replace("-07-01","")%>% as.numeric()
my_dat$Jahr<-Jahr
my_mdl<-gam(Traffic ~ s(Jahr),data= my_dat, method = "REML")
BIP_mdl <- gam(BiP_Billion ~ s(Jahr),data= BIP,method = "REML")
bip<- getViz(BIP_mdl)
bip_plt<-plot(sm(bip,1))
listLayers(bip_plt)
bip_plt+
  l_fitLine(colour = "red")+
  l_ciLine(mul = 5, colour = "blue", linetype = 2)+
  l_points(shape = 1, size = 3) + 
  theme_classic()
summary(my_mdl)
my_b<- getViz(my_mdl)
my_o<-plot(sm(my_b,1))
listLayers(my_o)
my_o+l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y)) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 3, size = 1, alpha = 0.5) + 
  theme_classic()

plt_0<- KBA_mdl%>%ggplot()+
  geom_point(aes(x= Jahr,y= Miotkm))+
  geom_smooth(aes(x= Jahr,y= .fitted))+
  geom_point(aes(x= Jahr,y= .fitted),col= "blue")+
  labs(x= "Jahr")+
  ggtitle("Freight (road-transport)
          Germany",
          subtitle="KBA-data & Trend")
KBA_mdl%>% scale()%>% as_tibble()%>% mutate(Jahr = 2010:2020)%>%ggplot()+
  geom_point(aes(x= Jahr,y= Miotkm))+
  geom_smooth(aes(x= Jahr,y= .fitted))+
  geom_point(aes(x= Jahr,y= .fitted),col= "blue")+
  labs(x= "Jahr")
plot(sm(my_b,1))+geom_point(data = my_dat, aes(x= Jahr, y= Miotkm/10^3))
# combine Freight BIP
my_dat%>%summary()
BIP%>% summary()
Traff_dat<-coredata(my_dat)%>% as_tibble()%>% mutate(Year=index(my_dat))
Traff_dat <- Traff_dat %>%
  left_join(BIP, by= "Jahr") %>%
  dplyr::select(Jahr,Traffic,BiP_Billion,Year)%>%
  mutate(BiP_10_Billion= BiP_Billion/10)%>% dplyr::select(-BiP_Billion)
Traff_dat%>% ggplot(aes(x= Year))+
  geom_smooth(aes(x =Jahr,y=BiP_10_Billion))+
  geom_smooth(aes(x= Jahr, y = Traffic),col ="black")+
  ggtitle("German Gross Domestic Product [10-Billion-€] & Road Freight [Billion tkm]",
          subtitle = "Road Freight (black); GDP (blue)")
