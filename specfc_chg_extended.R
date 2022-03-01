Freight_growth$specfc_chg
frght_mdl<- lm(specfc_chg ~ Traffic, data = Freight_growth)
frght_mdl$model
Freight_growth$fitted<-frght_mdl$fitted.values
Freight_growth%>% 
  ggplot(aes(y=specfc_chg,x = Traffic))+
  geom_point()+
  geom_abline(intercept=frght_mdl$coefficients[1],slope = frght_mdl$coefficients[2])
# (dP/dt)/P = 0.1326718-0.0002732*Traffic
dgl <- frght_mdl$model
dgl$fit <- frght_mdl$fitted.values 
summary(dgl)
dgl<-dgl%>% mutate(diff=(frght_mdl$coefficients[1]+frght_mdl$coefficients[2]*Traffic )*Traffic)
dgl%>%  ggplot(aes(x=Traffic,y=diff))+
  geom_point()
dgl %>% ggplot(aes(x= Traffic,y = fit))+
  geom_line()
# extended range of traffic
dx<- 463.9-279.7 # Traffic change
dy<- -0.056260 +0.005915 # fit range
slp <- dy/dx#  -0.000273317
y1 <- 0.056260-slp*(279.7-1)# 0.1324
dgl_ext<- tibble(x= 1:650,
                 y= y1+slp*x)
dgl_ext%>% ggplot(aes(x,y))+geom_line()+
  geom_point(data = dgl,aes(x=Traffic,y= fit),shape=1)
