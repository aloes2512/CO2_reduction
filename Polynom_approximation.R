
install.packages("polynom")
library(polynom)
x <- c(0, 2, 3, 4)
y <- c(7, 11, 28, 63)
dat<- tibble(x,y)
poly.calc(x,y)  # 7 - 2*x + x^3
#pop.xts from Logistic_model.R
pop_xts%>% summary()
pop_xts%>% ggplot(aes(x=Index, y = pop))+
  geom_point(col = "blue", size = 3)

 Jahre <- index(pop_xts)%>% format("%Y")%>% as.numeric()
pop_data <-coredata(pop_xts)%>% as_tibble()
pop_data$Jahre <- Jahre
pop<- pop_data$pop
poly.calc(pop,Jahre)
poly<- function(x) return(poly.calc(pop,Time))
pop_data%>% mutate(pop_int= 3.7-572.2214*Time+0.4705344*Time^2- 0.0001617589*Time^3+Time^4*2.805068e-08)
(poly(x=-62091))
pop_data 
indx10 %>% format("%Y")
sel_Jahr<- c(1800,1900,1970,1990,2010)
 Jahre %in% sel_Jahr 
select_data<-pop_data[Jahre %in% sel_Jahr,]
poly.calc((select_data$Jahre-1800),select_data$pop)
fJ<- function(x){ 1 + 0.1620329*x - 0.003210336*x^2 + 2.05613e-05*x^3 - 3.961223e-08*x^4   }
fJ(200)
pop_data<- pop_data%>% mutate(pop_int = fJ(Jahre-1800))
pop_data%>% ggplot(aes(x=Jahre,y = pop))+
  geom_point()+
  geom_point(data=select_data, mapping = aes(x=Jahre,y=pop),shape= 2,col="red")+
  geom_smooth(mapping = (aes(x=Jahre,y= pop_int)))
