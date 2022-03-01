# Prognosen Güterverkehr
SNF<-read_excel(file.path("~/desktop/Klima_Energiewende/Verkehr/Emissionen","SNF_emissionen.xls"))
colnames(SNF)<-colnames(SNF)%>% str_replace_all(" ","_")
colnames(SNF)
SNF_prognosen<-SNF%>% dplyr::select(Year,Traffic,Prognos="Prognos_Szenario_[tkm/10^9]")%>% mutate(Traffic= Traffic*10^-9)
SNF_prognosen%>% tail(8)
SNF_data<- SNF_data%>% dplyr::select(Year,Traffic= Mrdtkm)
SNF_data%>% tail()# Daten bis 2020 aktuell
x<-SNF_data%>% 
 full_join(SNF_prognosen, by= "Year")
tail(x)
x<-x %>% mutate(Tr = ifelse (is.na(Traffic.x),Prognos,Traffic.x))%>% dplyr::select(Year,Traffic=Tr,Prognos)
x<-x%>% mutate(stg= Prognos-lag(Prognos))
summary(x)
tail(x,4)

data_add<-KBA_mdl%>% dplyr::select(Year= Jahr,Traffic=Miotkm)%>% mutate(Traffic= Traffic/10^3)
SNF_prognosen%>% filter(Year<=2020) %>% dplyr::select(-Prognos)
tail(SNF_prognosen,10)
dim(SNF_prognosen)#29 3
SNF_prognosen[c(24:26),1:2]<- data_add%>% tail(3)
SNF_prognosen%>% tail(7)

SNF_update<- tail(SNF_prognosen,4)%>% 
  mutate(dffr= -lag(Prognos)+Prognos)%>% 
  tail(3)
SNF_update<-SNF_update%>% mutate(Traffic = Traffic+dffr)%>% dplyr::select(-dfr)
names(SNF_update)<- names(SNF_prognosen)
dffr<-SNF_prognosen%>% head(26)
dffr%>% tail()
dffr%>% bind_rows(SNF_update)
SNF_new<- tibble(Year= pull(SNF_update,Year),
                 Traffic= as.vector(pull(SNF_update,Traffic)),
                 Prognos= pull(SNF_update,Prognos))%>% as_tibble()
SNF_prognosen%>% head(26)%>% as_tibble()%>% bind_rows(SNF_new)
