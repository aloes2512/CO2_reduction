loc_path<-"~/Desktop/Klima_Energiewende/Verkehr/Emissionen/Bruttoinlandsprodukt.rds"
saveRDS(Traff_dat,file = "~/Desktop/Klima_Energiewende/Verkehr/Emissionen/Bruttoinlandsprodukt.rds" )
Traff_dat<- readRDS(file=loc_path)
Traff_dat %>% head(1)
Traff_mdl<-gam(Traffic~ s(Jahr) + s(BiP_10_Billion) , data = Traff_dat, method = "REML")
summary(Traff_mdl)
plt<- getViz(Traff_mdl)
plot(sm(plt,1))
