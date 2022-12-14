# andmeteadus
library(dplyr)
library(ggplot2)
library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))

andmed1 <-  read.csv2("avalik_2.csv", sep = "\t")
andmed2 <-  read.csv2("avalik_3.csv", sep = "\t")
asukoht <- read.csv2("asukoht.csv", sep = ",", skip = 1)
asukoht1 <- asukoht %>% mutate(Elukoht = str_replace(Elukoht, "\\.\\.", ""))
asukoht2 <- asukoht1 %>% mutate(Elukoht = str_replace(Elukoht, "\\.\\.", ""))
asukoht3 <- asukoht2 %>% mutate(Elukoht = tolower(Elukoht))
asukoht4 <- asukoht3 %>% filter(grepl("maakond", Elukoht)) %>% select(Elukoht, Vanus.kokku)
asukoht5 <- asukoht3 %>% select(Elukoht, Vanus.kokku)
andmed <- read.csv2("clean_data.csv", sep = ",")
#andmed <-  rbind(andmed1, andmed2)
andmed1.1 <- andmed  %>% group_by_all() %>% summarise(n = n()) # kontrollin kas on ikka erinevad 
andmed1.2 <- andmed %>%  group_by(KohtNimetus) %>% summarise(n = n())
andmed1.3 <- andmed1.1 %>%  group_by(KohtLiik) %>% summarise(n = n())
andmed1.4 <- andmed1.1 %>%  group_by(SyndmusLiik, KohtLiik) %>% summarise(n = n())
andmed1.5 <-  andmed1.1 %>% mutate(Elukoht = tolower(MaakondNimetus))
andmed1.6 <- merge(andmed1.5, asukoht4, by= "Elukoht", all.x = T)
andmed1.7 <- andmed1.6 %>%  group_by(MaakondNimetus, Vanus.kokku) %>% summarise(n = n()) %>% mutate(osakaal = n/Vanus.kokku)
andmed1.8 <-  andmed1.1 %>% mutate(Elukoht = tolower(ValdLinnNimetus))
andmed1.9 <- merge(andmed1.8, asukoht5, by= "Elukoht", all.x = T)
andmed1.10 <-  andmed1.1 %>% mutate(Elukoht = tolower(KohtNimetus))
andmed1.11 <- merge(andmed1.10, asukoht5, by= "Elukoht", all.x = T)
andmed1.12 <-  andmed1.11 %>% group_by(KohtNimetus, Vanus.kokku) %>% summarise(n = n()) %>% mutate(osakaal = n/Vanus.kokku)
andmed1.13 <-  andmed %>% group_by(MaakondNimetus) %>% summarise(mitu = n())
andmed1.14 <-  andmed %>% group_by(ValdLinnNimetus) %>% summarise(mitu = n())
andmed1.15 <-  andmed %>% group_by(KohtNimetus) %>% summarise(mitu = n())
andmed1.16 <-  andmed %>% group_by(ToimNadalapaev) %>% summarise(mitu = n())
andmed1.17 <-  andmed %>% group_by(ToimKell) %>% summarise(mitu = n())
andmed1.18 <-  andmed %>% group_by(SyndmusLiik) %>% summarise(mitu = n())
andmed1.19 <-  andmed %>% group_by(SyndmusLiik) %>% summarise(mitu = n())
andmed1.19 <-  andmed %>% group_by(SyyteoLiik) %>% summarise(mitu = n())
andmed1.20 <-  andmed %>% group_by(MaakondNimetus, SyyteoLiik) %>% summarise(mitu = n()) 
andmed1.21 <- andmed1.20 %>% group_by(MaakondNimetus) %>% summarise(mitu = sum(mitu)) %>% mutate(SyyteoLiik = "kokku")
andmed1.22 <- rbind(andmed1.21, andmed1.20)
andmed1.23 <-  pivot_wider(andmed1.22, names_from = SyyteoLiik, values_from = mitu)
andmed1.24 <- andmed1.23 %>% mutate(osakaal_V????r = VT/kokku, osakaal_Kuri = KT/kokku)
andmed1.25 <- andmed %>% group_by(ToimNadalapaev, SyyteoLiik) %>% summarise(mitu = n())

andmed1.26 <- andmed1.25 %>% group_by(ToimNadalapaev) %>% summarise(mitu = sum(mitu)) %>% mutate(SyyteoLiik = "kokku")
andmed1.27 <- rbind(andmed1.26, andmed1.25)
andmed1.28 <-  pivot_wider(andmed1.27, names_from = SyyteoLiik, values_from = mitu)
andmed1.29 <- andmed1.28 %>% mutate(osakaal_V????r = VT/kokku, osakaal_Kuri = KT/kokku)
andmed1.30 <-  andmed %>% group_by(KohtLiik) %>% summarise(mitu = n())
  
andmed1.31 <- andmed %>% group_by(SyyteoLiik) %>% summarise(mitu = n())
andmed1.32 <- andmed %>% group_by(SyyteoLiik,SyndmusLiik) %>% summarise(mitu = n())
andmed1.33 <- andmed %>% group_by(Kahjusumma) %>% summarise(mitu = n())

tunniga <- andmed %>% mutate(hour = format(as.POSIXct(ToimKell,format="%H:%M"),"%H"))
tunniga2 <-  tunniga %>% group_by(hour) %>% summarise(mitu = n())
tunniga3 <-  tunniga %>% group_by(hour, SyyteoLiik) %>% summarise(mitu = n())

41422+ 37872

41422/79294

andmed1.7 %>% ggplot(aes(x=MaakondNimetus, y = osakaal)) + geom_bar(stat="identity") + theme_bw() 
tunniga2 %>% ggplot(aes(x=hour, y = mitu)) + geom_bar(stat="identity") + theme_bw()
tunniga3 %>% ggplot(aes(x=hour, y = mitu, fill = SyyteoLiik)) + geom_bar(position = position_dodge(width = 0.8),stat="identity") + 
  theme_bw() + xlab("Hour") + ylab("") + scale_fill_manual(values=c("#2E86C1", "#BB8FCE"), name="",
                                                               labels=c("Crime", "Misdemeanor"))#+

andmed %>% group_by(ToimNadalapaev)
aa <- andmed %>% group_by(ToimNadalapaev, SyyteoLiik) %>% summarise(mitu = n())
aa %>% mutate(ToimNadalapaev = factor(ToimNadalapaev,levels = c( 
                                "Esmasp??ev", "Teisip??ev", "Kolmap??ev", 
                                "Neljap??ev", "Reede     ", "Laup??ev  ", 
                                "P??hap??ev"))) %>% 
  mutate(ToimNadalapaev = case_when(ToimNadalapaev == "Esmasp??ev" ~ "Monday",
                                    ToimNadalapaev == "Teisip??ev" ~ "Tuesday",
                                    ToimNadalapaev == "Kolmap??ev" ~ "Wednesday",
                                    ToimNadalapaev == "Neljap??ev" ~ "Thursday",
                                    ToimNadalapaev == "Reede     " ~ "Friday",
                                    ToimNadalapaev == "Laup??ev  " ~ "Saturday",
                                    ToimNadalapaev == "P??hap??ev" ~ "Sunday")) %>%
  mutate(ToimNadalapaev = factor(ToimNadalapaev,levels = c( 
                                      "Monday", "Tuesday","Wednesday","Thursday" , "Friday", "Saturday","Sunday"))) %>% 
  ggplot(aes(x=ToimNadalapaev, y = mitu, fill = SyyteoLiik)) + geom_bar(position = position_dodge(width = 0.8),stat="identity") + 
  theme_bw() + xlab("Day") + ylab("") + scale_fill_manual(values=c("#2E86C1", "#BB8FCE"), name="",
                                                               labels=c("Crime", "Misdemeanor"))#+
andmed %>% group_by(MaakondNimetus, SyyteoLiik) %>% summarise(mitu = n()) %>% 
  ggplot(aes(x=MaakondNimetus, y = mitu, fill = SyyteoLiik)) + geom_bar(position = position_dodge(width = 0.8),stat="identity") + 
  theme_bw() + xlab("Day") + ylab("") + scale_fill_manual(values=c("#2E86C1", "#BB8FCE"), name="",
                                                          labels=c("Crime", "Misdemeanor"))#+

andmed %>% group_by(MaakondNimetus, SyyteoLiik) %>% summarise(mitu = n()) %>% 
  ggplot(aes(y=MaakondNimetus, x="", fill = MaakondNimetus)) + geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

#  theme(legend.title=element_blank(),labels=c("Kuritegu", "V????rtegu"))
andmed1.7$MaakondNimetus
andmed1.7 %>%mutate(MaakondNimetus = case_when(MaakondNimetus == "Harju maakond"~ "Harjumaa",
                                               MaakondNimetus == "Hiiu maakond"~ "Hiiumaa",
                                               MaakondNimetus == "Ida-Viru maakond"~ "Ida-Virumaa",
                                               MaakondNimetus == "J??geva maakond"~ "J??gevamaa",
                                               MaakondNimetus == "J??rva maakond"~ "J??rvamaa",
                                               MaakondNimetus == "L????ne maakond"~ "L????nemaa",
                                               MaakondNimetus == "L????ne-Viru maakond"~ "L????ne-Virumaa",
                                               MaakondNimetus == "P??lva maakond"~ "P??lvamaa",
                                               MaakondNimetus == "P??rnu maakond"~ "P??rnumaa",
                                               MaakondNimetus == "Rapla maakond"~ "Raplamaa",
                                               MaakondNimetus == "Saare maakond"~ "Saaremaa",
                                               MaakondNimetus == "Tartu maakond"~ "Tartumaa",
                                               MaakondNimetus == "Valga maakond"~ "Valgamaa",
                                               MaakondNimetus == "Viljandi maakond"~ "Viljandimaa",
                                               MaakondNimetus == "V??ru maakond"~ "V??rumaa")) %>%  
  ggplot(aes(x = fct_rev(fct_reorder(MaakondNimetus, n)), y= osakaal, fill = n)) + geom_bar(position = position_dodge(width = 0.8),stat="identity") + 
  theme_bw() + theme(axis.text.x = element_text(angle=60, hjust=1)) + labs(x = "MaakondNimetus") + xlab("County") + ylab("Proportion")+guides(fill="none")
my_breaks= c(500000, 100000, 30000, 20000,10000)

res <- cor(andmed)
round(res, 2)

#c("Harjumaa","Hiiumaa","Ida-Virumaa","J??gevamaa",
#"J??rvamaa","L????nemaa","L????ne-Virumaa","P??rnumaa",
#"P??lvamaa","Raplamaa","Saaremaa","Tartumaa","Valgamaa","Viljandi","V??rumaa")
