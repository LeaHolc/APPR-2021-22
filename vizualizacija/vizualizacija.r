# 3. faza: Vizualizacija podatkov

library(tidyverse)
library(ggplot2)
library(dplyr)
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(tmap)

########################################## GRAFIČNA ANALIZA #########################################################

# ANALIZA PO STATISTIČNIH REGIJAH


# GRAF 1: Delež visokoizobraženih po statističnih regijah,ločeno po spolu za leto 2020

visokoizobrazeni.regije <- struktura.prebivalstva %>%
  filter(leto == "2020", izobrazba == " Višješolska, visokošolska - Skupaj"
           ) %>%
  group_by(regija, spol
           ) %>% summarise(delez.visoko.izobrazenih = round(sum(stevilo) / sum(prebivalci) * 100,2))

graf1 <- ggplot(visokoizobrazeni.regije) + 
  aes(x = spol, y = delez.visoko.izobrazenih, fill = spol) + geom_col(position = "dodge")+
  xlab("Spol")+ ylab("Delež visoko izobraženih") +
  ggtitle("Delež visokoizobraženih po statističnih regijah za leto 2020") + 
  scale_fill_manual(values = c("lightskyblue", "hotpink")) + facet_wrap(.~regija)
graf1


# GRAF 2: Delež brezposelnih po statističnih regijah za leto 2020

stevilo.prebivalcev.regije <- prebivalstvo.pripadnost.regijam %>%
  filter(leto == "2020") %>%
  group_by(regija) %>%
  summarise(prebivalci.regija = sum(prebivalci))

brezposelni.regije <- placa.in.brezposelnost.po.obcinah %>%
  filter(leto == "2020", stopnja.izobrazbe == "Skupaj", vrsta.place == "neto"
         ) %>%
  group_by(regija) %>% summarise(brezposelnost.regija = sum(stevilo.brezposelnih))

prebivalstvo.in.brezposelni.po.regijah <- stevilo.prebivalcev.regije %>%
  left_join(brezposelni.regije, by = "regija"
            ) %>%
  mutate(delez.brezposelnih = round((brezposelnost.regija/prebivalci.regija) * 100,2))

graf2 <- ggplot(prebivalstvo.in.brezposelni.po.regijah) + 
  aes(x = regija, y = delez.brezposelnih) + geom_bar(stat = "identity", fill = "lightblue")+
  xlab("Regija")+ ylab("Delež brezposelnih") + 
  ggtitle("Delež brezposelnih po statističnih regijah za leto 2020")+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.title.x = element_text(vjust = 0)
  )
graf2


# GRAF 3: Porazdelitev plač po statističnih regijah za leto 2020

povprecna.slovenska.placa.tabela.2020 <- placa.in.brezposelnost.po.obcinah %>% group_by(obcina) %>%
  filter(leto == "2020", vrsta.place == "neto", stopnja.izobrazbe == "Skupaj")
povprecna.slovenska.placa.tabela.2020$povprecna.placa[povprecna.slovenska.placa.tabela.2020$obcina == "OSILNICA"] <- 714.13


povprecna.slovenska.placa.tabela.2019 <- placa.in.brezposelnost.po.obcinah %>% group_by(obcina) %>%
   filter(leto == "2019", vrsta.place == "neto", stopnja.izobrazbe == "Skupaj")

izracun.povprecne.place <- function(x){
  tabela.regija <- povprecna.slovenska.placa.tabela.2020 %>% filter(regija == as.character(x))
  placa <-round(sum(tabela.regija$povprecna.placa/nrow(tabela.regija)),2)
  return(placa)
} 

graf3 <- ggplot(povprecna.slovenska.placa.tabela.2020) +
  aes(x = regija , y = povprecna.placa) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.title.x = element_text(vjust = 0)
  ) +
    geom_boxplot() + ggtitle("Porazdelitev plač po statističnih regijah za leto 2020")
graf3


# ANALIZA PO OBČINAH


# GRAF 4: Občine po številu študentk in plačni vrzeli za leto 2020

povprecna.slovenska.placa <- round(sum(povprecna.slovenska.placa.tabela.2020$povprecna.placa)
                                   /nrow(povprecna.slovenska.placa.tabela.2020),2)
povprecna.slovenska.placa

podatki.graf.4 <- place.in.studentke %>% 
  filter(leto == "2020") %>%
  left_join(filter(placa.in.brezposelnost.po.obcinah, leto == "2020" & 
                     stopnja.izobrazbe == "Skupaj" &
                     vrsta.place == "neto"), by = c("obcina", "regija", "leto")) %>%
  dplyr::select("obcina", "leto", "stevilo.studentk", "stopnja.razlike", "povprecna.placa") %>%
  transform(stopnja.razlike = as.numeric(stopnja.razlike))

podatki.graf.4 <- podatki.graf.4 %>%
  mutate(povprecna = ifelse(podatki.graf.4$povprecna.placa >= povprecna.slovenska.placa, "nadpovprečna", 
                               ifelse(podatki.graf.4$povprecna.placa < povprecna.slovenska.placa, "podpovprečna", 0))
         )%>%
  filter(stopnja.razlike > - 3.0 & stopnja.razlike < 3.0) 

podatki <- podatki.graf.4 %>%
  filter(stopnja.razlike > - 0.5 & stopnja.razlike < 0.5)

graf4 <- ggplot(podatki.graf.4) + 
  aes(x = stopnja.razlike, y = stevilo.studentk, colour = povprecna ) + 
  geom_point(size = 1) + 
  geom_text(aes(label = ifelse(stopnja.razlike %in% podatki$stopnja.razlike, as.character(obcina), '')),
                                    hjust = 0, vjust= 0, size = 3) +
  xlab("Stopnja razlike v plači") + ylab("Število študentk na 100 študentov")+
  ggtitle("Občine po številu študentk in plačni vrzeli za leto 2020") +
  scale_colour_manual("Plača", values = c("plum", "lightcyan4"), labels = c("nadpovprečna", "podpovprečna")) 
graf4
 

# "Rmd"
# 3 graf shraniš plus v r okolje kličeš graf po imenu, tukaj ga ne smeš klicat
# echo je false- ta blok se ne izpiše, $$ ZA IZPIS, 'r spremenljivka' se v kodi izpiše

########################################## ZEMLJEVIDI #########################################################


# pri zemljevidih popravi še legendo

source("lib/uvozi.zemljevid.r")
obcine <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
                           pot.zemljevida="OB", encoding="Windows-1250")

tm_shape(obcine) + tm_polygons("OB_UIME") + tm_legend(show=FALSE)
obcine$OB_UIME <- factor(obcine$OB_UIME)

lvls <- levels(obcine$OB_UIME)
povprecna.placa.zemljevid <- unique(povprecna.slovenska.placa.tabela.2020$obcina) %>% sort()

razlicni <- lvls!= povprecna.placa.zemljevid

primerjava <- data.frame(obcina.zemljevid = parse_character(lvls),
                         obcina.placa = povprecna.placa.zemljevid)[razlicni,]

placa.na.zemljevidu <- povprecna.slovenska.placa.tabela.2020 %>% 
  left_join(primerjava, by = c("obcina" = "obcina.placa")) %>%
  mutate(obcina = ifelse(is.na(obcina.zemljevid), obcina, obcina.zemljevid) %>% factor()) %>%
  dplyr::select(-obcina.zemljevid)

obcine.placa.zemljevid <- merge(obcine, placa.na.zemljevidu,
                 by.x = "OB_UIME", by.y = "obcina")

tm_shape(obcine.placa.zemljevid) + 
  tm_polygons("povprecna.placa", popup.vars = c("Višina povprečne plače: " = "povprecna.placa"))
tmap_mode("view")

prebivalstvo.obcin.2020 <- prebivalstvo.obcin %>% filter(leto == "2020") %>%
  group_by(obcina) %>% mutate(prebivalstvo = sum(prebivalci)) %>% 
  filter(spol == "Ženske") %>% dplyr::select(- c("spol", "prebivalci"))

brezposelnost.zemljevid <- unique(prebivalstvo.obcin.2020$obcina) %>% sort()

razlicni.elementi <- lvls!= brezposelnost.zemljevid

primerjava.elementi <- data.frame(obcina.zemljevid = parse_character(lvls),
                         obcina.brezposelnost = brezposelnost.zemljevid)[razlicni,]

brezposelnost.na.zemljevidu <- prebivalstvo.obcin.2020 %>%
  left_join(primerjava.elementi, by = c("obcina" = "obcina.brezposelnost")) %>%
  mutate(obcina = ifelse(is.na(obcina.zemljevid), obcina, obcina.zemljevid) %>% factor()) %>%
  dplyr::select(-obcina.zemljevid) 

brezposelnost.na.zemljevidu <- placa.na.zemljevidu %>%
  left_join(brezposelnost.na.zemljevidu, by = c("leto", "obcina")) %>%
  mutate(delez.brezposelnih = round((stevilo.brezposelnih / prebivalstvo) * 100, 2))


obcine.brezposelnost.zemljevid <- merge(obcine, brezposelnost.na.zemljevidu,
                                by.x = "OB_UIME", by.y = "obcina")

tm_shape(obcine.brezposelnost.zemljevid) + 
  tm_polygons("delez.brezposelnih", popup.vars = c("Delež brezposelnih: " = "delez.brezposelnih"))
tmap_mode("view")

