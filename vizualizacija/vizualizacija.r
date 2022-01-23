# 3. faza: Vizualizacija podatkov

# library(tidyverse)
# library(ggplot2)
# library(dplyr)
# library(sp)
# library(rgdal)
# library(rgeos)
# library(raster)
# library(tmap)

# source("lib/libraries.r", encoding="UTF-8")

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
  ) + geom_boxplot() + 
  xlab("Regija")+ ylab("Povprečna plača") +
  ggtitle("Porazdelitev plač po statističnih regijah za leto 2020")
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
  scale_colour_manual("Plača", values = c("maroon", "slateblue1"), labels = c("nadpovprečna", "podpovprečna")) 
graf4


# GRAF 5: Gibanje števila visokoizobraženih v občinah z najmanjšo plačno vrzeljo

enake.obcine <- struktura.prebivalstva %>% 
  mutate(obcine = casefold(obcina, upper = TRUE), .keep = "unused"
         ) %>%
  dplyr::select(obcina = obcine, leto, spol, izobrazba, stevilo, odstotek
                ) %>%
  filter(obcina %in% podatki$obcina, izobrazba == " Višješolska, visokošolska - Skupaj")

graf5 <- ggplot(enake.obcine) + aes(x = leto, y = odstotek, color = spol) +
  geom_line() + 
  scale_color_manual("Spol",
                     values = c("lightskyblue",  "orchid"),
                     labels = c("Moški", "Ženske")) +
  facet_wrap(.~obcina) +
  xlab("Leto") + ylab("Delež visokoizobraženih") + 
  ggtitle("Gibanje deleža visokoizobraženih v občinah \n z najmanjšo plačno vrzeljo od leta 2012 do 2020") 
  
graf5


# GRAF 6: Izobrazbena struktura v občini z najvišjo povprečno plačo za leto 2020


podatki.graf.6 <- povprecna.mesecna.placa %>% filter(leto == 2020, vrsta.place == "neto")
podatki.graf.6$povprecna.placa[podatki.graf.6$obcina == "Osilnica"] <- 714.13

podatki.graf.6 <- podatki.graf.6 %>% 
  left_join(filter(izobrazbena.struktura, leto == 2020, izobrazba != 
                     " Višješolska, visokošolska - Skupaj"), by = c("obcina", "leto"))
  

podatki.graf.6 <- podatki.graf.6 %>%
  filter(povprecna.placa == max(povprecna.placa)
         ) %>% group_by(izobrazba) %>%
  mutate(sestav = sum(stevilo)) %>%
  filter(spol == "Ženske")

graf6 <- ggplot(podatki.graf.6) +
  aes(x = "", y = sestav, fill = izobrazba) + geom_col(width = 1) + 
  scale_fill_manual("Stopnja izobrazbe",
                    values = c("seagreen1", "lightskyblue", "plum", "greenyellow", "salmon"),
                    labels = c("Osnovnošolska ali manj", "Srednješolska", "Visokošolska 1. stopnje ipd",
                               "Visokošolska 2. stopnje ipd", "Visokošolska 3. stopnje ipd")) +
  coord_polar(theta = "y") +
  xlab("") + ylab("")  + 
  theme(axis.text = element_blank(),
                              axis.ticks = element_blank(),
                              panel.grid = element_blank()) +
  ggtitle(paste("Izobrazbena struktura za občino", podatki.graf.6$obcina[1], "v letu 2020", sep = " "))
graf6


########################################## ZEMLJEVIDI #########################################################


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

mapa1 <- tm_shape(obcine.placa.zemljevid) +
  tm_polygons("povprecna.placa", popup.vars = c("Višina povprečne plače: " = "povprecna.placa"))
tmap_mode("view")
mapa1

prebivalstvo.obcin.2020 <- prebivalstvo.obcin %>% filter(leto == "2020") %>%
   group_by(obcina) %>% mutate(prebivalstvo = sum(prebivalci)) %>%
  filter(spol == "Ženske") %>% dplyr::select(- c("spol", "prebivalci"))

brezposelnost.zemljevid <- unique(prebivalstvo.obcin.2020$obcina) %>% sort()

razlicni.elementi <- lvls!= brezposelnost.zemljevid

primerjava.elementi <- data.frame(obcina.zemljevid = parse_character(lvls),
                         obcina.brezposelnost = brezposelnost.zemljevid)[razlicni,]

brezposelnost.na.zemljevidu <- prebivalstvo.obcin.2020 %>%
  left_join(primerjava.elementi, by = c("obcina" = "obcina.brezposelnost"))  %>%
   mutate(obcina = ifelse(is.na(obcina.zemljevid), obcina, obcina.zemljevid) %>% factor()) %>%
   dplyr::select(-obcina.zemljevid)

brezposelnost.na.zemljevidu <- placa.na.zemljevidu %>%
  left_join(brezposelnost.na.zemljevidu, by = c("leto", "obcina")) %>%
  mutate(Brezposelni = round((stevilo.brezposelnih / prebivalstvo) * 100, 2))


obcine.brezposelnost.zemljevid <- merge(obcine, brezposelnost.na.zemljevidu,
                                by.x = "OB_UIME", by.y = "obcina")

tm_shape(obcine.brezposelnost.zemljevid) +
  tm_polygons("Brezposelni", popup.vars = c("Delež brezposelnih: " = "Brezposelni"))
tmap_mode("view")




# graf = ggplot(brezposelnost %>% filter(obcina == "LJUBLJANA", stopnja.izobrazbe == "OŠ ali manj")) +
#   aes(x = leto, y = stevilo.brezposelnih) +
#   geom_col(position = "dodge", fill = "lightblue") +
#   labs(
#     x = "leto",
#     y = "število brezposelnih",
#     title = "obcina.vnos"
#   )
# print(graf)
