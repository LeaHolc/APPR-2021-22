# 4. faza: Napredna analiza podatkov

library(ggplot2)
library(GGally)
library(dplyr)
library(tmap)

placa <- povprecna.mesecna.placa %>% filter(leto == "2020", vrsta.place == "neto")
placa$povprecna.placa[placa$obcina == "Osilnica"] <- 714.13

tabela.za.analizo <- struktura.prebivalstva %>% filter(leto == "2020", 
                                                       izobrazba == " Višješolska, visokošolska - Skupaj"
                                                       ) %>% group_by(obcina
                                                                      ) %>% 
  summarise(delez.visokoizobrazenih = round((sum(stevilo) / sum(prebivalci))* 100,2))  

tabela.za.analizo<- tabela.za.analizo %>% left_join(placa, by = c("obcina"))

g <- ggplot(tabela.za.analizo, aes(x=povprecna.placa, y=delez.visokoizobrazenih)) + geom_point()
print(g)

g + geom_smooth(method="lm", formula = y ~ x)


###################### METODA VODITELJEV ###############################################################

set.seed(123)

tabela.za.analizo.skupine <- unique(tabela.za.analizo$obcina) %>% sort()
razlicni.elementi.skupine <- lvls != tabela.za.analizo.skupine
primerjava.elementi.skupine <- data.frame(obcina.zemljevid = parse_character(lvls),
                                          obcina.skupine = tabela.za.analizo.skupine)[razlicni,]

primerjava.elementi.skupine.na.zemljevidu <- tabela.za.analizo %>% 
  left_join(primerjava.elementi.skupine, by = c("obcina" = "obcina.skupine")) %>%
  mutate(obcina = ifelse(is.na(obcina.zemljevid), obcina, obcina.zemljevid) %>% factor()) %>%
  dplyr::select(-obcina.zemljevid)

tabela.za.analizo.norm <- primerjava.elementi.skupine.na.zemljevidu %>% 
  dplyr:: select(delez.visokoizobrazenih, povprecna.placa) %>% scale()

rownames(tabela.za.analizo.norm) <- primerjava.elementi.skupine.na.zemljevidu$obcina
skupine <- kmeans(tabela.za.analizo.norm, 5, nstart=1000)
skupine$tot.withinss

skupine.zemljevid <- data.frame(obcina = primerjava.elementi.skupine.na.zemljevidu$obcina, 
                                Skupina = factor(skupine$cluster))

tm_shape(merge(obcine, skupine.zemljevid, by.x = "OB_UIME", by.y = "obcina")) + tm_polygons("Skupina")
tmap_mode("view")

