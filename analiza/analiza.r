# 4. faza: Napredna analiza podatkov

# library(ggplot2)
# library(GGally)
# library(dplyr)
# library(tmap)

# source("lib/libraries.r", encoding="UTF-8")


placa <- povprecna.mesecna.placa %>% filter(leto == "2020", vrsta.place == "neto")
placa$povprecna.placa[placa$obcina == "Osilnica"] <- 714.13

tabela.za.analizo <- struktura.prebivalstva %>% filter(leto == "2020",
                                                       izobrazba == " Višješolska, visokošolska - Skupaj"
) %>% group_by(obcina
) %>%
  summarise(delez.visokoizobrazenih = round((sum(stevilo) / sum(prebivalci))* 100,2)) %>%
  left_join(placa, by = "obcina") %>% dplyr::select(obcina, delez.visokoizobrazenih, povprecna.placa)


################################# METODA VODITELJEV ####################################################

library(cluster)

obrisi = function(podatki, hc = TRUE, od = 2, do = NULL) {
  n = nrow(podatki)
  if (is.null(do)) {
    do = n - 1
  }
  
  razdalje = dist(podatki)
  
  k.obrisi = tibble()
  for (k in od:do) {
    if (hc) {
      o.k = hclust(razdalje) %>%
        cutree(k) %>%
        silhouette(razdalje)
    } else {
      set.seed(123)
      o.k = kmeans(podatki, k)$cluster %>%
        silhouette(razdalje)
    }
    k.obrisi = k.obrisi %>% bind_rows(
      tibble(
        k = rep(k, n),
        obrisi = o.k[, "sil_width"]
      )
    )
  }
  k.obrisi$k = as.ordered(k.obrisi$k)
  
  k.obrisi
}

obrisi.povprecje = function(k.obrisi) {
  k.obrisi.povprecje = k.obrisi %>%
    group_by(k) %>%
    summarize(obrisi = mean(obrisi))
}

obrisi.k = function(k.obrisi) {
  obrisi.povprecje(k.obrisi) %>%
    filter(obrisi == max(obrisi)) %>%
    summarize(k = min(k)) %>%
    unlist() %>%
    as.character() %>%
    as.integer()
}


r.km <- tabela.za.analizo[, -1] %>% obrisi(hc = FALSE)

optimalno.stevilo.skupin <- obrisi.k(r.km)

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
skupine <- kmeans(tabela.za.analizo.norm, 7, nstart=1000)
skupine$tot.withinss

skupine.zemljevid <- data.frame(obcina = primerjava.elementi.skupine.na.zemljevidu$obcina,
                                Skupina = factor(skupine$cluster))

mapa3 <- tm_shape(merge(obcine, skupine.zemljevid, by.x = "OB_UIME", by.y = "obcina")) + tm_polygons("Skupina")
tmap_mode("view")

#################################### NAPOVED ############################################################

library(ranger)

slovenija.place.mesecno <- read_csv2("podatki/slovenija_place_mesecno.csv", skip = 2, na="-",
                                     locale=locale(encoding="Windows-1250"), col_names = TRUE)


slovenija.place.mesecno <- slovenija.place.mesecno %>%
  rename("mesec" = "MESEC", "placa" = "Neto plača" 
         ) %>% 
  mutate(neto = placa / 100
         ) %>%
  dplyr::select(mesec, neto
                ) %>%
  map_df(rev) 

Lag <- function(x, n){c(rep(NA, n), x)[1:length(x)]}

naredi.df <- function(x){
  data.frame(placa = x,
             placa1 = Lag(x, 1),
             placa2 = Lag(x, 2),
             placa3 = Lag(x, 3),
             placa4 = Lag(x, 4))
  
}

df <- naredi.df(slovenija.place.mesecno$neto)
model = ranger(formula = placa ~ ., data = df %>% drop_na())


n = nrow(df)

df2 <- naredi.df(c(slovenija.place.mesecno$neto, NA))
napoved <- predict(model, data = df2[n+1,])$predictions
df2[n+1,1] = napoved

datumi<- seq(as.Date("2006-1-1"), as.Date("2021-12-1"), by = "months")

pricakovanje <- as_tibble(data.frame(
  datumi,
  df2$placa))
df2[(n+1):(n+10),1]

napoved.graf <- ggplot(pricakovanje) + geom_line(mapping = aes(x = datumi, y = df2.placa), color = "red")+
  geom_line(mapping = aes(x = datumi, y = df2.placa, color = datumi <= as.Date("2021-11-1")), show.legend = FALSE)+
  labs(
    x = "Leto",
    y = "Povprečna mesečna neto plača",
    title = "Gibanje povprečne mesečne neto plače v Sloveniji od leta 2006 do 2021"
  )
napoved.graf

################################### LINEARNA REGRESIJA #####################################################

graf.regresija <- ggplot(pricakovanje, aes(x=datumi, y=df2.placa)) + geom_point() + 
  xlab("Leto") + ylab("Povprečna mesečna neto plača")+
  ggtitle("Rast povprečne mesečne plače v Sloveniji od leta 2006 do 2021")
linearna.regresija <- graf.regresija + geom_smooth(method="lm", formula = y ~ x)
linearna.regresija
