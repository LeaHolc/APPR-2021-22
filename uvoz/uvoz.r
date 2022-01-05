# 2. faza: Uvoz podatkov


sl <- locale("sl", decimal_mark=",", grouping_mark=".")

library(readr)
library(tibble)
library(tidyr)
library(dplyr)
library(readxl)
library(tidyverse)

# PREBIVALSTVO

prebivalstvo.obcin.polletno <- read_csv2("podatki/prebivalstvo_po_spolu_obcine_polletno.csv", skip = 2, na="-",
                               locale=locale(encoding="Windows-1250"), col_names = TRUE, 
                               col_types = cols(
                                 .default = col_guess(),
                                 SPOL = col_factor()
                               ))

prebivalstvo.obcin.polletno$OBČINE[prebivalstvo.obcin.polletno$OBČINE == "Ankaran/Ancarano"] <- "Ankaran"
prebivalstvo.obcin.polletno$OBČINE[prebivalstvo.obcin.polletno$OBČINE == "Dobrovnik/Dobronak"] <- "Dobrovnik"
prebivalstvo.obcin.polletno$OBČINE[prebivalstvo.obcin.polletno$OBČINE == "Hodoš/Hodos"] <- "Hodoš"
prebivalstvo.obcin.polletno$OBČINE[prebivalstvo.obcin.polletno$OBČINE == "Izola/Isola"] <- "Izola"
prebivalstvo.obcin.polletno$OBČINE[prebivalstvo.obcin.polletno$OBČINE == "Koper/Capodistria"] <- "Koper"
prebivalstvo.obcin.polletno$OBČINE[prebivalstvo.obcin.polletno$OBČINE == "Lendava/Lendva"] <- "Lendava"
prebivalstvo.obcin.polletno$OBČINE[prebivalstvo.obcin.polletno$OBČINE == "Piran/Pirano"] <- "Piran"

prebivalstvo.obcin.polletno <- prebivalstvo.obcin.polletno %>% 
  pivot_longer(
    cols = colnames(prebivalstvo.obcin.polletno)[-c(1,2)],
    names_to = "polletje.starost",
    values_to = "stevilo"
    ) %>%
   tidyr::extract(
     col = polletje.starost,
     into = c("polletje", "starost"),
     regex = "^(\\d{4}H[12])\\s+(.*)$"
     )%>%
  rename("obcina" = "OBČINE", "spol" = "SPOL"
         ) %>%
  select("spol", "obcina", "polletje", "stevilo"
         ) %>%
  relocate(obcina, polletje, spol, stevilo)

prebivalstvo.obcin.polletno <- prebivalstvo.obcin.polletno %>%
  tidyr::extract(
    col = polletje,
    into = c("leto", "obdobje"),
    regex = "^(\\d{4})+(.*)$"
  )

prebivalstvo.obcin <- prebivalstvo.obcin.polletno %>%
  group_by(obcina, leto, spol
           ) %>% 
  summarise(prebivalci = as.integer(sum(stevilo) / 2)) 

# IZOBRAZBENA STRUKTURA PREBIVALSTVA

izobrazbena.struktura <- read_csv2("podatki/prebivalstvo_po_spolu_in_st_izobrazbe.csv", skip = 2, na="-",
                                  locale=locale(encoding="Windows-1250"), col_names = TRUE)

izobrazbena.struktura$OBČINE[izobrazbena.struktura$OBČINE == "Ankaran/Ancarano"] <- "Ankaran"
izobrazbena.struktura$OBČINE[izobrazbena.struktura$OBČINE == "Dobrovnik/Dobronak"] <- "Dobrovnik"
izobrazbena.struktura$OBČINE[izobrazbena.struktura$OBČINE == "Hodoš/Hodos"] <- "Hodoš"
izobrazbena.struktura$OBČINE[izobrazbena.struktura$OBČINE == "Izola/Isola"] <- "Izola"
izobrazbena.struktura$OBČINE[izobrazbena.struktura$OBČINE == "Koper/Capodistria"] <- "Koper"
izobrazbena.struktura$OBČINE[izobrazbena.struktura$OBČINE == "Lendava/Lendva"] <- "Lendava"
izobrazbena.struktura$OBČINE[izobrazbena.struktura$OBČINE == "Piran/Pirano"] <- "Piran"

izobrazbena.struktura <- izobrazbena.struktura %>%
  pivot_longer(
    cols = colnames(izobrazbena.struktura)[-c(1,2)],
    names_to = "leto.izobrazba",
    values_to = "stevilo"
    ) %>%
  tidyr::extract(
    col = leto.izobrazba,
    into = c("leto", "izobrazba"),
    regex = "^(\\d{4})+(.*)$"
  ) %>%
  relocate(obcina = OBČINE, leto, spol = SPOL, izobrazba, stevilo
           ) %>%
  group_by(leto, obcina) %>%
  arrange(obcina, .by_group = TRUE)


# TABELA 1: STRUKTURA PREBIVALSTVA GLEDE NA STOPNJO IZOBRAZBE PO OBČINAH

struktura.prebivalstva <- izobrazbena.struktura %>%
  left_join(prebivalstvo.obcin, by = c("obcina", "leto", "spol")
            ) %>% 
  mutate(odstotek = round((stevilo / prebivalci)*100, 2)
         )%>%
  group_by(obcina) %>%
  arrange(obcina, .by_group = TRUE)

# poskus.pretvorbe <- pivot_wider(izobrazbena.struktura, names_from = "izobrazba", values_from = "stevilo")
# 
# prebivalstvo.izobrazba.struktura <- poskus.pretvorbe %>%
#   left_join(prebivalstvo.obcin, by = c("obcina", "leto", "spol")) 


# ŠTEVILO ŠTUDENTK NA 100 ŠTUDENTOV

studentke.na.100.studentov <- read_csv2("podatki/stevilo_studentk_na_100_studentov.csv", skip = 2, na="-",
                                        locale=locale(encoding="Windows-1250"), col_names = TRUE)

studentke.na.100.studentov$OBČINE[studentke.na.100.studentov$OBČINE == "Ankaran/Ancarano"] <- "Ankaran"
studentke.na.100.studentov$OBČINE[studentke.na.100.studentov$OBČINE == "Dobrovnik/Dobronak"] <- "Dobrovnik"
studentke.na.100.studentov$OBČINE[studentke.na.100.studentov$OBČINE == "Hodoš/Hodos"] <- "Hodoš"
studentke.na.100.studentov$OBČINE[studentke.na.100.studentov$OBČINE == "Izola/Isola"] <- "Izola"
studentke.na.100.studentov$OBČINE[studentke.na.100.studentov$OBČINE == "Koper/Capodistria"] <- "Koper"
studentke.na.100.studentov$OBČINE[studentke.na.100.studentov$OBČINE == "Lendava/Lendva"] <- "Lendava"
studentke.na.100.studentov$OBČINE[studentke.na.100.studentov$OBČINE == "Piran/Pirano"] <- "Piran"

studentke.na.100.studentov[2] = as.double(unlist(studentke.na.100.studentov[2]))
studentke.na.100.studentov[3] = as.double(unlist(studentke.na.100.studentov[3]))
studentke.na.100.studentov[4] = as.double(unlist(studentke.na.100.studentov[4]))
studentke.na.100.studentov[5] = as.double(unlist(studentke.na.100.studentov[5]))
studentke.na.100.studentov[6] = as.double(unlist(studentke.na.100.studentov[6]))
studentke.na.100.studentov[7] = as.double(unlist(studentke.na.100.studentov[7]))
studentke.na.100.studentov[8] = as.double(unlist(studentke.na.100.studentov[8]))
studentke.na.100.studentov[9] = as.double(unlist(studentke.na.100.studentov[9]))
studentke.na.100.studentov[10] = as.double(unlist(studentke.na.100.studentov[10]))

studentke.na.100.studentov <- studentke.na.100.studentov %>% 
  pivot_longer(
    cols = colnames(studentke.na.100.studentov)[-1],
    names_to = "leto.mera",
    values_to = "stevilo.studentk"
    ) %>%
  tidyr::extract(
    col = leto.mera,
    into = c("leto", "mera"),
    regex = "^(\\d{4})+(.*)$"
  ) %>%
  rename("obcina" = "OBČINE"
         ) %>%
  select("obcina", "leto", "stevilo.studentk")

# RAZLIKA V PLAČI MED SPOLOMA

placna.vrzel <- read_csv2("podatki/placna_vrzel_med_spoloma.csv", skip = 2, na="-",
                          locale=locale(encoding="Windows-1250"), col_names = TRUE)

placna.vrzel$OBČINE[placna.vrzel$OBČINE == "Ankaran/Ancarano"] <- "Ankaran"
placna.vrzel$OBČINE[placna.vrzel$OBČINE == "Dobrovnik/Dobronak"] <- "Dobrovnik"
placna.vrzel$OBČINE[placna.vrzel$OBČINE == "Hodoš/Hodos"] <- "Hodoš"
placna.vrzel$OBČINE[placna.vrzel$OBČINE == "Izola/Isola"] <- "Izola"
placna.vrzel$OBČINE[placna.vrzel$OBČINE == "Koper/Capodistria"] <- "Koper"
placna.vrzel$OBČINE[placna.vrzel$OBČINE == "Lendava/Lendva"] <- "Lendava"
placna.vrzel$OBČINE[placna.vrzel$OBČINE == "Piran/Pirano"] <- "Piran"

names(placna.vrzel)[10] <- 2020

placna.vrzel <- placna.vrzel %>%
  pivot_longer(
    cols = colnames(placna.vrzel)[-1],
    names_to = "leto",
    values_to = "stopnja.razlike"
  ) %>%
  relocate(obcina = OBČINE, leto, stopnja.razlike)


# TABELA 3: PRIMERJAVA ŠTEVILA ŠTUDENTK IN ŠTUDENTOV TER PLAČNE VRZELI MED SPOLOMA

place.in.studentke <- studentke.na.100.studentov %>%
  left_join(placna.vrzel, by = c("obcina", "leto")) 

# PLAČE

povprecna.mesecna.placa <- read_delim("podatki/povprecna_mesecna_placa.csv", skip = 2, na="-",
                                                    locale=locale(encoding="Windows-1250", 
                                                                  decimal_mark = ".", grouping_mark = ","),
                                      col_types = cols(
                                       .default = col_double(),
                                       "OBČINE" = col_character()
                                     ))

povprecna.mesecna.placa$OBČINE[povprecna.mesecna.placa$OBČINE == "Ankaran/Ancarano"] <- "Ankaran"
povprecna.mesecna.placa$OBČINE[povprecna.mesecna.placa$OBČINE == "Dobrovnik/Dobronak"] <- "Dobrovnik"
povprecna.mesecna.placa$OBČINE[povprecna.mesecna.placa$OBČINE == "Hodoš/Hodos"] <- "Hodoš"
povprecna.mesecna.placa$OBČINE[povprecna.mesecna.placa$OBČINE == "Izola/Isola"] <- "Izola"
povprecna.mesecna.placa$OBČINE[povprecna.mesecna.placa$OBČINE == "Koper/Capodistria"] <- "Koper"
povprecna.mesecna.placa$OBČINE[povprecna.mesecna.placa$OBČINE == "Lendava/Lendva"] <- "Lendava"
povprecna.mesecna.placa$OBČINE[povprecna.mesecna.placa$OBČINE == "Piran/Pirano"] <- "Piran"

names(povprecna.mesecna.placa)[2] <- "bruto 2017"
names(povprecna.mesecna.placa)[3] <- "bruto 2018"
names(povprecna.mesecna.placa)[4] <- "bruto 2019"
names(povprecna.mesecna.placa)[5] <- "bruto 2020"
names(povprecna.mesecna.placa)[6] <- "neto 2017"
names(povprecna.mesecna.placa)[7] <- "neto 2018"
names(povprecna.mesecna.placa)[8] <- "neto 2019"
names(povprecna.mesecna.placa)[9] <- "neto 2020"

povprecna.mesecna.placa <- povprecna.mesecna.placa %>%
  pivot_longer(
    cols = colnames(povprecna.mesecna.placa)[-1],
    names_to = "vrsta.place.leto",
    values_to = "povprecna.placa"
  ) %>%
  tidyr:: extract(
    col = vrsta.place.leto,
    into = c("vrsta.place", "leto"),
    regex = "^(.*)\\s+(\\d{4})$"
  ) %>%
  relocate(obcina = OBČINE, leto, vrsta.place, povprecna.placa
           ) %>%
  filter(obcina != "SLOVENIJA")

povprecna.mesecna.placa.velike.crke <- povprecna.mesecna.placa %>%
  mutate(obcine = casefold(obcina, upper = TRUE), .keep = "unused") %>%
  relocate(obcina = obcine, leto, vrsta.place, povprecna.placa)

# BREZPOSELNOST

brezposelnost.17 <- read_xls("podatki/2017_BP_obcine_ravni_izobrazbe.xls", skip = 2)

brezposelnost.17 <- brezposelnost.17 %>%
  filter(Občina != "Občina izven RS")

brezposelnost.17 <- brezposelnost.17 %>% 
  pivot_longer(
    cols = colnames(brezposelnost.17)[-1],
    names_to = "stopnja.izobrazbe",
    values_to = "stevilo.brezposelnih"
    ) %>%
  mutate(leto = 2017
    ) %>%
  relocate(obcina = Občina, leto, stopnja.izobrazbe, stevilo.brezposelnih) %>%
  pivot_wider(names_from = "leto", values_from = stevilo.brezposelnih, values_fn = list(stevilo.brezposelnih = mean))

brezposelnost.18 <- read_xls("podatki/2018_BP_obcine_ravni_izobrazbe.xls", skip = 2)

brezposelnost.18 <- brezposelnost.18 %>%
  filter(Občina != "Občina izven RS")

brezposelnost.18 <- brezposelnost.18 %>% 
  pivot_longer(
    cols = colnames(brezposelnost.18)[-1],
    names_to = "stopnja.izobrazbe",
    values_to = "stevilo.brezposelnih"
    ) %>%
  mutate(leto = 2018
    ) %>%
  relocate(obcina = Občina, leto, stopnja.izobrazbe, stevilo.brezposelnih) %>%
  pivot_wider(names_from = "leto", values_from = stevilo.brezposelnih, values_fn = list(stevilo.brezposelnih = mean))

brezposelnost.19 <- read_xls("podatki/2019_BP_obcine_ravni_izobrazbe.xls", skip = 2)

brezposelnost.19 <- brezposelnost.19 %>%
  filter(Občina != "Občina izven RS")

brezposelnost.19 <- brezposelnost.19 %>% 
  pivot_longer(
    cols = colnames(brezposelnost.19)[-1],
    names_to = "stopnja.izobrazbe",
    values_to = "stevilo.brezposelnih"
  ) %>%
  mutate(leto = 2019
  ) %>%
  relocate(obcina = Občina, leto, stopnja.izobrazbe, stevilo.brezposelnih) %>%
  pivot_wider(names_from = "leto", values_from = stevilo.brezposelnih, values_fn = list(stevilo.brezposelnih = mean))

brezposelnost.20 <- read_xls("podatki/2020_BP_obcine_ravni_izobrazbe.xls", skip = 2)

brezposelnost.20 <- brezposelnost.20 %>%
  filter(Občina != "Občina izven RS")

brezposelnost.20 <- brezposelnost.20 %>% 
  pivot_longer(
    cols = colnames(brezposelnost.20)[-1],
    names_to = "stopnja.izobrazbe",
    values_to = "stevilo.brezposelnih"
  ) %>%
  mutate(leto = 2020
  ) %>%
  relocate(obcina = Občina, leto, stopnja.izobrazbe, stevilo.brezposelnih) %>%
  pivot_wider(names_from = "leto", values_from = stevilo.brezposelnih, values_fn = list(stevilo.brezposelnih = mean))


# TABELA 3: BREZPOSELNOST GLEDE NA STOPNJO IZOBRAZBE PO OBČINAH

brezposelnost.glede.na.stopnjo.izobrazbe <- brezposelnost.17 %>%
  left_join(brezposelnost.18, by = c("obcina", "stopnja.izobrazbe")
             ) %>%
  left_join(brezposelnost.19, by = c("obcina", "stopnja.izobrazbe")
            ) %>%
  left_join(brezposelnost.20, by = c("obcina", "stopnja.izobrazbe"))

brezposelnost <- brezposelnost.glede.na.stopnjo.izobrazbe %>%
  pivot_longer(
    cols = colnames(brezposelnost.glede.na.stopnjo.izobrazbe)[-c(1,2)],
    names_to = "leto",
    values_to = "stevilo.brezposelnih"
     )  %>%
  relocate(obcina, leto, stopnja.izobrazbe, stevilo.brezposelnih)

# TABELA 3:  POVPREČNE PLAČE V SLOVENSKIH OBČINAH IN BREZPOSELNOST

placa.in.brezposelnost.po.obcinah <- brezposelnost %>%
  left_join(povprecna.mesecna.placa.velike.crke, by = c("obcina", "leto"))

