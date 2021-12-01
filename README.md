# Analiza podatkov s programom R - 2021/22

Vzorčni repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Struktura prebivalstva Slovenije glede na stopnjo izobrazbe

V projektu bom analizirala strukturo prebivalstva Slovenije po občinah glede na stopnjo izobrazbe. Zanimali me bodo deleži (delež prebivalstva z določeno stopnjoizobrazbe), saj ti omogočajo najboljšo primerjavo glede na razlike v velikosti občin. 

V nadaljevanju se bom osredotočila predvsem na makroekonomske kazalce; torej višino plač in stopnjo brezposelnosti. Opazovala bom povezave med stopnjo izobrazbe, povprečno mesečno neto plačo in stopnjo brezposelnosti po občinah.

Zanimale me bodo tudi razlike med spoloma. Predvsem razlika v višini plač in število moških in žensk z višje ali visokošolsko izobrazbo.

Kot vir podatkov bom uporabila naslednje spletne strani:

* https://pxweb.stat.si/SiStat/sl
* https://www.ess.gov.si/trg_dela/trg_dela_v_stevilkah/registrirana_brezposelnost#Ob%C4%8Dine

Tabele:

1. Tabela 1: Struktura prebivalstva Slovenije po občinah glede na stopnjo izobrazbe od leta 2012 do leta 2020

* `leto`
* `občina`
* `stevilo_prebivalcev`
* `osnovnosolska_ali_manj`
* `srednjesolska`
* `visokosolska_1_stopnja`
* `visokosolska_2_stopnja`
* `visokosolska_3_stopnja`
* `visjesolska_visokosolska_skupaj`

2. Tabela 2: Stopnja brezposelnosti glede na stopnjo izobrazbe od leta 2017 do 2020

* `leto`
* `občina`
* `stevilo_prebivalcev`
* `brezposelnost_osnovnosolska_ali_manj`
* `brezposelnost_srednjesolska`
* `brezposelnost_visjesolska_visokosolska_skupaj`
* `brezposelnost_skupaj`

3. Tabela 3: Primerjava višine plač po občinah ter razlike v plači med moškimi in ženskami od leta 2012 do 2020

* `leto`
* `občina`
* `stevilo_studentk_na_100_studentov`
* `povprecna_neto_placa`
* `povprecna_bruto_placa`
* `razlika_v_placi`

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Potrebne knjižnice so v datoteki `lib/libraries.r`
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).
