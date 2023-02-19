Keskiarvosanat ja S2-oppilaat sekä korkeakoulutustaso
================
2023-02-19

### Johdanto

Tämä dokumentti käsittelee Ylen artikkelia, joka käsitteli koulujen
päättäneiden keskiarvosanoja ja alueen mediaanituloja,
korkeakoulutustasoa, työllisyysastetta sekä S2-oppilaiden osuutta
(<https://yle.fi/a/74-20016772>). Tässä dokumentissa pyritään
selvittämään näiden ja muutaman muun taustamuuttujan vaikutus koulujen
keskiarvosanoihin.

### Huomautus datasta

Yli 60% kouluista ei ollut keskiarvosanaa datassa, eikä puuttumisen
syytä tiedetä, joten mallin johtopäätökset eivät välttämättä pidä
paikkansa.

``` r
mean(is.na(data$keskiarvo))
```

    ## [1] 0.6233583

### Datan esikäsittely

Valitaan käytettävät sarakkeet, ja poistetaan datasta puuttuvat arvot.

``` r
sarakkeet = c("oppilaitostyyppi_nimi", "oppilaitosnumero", "postinumeroalue", "kunta", "asukkaat_yht", "koulutus_kantaluku", "koulutus_alempi_kork", "koulutus_ylempi_kork", "asukkaiden_mediaanitulo", "oppilaita_yhteensa", "kieli", "tyollisyys_18_74", "suomi_toisena_kielena_ja_kirjallisuus", "keskiarvo")

data = data %>% dplyr::select(all_of(sarakkeet)) %>% drop_na()
```

Min max skaalataan sarakkeet välille \[0,1\] (poislukien keskiarvot
tulkitsemisen helpottamiseksi), ja lasketaan työllisyysaste,
korkeakoulutusaste ja S2-oppilaiden osuus.

``` r
range01 <- function(x){(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}
data_scaled = as_tibble(sapply(data[,-c(1,14)], range01))
data_scaled$oppilaitosnimi = data$oppilaitostyyppi_nimi
data_scaled$keskiarvo = data$keskiarvo
data_scaled$tyollisyysaste = (data$tyollisyys_18_74 / data$asukkaat_yht)
data_scaled$korkeakouluaste = (data$koulutus_alempi_kork + data$koulutus_ylempi_kork) / data$koulutus_kantaluku
data_scaled$s2_osuus = data$suomi_toisena_kielena_ja_kirjallisuus / data$oppilaita_yhteensa
data_scaled[is.na(data_scaled)] = 0
```

### Malli ja tulokset

Tehdään robusti lineaarinen malli.

``` r
model1 = rlm(keskiarvo ~ tyollisyysaste + s2_osuus + korkeakouluaste + oppilaitosnimi + postinumeroalue+ asukkaat_yht + asukkaiden_mediaanitulo + oppilaita_yhteensa, data = data_scaled)
summary(model1)
```

    ## 
    ## Call: rlm(formula = keskiarvo ~ tyollisyysaste + s2_osuus + korkeakouluaste + 
    ##     oppilaitosnimi + postinumeroalue + asukkaat_yht + asukkaiden_mediaanitulo + 
    ##     oppilaita_yhteensa, data = data_scaled)
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.80307 -0.17528 -0.01038  0.16731  1.61104 
    ## 
    ## Coefficients:
    ##                                              Value    Std. Error t value 
    ## (Intercept)                                    7.9491   0.2333    34.0784
    ## tyollisyysaste                                 0.0200   0.2514     0.0797
    ## s2_osuus                                      -0.4009   0.1097    -3.6548
    ## korkeakouluaste                                1.8756   0.1926     9.7375
    ## oppilaitosnimiPeruskouluasteen erityiskoulut  -1.1206   0.0849   -13.2022
    ## oppilaitosnimiPeruskoulut                     -0.2220   0.0613    -3.6245
    ## postinumeroalue                               -0.0030   0.0453    -0.0659
    ## asukkaat_yht                                  -0.1261   0.0842    -1.4980
    ## asukkaiden_mediaanitulo                       -0.1951   0.2071    -0.9417
    ## oppilaita_yhteensa                            -0.0212   0.0825    -0.2572
    ## 
    ## Residual standard error: 0.2547 on 606 degrees of freedom

Keskiarvosana kouluilla on 7.9. Tilastollisesti merkitsevät muuttujat
ovat laskevassa järjestyksessä:

1.  Oppilaitostyyppi (Peruskoulut ja lukio (vertailutaso), Peruskoulut,
    peruskouluasteen erityiskoulut). Verrattuna peruskouluihin ja
    lukioihin erityiskoulut saavat keskimäärin 1.1 arvosanaa heikompia
    arvosanoja, ja peruskoulut saavat 0.2 arvosanaa heikompia arvosanoja
    kuin peruskoulut ja lukiot.

2.  Korkeakoulutusaste. Korkeakoulutusasteella on erittäin merkittävä
    vaikutus koulun keskiarvosanoihin. Alueilla, jolla kaikki ovat
    korkeakoulutettuja saadaan 1.9 arvosanaa parempia arvosanoja kuin
    alueilla jossa kukaan ei ole korkeasti koulutettu.

3.  S2-oppilaiden osuus. Alueilla, jossa koulussa on pelkästään
    S2-oppilaita saadaan keskimäärin 0.4 arvosanaa heikompia arvosanoja
    kuin alueilla, jossa ei ole yhtään S2-oppilasta.

Loput taustamuuttujista eivät olleet tilastollisesti merkityksellisiä.

### Mallin diagnostiikkaa

``` r
par(mfrow = c(2,2))
plot(model1)
```

![](s2mass_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Residuaalit poikkeavat hieman normaalista jakauman hännillä. Muuten
diagnostiikka näyttää hyvältä.

``` r
vif(model1)
```

    ##                             GVIF Df GVIF^(1/(2*Df))
    ## tyollisyysaste          1.374257  1        1.172287
    ## s2_osuus                1.429622  1        1.195668
    ## korkeakouluaste         3.377472  1        1.837790
    ## oppilaitosnimi          1.583023  2        1.121687
    ## postinumeroalue         1.489101  1        1.220287
    ## asukkaat_yht            1.548416  1        1.244354
    ## asukkaiden_mediaanitulo 2.757717  1        1.660637
    ## oppilaita_yhteensa      1.485490  1        1.218807

Selittävät muuttujat eivät ole liian riippuvia toisistaan.
