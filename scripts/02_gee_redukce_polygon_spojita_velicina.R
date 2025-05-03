
# Redukování rastrových dat z GEE do polygonu -----------------------------

# nejprve ukážeme jak pracovat se spojitými daty
# řekněme, že budeme chtít dostat časovou řadu denních hodnot (srážky a teploty) do polygonu povodí
# využijeme přitom předpřipravený dataset ERA5-Land

# načteme potřebne balíčky
# konflikty funkcií ignorujeme
xfun::pkg_attach2("tidyverse",
                  "sf",
                  "rgee",
                  "tidyrgee")

# inicializujeme připojení k GEE
ee_Initialize(user = "ledvinka@natur.cuni.cz",
              drive = T)

col <- ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR")

# můžeme se podívat na nějaké parametry této kolekce
col_tidy <- col |> 
  as_tidyee()

# máme zde nějaká pásma se srážkami?
col_tidy$vrt |> 
  pluck("band_names", 1) |> 
  str_subset("precipitation")

# a co teplota?
col_tidy$vrt |> 
  pluck("band_names", 1) |> 
  str_subset("temperature")

# kromě toho si budeme muset vzít také náš polygon reprezentující zájmové povodí
catch <- ee$FeatureCollection("users/ledvinka/milesovsky") # každý bude mít svoji cestu

# řekněme, že chceme pro povodí za každý den dostat průměrný úhrn srážek
# použijeme agregaci všech hodnot buněk, které do polygonu spadají
# k takovému účelu se používají tzv. reducery, jejichž vlastnosti le liší polde aplikované funkce a dalších argumentů

# nejprve se omezíme na pásmo s názvem 'total_precipitation_sum'
# aby se tohle povedlo pro všechny obrázky, je vhodné postupovat přes funkci map (na straně cloudu, ne na straně klienta)
prec <- col$map(\(x)
                x$select("total_precipitation_sum"))

# stojíme před celkem jednoduchým úkolem, protože máme jen jeden plygon - více či méně budeme reducer mapovat jen přes obrázky kolekce
# následující řádky jsou ale napsány obecně pro mapování jak přes kolekci obrázků, tak přes kolekci polygonů (může se hodit pro složitější případy, kdy nebudeme mít jen jeden polygon)
# vyhýbáme se aplikaci funkce map() na straně klienta podle příkladů dobré praxe uvedených ve vinětě na https://cran.r-project.org/web/packages/rgee/vignettes/rgee03.html
# tím se vše urychlí
data <- catch$map(\(feature) { # mapování přes polygony (ale aktuálně máme jen jeden); hranaté závorky vymezují funkci vevnitř loopu
  prec$map( # mapování přes obrázky (kolik jich máme?; lze zjistit po aplikaci as_tidyee())
    \(image) {
      ee$Feature(
        feature$geometry(), # získáváme jen geometrii
        image$reduceRegion(
          reducer = ee$Reducer$mean(),
          geometry = feature$geometry(),
          scale = 11100, # pokud možno, řídíme se detailním horizontálním rozlišením (jinak mohou být aplikovány reducery nad menším rozlišením)
          maxPixels = 1e+09, # zde zbytečné, protože tolik buněk určitě mít v polygonu nebudeme (ale může se hodit při jiných aplikacích třeba i s navýšením)
          bestEffort = TRUE # pokud počet buněk v polygonu přesáhne určitou mez, přepne se na horší rozlišení
        )
      )$set(
        list(
          date = image$date()$format("YYYY-MM-dd") # je dobré si připojit údaj o datumu (příp. čase) z metadat (properties) kolekce obrázků
        )
      )$copyProperties(feature) # pro případ, že máme skutečně feature collection, připojí se užitečné atributy, ať pak víme, k jakému polygonu jaké výsledky patří
    }
  )
})$flatten() # pro zjednodušení výsledného seznamu

# dobré je nespouštět zbrkle hned všechno (může to trvat nebezpečně dlouho)
# raději se podíváme zatím jen na první případ
print(data$first()$getInfo())

# výsledkem by měla být FeatureCollection
# pokud již nepotřebujeme pro další výsledky geometrii, je vhodné ji zahodit, jinak dost zdržuje
data_no_geom <- data |> 
  ee$FeatureCollection$map( # ani tady nemáme více polygonů, tak by vše stačilo napsat bez mapování
    \(x) {
      ee$Feature(NULL, x$toDictionary()) # konvertujeme do tzv. slovníku, který ovšem v R není moc využíván (spíše pracujeme se seznamy)
    }
  )

# na závěr spustíme konverzi do lokálního datasetu, což je nejdelší proces
# v takovém případě je dobré si udělat představu o stráveném čase
# výpočet také můžeme nechat běžet a k tomu si něco pustit:-); zvuková fanfára nás upozorní, že je vše připraveno pro další zpracování
tictoc::tic(); res <- ee_as_sf(data_no_geom,
                               via = "drive"); tictoc::toc(); beepr::beep(3)

# výsledkem je tedy simple feature collection s prázdným geometrickým sloupcem (tento objekt je již v RAM)
# nám teď jde jen o zisk tabulky o dvou sloupcích - date a total_precipitation_sum (tento dlouhý název si ještě můžeme zkrátit)
res2 <- res |> 
  st_drop_geometry() |> 
  select(date,
         val_num = total_precipitation_sum)

# asi by to chtělo ještě respektovat fakt, že srážky se uvádí s přesností na desetiny milimetru
# bacha na původní jednotky, kterými jsou metry!; viz https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_DAILY_AGGR#bands
res2 <- res2 |> 
  mutate(val_num = units::set_units(val_num, "m") |> # jsme líní si sami posunout desetinnou tečku:-)
           units::set_units("mm") |> # tak pomocí funkcí balíčku units převádíme na mm
           units::drop_units() |> # zahazujeme nepotřebné jednotky
           round(1)) # a zaokrouhlujeme

# pracně získaný výsledek je vhodné uložit
res2 |> 
  write_rds("milesovsky_potok_denni_srazky_era5-land.rds",
            compress = "gz")

# obdobně si počínejte s kolecí obrázků a jeho pásmem 'temperature_2m' (nebo klidně i 'potential_evaporation_sum')
