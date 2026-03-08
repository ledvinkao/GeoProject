
# Redukování rastrových dat z GEE do polygonu -----------------------------

# nejprve ukážeme jak pracovat se spojitými daty
# řekněme, že budeme chtít dostat časovou řadu denních hodnot (srážky a teploty) do polygonu povodí
# využijeme přitom předpřipravený dataset (kolekci obrázků) ERA5-Land

# načteme potřebne balíčky
# konflikty funkcí ignorujeme
xfun::pkg_attach2("tidyverse", # známe
                  "sf", # pro podporu vektorových geodat v R
                  "rgee", # pro základ práce s GEE v prostředí R a RStudio
                  "tidyrgee") # pokud nejsme zvyklí na práci s rgee, ale známe tidyverse, toto pomůže prohlížet metadata kolekcí

# inicializujeme připojení k GEE
ee_Initialize(user = "ledvinka@natur.cuni.cz",
              drive = T) # někdy se vyplatí inicializovat i Google Drive, na který se mohou ukládat mezivýsledky při práci s většími datasety

# takto se odkážeme na kolekci obrázků, která je již na GEE připravena
# v tomto případě klimatology z ECMWF
col <- ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR")

# kdybychom se chtěli omezit na určité rozmezí datumů, použili bychom serverovou funkci filterDate('počáteční datum', 'konečné datum' + 1 den)
# důvodem, proč konečné datum vybíráme o jeden den větší, je fakt, že poslední datum (např. z nějakého roku) by bylo jinak vynecháno

# můžeme se podívat na nějaké parametry této kolekce
# což umožňují právě funkce balíčku tidyrgee
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
# tento polygon můžeme nahrát přes API (viz skript 01) nebo v prostředí https://code.earthengine.google.com/
# každopádně odkaz lze jednoduše kopírovat, podíváme-li se na detaily assetu s využitím prostředí 
catch <- ee$FeatureCollection("users/ledvinka/milesovsky") # každý bude mít svoji cestu podle zvoleného názvu svého povodí

# řekněme, že chceme pro povodí za každý den dostat průměrný úhrn srážek
# použijeme agregaci všech hodnot buněk, které do polygonu spadají
# k takovému účelu se používají tzv. reducery, jejichž vlastnosti le liší polde aplikované funkce a dalších argumentů

# nejprve se omezíme na pásmo s názvem 'total_precipitation_sum'
# není třeba používat mapování funkce, serverová funkce select() funguje výborně i na celou kolekci
# tato funkce select() je určena k výběru pásem, a to buď dle názvu, nebo dle pořadí
prec <- col$select("total_precipitation_sum")

# stojíme před celkem jednoduchým úkolem, protože máme jen jeden plygon - více či méně budeme reducer mapovat jen přes obrázky kolekce
# následující řádky jsou ale napsány obecně pro mapování jak přes kolekci obrázků, tak přes kolekci polygonů (může se hodit pro složitější případy, kdy nebudeme mít jen jeden polygon)
# vyhýbáme se aplikaci funkce map() na straně klienta podle příkladů dobré praxe uvedených ve vinětě na https://cran.r-project.org/web/packages/rgee/vignettes/rgee03.html
# tím se vše urychlí
data <- catch$map(\(feature) { # mapování přes polygony (ale aktuálně máme jen jeden); složené závorky vymezují funkci vevnitř cyklu
  prec$filterBounds(feature$geometry())$map( # mapování přes obrázky (kolik jich máme?; lze zjistit po aplikaci as_tidyee()); doporučuje se také u obrázku napřed omezit se na bounding box geometrie
    \(image) {
      ee$Feature( # tvoříme nový seznam, který naplňujeme jednotlivými featury na bázy původních a ty zase plníme novými poli (s redukovanými hodnotami z rastru apod.)
        feature$geometry(), # získáváme jen geometrii
        image$reduceRegion(
          reducer = ee$Reducer$mean(),
          geometry = feature$geometry(),
          scale = 11100, # pokud možno, řídíme se detailním horizontálním rozlišením (jinak mohou být aplikovány reducery nad menším rozlišením)
          maxPixels = 1e+09, # zde možná zbytečné, protože tolik buněk určitě mít v polygonu nebudeme (ale může se hodit při jiných aplikacích třeba i s navýšením)
          bestEffort = TRUE # pokud počet buněk v polygonu přesáhne určitou mez, přepne se na horší rozlišení
        )
      )$set( # nastavujeme další pole, jako je datum převzaté z vlastností obrázků
        list(
          date = image$date()$format("YYYY-MM-dd") # údaj o datumu (příp. čase) lze připojit z metadat (properties) kolekce obrázků, což se hodí dále
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
  ee$FeatureCollection$map( # ani tady nemáme více polygonů, tak by vše stačilo napsat bez mapování (ale může se hodit, když budeme mít více polygonů)
    \(x) {
      ee$Feature(NULL, x$toDictionary()) # konvertujeme do tzv. slovníku, který ovšem v R není moc využíván (spíše pracujeme se seznamy)
    }
  )

# na závěr spustíme konverzi do lokálního datasetu, což je nejdelší proces
# v takovém případě je dobré si udělat představu o stráveném čase
# výpočet také můžeme nechat běžet a k tomu si něco pustit:-); zvuková fanfára nás upozorní, že je vše připraveno pro další zpracování
tictoc::tic(); res <- ee_as_sf(data_no_geom,
                               via = "drive"); tictoc::toc(); beepr::beep(3) # varianta 'getInfo' zde nestačí, dat je příliš mnoho

# zde může aktuálně nastat chyba při přetahování výsledku do R
# tato chyba se dá obejít tím, že se podíváme do našeho Drivu a stáhneme si nejaktuálnější soubor s příponou geojson
# ten se pak dá načíst funkcí sf::read_sf()

# výsledkem je tedy simple feature collection s prázdným geometrickým sloupcem
# nám teď jde jen o zisk tabulky o dvou sloupcích - 'date' a 'total_precipitation_sum' (tento dlouhý název si ještě můžeme zkrátit)
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

# obdobně si můžeme počínat s kolecí obrázků a jeho pásmem 'temperature_2m' (nebo klidně i 'potential_evaporation_sum')
