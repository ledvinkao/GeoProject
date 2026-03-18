
# Redukování rastrových dat z GEE do polygonu -----------------------------

# nejprve ukážeme jak pracovat se spojitými daty
# řekněme, že budeme chtít dostat časovou řadu denních hodnot (např. srážky) do polygonu povodí
# využijeme přitom předpřipravený dataset (kolekci obrázků) ERA5-Land

# načteme potřebne balíčky
# konflikty funkcí ignorujeme
xfun::pkg_attach2("tidyverse", # známe
                  "sf", # pro podporu vektorových geodat v R
                  "rgee", # pro základ práce s GEE v prostředí R a RStudio
                  "tidyrgee") # pokud nejsme zvyklí na práci s rgee, ale známe tidyverse, toto pomůže prohlížet metadata kolekcí

# inicializujeme připojení k GEE
ee_Initialize(user = "ledvinka@natur.cuni.cz",
              drive = T) # někdy se vyplatí inicializovat i Google Drive, přes který se mohou zpracovávat mezivýsledky při práci s většími datasety

# takto se odkážeme na kolekci obrázků, která je již na GEE připravena
# v tomto případě klimatology z ECMWF
# schválně vybereme kolekci s hodinovými daty, ať si můžeme vypočítat úhrny mezi 07:00 CET a 07:00 CET následujícího dne, na což jsme v Česku zvyklí
# na GEE najdeme i kolekci s denními agregacemi, ale zde jsou hodnoty agregovány jinak, než chceme
era <- ee$ImageCollection("ECMWF/ERA5_LAND/HOURLY")

# když se chceme omezit na určité rozmezí datumů (např. dnů), lze použít serverovou funkci filterDate(), např. filterDate('počáteční datum', 'konečné datum' + 1 den)
# důvodem, proč konečné datum vybíráme o jeden den větší, je fakt, že poslední datum (např. z nějakého roku) by bylo jinak vynecháno (vyběry jsou tzv. exkluzivní, pokud jde o horní meze)

# můžeme se podívat na nějaké parametry této kolekce
# což umožňují právě funkce balíčku tidyrgee
# raději se přitom omezíme jen na několik dnů

era_tidy <- era$filterDate("2025-12-31", "2026-01-02") |> 
  as_tidyee()

# prohlédneme, jak takový uklizený objekt vypadá  
era_tidy

# máme zde nějaká pásma se srážkami?
era_tidy$vrt |> 
  pluck("band_names", 1) |> 
  str_subset("precipitation")

# a co teplota?
era_tidy$vrt |> 
  pluck("band_names", 1) |> 
  str_subset("temperature")

# kromě toho si budeme muset vzít také náš polygon reprezentující zájmové povodí
# tento polygon můžeme nahrát přes API (viz skript 01) nebo v prostředí https://code.earthengine.google.com/
# každopádně odkaz lze jednoduše kopírovat, podíváme-li se na detaily assetu s využitím prostředí
# existují i možnosti sdílení si vlastních assetů navzájem
catch <- ee$FeatureCollection("users/ledvinka/milesovsky") # každý bude mít svoji cestu podle zvoleného názvu nebo identifikátoru svého povodí

# řekněme, že chceme pro území povodí za každý den dostat průměrný úhrn srážek
# použijeme agregaci všech hodnot buněk, které do polygonu spadají
# k takovému účelu se používají tzv. reducery, jejichž vlastnosti se liší podle aplikované funkce a dalších argumentů
# než ale budeme moci aplikovat prostorový reducer, musíme napřed redukovat (agregovat) hodinové úhrny srážek v čase
prec <- era$
  filterBounds(catch$geometry())$ # v příkladech dobré praxe rgee, uvedených ve vinětě na https://cran.r-project.org/web/packages/rgee/vignettes/rgee03.html, je doporučováno omezit se na zájmový bounding box
  select("total_precipitation_hourly") # serverovou funkcí select() vybíráme pásma, zde hodinové desagregace srážkových úhrnů

# tato funkce select() je určena k výběru pásem, a to buď dle názvu, nebo dle pořadí
# přitom není třeba používat mapování funkce, serverová funkce select() funguje výborně i na celou kolekci

# vytvořme si napřed také sekvenci datumů s hodinami (tady s časovou zónou UTC)
datetimes <- seq(ymd_hms("1950-01-01T07:00:00"), # časy vybíráme tak, abychom se trefili do součtů srážek mezi 07:00 CET a 07:00 CET následujícího dne
                  ymd_hms("2025-12-31T07:00:00"), # dejme tomu, že jako poslední den chceme 2025-12-21
                  "day") |> 
  format("%Y-%m-%dT%H:%M:%S") |> # sekvenci vytvořenou známým způsobem převádíme na text
  as.list() |> # tento vektor pak na seznam řetězců (funkce klienta)
  ee$List() # a pak ještě jednou na seznam řetězců (funkce serveru)

# nyní můžeme redukovat v čase
# aplikujeme přitom mapovací funkci serveru
# anonymní funkci doporučují principy dobré praxe uzavřít do funkce ee_utils_pyfunc(), bez které bychom stejně nepokročili
prec_daily <- datetimes$map(ee_utils_pyfunc(\(x) { # složené závorky vymezují funkci vevnitř cyklu
  prec$
    filterDate(x, ee$Date(x)$advance(1, "day"))$ # vybíráme časová rozmezí s využitím serverové funkce advance(); aby ta byla aplikovatelná, převádíme nejprve text na datum
    sum() # určujeme, co se bude dít s obrázky kolekce, které spadají do časového rozmezí
}))

# výsledkem je seznam, který ještě můžeme převést na redukovanou kolekci obrázků
prec_daily <- ee$ImageCollection(prec_daily)

# jsme ve stavu, kdy na jedné straně máme kolekci obrázků a nadruhé straně kolekci polygonů
# povodí je skutečně kolekce, i když je obsažen pouze jeden polygon
class(prec_daily)

class(catch)

# jsme tedy připraveni na prostorové redukování
# stojíme před celkem jednoduchým úkolem, protože máme jen jeden plygon - více či méně budeme reducer mapovat jen přes obrázky kolekce
# následující řádky jsou ale napsány obecně pro mapování jak přes kolekci obrázků, tak přes kolekci polygonů (může se hodit pro složitější případy, kdy nebudeme mít jen jeden polygon)
# vyhýbáme se aplikaci funkce map() na straně klienta podle příkladů dobré praxe
# tím se vše urychlí
data <- catch$map(\(feature) { # mapování přes polygony (ale aktuálně máme jen jeden)
  prec_daily$filterBounds(feature$geometry())$map( # mapování přes obrázky; doporučuje se také u obrázku napřed omezit se na bounding box geometrie, pokud jsme tak ještě neučinili
    \(image) {
      ee$Feature( # tvoříme nový seznam, který naplňujeme jednotlivými featury na bázi původních a ty zase plníme novými poli (s redukovanými hodnotami z rastru apod.)
        feature$geometry(), # získáváme jen potřebnou geometrii
        image$reduceRegion(
          reducer = ee$Reducer$mean(),
          geometry = feature$geometry(),
          scale = 11100, # pokud možno, řídíme se detailním horizontálním rozlišením (jinak mohou být aplikovány reducery nad horším rozlišením)
          maxPixels = 1e+09, # zde možná zbytečné, protože tolik buněk určitě mít v polygonu nebudeme (ale může se hodit při jiných aplikacích třeba i s navýšením hodnoty)
          bestEffort = TRUE # pokud počet buněk v polygonu přesáhne určitou mez, přepne se na horší rozlišení
        )
      )$copyProperties(feature) # pro případ, že máme skutečně feature collection s více řádky, připojí se užitečné atributy, ať pak víme, k jakému polygonu jaké výsledky patří
    }
  )
})$flatten() # pro zjednodušení výsledného seznamu

# vlastnosti obrázů v tomto případě nelze využít, protože jsme je redukcí v čase ztratili

# výsledkem by měla být FeatureCollection
# pokud již nepotřebujeme pro další výsledky geometrii, je vhodné ji zahodit, jinak dost zdržuje
data_no_geom <- data |> 
  ee$FeatureCollection$map( # ani tady nemáme více polygonů, tak by vše stačilo napsat bez mapování (ale může se hodit, když budeme mít více polygonů)
    \(x) {
      ee$Feature(NULL, x$toDictionary()) # konvertujeme do tzv. slovníku, který ovšem v R není moc využíván (spíše pracujeme se seznamy, nebo pojmenovanými seznamy)
    }
  )

# na závěr spustíme konverzi do lokálního datasetu, což je nejdelší proces
# v takovém případě je dobré si udělat představu o stráveném čase
# výpočet také můžeme nechat běžet a k tomu si něco pustit:-); zvuková fanfára nás upozorní, že je vše připraveno pro další zpracování
res <- ee_as_sf(data_no_geom,
                via = "drive"); beepr::beep(3) # varianta 'getInfo' zde nestačí, dat je příliš mnoho

# zde může aktuálně nastat chyba při přetahování výsledku do R
# tato chyba se dá obejít tím, že se podíváme do našeho Drivu a stáhneme si nejaktuálnější soubor s příponou geojson
# ten se pak dá načíst funkcí sf::read_sf()

# výsledkem je tedy simple feature collection s prázdným geometrickým sloupcem
# nám teď jde jen o zisk tabulky s jedním sloupcem, a to 'total_precipitation_hourly' (tento dlouhý název si ještě můžeme zkrátit)
# druhý důležitý sloupec s datumem musíme přidat sami
res2 <- res |> 
  st_drop_geometry() |> 
  select(val_num = total_precipitation_hourly) |> 
  mutate(date = seq(ymd(19500101),
                    ymd(20251231),
                    "day"),
         .before = val_num)

# asi by to chtělo ještě respektovat fakt, že srážky se uvádí s přesností na desetiny milimetru
# bacha na původní jednotky, kterými jsou metry!; viz https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_HOURLY#bands
res2 <- res2 |> 
  mutate(val_num = units::set_units(val_num, "m") |> # jsme líní si sami posunout desetinnou tečku:-)
           units::set_units("mm") |> # tak pomocí funkcí balíčku units převádíme na mm
           units::drop_units() |> # zahazujeme nepotřebné jednotky
           round(1)) # a zaokrouhlujeme

# pracně získaný výsledek je vhodné uložit
res2 |> 
  write_rds("milesovsky_potok_denni_srazky_era5-land.rds",
            compress = "gz")
