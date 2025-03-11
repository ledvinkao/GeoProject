
# Redukování rastrových dat z GEE do polygonu - kategorické rastry --------

# někdy narazíme na kategorické rastry, jako je tomu v případě krajinného pokryvu
# tato rastrová data zasluhují speciální pozornost, neboť při jejich redukci musíme postupovat trochu jinak

# načteme potřebné balíčky
xfun::pkg_attach2("tidyverse",
                  "sf",
                  "rgee",
                  "tidyrgee")

# připojíme se
ee_Initialize(user = "ledvinka@natur.cuni.cz",
              drive = T)

# odkážeme se na polygon
catch <- ee$FeatureCollection("users/ledvinka/milesovsky")

# odkážeme se na obě kolekce (každá reprezentuje jiný rok)
# vybíráme jedný obrázek, který v kolekci vždycky je
esa1 <- ee$ImageCollection("ESA/WorldCover/v100")$first()

esa2 <- ee$ImageCollection("ESA/WorldCover/v200")$first()

# sestavíme novou kolekci z obrázků
esa <- ee$ImageCollection$fromImages(list(esa1, esa2))

# klidně se ještě podíváme na parametry pohodlným způsobem
esa_tidy <- as_tidyee(esa)

esa_tidy

# dobrá strategie je nyní přidat si ke každému elementu kolekce nové pásmo reprezentující velikosti buněk
esa <- esa$map(\(x)
               ee$Image$pixelArea()$addBands(x))

# dvojitý mapping je opět vhodný pro vektorové vrstvy s více polygony
data <- catch$map(\(feature) {
  esa$map(
    \(image) {
      ee$Feature(
        feature$geometry(),
        image$reduceRegion(
          reducer = ee$Reducer$sum()$group( # všimněte si jiného typu reduceru
            groupField = 1,
            groupName = "code"
          ),
          geometry = feature$geometry(),
          scale = 10,
          maxPixels = 1e+11,
          bestEffort = TRUE
        )
      )$set(
        list(
          year = image$get('system:index') # potřebujeme do výsledku dostat nějakou informaci o roku
        )
      )$copyProperties(feature)
    }
  )
})$flatten()

# podíváme se na nějaké info před výpočtem
print(data$first()$getInfo())

# zbavíme se geometrie
data_no_geom <- data |> 
  ee$FeatureCollection$map(
    \(x) {
      ee$Feature(NULL, x$toDictionary())
    }
  )

# a konvertujeme na lokální dataset
tictoc::tic(); res <- ee_as_sf(data_no_geom,
                               via = "drive"); tictoc::toc(); beepr::beep(3)

# nejdůležitější výsledky jsou ve sloupci groups
# ale musíme se k nim nějak dostat
# tady nám hodně pomůže tidyverse
res2 <- res |> 
  st_drop_geometry() |> 
  mutate(split = map(groups,
                     \(x) str_split(x, ",") |> 
                       unlist()) |> 
           map(\(x) map_dbl(x, 
                            \(y) parse_number(y))))

res2 <- res2 |> 
  mutate(tab = map(split,
                   \(x) tibble(class = x[c(T, F)],
                               val_num = x[c(F, T)])))

# tohle je  pro případy přidání nul za roky, kde se nějaká třída nevyskytla, ale jinak se běžně vyskytuje
out <- res2 |> 
  select(id,
         year,
         tab) |> 
  unnest(tab) |> 
  mutate(year = as.numeric(year)) |> 
  group_by(id) |> 
  nest(data = year:val_num) |> 
  mutate(data = data |> 
           map(\(x) complete(x,
                             year,
                             class))) |> 
  unnest(data) |> 
  arrange(id,
          year,
          class) |> 
  group_by(id, year)

# zde již počítáme samotná procenta
out <- out |> 
  mutate(val_num = coalesce(val_num, 0L) |> 
           units::set_units("m2") |> 
           units::set_units("ha"),
         fraction = (val_num / sum(val_num)) |> # pipe operátor má přednost, tak proto závorky
           units::set_units("%"),
         class = as.numeric(class))

# dodejme ještě nějaký popis tříd vyskytujících se v povodí
library(rvest)

url <- "https://developers.google.com/earth-engine/datasets/catalog/ESA_WorldCover_v100?hl=en"

tab <- url |> 
  read_html() |> 
  html_table() |> # výsledek je seznam
  pluck(2) # a z něj bereme jen druhý element

# k našemu výsledku můžeme tuto deskriptivní tabulku připojit
out <- out |> 
  left_join(tab,
            join_by(class == Value)) |> 
  janitor::clean_names() |> # upravíme názvy sloupců
  ungroup() |> # potřebné, pokud se chceme zbavit grupující proměnné
  select(-id) # sloupec je zbytečný, tak jej odstraníme

# a můžeme ukládat do souboru (máme vlastně i hexadecimální kódy barev pro legendu)
out |> 
  write_rds("milesovsky_potok_krajinny_pokryv_esa_worldcover.rds")

# změnilo se něco mezi lety 2020 2021 v našem povodí?
