
# Zisk sdíleného polygonu povodí a jeho nahrání do GEE --------------------

# načteme potřebné balíčky
xfun::pkg_attach2("tidyverse",
                  "sf",
                  "googledrive",
                  "rgee")

# následující odkaz by měl být stejný pro všechny a setrvávat, dokud sdílený ZIP soubor v Classroomu neodstraní
url <- "https://drive.google.com/file/d/1gvCm4pqgMQObUMdvi8TOgKbGcoySQklC/view?usp=sharing"

# vytváříme dočasný ZIP soubor, který zmizí po restartu R sezení
tmp <- tempfile(fileext = ".zip")

# plníme dočasný ZIP soubor stažením; z něj si pak vezmeme bez rozbalení polygon Milešovského potoka
drive_download(url,
               path = tmp)

# zjistímě, jaké všechny vrstvy máme v archivu
st_layers(str_c("/vsizip/",
                tmp,
                "/Milesovsky_GIS")) # bohužel tohle musíme do cesty dodat, protože v souboru je ještě další složka

# a vybereme si jen polygon povodí
catch <- read_sf(str_c("/vsizip/",
                       tmp,
                       "/Milesovsky_GIS"),
                 layer = "Milesovsky_watershed")

# připojujeme se k GEE
# specifikujeme uživatele i možnost práce přes Drive
# každý bude mít svoje údaje
ee_Initialize(user = "ledvinka@natur.cuni.cz",
              drive = T)

# nastavujeme název assetu v GEE
# inspirace nápovědou k funkci sf_as_ee()
assetID <- sprintf("%s/%s", 
                   ee_get_assethome(), 
                   'milesovsky')

# nahrajeme polygon do GEE
# a rovnou si vytváříme objekt, který na asset odkazuje
catch_ee <- sf_as_ee(
  x = catch,
  overwrite = TRUE,
  assetId = assetID,
  via = "getInfo_to_asset")
