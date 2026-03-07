
# Zisk sdíleného polygonu povodí a jeho nahrání do GEE --------------------

# načteme potřebné balíčky
xfun::pkg_attach2("tidyverse",
                  "sf",
                  "googledrive",
                  "rgee")

# následující odkaz by měl být stejný pro všechny a setrvávat, dokud sdílený ZIP soubor nebude z Classroomu odstraněn
url <- "https://drive.google.com/file/d/1gvCm4pqgMQObUMdvi8TOgKbGcoySQklC/view?usp=sharing"

# vytváříme dočasný ZIP soubor, který zmizí po restartu R sezení
tmp <- tempfile(fileext = ".zip")

# plníme dočasný ZIP soubor stažením
# z něj si pak vezmeme bez rozbalení polygon Milešovského potoka
drive_download(url,
               path = tmp)

# zjistímě, jaké všechny vrstvy máme v archivu
st_layers(str_c("/vsizip/", # namísto str_c() je možné používat i funkci str_glue()
                tmp,
                "/Milesovsky_GIS")) # tohle musíme do cesty dodat, protože v souboru je ještě další složka

# a vybereme si jen polygon povodí
catch <- read_sf(str_c("/vsizip/",
                       tmp,
                       "/Milesovsky_GIS"),
                 layer = "Milesovsky_watershed")

# připojujeme se k GEE
# specifikujeme uživatele i možnost práce přes Drive
# každý bude mít svoje údaje
ee_Initialize(user = "ledvinka@natur.cuni.cz")

# nastavujeme název assetu v GEE
# adresář můžeme nastavit podle toho, co vidíme u svého profilu v https://code.earthengine.google.com/
assetID <- str_c("users/ledvinka/", 
                 'milesovsky')

# nahrajeme polygon do GEE
# a rovnou si vytváříme objekt, který na asset odkazuje
catch_ee <- sf_as_ee( # takto se vlastně ihned polygon přiřadí k objektu, ale jinak lze kopírovat cesty z assetů
  x = catch,
  overwrite = TRUE, # v případě, že vrstva se stejným názvem již existuje, ji přepíšeme
  assetId = assetID,
  via = "getInfo_to_asset") # abychom vložili polygon povodí mezi ostatní assety

# může se objevit chyba, ale nakonec se polygon nahrát podaří, o čemž se můžeme snadno přesvědčit prohlížením assetů

# nakonec můžeme nové vrstvě v GEE dodat popisek atd.
