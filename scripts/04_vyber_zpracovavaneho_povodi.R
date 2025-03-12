
# Výběr zpracovávaného povodí ---------------------------------------------

# v případě rds souboru nemusíme stahovat soubor vůbec (tedy varianta bez zakládání dočasného ZIP souboru)

# načteme potřebné balíčky
xfun::pkg_attach2("tidyverse",
                  "sf", # musí být, jinak soubor neotevřeme
                  "googledrive") 

url <- "https://drive.google.com/file/d/1NRJ7QyPuRvBtLBTwG-CSUgvN34BxPmvP/view?usp=sharing"

vse <- drive_read_raw(url) |> 
  rawConnection() |> 
  gzcon() |> # protože soubor je komprimovaný
  readRDS() # nejde o funkci read_rds() ale readRDS()!

moje <- vse |> 
  filter(id == "054800")

# podívám se, jak povodí vypadá, vezmu si na pomoc třeba OpenStreetMap
# díky tomu můžeme i jednoduše zjistit, o jaký tok jde (id značí vodoměrnou stanici, která povodí uzavírá)
mapview::mapview(moje)

# bližsí informace o stanici lze nalézt v metadatech k opendatům zde: https://opendata.chmi.cz/hydrology/historical/metadata/meta1.json

# jakmile máme vybrané povodí, můžeme s nahráváním do GEE postupovat podobně jako ve skriptu 01
# rozdílný CRS by neměl vadit, vše se stejně transformuje na EPSG:4326
