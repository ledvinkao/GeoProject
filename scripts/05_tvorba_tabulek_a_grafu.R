
# Předpokládá se, že budeme kreslit především grafy, a to ve smyslu ggplot2.
# Tabulky zkusíme přes funkce balíčku gt.
# Proto načteme tyto základní balíčky hned na začátku najednou, což umožňuje např. funkce pkg_attach2() z balíčku xfun.
xfun::pkg_attach2("tidyverse",
                  "gt")


# Tabulka -----------------------------------------------------------------

# pro tabulku načteme dříve uložená data s krajinnými třídami v našem povodí
# při odkazech na soubory předpokládáme založený R projekt a v něm připravené složky
tab <- read_rds("results/milesovsky_potok_krajinny_pokryv_esa_worldcover.rds")

# prohlédneme
tab

# některé sloupce zřejmě nebudou zapotřebí vůbec
# některé sloupce potřebují úpravy tak, aby např. nebyla čísla uváděna na mnoho desetinných míst
tab <- tab |> 
  mutate(across(val_num:fraction, # tímto zaokrouhlíme dva sloupce najednou
                \(x) round(x, 2))) |> 
  relocate(description, # přesuneme si popis tříd
           .after = class) |> 
  group_by(year) |> # funkce pro tvorbu tabuky reagují i na grupování
  select(-c(color)) |> # odebereme zbytečný sloupec s definicemi barev
  rename(třída = class, # přejmenujeme sloupce, na popis tříd schválně zapomeneme:-)
         popis = description,
         ha = val_num, # vzpomeneme si, že jsme plochy převáděli na hektary
         `%` = fraction) # raději použijeme tzv. backticks, ptotože samotný znak % může být v R používaný jinak

# lokalizaci zřejmě nemá smysl řešit, tak si pak opravíme desetinné čárky ručně (pokud následně uložíme třeba do .rtf)
tabulka <- gt(tab,
              caption = "Třídy krajinného pokryvu v povodí Milešovského potoka dle ESA World Cover",
              row_group_as_column = T)

# tabulku lze i prohlížet
tabulka

# vypadá to, že nejlepší je asi .html formát, ale my zřejmě budeme dál pracovat s formáty, které lze otevírat v MS Word
# proveďte pro všechna svoje povodí tak, aby do reportu mohly být vloženy všechny tabulky
tabulka |> 
  gtsave("results/milesovsky_potok_krajinne_tridy.rtf")


# Grafy -------------------------------------------------------------------

# načteme podkladovou tabulku s časovými řadami srážek
# a asi se rovnou omezíme na celé kalendářní roky
tab2 <- read_rds("results/milesovsky_potok_denni_srazky_era5-land.rds") |> 
  filter(between(date, ymd(19510101), ymd(20241231)))

# 1) příklad heatmapy ukazující měsíční úhrny za celé vybrané období
heatmapa <- tab2 |> 
  group_by(year = year(date), # grupující proměnné můžeme získat rovnou ve funkci group_by()
           month = str_pad(as.character(month(date)), width = 2, pad = "0")) |> # tady si ještě strategicky přidáváme vodící nulu, abychom dostali správné pořadí měsíců
  summarize(val_num = sum(val_num)) |> # sumarizujeme, aplikujeme sumu na srážky
  ggplot(aes(x = month, # začínáme kreslit a skáčeme z |> na + (a nastavujeme mapping pro osy)
             y = year)) +
  geom_raster(aes(fill = val_num)) + # zde nastavujeme výplň (jde do legendy)
  scale_fill_distiller(palette = "BuPu", # a tímto měníme paletu barev pro spojitou veličinu (přechod od modré k fialové)
                       direction = 1) + # nastavujeme opačný směr barev od původního, kterým je direction = -1
  scale_y_continuous(breaks = seq(1951, 2024, 3)) + # hrajeme si trochu také s rozestupy let na ose y
  labs(fill = "úhrn [mm]", # popisujeme výplň (jde do legendy)
       x = "měsíc", # osu x
       y = "rok")

# protože jsme si graf uložili do objektu heatmapa, lze jej zobrazit v panelu Plots pouhým enterováním názvu objektu
heatmapa

# ukládání grafů do objektů nám dopomůže také k ukládání do souborů - prostudujte funkci ggsave() a její argumenty; pro report v MS Word zřejmě využijeme formát PNG

# 2) příklad sloupcového grafu s dlouhodobými (průměrnými) měsíčními úhrny za období 1951-2024
longterm <- tab2 |> 
  group_by(year = year(date),
           month = str_pad(as.character(month(date)), width = 2, pad = "0")) |>  
  summarize(val_num = sum(val_num)) |> 
  group_by(month) |> # zde si musíme po prvním sumarizování přegrupovat
  summarize(val_num = mean(val_num) |> 
              round(1)) |> # rovnou zaokrouhlujeme na symsluplný počet desetinných míst
  ggplot() +
  geom_col(aes(x = month, # pokud u různých geometrií nechceme dědit obecný mapping, můžeme mapping nastavit právě až ve funkci určující geometrii
               y = val_num),
           col = "blue", # col je pro okraj sloupce
           fill = "blue") + # fill je pro výplň sloupce
  labs(x = "měsíc",
       y = "průměrný úhrn [mm]")

longterm

# 3) příklad sloupcového grafu pro roční úhrny
annual <- tab2 |> 
  group_by(year = year(date)) |>  
  summarize(val_num = sum(val_num)) |> 
  ggplot(aes(x = year,
             y = val_num)) +
  geom_col(col = "black",
           fill = "blue") + 
  geom_smooth(method = "lm", # přidáváme ještě vyrovnávací křivku (zde přímku danou linárním modelem - funkcí lm())
              se = F, # když nechceme zobrazit pás daný standardní chybou kolem přímky
              col = "red", # barva čáry
              lwd = 2) + # tloušťka čáry
  scale_x_continuous(breaks = seq(1951, 2024, 5)) + # nastavujeme ještě lepší rozestupy let na ose x)
  coord_cartesian(ylim = c(300, 850)) + # takto se můžeme ještě zaměřit na část osy y, která je více zajímavá z hlediska interpretace (nemusíme začínat od hodnoty 0 mm)
  labs(x = "rok",
       y = "úhrn [mm]")

annual

# zkuste také zjistit, zdali je směrnice přímky statisticky významná a v reportu okomentujte

# 4) krabicové grafy pro jednotlivé roky za účelem zjištění, zda se nějak mění variabilita
boxplots <- tab2 |> 
  mutate(year = year(date)) |> # proměnnou s rokem přidáváme přímo do tabulky
  ggplot() + 
  geom_boxplot(aes(val_num, 
                   as.character(year))) + # potřebujeme kategorie a spojitou veličinu, dle nápovědy funkce geom_boxplot() je orientace krabicového grafu závislá na pořadí proměnných
  scale_x_sqrt() + # abychom lépe viděli rozdíly, osu x zobrazíme ve vhodnějším měřítku (výhodou je, že s veličinou zůstáváme u stejných jednotek)
  labs(y = "rok",
       x = "úhrn [mm]")

boxplots

# 5) Graf indexu SPI (Standardized Precipitation Index)

# indexy tohoto typu nejčastěji vycházejí z řad měsíčních hodnot
# raději si tedy vytvořme novou tabulku s měsíčními úhrny
tab3 <- tab2 |> 
  group_by(year = year(date),
           month = month(date)) |> 
  summarize(val_num = sum(val_num))

# existují nějaké nulové hodnoty ve výsledné tabulce?
tab3 |> 
  filter(val_num == 0)

# můžeme se přesvědčit i pomocí funkce near(), která umožnuje i velmi malé odchylky
tab3 |> 
  filter(near(val_num, 0))

# k výpočtům řad těchto indexů může dopomoci např. balíček SCI
# tak jej načtěme
library(SCI)

# dle nápovědy k funkci fitSCI najděme potřebné parametry
spi.12.para <- fitSCI(tab3$val_num, # takto vytáhneme sloupec tabulky jako vektor (alternativně můžeme využít i funkci pull())
                      first.mon = 1,
                      time.scale = 12, # jako časové okno zvolíme 12 měsíců, abychom se vyhnuli vlivu sezonnosti
                      distr = "gamma", # jako rozdělení podle kterého pak transformujeme na Gaussovo rozdělení vezmeme rodělení gama
                      p0 = TRUE) # sice žádné nulové hodnoty nejsou, ale kdyby se náhodou v nějaké jiné tabulce vyskytly, je dobré modelovat i výskyt nul

# tímto se dostaneme do měřítek Gaussova rozdělení, a tedy i indexu SPI
spi.12 <- transformSCI(tab3$val_num,
                       first.mon = 1,
                       obj = spi.12.para)

# vytvořme si tabulku pro kreslení ve smyslu ggplot2
spi.12.tab <- tibble(date = seq(ymd(19510101), ymd(20241201), "month"),
                     val_num = spi.12)

# nyní můžeme kreslit
ggplot(data = spi.12.tab) + 
  geom_line(aes(x = date,
                y = val_num)) + 
  geom_hline(yintercept = 0,
             lwd = 1.5) +
  geom_area(aes(x = date,
                ymin = val_num - 1,
                ymax = val_num + 1))



spi.12
matplot(seq(ymd(19510101), ymd(20241201), "month"),
        cbind(spi.12),
        t = "l",
        lty = 1,
        col = c("red", "blue"),
        lwd=c(1, 2))
