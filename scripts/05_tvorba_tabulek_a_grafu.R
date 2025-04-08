xfun::pkg_attach2("tidyverse",
                  "gt")

tab <- read_rds("results/milesovsky_potok_krajinny_pokryv_esa_worldcover.rds")

tab <- tab |> 
  mutate(across(val_num:fraction, 
                \(x) round(x, 2))) |> 
  relocate(description,
           .after = class) |> 
  group_by(year) |> 
  select(-c(color, val_num)) |> 
  rename(třída = class,
         popis = description,
         `%` = fraction)

tabulka <- gt(tab,
              caption = "Třídy krajinného pokryvu v povodí Milešovského potoka dle ESA World Cover")

tab2 <- read_rds("results/milesovsky_potok_denni_srazky_era5-land.rds")

tab2 |> 
  filter(between(date, ymd(19510101), ymd(20241231))) |> 
  group_by(year = year(date),
           month = str_pad(as.character(month(date)), width = 2, pad = "0")) |> 
  summarize(val_num = sum(val_num)) |> 
  ggplot(aes(x = month,
             y = year)) +
  geom_raster(aes(fill = val_num)) + 
  scale_fill_distiller(palette = "BuPu",
                       direction = 1) + 
  scale_y_continuous(breaks = seq(1951, 2024, 3)) + 
  labs(fill = "úhrn [mm]",
       x = "měsíc",
       y = "rok")

tab2 |> 
  filter(between(date, ymd(19510101), ymd(20241231))) |>
  group_by(year = year(date),
           month = str_pad(as.character(month(date)), width = 2, pad = "0")) |>  
  summarize(val_num = sum(val_num)) |> 
  group_by(month) |> 
  summarize(val_num = mean(val_num) |> 
              round(1)) |> 
  ungroup() |> 
  ggplot() +
  geom_col(aes(x = month,
               y = val_num),
           col = "blue",
           fill = "blue") + 
  labs(x = "měsíc",
       y = "průměrný úhrn [mm]")

tab2 |> 
  filter(between(date, ymd(19510101), ymd(20241231))) |>
  group_by(year = year(date)) |>  
  summarize(val_num = sum(val_num)) |> 
  ggplot(aes(x = year,
             y = val_num)) +
  geom_col(col = "black",
           fill = "blue") + 
  geom_smooth(method = "lm",
              se = F,
              col = "red",
              lwd = 2) +
  scale_x_continuous(breaks = seq(1951, 2024, 5)) + 
  labs(x = "rok",
       y = "úhrn [mm]")
