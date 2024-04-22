library(dplyr)
library(tidyindex)
library(tsibble)
library(patchwork)
library(ozmaps)
library(rmapshaper)
library(colorspace)
load(here::here("data/idx.rda"))

dplyr::glimpse(idx)
queensland_map <- ozmaps::abs_ste %>% filter(NAME == "Queensland") %>% 
  rmapshaper::ms_simplify(keep = 0.02)
queensland_map %>%
  ggplot() +
  geom_sf(fill = "transparent", color = "grey50", linewidth = 0.4) +
  geom_point(
    data = idx %>%
      filter(.scale == 12) %>%
      filter(((year(ym) %in% c(2010, 2019) & month(ym) >= 10) )|
               ((year(ym) %in% c(2011, 2020) & month(ym) <= 3) )),
    aes(x = long, y = lat, color = .index)) +
  colorspace::scale_color_continuous_divergingx(palette = "Geyser", rev = TRUE, name = "Index") + 
  theme_void() +
  facet_wrap(vars(ym), ncol = 6)

texas <- idx %>%
  filter(name == "TEXAS POST OFFICE", .idx == "spei") %>% distinct(long, lat)
p1 <- queensland_map %>%
  ggplot() +
  geom_sf(fill = "transparent") +
  geom_point(data = queensland %>%
               distinct(id, long, lat, name),
             aes(x = long, y = lat)) +
  geom_point(data = texas, aes(x = long, y = lat),
             color = "orange", shape = 18, fill = "orange", size = 4) +  
  theme_void()


p2 <- idx %>%
  filter(name == "TEXAS POST OFFICE", .idx == "spei") %>%
  ggplot(aes(x = ym, y = .index, color = .dist, group = .dist)) +
  geom_line() +
  theme_benchmark() +
  facet_wrap(vars(.scale), labeller = label_both, ncol = 1) +
  scale_x_yearmonth(breaks = "4 year", date_labels = "%Y") +
  scale_color_brewer(palette = "Dark2", name = "Distribution") + 
  xlab("Year") + 
  ylab("Index")

(p1 | p2)  + 
  patchwork::plot_annotation(tag_levels = "a") + 
  patchwork::plot_layout(guides = "collect", widths = c(1, 4))  &
  theme(legend.position = "bottom") 


### code for generating the idx data
library(lmomco)
.scale <- c(6, 12, 24, 36)
idx <- queensland %>%
  mutate(month = lubridate::month(ym)) |>
  init(id = id, time = ym, group = month) |> 
  compute_indexes(
    spei = idx_spei(
      .tavg = tavg, .lat = lat, 
      .scale = .scale, .dist = list(dist_gev(), dist_glo())),
    spi = idx_spi(.scale = .scale)
  )
