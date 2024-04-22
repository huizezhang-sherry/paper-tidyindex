library(tidyindex)
library(dplyr)
library(ggplot2)
library(lmomco)
library(tsibble)
res <- queensland %>% 
  filter(name == "TEXAS POST OFFICE") %>% 
  mutate(month = lubridate::month(ym)) |>
  init(id = id, time = ym, group = month) |> 
  temporal_aggregate(.agg = temporal_rolling_window(prcp, scale = 24)) |> 
  distribution_fit(.fit = dist_gamma(var = ".agg", method = "lmoms", .n_boot = 100)) |>
  tidyindex::normalise(.index = norm_quantile(.fit))

res_band <- res$data %>% 
  group_by(ym, id, name) %>% 
  summarise(
    q025 = quantile(.index, 0.025),
    q10 = quantile(.index, 0.1),
    q50 = quantile(.index, 0.5),
    q90 = quantile(.index, 0.9),
    q975 = quantile(.index, 0.975)
  ) %>% 
  ungroup()


# the sampling uncertainty is quite large here
# and this is mainly due to we only have 30 year worth of data
# each month fit has only 30 points
res_band %>% 
  ggplot(aes(x = ym)) + 
  geom_hline(yintercept = -2, linetype = "dashed", color = "grey50") + 
  #geom_text(aes(x = loc, y = -2.7, label = "SPI = -2"), color = "grey50") + 
  geom_ribbon(aes(ymin = q025, ymax = q975), fill = "steelblue1", alpha = 0.5) + 
  geom_ribbon(aes(ymin = q10, ymax = q90), fill = "steelblue4", alpha = 0.5) + 
  geom_line(aes(y = q50)) + 
  scale_x_yearmonth(name = "Year", date_break = "2 years", date_label = "%Y") +
  theme_bw() + 
  facet_wrap(vars(name), ncol = 1) + 
  theme(panel.grid = element_blank(), 
        legend.position = "bottom") + 
  ylab("SPI")
