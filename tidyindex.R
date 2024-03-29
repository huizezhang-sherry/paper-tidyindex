## ----setup, include=FALSE-----------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
options(width = 70, pillow.print_max = 5, pillar.print_min = 5)
library(tidyverse)
library(latex2exp)
library(patchwork)
library(tidyindex)
library(lmomco)
library(lubridate)
library(SPEI)
library(tsibble)
library(GGally)
library(patchwork)
library(tourr)


## ----fig-pipeline-steps-------------------------------------------------
#| fig-align: center
#| fig-cap: "Diagram of pipeline modules for index construction. The highlighted path illustrates one possible construction using the dimension reduction and simplification module."
#| out-height: 90%
#| out-width: 100%
knitr::include_graphics(here::here("figures/pipeline-overall.png"))


## ----fig-scale-var-trans-compare----------------------------------------
#| fig-cap: "Comparison of the module scaling (green) and variable transformation (orange). While both modules change the variable range, scaling maintains the same distributional shape, which is not the case with variable transformation. "
#| fig-height: 6
#| fig-width: 8
set.seed(123)
x <- rgamma(n = 1000, shape = 1, scale = 2)


trans_df <- tibble(origin = x, 
       `z-score` = (x - mean(x)/ sd(x)), 
       quadratic = x^2,
       log = log(x),
       `square root` = sqrt(x),
       `cubic root` = x^(1/3),
       minmax = (x - min(x))/ (max(x) - min(x)),
       boxcox = (x^0.5 - 1)/0.5,
       centering = x - mean(x)
       ) %>% 
  mutate(id = row_number(),
         ) %>% 
  pivot_longer(-id, names_to = "var", values_to = "value") %>% 
  mutate(category = ifelse(
    var %in% c("origin", "z-score", "minmax", "centering"),
    "Scaling", "Variable Transformation")
    )

latex_df <- tibble(
  var = c("origin", "z-score", "quadratic", "log", "square root",
      "cubic root","minmax", "boxcox", "centering"),
  latex = c(
           r"($x$)",
           r"($\frac{x - \bar{x}}{sd(x)}$)",
           r"($x^2$)",
           r"($log(x)$)",
           r"($\sqrt{x}$)",
           r"($x^{1/3}$)",
           r"($\frac{x - x_{min}}{x_{max} - x_{min}}$)",
           r"($\frac{x^{0.5} - 1}{0.5}$)",
           r"($x - \bar{x}$)"
         )
) %>% 
  left_join(trans_df %>% group_by(var) %>% summarise(max = max(value)))

trans_df %>% 
  ggplot() +
  geom_density(aes(x = value, color = category, y = after_stat(scaled))) + 
  geom_label(data = latex_df, 
             aes(x = 0.8 * max, y = 0.8, label = TeX(latex, output = "character")), 
             parse = TRUE) + 
  facet_wrap(vars(factor(var, levels = c(
      "origin", "centering", "minmax","z-score", "boxcox", "cubic root",
      "log", "quadratic", "square root"))), scales = "free") + 
  scale_color_brewer(palette = "Dark2", name = "module") + 
  theme_bw() + 
  theme(legend.position = "bottom")


## ----eval = FALSE, echo = TRUE------------------------------------------
## DATA |> module1(...) |> module2(...) |> module3(...) |> ...


## ----eval = FALSE, , echo = TRUE----------------------------------------
## dimension_reduction(V1 = aggregate_linear(...))
## dimension_reduction(V2 = aggregate_geometrical(...))
## dimension_reduction(V3 = manual_input(...))


## ----eval = FALSE, , echo = TRUE----------------------------------------
## manual_input(~x1 + x2)


## ----eval = FALSE, echo = TRUE------------------------------------------
## idx_spi <- function(...){
##   DATA |> temporal_aggregate(...) |> distribution_fit(...)|> normalise(...)
## }


## -----------------------------------------------------------------------
queensland %>% head(5)


## ----fig-spei-----------------------------------------------------------
#| fig-align: center
#| fig-cap: "Index pipeline for two drought indexes: the Standardized Precipitation Index (SPI) and the Standardized Precipitation-Evapotranspiration Index (SPEI). Both indexes share similar construction steps with SPEI having two additional steps (variable transformation and dimension reduction) to convert temperature into evapotranspiration and combine it with the precipitation series. "
#| out-height: 90%
#| out-width: 100%
knitr::include_graphics(path = here::here("figures/pipeline-spei.png"))


## ----cache=TRUE, echo = TRUE, message=FALSE, eval=FALSE-----------------
## .scale <- c(6, 12, 24, 36)
## idx <- queensland %>%
##   mutate(month = lubridate::month(ym)) |>
##   init(id = id, time = ym, group = month) |>
##   compute_indexes(
##     spei = idx_spei(
##       .tavg = tavg, .lat = lat,
##       .scale = .scale, .dist = list(dist_gev(), dist_glo())),
##     spi = idx_spi(.scale = .scale)
##   )
## 


## ----eval = FALSE-------------------------------------------------------
## save(idx, file = here::here("data/idx.rda"))


## -----------------------------------------------------------------------
load(here::here("data/idx.rda"))


## -----------------------------------------------------------------------
idx


## ----fig-compute-spatial------------------------------------------------
#| fig-cap: "Spatial distribution of Standardized Precipitation Index (SPI-12) in Queensland, Australia during two major flood and drought events: 2010/11 and 2019/20. The map shows a continuous wet period during the 2010/11 flood period and a mitigated drought situation, after its worst in 2019 December and 2020 January, likely due to the increased rainfall in February from the meteorological record."
#| fig-width: 6
#| fig-height: 3
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


## ----fig-compute-temporal-----------------------------------------------
#| fig-cap: 'Time series plot of Standardized Precipitation-Evapotranspiration Index (SPEI) at the Texas post office station (highlighted by a diamond shape in panel a). The SPEI is calculated at four time scales (6, 12, 24, and 36 months) and fitted with two distributions (Log Logistic and GEV). The dashed line at -2 represents the class "extreme drought" by the SPEI. A larger time scale gives a smoother index series, while also taking longer to recover from an extreme situation as seen in the  2019/20 drought period. The SPEI values from the two distributional fits mostly agree, while GEV can result in more extreme values, i.e. in 1998 and 2020.'
#| fig-width: 8
#| fig-height: 5
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

library(patchwork)
(p1 | p2)  + 
  patchwork::plot_annotation(tag_levels = "a") + 
  patchwork::plot_layout(guides = "collect", widths = c(1, 4))  &
  theme(legend.position = "bottom") 
  


## ----fig-pp-gggi--------------------------------------------------------
#| fig-align: center
#| fig-cap: "Index pipeline for the Global Gender Gap Index (GGGI). The index is constructed as applying the module dimension reduction twice on the data."
#| out-height: 90%
#| out-width: 100%
knitr::include_graphics(here::here("figures/pipeline-gggi.png"))


## ----tbl-gggi-weights---------------------------------------------------
#| tbl-cap: "Weights for the two applications of dimension reduction to compute the Global Gender Gap Index. V-wgt is used to compute four new variables from the original 14. These are then equally combined to get the final index value."
#| tbl-cap-location: bottom
# table cap location is not working: https://github.com/quarto-dev/quarto-cli/issues/6417
library(kableExtra)
gggi_weights %>%  
   mutate(variable = str_replace_all(variable, "_", " ") %>%
            str_to_sentence()) %>% 
  select(c(1, 5, 2, 6, 7)) %>% 
  mutate(dimension = c("Economy", rep("", 4), 
                       "Education", rep("" , 3), 
                       "Health", "", 
                       "Politics", rep("", 2)),
         dim_weight = c(0.25, rep("", 4),
                        0.25, rep("", 3),
                        0.25, "",
                        0.25, rep("", 2))) %>% 
  knitr::kable(
    digits = 3, booktabs = TRUE, 
    col.names = c("Variable", "V-wgt", "Dimension", "D-wgt", "wgt"),
    linesep = c("", "", "", "", "\\hline",
                "", "", "", "\\hline",
                "", "\\hline",
                "", "", "\\hline"),
      align = c("l", "r", "l", "r", "r")
    ) %>%
  kable_styling(font_size = 11) %>%
  row_spec(0, bold = TRUE) %>% 
  column_spec(1, width = "17.5em") %>% 
  column_spec(3, width = "3.5em") %>% 
  column_spec(c(2, 4), width = "4em") %>% 
  column_spec(5, width = "3em")


## ----eval = FALSE, echo = TRUE------------------------------------------
## gggi %>%
##   init(id = country) %>%
##   add_paras(gggi_weights, by = "variable") %>%
##   dimension_reduction(
##     index_new = aggregate_linear(
##       ~labour_force_participation:years_with_female_head_of_state,
##       weight = weight))


## ----eval = FALSE-------------------------------------------------------
## dt <- tidyindex::gggi %>%
##   dplyr::select(country: political_empowerment) %>%
##   filter(between(index, 0.72, 0.729)  | index < 0.6 | index >= 0.85) %>%
##   arrange(index) %>%
##   mutate(country = ifelse(country == "Iran (Islamic Republic of)", "Iran", country))
## colnames(dt) <- c(colnames(dt)[1:4], "economy", "education", "health", "politics")
## 
## w_proj2idx <- function(w){w/sum(w)}
## w_idx2proj <- function(w){w/(sqrt(sum(w^2))) %>% matrix(nrow = length(w))}
## find_next <- function(matrix, angle = 0.4){
##   start <- matrix(c(0.25, 0.25, 0.25, 0.25), ncol = 1)
##   tourr:::step_angle(tourr:::geodesic_info(start, matrix), angle = angle)
## }
## b <- array(dim = c(4, 1, 3))
## b[,,1] <- matrix(c(0.3, 0.3, 0.3, 0.1), ncol = 1) %>% find_next()
## b[,,2] <- matrix(c(0.25, 0.25, 0.25, 0.25), ncol = 1) %>% w_idx2proj()
## b[,,3] <- matrix(c(0.1, 0.1, 0.1, 0.7), ncol= 1) %>% find_next(angle = 0.6)
## 
## render(
##   dt[,5:8], planned2_tour(b), dev = "png",
##   display_idx(half_range = 2,cex = 6, label_cex = 4, col = "black",
##               panel_height_ratio = c(4,1), label_x_pos = 0.1,
##               axis_bar_lwd = c(rep(7, 3), 9), label_col = "grey70",
##               axis_bar_col = c(rep("#000000", 3), "red"),
##               axis_bar_label_cex = 5,
##               axis_bar_label_col = c(rep("#000000", 3), "red"),
##               axis_label_cex_upper = 3, axis_label_cex_lower = 3,
##               axis_var_cex = 5,
##               label = dt$country, abb_vars = FALSE),
##   width = 1200, height = 2000, apf = 1/20, frames = 120,
##   here::here("figures/idx-tour/idx-tour-%03d.png")
## )
## 
## library(magick)
## path <- list.files(here::here("figures/idx-tour"), full.names = TRUE)
## frame_num <- str_extract(path, "-[0-9]+") %>% as.numeric() %>% -.
## walk2(path, frame_num, ~{
##   img <- image_read(.x)
##   img <- image_annotate(img, glue::glue("Frame {.y}"), size = 60, location = "+920+10")
##   image_write(img, path = .x)
## })
## 
## # img <- image_read(here::here("figures/idx-tour/idx-tour-015.png"))
## # img <- image_annotate(img, "Frame 10", size = 40, location = "+712+10")
## # image_write(img, path = here::here("figures/idx-tour/idx-tour-014.png"))
## 
## gifski::gifski(
##   list.files(here::here("figures/idx-tour"), full.names = TRUE),
##   gif_file = here::here("figures/idx-tour.gif"),
##   delay = 0.15, width = 1200, height = 2000)
## 


## ----fig-idx-tour, fig.align='center', fig.height=6, fig.width=6--------
#| fig-cap: "Exploring the sensitivity of the GGGI, by varying the politics component's contribution, for a subset of countries. Each panel shows a dotplot of the index values, computed for the linear combination represented by the segment plots below. Frame 12 shows the actual GGGI values, and countries are sorted from highest to lowest on this. Frames 1 and 6 show the GGGI if the politics component is reduced. Frames 18, 24, 29 show the GGGI when the politics component is increased. The most notable feature is that Bangladesh's GGGI drops substantially when politics is removed, indicating that this component plays a large role in its relatively high value. Also, politics plays a substantial role in the GGGI's for the top ranked countries, because each of them drops, to the state of being similar to the middle ranked countries when the politics component's contribution is reduced. The animation can be viewed at https://vimeo.com/847874016."
frames <- c("001", "006", "012", "018", "024", "029") 
ani <- paste0(here::here("figures/"), "idx-tour/", "idx-tour-", frames, ".png")
rl <- lapply(ani, png::readPNG)
gl <-  lapply(rl, grid::rasterGrob)
wrap_plots(gl, ncol = 3)


## ----echo = TRUE, eval = FALSE------------------------------------------
## DATA |>
##   temporal_aggregate(.agg = temporal_rolling_window(prcp, scale = 24)) |>
##   distribution_fit(.fit = dist_gamma(var = ".agg", method = "lmoms",
##                                      .n_boot = 100)) |>
##   normalise(.index = norm_quantile(.fit))


## ----fig-conf-interval--------------------------------------------------
#| fig-width: 8
#| fig-height: 2
#| fig-cap: "80% and 95% confidence interval of the Standardized Precipitation Index (SPI-24) for the Texas post office station, in Queensland, Australia. A bootstrap sample of 100 is taken from the aggregated precipitation series to estimate gamma parameters and to calculate the index. The dashed line at SPI = -2 represents an extreme drought as defined by the SPI. Most parts of the confidence intervals from 2019 to 2020 sit below the extreme drought line and are relatively wide compared to other time periods. This suggests that while it is certain that the Texas post office is suffering from a drastic drought, there is considerable uncertainty in quantifying its severity, given the extremity of the event."
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


