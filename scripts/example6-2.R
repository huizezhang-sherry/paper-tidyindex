library(tidyindex)
library(dplyr)
library(patchwork)
library(tourr)
gggi %>% 
  init(id = country) %>%
  add_paras(gggi_weights, by = "variable") %>% 
  dimension_reduction(
    index_new = aggregate_linear(
      ~labour_force_participation:years_with_female_head_of_state,
      weight = weight)) 

frames <- c("001", "006", "012", "018", "024", "029") 
ani <- paste0(here::here("figures/"), "idx-tour/", "idx-tour-", frames, ".png")
rl <- lapply(ani, png::readPNG)
gl <-  lapply(rl, grid::rasterGrob)
wrap_plots(gl, ncol = 3)


## code to generate the animation frames: 
dt <- tidyindex::gggi %>% 
  dplyr::select(country: political_empowerment) %>%
  filter(between(index, 0.72, 0.729)  | index < 0.6 | index >= 0.85) %>%
  arrange(index) %>% 
  mutate(country = ifelse(country == "Iran (Islamic Republic of)", "Iran", country))
colnames(dt) <- c(colnames(dt)[1:4], "economy", "education", "health", "politics")

w_proj2idx <- function(w){w/sum(w)}
w_idx2proj <- function(w){w/(sqrt(sum(w^2))) %>% matrix(nrow = length(w))}
find_next <- function(matrix, angle = 0.4){
  start <- matrix(c(0.25, 0.25, 0.25, 0.25), ncol = 1)
  tourr:::step_angle(tourr:::geodesic_info(start, matrix), angle = angle)
}
b <- array(dim = c(4, 1, 3))
b[,,1] <- matrix(c(0.3, 0.3, 0.3, 0.1), ncol = 1) %>% find_next()
b[,,2] <- matrix(c(0.25, 0.25, 0.25, 0.25), ncol = 1) %>% w_idx2proj()
b[,,3] <- matrix(c(0.1, 0.1, 0.1, 0.7), ncol= 1) %>% find_next(angle = 0.6)

render(
  dt[,5:8], planned2_tour(b), dev = "png",
  display_idx(half_range = 2,cex = 6, label_cex = 4, col = "black", 
              panel_height_ratio = c(4,1), label_x_pos = 0.1, 
              axis_bar_lwd = c(rep(7, 3), 9), label_col = "grey70",
              axis_bar_col = c(rep("#000000", 3), "red"),
              axis_bar_label_cex = 5,
              axis_bar_label_col = c(rep("#000000", 3), "red"), 
              axis_label_cex_upper = 3, axis_label_cex_lower = 3,
              axis_var_cex = 5, 
              label = dt$country, abb_vars = FALSE),
  width = 1200, height = 2000, apf = 1/20, frames = 120,
  here::here("figures/idx-tour/idx-tour-%03d.png")
)

library(magick)
path <- list.files(here::here("figures/idx-tour"), full.names = TRUE)
frame_num <- stringr::str_extract(path, "-[0-9]+") %>% as.numeric() %>% -.
purrr::walk2(path, frame_num, ~{
  img <- image_read(.x)
  img <- image_annotate(img, glue::glue("Frame {.y}"), size = 60, location = "+920+10")
  image_write(img, path = .x)
})
gifski::gifski(
  list.files(here::here("figures/idx-tour"), full.names = TRUE),
  gif_file = here::here("figures/idx-tour.gif"),
  delay = 0.15, width = 1200, height = 2000)
