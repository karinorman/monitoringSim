library(dplyr)
library(terra)
library(viridis)
library(tidyterra)
library(ggthemes)

load(here::here("data/samplesize_sim.rda"))
load(here::here("data/strata_sim_points.rda"))
load(here::here("data/qc_cand_points.rda"))
load(here::here("data/qc.rda"))

# get data to play nice with each other
qc_proj <- sf::st_transform(qc, crs = 5070)

ndvi <- rast(here::here("data/env_layers/silvisNDVI16_cumulative_2003-01-01.tif")) %>%
  crop(qc_proj, mask = TRUE)

points <- vect(qc_cand_points)

ecoreg <- vect(here::here("data/wwf_ecoregions/wwf_terr_ecos.shp")) %>%
  terra::project(qc_proj) %>%
  crop(qc_proj)

# unfortunately it gotta be this way, can't get terra::intersect to work
ecoreg_boundary <- sf::st_intersection(sf::st_as_sf(ecoreg %>% select(ECO_NAME)), 
                                       sf::st_as_sf(qc_proj %>% select(NAME_0))) %>%
  vect() 

#### Create figure that shows uequal prob and stratified  for some example algorithms ####

# get dataframe of example designs, one each for each algorithm we'll visualize
ex_designs <- samplesize_sim %>% 
  mutate(type = "equal") %>%
  filter(algorithm %in% c("cube", "lpm1", "scps"), rep == 67, sample_size == 110) %>%
  bind_rows(strata_sim_points %>%
              select(all_of(colnames(samplesize_sim))) %>%
              filter(algorithm %in% c("cube", "lpm1_uneq", "scps_uneq"), rep == 67, sample_size == 110) %>%
              mutate(type = "stratified",
                     algorithm = case_when(
                       algorithm == "lpm1_uneq" ~ "lpm1",
                       algorithm == "scps_uneq" ~ "scps",
                       .default = algorithm
                     )))


plot_design <- function(algorithm_name, sim_type){
  
  ex_design_pts <- ex_designs %>% filter(algorithm == algorithm_name, type == sim_type) %>% pull(id)
  
  if(sim_type == "equal"){  
    design_plot <- ggplot() +
      geom_spatraster(data = ndvi) +
      scale_fill_continuous(type = "viridis", na.value = "transparent") +
      geom_spatvector(data = points %>% filter(id %in% ex_design_pts), fill = "transparent", colour = "black") +
      theme_void() +
      theme(legend.position = "none")
  } else{
    design_plot <- ggplot() +
      geom_spatvector(data = ecoreg_boundary, color = "darkgrey", fill = "white") +  
      geom_spatvector(data = ecoreg_boundary %>% aggregate(), color = "black", fill = "transparent") + 
      geom_spatvector(data = points %>% filter(id %in% ex_design_pts), fill = "transparent", colour = "black") +
      theme_void() +
      theme(legend.position = "none")
  }
  
  return(design_plot)
}


plot_df <- tibble(algorithm_name = rep(c("cube", "lpm1", "scps"), 2),
                  sim_type = rep(c("equal", "stratified"), each = 3))  

plot_list <- purrr::pmap(plot_df, plot_design)

map_plot <- cowplot::plot_grid(plotlist = plot_list, nrow = 2, labels = "AUTO")

save_plot(here::here("figures/map_plot.jpeg"), plot = map_plot,nrow =2, ncol = 2)
