#
# Get World Map Data
#
#

get_pew_world <- function() {
  
  # this ensures you only download the shapefile once and hides
  # errors and warnings. remove `try` and `invisible` to see messages
  try(invisible(httr::GET("http://www.pewglobal.org/wp-content/lib/js/world-geo.json",
                          httr::write_disk("data/world-geo.json"))), silent=TRUE)
  
  # use ogrListLayers("world-geo.json") to see file type & 
  # layer info to use in the call to readOGR
  #ogrListLayers("world-geo.json")
  world <- rgdal::readOGR("data/world-geo.json")
  world_wt <- rgdal::spTransform(world, CRS("+proj=robin"))
  world_map <- ggplot2::fortify(world_wt)
  
  world_map <- world_map %>%
    dplyr::left_join(data_frame(id = rownames(world@data), name = world@data$name)) %>%
    dplyr::select(-id) %>%
    dplyr::rename(id = name) %>% 
    dplyr::mutate(id_new = countrycode::countrycode(id, "country.name", "country.name")) %>% 
    dplyr::mutate(id = ifelse(is.na(id_new), id, id_new)) %>% 
    dplyr::select(-id_new)
  
  tidytemplate::save_it(world_map)
  
}


## Example

# world_map %>% 
#   ggplot() +
#   geom_map(map = world_map,
#            aes(x = long, y = lat, group = group, map_id = id),
#            color = "#7f7f7f", fill = "gray80", size = 0.15) +
#   geom_map(data = map_rif, 
#            map = world_map,
#            aes(map_id  = id, 
#                fill = `Rules in Form - Top-Down/Bottom-Up`),
#            color = "black", size = 0.01) + 
#   theme_map() +
#   coord_equal() 