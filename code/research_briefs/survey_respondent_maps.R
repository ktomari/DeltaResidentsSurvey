library(tidyverse)
library(ggplot2)
library(ggnewscale)
library(ggrepel)
library(patchwork)
# library(ggpattern)
library(RColorBrewer)
# to add google fonts
library(sysfonts)
library(showtext)
library(readxl)
library(openxlsx)
library(survey)
library(haven)
library(sf)
library(ggspatial)
survey_path = "/Users/sonra/Documents/Delta_Resident_Survey/collaborator_generic/"

### read in data

drs_data <- cdrs::cdrs_read(survey_path, 
                            relevel_ = "default")
zone1 <- st_read("/Users/sonra/Documents/Delta_Resident_Survey/Primary_Delta/Primary_Delta.shp")
zone2 <- st_read("/Users/sonra/Documents/Delta_Resident_Survey/Secondary_Delta/Secondary_Delta.shp")

zone3a <- st_read("/Users/sonra/Documents/Delta_Resident_Survey/2019_stck_shapefile/Community Boundary.shp")
zone3b <- st_read("/Users/sonra/Documents/Delta_Resident_Survey/2018_ssacf_shapefile/Community Boundary.shp")

st <- "06"  # US FIPS state
epsg <- 3310
yr <- 2020  # TIGER year

###read in census data
counties_adj <- c("067", "009", "097", "061", "075", "041", "043", "055", "109" 
                  , "095", "013", "017", "113", "011", "085", "077"
                  , "001", "047", "099", "101", "005", "033", "081")
counties_adj2 <- c(counties_adj,"045", "051", "003", "115", "069"
                   , "019", "039", "057", "007", "087", "021" )

zone1 <- zone1%>%
  st_transform(crs = epsg)

zone2 <- zone2%>%
  st_transform(crs = epsg)

zone3a <- zone3a%>%
  st_transform(crs = epsg)

zone3b <- zone3b%>%
  st_transform(crs = epsg)


tracts <- tigris::tracts(
  state = st,
  county = counties_adj
)

tracts <- tracts %>%
  st_transform(crs = epsg)
# # delta_water <- tigris::area_water(state = st, 
# #                                       county = counties_adj2, 
# #                                       year = 2021)
# # 
# delta_water <- delta_water %>%
#   st_transform(crs = epsg)
# delta_water2 <- delta_water %>%
#   st_buffer(., dist = 40)
# 



# Load Spatial Data
data <- drs_data$data
data <- data %>%
  mutate(GEOID = paste0(geoid.state, geoid.county, geoid.tract))

respondents_tracts <- data %>%
  count(GEOID)

cdp <- respondents_tracts %>%
  left_join(tracts, by = "GEOID")

sf_data <- respondents_tracts %>%
  left_join(tracts, by = "GEOID") 
sf_data <- st_as_sf(sf_data)
# delta_water <- st_as_sf(delta_water)

sf_data <- sf_data%>%
  st_transform(crs = epsg)
# water_color_ <- "cyan"

# Create an n value range for defining the continuous color scale
n_values <- range(sf_data$n, na.rm = TRUE)
bin_width <- 20
sf_data$n_bin <- cut(sf_data$n, breaks=seq(1, max(sf_data$n, na.rm=TRUE)+bin_width, by=bin_width), include.lowest=TRUE, labels=FALSE)
sf_data$n_bin <- paste0((20*sf_data$n_bin-19), "-", (sf_data$n_bin*20))

# Extract the starting number of each bin (assumes it is the number before the hyphen)
get_bin_number <- function(bin) {
  as.numeric(strsplit(as.character(bin), "-")[[1]][1])
}

tiger_places <- tigris::places(state = "06", year = yr)
# california_state <- tigris::states(cb = TRUE, resolution = "20m")
# california_state <- california_state %>% filter(STATEFP == "06")

tiger_places <- tiger_places %>%
  st_transform(crs = epsg)
# delta_water <- delta_water %>%
#   st_transform(crs = epsg)

# Order the unique bins based on their numeric start
ordered_bins <- unique(sf_data$n_bin)[order(sapply(unique(sf_data$n_bin), get_bin_number))]

sf_data$n_bin <- factor(sf_data$n_bin, levels = ordered_bins)

rural_names <- c("Walnut Grove", "Isleton", "Hood", "Freeport", "Clarksburg"
                 , "Courtland", "Locke", "Ryde", "Rio Vista","Bethel Island", "Knightsen")
# urb_sub_names <- c("West Sacramento","Tracy", "Davis", "Antioch"
#                    , "Stockton", "Sacramento", "Rio Vista", "Pittsburg", "Brentwood")  
urb_sub_names <- c("Tracy", "Davis", "Antioch"
                   , "Stockton", "Sacramento", "Rio Vista", "Pittsburg", "Brentwood")  
urb_sub_names_label <- c("West Sacramento","Tracy", "Davis", "Antioch"
                   , "Rio Vista", "Pittsburg", "Brentwood")  
ref_names <- c("San Francisco", "Oakland", "Bridgeport")

city_labs <- tiger_places %>%
  filter(NAME %in% rural_names) 
top5places_points_rural <- st_centroid(city_labs)
city_labs <- tiger_places %>%
  filter(NAME %in% urb_sub_names) 
top5places_points <- st_centroid(city_labs)

ref_labs <- tiger_places %>%
  filter(NAME %in% ref_names) 
ref_points <- st_centroid(ref_labs)

# intersecting_water <- st_intersection(delta_water, sf_data)
# crop <- st_crop(delta_water, sf_data)

# # Buffer the intersecting geometries
# intersecting_water_buffered <- st_buffer(intersecting_water, dist = 40)

# bounding_box <- st_bbox(sf_data)
# bounding_box[1] <- round(bounding_box[1]*1.4)
# bounding_box <- round(bounding_box)
# top5places_points$position <- seq(-0.1, by = 0.1, length.out = nrow(top5places_points))
# Now create the plot with only the intersecting water polygons
# 
# basemap <- get_stadiamap(bounding_box, zoom = 1, maptype = "stamen_toner")

ggplot() +
  # ggmap(basemap) +
  # annotation_map_tile() +
  annotation_map_tile("cartolight" , zoomin = -1) +
  # geom_sf(data = crop, fill = water_color_, color = NA) +
  # geom_sf(data = intersecting_water_buffered, fill = water_color_, color = NA) +
  geom_sf(data = sf_data, aes(fill = n_bin), color = "grey", lwd = 0.2, alpha = 0.5) +
  scale_fill_viridis_d(begin = 0, end = 1, option = "D", direction = 1) +
  labs(fill = "Number \nof Survery \nRespondents") +
  # Add 'halo' around text by first plotting in white with a larger size
  # geom_sf_text(data = top5places_points, aes(label = NAME), color = "white", size = 2.5,
  #              vjust = -.6, hjust = .5, # Use staggered vertical positions
  #              fontface = "bold",
  #              nudge_y = 0.1, check_overlap = TRUE) +
  geom_sf(data = zone2,
          color = "cyan",
          fill = NA,
          linewidth = 1) +
  geom_sf(data = zone3a,
          color = "cyan",
          fill = NA,
          linewidth = 1) +
  geom_sf(data = zone3b,
          color = "cyan",
          fill = NA,
          linewidth = 1) +
  geom_sf(data = zone1,
          color = "chartreuse",
          fill = NA,
          linewidth = 1) +
  # geom_sf(data = california_state, 
  #         color = "black", 
  #         fill = NA,
  #         linewidth = 1) +
  # Add city name labels
  # geom_sf_text(data = 
  #                top5places_points
  #              # top5places_points%>%filter(NAME %in% urb_sub_names_label)
  #              , aes(label = NAME), color = "black", 
  #              # vjust = 0, hjust = -.1,# Use staggered vertical positions
  #              vjust = 1.6, hjust = .5,# Use staggered vertical positions
  #              size = 3,# fontface = "bold", 
  #              nudge_y = 0.1, check_overlap = TRUE) +
  # Add city points
  geom_sf(data = top5places_points, color = "red", size = 1.5)+   
  # geom_sf_text(data = ref_points, aes(label = NAME), color = "black",
  #              vjust = -.6, hjust = .5,# Use staggered vertical positions
  #              size = 2.5,# fontface = "bold",
  #              nudge_y = 0.1, check_overlap = TRUE) +
  # # Add city points
  geom_sf(data = ref_points, color = "lightgrey", size = .01)+   
  # coord_sf(xlim = c(bounding_box$xmin, bounding_box$xmax), ylim = c(bounding_box$ymin, bounding_box$ymax), expand = FALSE)+
  theme(
    text = element_text(family = "sans"),
    # Remove axis text and ticks
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    # Remove panel grid
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Customize plot background, if desired (optional)
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  guides(fill = guide_legend(barwidth = 1, barheight = rel(2))) # Adjust legend for discrete scale



ggplot() +
  # ggmap(basemap) +
  annotation_map_tile("cartolight", zoomin = -1) +
  # geom_sf(data = crop, fill = water_color_, color = NA) +
  # geom_sf(data = intersecting_water_buffered, fill = water_color_, color = NA) +
  geom_sf(data = sf_data, aes(fill = n_bin), color = "grey", lwd = 0.2, alpha = 0.5) +
  scale_fill_viridis_d(begin = 0, end = 1, option = "D", direction = 1) +
  labs(fill = "Number \nof Survery \nRespondents") +
  # Add 'halo' around text by first plotting in white with a larger size
  # geom_sf_text(data = top5places_points, aes(label = NAME), color = "white", size = 2.5,
  #              vjust = -.6, hjust = .5, # Use staggered vertical positions
  #              fontface = "bold",
  #              nudge_y = 0.1, check_overlap = TRUE) +
  # geom_sf(data = zone2, 
  #         color = "cyan", 
  #         fill = NA,
  #         linewidth = 1) +
  geom_sf(data = zone2,
          color = "cyan",
          fill = NA,
          linewidth = 1) +
  geom_sf(data = zone3a,
          color = "cyan",
          fill = NA,
          linewidth = 1) +
  geom_sf(data = zone3b,
          color = "cyan",
          fill = NA,
          linewidth = 1) +
  geom_sf(data = zone1,
          color = "chartreuse",
          fill = NA,
          linewidth = 1) +
  # geom_sf(data = california_state, 
  #         color = "black", 
  #         fill = NA,
  #         linewidth = 1) +
  # Add city name labels
  # geom_sf_text(data = top5places_points_rural, aes(label = NAME), color = "black", 
  #              vjust = -.6, hjust = .5,# Use staggered vertical positions
  #              size = 3,# fontface = "bold", 
  #              nudge_y = 0.1, check_overlap = TRUE) +
  # Add city points
  geom_sf(data = top5places_points_rural, color = "red", size = 1.5)+   
  # geom_sf_text(data = ref_points, aes(label = NAME), color = "black",
  #              vjust = -.6, hjust = .5,# Use staggered vertical positions
  #              size = 2.5,# fontface = "bold",
  #              nudge_y = 0.1, check_overlap = TRUE) +
  # # Add city points
  geom_sf(data = ref_points, color = "lightgrey", size = .05)+   
  # coord_sf(xlim = c(bounding_box$xmin, bounding_box$xmax), ylim = c(bounding_box$ymin, bounding_box$ymax), expand = FALSE)+
  theme(
    text = element_text(family = "sans"),
    # Remove axis text and ticks
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    # Remove panel grid
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Customize plot background, if desired (optional)
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  guides(fill = guide_legend(barwidth = 1, barheight = rel(2))) # Adjust legend for discrete scale

