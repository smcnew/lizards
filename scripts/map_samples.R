# Make a map

library(sf)
library(rnaturalearth)
library(ggplot2)
library(ggspatial)
library(dplyr)



# read in data ------------------------------------------------------------

screened_samps2 <- read.csv("data_analysis/combined_lizard_data.csv") %>%
  rename(Latitude = latitude,
         Longitude = longitude,
         Species = species,
         Reproduction = reproduction)

head(screened_samps2)
summary(screened_samps2$Longitude) # 82 samples without locality information
as.Date(screened_samps2$date, "%Y-%m-%d") %>% min(na.rm=T)
# map ---------------------------------------------------------------------

# find reasonable bound box
fivenum(screened_samps2$Latitude) # min = 10.55, max = 39.79 (y values)
fivenum(screened_samps2$Longitude) # min = -117.33, max -82

screened_samps2 <- screened_samps2 %>% filter(Species != "speciesG") %>% droplevels()


world <- ne_countries(returnclass = "sf")
states <- ne_states(returnclass = "sf")

pdf("output_plots/map.pdf")
ggplot(world) +
  geom_sf(data = states, fill = "grey90", color = "grey50") +
  geom_point(data = screened_samps2,
             aes(x = Longitude, y = Latitude, color = Species, shape = Reproduction),
             size = 3) +
  coord_sf(
    xlim = c(-120, -79),
    ylim = c(14, 42),
    expand = FALSE
  ) +
  theme_minimal() +
  theme(
    legend.text = element_text(face = "italic"),
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 14)) +
guides(
  color = guide_legend(order = 1),
  shape = guide_legend(order = 2)
) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr")
dev.off()

