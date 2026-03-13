# Make a map
install.packages("rnaturalearth")

library(sf)
library(rnaturalearth)
library(ggplot2)
library(dplyr)



# read in data ------------------------------------------------------------

locdata <- read.csv("local_dat.csv")



# map ---------------------------------------------------------------------

# find reasonable bound box
fivenum(locdata$Latitude) # min = 10.55, max = 39.79 (y values)
fivenum(locdata$Longitude) # min = -117.33, max -82



world <- ne_countries(returnclass = "sf")
ggplot(world) +
  geom_sf(fill = "gray90", color = "gray50") +
  coord_sf(
    xlim = c(-120, -82),
    ylim = c(11, 41),
    expand = FALSE
  ) +
  theme_minimal()


ggplot(world) +
  geom_sf(fill = "gray90", color = "gray50") +
  geom_point(data = locdata,
             aes(x = Longitude, y = Latitude, color = Species),
             size = 3) +
  coord_sf(
    xlim = c(-120, -79),
    ylim = c(11, 42),
    expand = FALSE
  ) +
  theme_minimal()
