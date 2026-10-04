############################################################
# GEOGRAPHIC DATA
#
# PURPOSE
# -------
# Load Natural Earth boundaries and derive country
# centroids used by Leaflet maps.
#
# OUTPUTS
# -------
# world
# centroids
#
# AI NOTES
# --------
# Leaflet maps use centroids.
#
# Cartograms use world polygons.
#
# Do not replace centroids with polygons unless
# intentionally moving to a choropleth map.
############################################################

library(sf)
library(rnaturalearth)

# Natural Earth countries
world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

# Country centroids
#
# of_largest_polygon = TRUE places each point on the
# country's largest landmass. Without it, overseas
# territories drag the point away, e.g. French Guiana
# pulls France into Spain and Alaska/Hawaii move the US.
centroids <- st_centroid(world, of_largest_polygon = TRUE)

coords <- st_coordinates(centroids)

centroids$lon <- coords[,1]
centroids$lat <- coords[,2]

# 2050 MSW by country
msw_2050 <- waw %>%
  filter(
    INDICATOR_LABEL == "Municipal Solid Waste (MSW) Generation",
    UNIT_MEASURE_LABEL == "Tonnes per year"
  ) %>%
  select(
    REF_AREA_LABEL,
    `2050`
  )