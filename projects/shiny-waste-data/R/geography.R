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
# plotly_geo_fallback_coords
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

# Plotly's ISO-3 lookup does not render these countries.
# Keep fallback coordinates limited to these codes so the
# remaining countries retain Plotly's established placement.
plotly_geo_fallback_codes <- c("BHR", "HKG")

plotly_geo_fallback_world <- world %>%
  filter(adm0_a3 %in% plotly_geo_fallback_codes)

missing_plotly_fallback_codes <- setdiff(
  plotly_geo_fallback_codes,
  plotly_geo_fallback_world$adm0_a3
)

if (length(missing_plotly_fallback_codes) > 0) {
  stop(
    "Natural Earth is missing fallback geometry for: ",
    paste(missing_plotly_fallback_codes, collapse = ", ")
  )
}

plotly_geo_fallback_points <- plotly_geo_fallback_world %>%
  st_transform(6933) %>%
  st_point_on_surface() %>%
  st_transform(st_crs(world))

plotly_geo_fallback_coordinates <- st_coordinates(
  plotly_geo_fallback_points
)

plotly_geo_fallback_coords <- data.frame(
  iso3 = plotly_geo_fallback_points$adm0_a3,
  lon = plotly_geo_fallback_coordinates[, 1],
  lat = plotly_geo_fallback_coordinates[, 2]
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