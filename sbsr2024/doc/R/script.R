##############################################################################
# CREATE THE FIGURES FOR SBSR2024
##############################################################################

library(dplyr)
library(ggplot2)
library(gstat)
library(sf)
library(terra)

library(treesburnareasdata)


# Create interpolation map.
subarea_tb <- treesburnareasdata::subarea_tb
subarea_sf <- treesburnareasdata::subarea_sf

# Map of spatial distribution of recurrent degradation.
events_tb <-
        subarea_tb %>%
        dplyr::group_by(xy_id) %>%
        dplyr::summarize(n_events = dplyr::n())

subarea_sf["area_ha"] <- sf::st_area(subarea_sf)
subarea_sf["area_ha"] <- units::drop_units(subarea_sf[["area_ha"]]) / 10000
subarea_sf <- merge(subarea_sf, events_tb, by = "xy_id")

grid_sf <- sf::st_make_grid(subarea_sf, n = 1000)

# idw_res <- gstat::idw(
#         n_events ~ 1,
#         locations = subarea_sf["n_events"],
#         newdata = grid_sf
# )

gs <- gstat(
        formula = n_events ~ 1,
        locations = subarea_sf["n_events"],
        nmax = 5,
        set = list(idp = 0)
)
nn <- interpolate(grid_sf, gs)

# TODO:: semivariogram.
