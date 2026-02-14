##############################################################################
# CREATE THE FIGURES FOR SBSR2024
# Map of spatial distribution of recurrent degradation.
##############################################################################

library(dplyr)
library(ggplot2)
library(gstat)
library(sf)
library(terra)
library(tidyr)

library(treesburnareasdata)



# Load data.
subarea_tb <- treesburnareasdata::subarea_tb
subarea_sf <- treesburnareasdata::subarea_sf

out_dir <- "/home/alber/Documents/github/slides/sbsr2024/doc/R"
stopifnot(dir.exists(out_dir))

grid_res <- 0.05



# Compute the number of DETER alerts for each subarea.
sa_file <- file.path(out_dir, "subarea_sf.gpkg")
if (!file.exists(sa_file)) {
    events_tb <-
            subarea_tb %>%
            dplyr::group_by(xy_id) %>%
            dplyr::summarize(n_events = dplyr::n())
    subarea_sf["area_ha"] <- sf::st_area(subarea_sf)
    subarea_sf["area_ha"] <- units::drop_units(subarea_sf[["area_ha"]]) / 10000
    subarea_sf <- merge(subarea_sf, events_tb, by = "xy_id")
    sf::write_sf(subarea_sf, "subarea_sf.gpkg")
    rm(events_tb)
}else {
    warning("Subareas file found!")
    subarea_sf <- sf::st_read("subarea_sf.gpkg")
}

# Convert the number of DETER alerts to points.
sa_cent_sf <-
    subarea_sf %>%
    sf::st_drop_geometry() %>%
    tidyr::separate_wider_delim(cols = xy_id,
                                delim = ";",
                                names = c("x", "y"),
                                cols_remove = FALSE) %>%
    sf::st_as_sf(coords = c("x", "y"),
    crs = sf::st_crs(subarea_sf))
stopifnot("Missing number of DETER alerts!" =
          sum(is.na(sa_cent_sf$n_events)) == 0)

# Create a vector grid.
grid_sf <- sf::st_make_grid(subarea_sf, cellsize = grid_res)
grid_sf <- sf::st_as_sf(grid_sf)



##############################################################################
# IDW
#NOTE: Interpolating all the samples takes too long (days)!
##############################################################################
if (FALSE) {
    idw_res <- gstat::idw( n_events ~ 1,
                          locations = sa_cent_sf["n_events"],
                          newdata = grid_sf)
}

##############################################################################
# NEAREST NEIGHBOR
#NOTE: interpolate throws an error.
##############################################################################
if (FALSE) {
    gs <- gstat(
                formula = n_events ~ 1,
                locations = sa_cent_sf["n_events"],
                nmax = 5,
                set = list(idp = 0)
    )

    sa_bbox <- sf::st_bbox(subarea_sf)
    grid_r <- terra::rast(nrows = 100, ncols = 100,
                          xmin = sa_bbox["xmin"],
                          xmax = sa_bbox["xmax"],
                          crs = "EPSG:4674"
    )

    nn <- interpolate(grid_r, gs)
}

##############################################################################
# SEMIVARIOGRAM.
##############################################################################
v_file <- file.path(out_dir, "v.rds")
if (!file.exists(v_file)) {
    v <- variogram(n_events ~ 1, sa_cent_sf["n_events"])
    saveRDS(v, "v.rds")
} else {
    warnings("Variogram data found!")
    v <- readRDS(v_file)
}

plot(v, xlab = "distance h [m]", ylab = expression(gamma(h)),
     xlim = c(0, 1.055 * max(v$dist)))

##############################################################################
# ANOTHER TRY AT INTERPOLATION.
#TOOD: It didn't work. Try something else.
##############################################################################

# Aggregate points.
data_df <- stats::aggregate(
  x  = sa_cent_sf["n_events"],
  by = grid_sf,
  FUN = max
)

# Remove incomplete rows.
data_df <- data_df[complete.cases(sf::st_drop_geometry(data_df)), ]

stopifnot("NAs found in samples!" = sum(is.na(data_df$n_event)) == 0)

# Interpolate.

gs <- gstat(
        formula = n_events ~ 1,
        locations = data_df["n_events"],
        nmax = 5,
        set = list(idp = 0)
)

# Create a grid centered in Brasilia.

brasilia <- c(-15.793889, -47.882778)
aoi <- terra::vect(sa_cent_sf)
center_cell <- rep(brasilia, each = 2) + c(-grid_res, grid_res) / 2
grid_r <- terra::rast(terra::ext(center_cell), crs = terra::crs(aoi),
                 ncol = 1, nrow = 1)
grid_r <- terra::extend(grid_r, aoi, snap = "out")




idw_res <- gstat::idw(
        n_events ~ 1,
        locations = sa_cent_sf["n_events"],
        newdata = grid_sf
)

idw_res <- sf::st_drop_geometry(idw_res)
idw_res <- sf::st_as_sf(idw_res,
                        coords = c("x", "y"),
                        crs = sf::st_crs(idw_res))

idw_r <- terra::rasterize(
                         x = terra::vect(idw_res),
                         y = grid_r,
                         field = "var1.pred"
                         )



