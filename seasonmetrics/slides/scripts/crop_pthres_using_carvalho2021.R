###############################################################################
# MASK PEAK AND THRESHOLD RESULTS USING DATA FROM CARVALHO ET AL., 2021.
###############################################################################

library(dplyr)
library(terra)

car_file <- "/home/alber/Documents/data/amazonian_fire_calendar/dry_season/AmzBasin_DrySeason_Length.tif"
pthres_file <- "/home/alber/Documents/results/r_packages/seasonmetrics/results_05/season_peak_thres.tif"


pthres_world <- "/home/alber/Documents/github/slides/seasonmetrics/slides/images/pthres_world.png"

car_r <- terra::rast(car_file)
pthres_r <- terra::rast(pthres_file)

car_ext <- terra::ext(car_r)
car_ext <- terra::project(car_ext,
        from = terra::crs(car_r),
        to = terra::crs(pthres_r)
)
car_r <- terra::project(car_r, y = pthres_r)
car_r <- terra::crop(x = car_r, y = car_ext)

pthres_r <- terra::crop(x = pthres_r, y = car_r)
pthres_r <- terra::resample(x = pthres_r, y = car_r, method = "near")

res <- terra::mask(x = pthres_r, mask = car_r)

out_file <- tools::file_path_sans_ext(pthres_file)
out_file <- paste0(out_file, "_crop.tif")
terra::writeRaster(res, filename = out_file)
