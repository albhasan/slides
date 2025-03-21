# Produce statistics and figures for the poster.

library(dplyr)
library(ggplot2)
library(sf)
library(terra)
library(tibble)

season_file <- "/home/alber/Documents/results/r_packages/seasonmetrics/results_07/viirs/tropical_mask/all_years_dsig_r.tif"

continent_file <- "/home/alber/Documents/data/geodata/continents/World_Continents.shp"

out_dir <- "/home/alber/Documents/github/slides/workshop_tropical_vegetation/latex/figures"

stopifnot("Season file not found!" = file.exists(season_file))
stopifnot("Continent file not found!" = file.exists(continent_file))
stopifnot("Output directory not found!" = dir.exists(out_dir))

season_r <- terra::rast(season_file)
continent_sf <- 
  continent_file |>
  sf::read_sf() |>
  dplyr::select(CONTINENT) |>
  sf::st_transform(crs = sf::st_crs(4326))

season_sf <-
  season_r |>
  terra::as.data.frame(xy = TRUE) |>
  tibble::as_tibble() |>
  dplyr::mutate(y_round = as.integer(trunc(y))) |>
  sf::st_as_sf(coords = c("x", "y"), crs = 4326, remove = FALSE) |>
  sf::st_join(y = continent_sf, join = st_nearest_feature)

season_df <-
  season_sf |>
  sf::st_drop_geometry()

plot_latitude_pixels_by_continent <-
  season_df |>
  dplyr::group_by(y_round, CONTINENT) |>
  dplyr::summarize(
    n_pixels  = dplyr::n()
  ) |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(
    mapping = aes(x = y_round, y = n_pixels, fill = CONTINENT),
    stat = "identity",
    position = "stack"
  ) +
  ggplot2::labs(x = "Latitude", y = "Number of pixels") +
  ggplot2::theme(legend.title = ggplot2::element_blank())

plot_latitude_pos_from <-
  season_df |>
  dplyr::mutate(
    CONTINENT = dplyr::recode(.x = CONTINENT,
			      "North America" = "America",
			      "South America" = "America",
			      "Australia" = "Asia - Oceania",
                              "Oceania" = "Asia - Oceania",
                              "Asia" = "Asia - Oceania") 
  ) |>
  dplyr::group_by(y_round, CONTINENT) |>
  ggplot2::ggplot() +
  ggplot2::geom_boxplot(
    mapping = ggplot2::aes(
      x = y_round,
      y = pos_from,
      group = y_round,
      fill = CONTINENT
    )
  ) +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(vars(CONTINENT)) +
  ggplot2::labs(x = "Latitude", y = "Fire season start (months)") +
  ggplot2::theme(
    legend.position = "none",
    axis.text.x = ggplot2::element_text(angle = 270)
  )

plot_latitude_val_len <-
  season_df |>
  dplyr::mutate(
    CONTINENT = dplyr::recode(.x = CONTINENT,
			      "North America" = "America",
			      "South America" = "America",
			      "Australia" = "Asia - Oceania",
                              "Oceania" = "Asia - Oceania",
                              "Asia" = "Asia - Oceania") 
  ) |>
  dplyr::group_by(y_round, CONTINENT) |>
  ggplot2::ggplot() +
  ggplot2::geom_boxplot(
    mapping = ggplot2::aes(
      x = y_round,
      y = val_len,
      group = y_round,
      fill = CONTINENT
    )
  ) +
  ggplot2::facet_wrap(vars(CONTINENT)) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "Latitude", y = "Fire season duration (months)") +
  ggplot2::theme(
    legend.position = "none",
    axis.text.x = ggplot2::element_text(angle = 270),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggplot2::ggsave(
  plot = plot_latitude_pos_from,
  filename = file.path(out_dir, "plot_latitude_pos_from.png"),
  units = "mm",
  width = 105,
  height = 148
)

ggplot2::ggsave(
  plot = plot_latitude_val_len,
  filename = file.path(out_dir, "plot_latitude_val_len.png"),
  units = "mm",
  width = 105,
  height = 148
)
