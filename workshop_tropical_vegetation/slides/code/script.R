#!/usr/bin/env Rscript

library(dplyr)
library(GGally)
library(ggplot2)
library(lubridate)
library(readr)

data_file <-
"/home/alber/Documents/trees_lab/viaje_alemanha_2025/workshop_guess_freising_amazon/FAPESP_BYLAT_WORKSHOP_DATA/BR-Ma2_HH_199901010000_201701010000_LBAv2.csv"
stopifnot("File not found!" = file.exists(data_file))

out_dir <-
  "/home/alber/Documents/github/slides/workshop_tropical_vegetation/slides/figures"
stopifnot("Directory not found!" = dir.exists(out_dir))

data_tb <-
  data_file %>%
  readr::read_csv() %>%
  dplyr::mutate(
    dplyr::across(dplyr::where(is.numeric), ~dplyr::na_if(., -9999))
  ) %>%
  dplyr::mutate(
    start_date = as.character(TIMESTAMP_START),
    start_date = lubridate::fast_strptime(start_date, format = "%Y%m%d%H%M"),
    start_year = lubridate::year(start_date),
    start_month = lubridate::month(start_date),
    start_hour = lubridate::hour(start_date),
    group = stringr::str_c(
      as.character(start_year),
      stringr::str_pad(as.character(start_month), width = 2, pad = "0")
    )
  ) %>%
  dplyr::filter(start_month == 8, start_hour %in% 6:18)

cloud_plot <-
  data_tb %>%
  dplyr::select(start_year, start_month, VPD_F, LW_OUT, RH, TA, FC) %>%
  tidyr::pivot_longer(
    cols = tidyselect::all_of(c("LW_OUT", "RH", "FC", "TA"))
  ) %>%
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(
    x = value,
    y = VPD_F,
    color = as.factor(start_year),
    shape = name,
  ),
  alpha = 0.2
  ) +
  ggplot2::geom_smooth(
    ggplot2::aes(
      x = value,
      y = VPD_F,
      alpha = 0.7
    ),
    method = "lm",
    formula = y ~ x + I(x^2),
    se = TRUE
  ) +
  ggplot2::facet_wrap(~name, scales = "free")

ggplot2::ggsave(
  plot = cloud_plot,
  filename = file.path(out_dir, "cloud_plot.png"),
  width = 297,
  height = 210,
  units = "mm"
)

# Plot the correlation among variables.
corr_plot <-
  GGally::ggcorr(
    data = data_tb,
    #label = TRUE,
    geom = "circle",
    method = c("pairwise.complete.obs", "pearson")
  )

ggplot2::ggsave(
  plot = corr_plot,
  filename = file.path(out_dir, "corr_plot.png"),
  width = 297,
  height = 210,
  units = "mm"
)
