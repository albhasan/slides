###############################################################################
# Analysis data for submission to the Simposio Brasileiro de Sensoramento
# Remoto 2024
#
# Analize DETER's warnings in São Felix do Xingu.
###############################################################################


library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
library(units)

library(treesburnareasdata)

#---- Configuration ----

out_dir <- "~/Documents/github/slides/sbsr2024/doc/latex"

stopifnot("Out directory not found!" = dir.exists(out_dir))

# Categorize the warnings' area.
area_breaks <- c(
  "0"        = 0,
  "6,25 ha"  = 6.25,
  "10 ha"    = 10,
  "25 ha"    = 25,
  "50 ha"    = 50,
  "100 ha"   = 100,
  "250 ha"   = 250,
  "500 ha"   = 500,
  "1000 ha"   = 1000,
  "> 1000 ha" = Inf
)


#---- Utilitary functions ----

source("~/Documents/github/slides/sbsr2024/doc/R/util.R")



#---- Load data ----

subarea_tb <-
  treesburnareasdata::subarea_tb %>%
  dplyr::filter(data_source == "DETER",
                year < 2022)



#---- Preprocess data ----

# Count the number of warnins covering each subarea.
warnings_by_subarea <-
  subarea_tb %>%
  dplyr::group_by(xy_id) %>%
  dplyr::summarize(n_warnings = dplyr::n(),
                   area_ha = dplyr::first(subarea_ha))



#---- Plot area by number of warnings ----

plot_area_by_warnings <-
  warnings_by_subarea %>%
  dplyr::mutate(area_type = cut(area_ha,
                                breaks = area_breaks,
                                labels = names(area_breaks)[-1])) %>%
  dplyr::group_by(n_warnings, area_type) %>%
  dplyr::summarize(area_ha = sum(area_ha)) %>%
  dplyr::mutate(prop = prop.table(area_ha),
                prop = dplyr::if_else(n_warnings > 2, NA, prop)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(dplyr::desc(area_type)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = n_warnings,
                                 y = area_ha,
                                 fill = area_type),
                    stat = "identity") +
  ggplot2::geom_text(ggplot2::aes(x = n_warnings,
                                  y = area_ha,
                                  label = scales::label_percent(0.1)(prop)),
                     position = ggplot2::position_stack(vjust = 0.5),
                     check_overlap = TRUE,
                     size = 3.0) +
  ggplot2::xlab("Number of wanings.") +
  ggplot2::ylab("Area (ha)") +
  ggplot2::labs(fill = "Area less than") +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_fill_viridis_d()

if (interactive()) {
    plot_area_by_warnings +
        ggplot2::ggtitle("Deter area by number of warnings.")
} else {
    ggplot2::ggsave(
        plot = plot_area_by_warnings,
        filename = file.path(out_dir, "figures", "plot_area_by_warnings.png"),
        height = 105,
        width = 148,
        units = "mm"
    )
}

rm(plot_area_by_warnings)

# Number of subareas by number of warnings.
subarea_tb %>%
  dplyr::group_by(xy_id) %>%
  dplyr::summarize(n_warnings = dplyr::n(),
                   area_ha = first(subarea_ha)) %>%
  dplyr::mutate(area_type = cut(area_ha,
                                breaks = area_breaks,
                                labels = names(area_breaks)[-1])) %>%
  dplyr::group_by(n_warnings, area_type) %>%
  dplyr::summarize(n_subareas = dplyr::n(),
                   total_area_ha = sum(area_ha)) %>%
  readr::write_csv(file.path(out_dir, "tables",
                             "warning_subareas_by_number_area.csv"))


#---- Plot time between warnings ----

plot_tb <-
  subarea_tb %>%
  dplyr::group_by(xy_id) %>%
  dplyr::arrange(VIEW_DATE, .by_group = TRUE) %>%
  dplyr::mutate(last_VIEW_DATE = dplyr::lag(VIEW_DATE),
                diff_days = as.vector(difftime(VIEW_DATE, last_VIEW_DATE,
                                               units = "days"))) %>%
  dplyr::left_join(dplyr::select(warnings_by_subarea, xy_id, n_warnings),
                   by = "xy_id")


#---- Plot days between warnings by area ----

# NOTE: This plot doesn't show the number of days between the first and last
#       Warnings!
# plot_days_between_warnings <-
#   plot_tb %>%
#   dplyr::select(xy_id, VIEW_DATE, diff_days, n_warnings, subarea_ha) %>%
#   dplyr::filter(n_warnings > 0,
#                 !is.na(diff_days),
#                 diff_days > 0) %>%
#   dplyr::mutate(area_type = cut(subarea_ha,
#                                 breaks = area_breaks,
#                                 labels = names(area_breaks)[-1])) %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_boxplot(ggplot2::aes(x = area_type, y = diff_days)) +
#   ggplot2::facet_wrap(~n_warnings) +
#   ggplot2::xlab("Number of DETER warnings") +
#   ggplot2::ylab("Days between DETER warnings") +
#   ggplot2::scale_y_continuous(labels = scales::comma) +
#   ggplot2::geom_hline(yintercept = 365,  linetype = 3, color = "gray50") +
#   ggplot2::geom_hline(yintercept = 730,  linetype = 3, color = "gray50") +
#   ggplot2::geom_hline(yintercept = 1095, linetype = 3, color = "gray50") +
#   ggplot2::geom_hline(yintercept = 1460, linetype = 3, color = "gray50") +
#   ggplot2::geom_hline(yintercept = 1825, linetype = 3, color = "gray50") +
#   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1,
#                                                      hjust=1))
#
# if (interactive()) {
#     plot_days_between_warnings +
#         ggplot2::ggtitle("Days between Deter warnings by area")
# }else{
#     ggplot2::ggsave(
#         plot = plot_days_between_warnings,
#         filename = file.path(out_dir, "plot_days_between_warnings.png"),
#         height = 105,
#         width = 148,
#         units = "mm"
#     )
# }
#
# rm(plot_days_between_warnings)
#

#---- Plot days from first to last warning by area ----

format_warnings <- function(x) {
    paste(x, "warnings")
}

plot_days_first_to_last <-
  plot_tb %>%
  dplyr::select(xy_id, VIEW_DATE, diff_days, n_warnings, subarea_ha) %>%
  dplyr::mutate(area_type = cut(subarea_ha,
                                breaks = area_breaks,
                                labels = names(area_breaks)[-1])) %>%
  dplyr::group_by(xy_id, area_type, n_warnings) %>%
  dplyr::summarize(days_first_last = sum(diff_days, na.rm = TRUE)) %>%
  dplyr::filter(days_first_last > 0) %>%
  dplyr::arrange(area_type, days_first_last) %>%
  ggplot2::ggplot() +
  ggplot2::geom_boxplot(ggplot2::aes(x = area_type, y = days_first_last)) +
  ggplot2::facet_wrap(~n_warnings,
                      labeller = labeller(n_warnings = format_warnings)) +
  ggplot2::xlab("Number of DETER warnings") +
  ggplot2::ylab("Days from first to last DETER warning") +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::geom_hline(yintercept = 365,  linetype = 3, color = "gray50") +
  ggplot2::geom_hline(yintercept = 730,  linetype = 3, color = "gray50") +
  ggplot2::geom_hline(yintercept = 1095, linetype = 3, color = "gray50") +
  ggplot2::geom_hline(yintercept = 1460, linetype = 3, color = "gray50") +
  ggplot2::geom_hline(yintercept = 1825, linetype = 3, color = "gray50") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1,
                                                     hjust=1))

if (interactive()) {
    plot_days_first_to_last +
        ggplot2::ggtitle("Days from first to last Deter warnings by area")
}else{
    ggplot2::ggsave(
        plot = plot_days_first_to_last,,
        filename = file.path(out_dir, "figures", "plot_days_first_to_last.png"),
        height = 105,
        width = 148,
        units = "mm"
    )
}

rm(plot_days_first_to_last)


#---- Plot DETER's area by type, and size ----

plot_deter_warnings_area_size <-
  subarea_tb %>%
  dplyr::mutate(area_type = cut(subarea_ha,
                                breaks = area_breaks,
                                labels = names(area_breaks)[-1]),
                year = ordered(year)) %>%
  dplyr::group_by(area_type, year) %>%
  dplyr::summarize(area_ha = sum(subarea_ha)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(prop = prop.table(area_ha)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(dplyr::desc(area_type)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_col(ggplot2::aes(x = year,
                                 y = area_ha,
                                 fill = area_type)) +
  ggplot2::geom_text(ggplot2::aes(x = year,
                                  y = area_ha,
                                  label = scales::label_percent(0.1)(prop)),
                     position = ggplot2::position_stack(vjust = 0.5),
                     check_overlap = TRUE,
                     size = 3.0) +
  ggplot2::xlab("Year (PRODES)") +
  ggplot2::ylab("Area (ha)") +
  ggplot2::labs(fill = "Area less than") +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_fill_viridis_d()

if (interactive()) {
    plot_deter_warnings_area_size +
        ggplot2::ggtitle("Deter warnings by area, type and size")
}else{
    ggplot2::ggsave(
        plot = plot_deter_warnings_area_size,
        filename = file.path(out_dir, "figures", "deter_warnings_area_size.png"),
        height = 105,
        width = 148,
        units = "mm"
    )
}

rm(plot_deter_warnings_area_size)


#---- Plot DETER's warnings by type, and size ----

plot_deter_warnings_size <-
  subarea_tb %>%
  dplyr::mutate(year = ordered(year)) %>%
  dplyr::mutate(area_type = cut(subarea_ha,
                                breaks = area_breaks,
                                labels = names(area_breaks)[-1])) %>%
  dplyr::group_by(area_type, year) %>%
  dplyr::summarize(events = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(prop = prop.table(events)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(dplyr::desc(area_type)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_col(ggplot2::aes(x = year,
                                 y = events,
                                 fill = area_type)) +
  ggplot2::geom_text(ggplot2::aes(x = year,
                                  y = events,
                                  label = scales::label_percent(0.1)(prop)),
                     position = ggplot2::position_stack(vjust = 0.5),
                     check_overlap = TRUE,
                     size = 3.0) +
  ggplot2::xlab("Year (PRODES)") +
  ggplot2::ylab("Number of DETER alerts") +
  ggplot2::labs(fill = "Area less than") +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_fill_viridis_d()

if (interactive()) {
    plot_deter_warnings_size +
        ggplot2::ggtitle("Deter alerts by type and size")
}else{
    ggplot2::ggsave(
        plot = plot_deter_warnings_size,
        filename = file.path(out_dir, "figures", "deter_warnings_size.png"),
        height = 105,
        width = 148,
        units = "mm"
    )
}

rm(plot_deter_warnings_size)


#---- Plot DETER's warnings by state, type, size, and month of the year ----

plot_deter_warnings_size_month <-
    subarea_tb %>%
    dplyr::mutate(moy = as.integer(format(as.Date(VIEW_DATE,
                                                  format = "%d/%m/%Y"), "%m")),
                  moy = ordered(moy, labels = month.abb),
                  year = ordered(year)) %>%
    dplyr::group_by(year, moy) %>%
    dplyr::summarize(area_ha = sum(subarea_ha)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(moy) %>%
    dplyr::mutate(prop = prop.table(area_ha)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(prop = dplyr::if_else(!(moy %in% c("Aug", "Sep", "Oct")),
                                        NA,
                                        prop)) %>%
    dplyr::arrange(dplyr::desc(year)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = moy,
                                   y = area_ha,
                                   fill = year)) +
    ggplot2::geom_text(ggplot2::aes(x = moy,
                                    y = area_ha,
                                    label = scales::label_percent(0.1)(prop)),
                       position = ggplot2::position_stack(vjust = 0.5),
                       check_overlap = TRUE,
                       size = 3.0) +
    ggplot2::xlab("Month") +
    ggplot2::ylab("Area (ha)") +
    ggplot2::labs(fill = "Year") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_fill_viridis_d()

if (interactive()) {
    plot_deter_warnings_size_month +
        ggplot2::ggtitle("Deter alerts by state, type, size, and month")
}else{
    ggplot2::ggsave(
        plot = plot_deter_warnings_size_month,
        filename = file.path(out_dir, "figures", "deter_warnings_size_month.png"),
        height = 105,
        width = 148,
        units = "mm"
    )
}

rm(plot_deter_warnings_size_month)

