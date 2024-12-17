
#' Estimate the PRODES year of the given date.
#'
#' @param x A date.
#' @param start_month The month when the year starts.
#' @param start_day   The day when the year starts.
#'
#' @return            An integer. The PRODES's year.
#'
to_year_prodes <- function(x, start_month = "08", start_day = "01") {
    stopifnot("At least a date is expected!" = length(x) != 0)
    stopifnot("Date object expected!" = lubridate::is.Date(x))
    if (length(x) > 1) {
        return(vapply(x,  FUN = to_year_prodes,  FUN.VALUE = integer(1)))
    }
    y <- as.integer(format(as.Date(x, format = "%d/%m/%Y"),"%Y"))
    start_year <- paste(y, start_month, start_day, sep = "-")
    if (x >= start_year)
        return(as.integer(y + 1))
    return(y)
}

#' Estimate the number of days since the start of the PRODES year.
#'
#' @param x A date.
#' @param start_month The month when the year starts.
#' @param start_day   The day when the year starts.
#'
#' @return            An integer.
#'
to_doy_prodes <- function(x, start_month = "08", start_day = "01") {
    stopifnot("At least a date is expected!" = length(x) != 0)
    stopifnot("Date object expected!" = lubridate::is.Date(x))
    if (length(x) > 1) {
        return(vapply(x,  FUN = to_doy_prodes,  FUN.VALUE = integer(1)))
    }
    y <- as.integer(format(as.Date(x, format = "%d/%m/%Y"),"%Y"))
    m <- as.integer(format(as.Date(x, format = "%d/%m/%Y"),"%m"))
    start_year <- paste(y, start_month, start_day, sep = "-")
    if (m < as.integer(start_month))
        start_year <- paste(y - 1, start_month, start_day, sep = "-")
    return(as.integer(difftime(x, start_year, units = "days")))
}



#' Fix the geometries of an SF object. If invalid geometries remain, filter
#' them out.
#'
#' @param data_sf an SF object.
#' @return        an SF object.
fix_geom_sf <- function(data_sf) {
    stopifnot("sf object expected" = inherits(data_sf, "sf"))
    data_sf %>%
    sf::st_make_valid() %>%
    dplyr::mutate(is_valid = sf::st_is_valid(.)) %>%
    dplyr::filter(is_valid) %>%
    dplyr::select(-is_valid) %>%
    return()
}



fix_geom_s2 <- function(data_sf) {
    stop ("Unfinished!")
    stopifnot("sf object expected" = inherits(data_sf, "sf"))
}



#' Build a tibble with the names of the files in the given directory.
#'
#' @param dir       A path to a directory.
#' @param pattern   A pattern for filtering files.
#' @param col_names Names of the columns for splitting the file names.
#' @param recursive Should it include files in subdirectories?
#' @param separator Character used for splitting the file names.
#' @return          A tibble.
list_files <- function(dir, pattern, col_names, separator, recursive) {
    dir %>%
        list.files(pattern = pattern,
                   full.names = TRUE,
                   recursive = recursive) %>%
        tibble::as_tibble() %>%
        dplyr::rename(file_path = value) %>%
        dplyr::mutate(
            file_name = tools::file_path_sans_ext(basename(file_path))
        ) %>%
        tidyr::separate(col = file_name,
                        into = col_names,
                        sep = separator) %>%
        return()
}




#' Set one meter precision on the given SF object. When the given object uses
#' geographic coordines, the correcponding angle is computed using the small
#' angle approximation using WGS84's semi-major axis.
#'
#' @param data_sf An SF object.
#' @return        An SF object.
set_meter_precison <- function(data_sf) {
    data_sf <- sf::st_set_precision(data_sf, 1)
    if (sf::st_is_longlat(data_sf)) {
        data_sf <- sf::st_set_precision(data_sf, 1/6378137)
    }
    return(data_sf)
}

