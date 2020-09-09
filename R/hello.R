##### Module 1 #####
#' Clean the earthquake data
#'
#' This function cleans the earthquakes dataset
#'
#' @param raw_data A dataframe containing the information about historical earthquakes
#'
#' @import magrittr
#' @importFrom dplyr mutate
#' @importFrom tidyr replace_na
#' @importFrom lubridate ymd years
#'
#' @return A dataframe with clean data about earthquakes
#'
#' @examples
#' \dontrun{
#' eq_clean_data(raw_data)
#' }
#'
#' @export
eq_clean_data <- function(raw_data) {

  raw_data %>%
    dplyr::mutate(MONTH = tidyr::replace_na(MONTH, 1),
                  DAY = tidyr:: replace_na(DAY, 1),
                  DATE = lubridate::ymd(paste("0000", MONTH, DAY, sep = "-")) +
                    lubridate::years(YEAR),
                  LATITUDE = as.numeric(LATITUDE),
                  LONGITUDE = as.numeric(LONGITUDE)) %>%
    eq_location_clean

}

#' Clean the LOCATION_NAME column
#'
#' This function cleans the LOCATION_NAME column
#'
#' @param raw_data A dataframe containing the information about historical earthquakes
#'
#' @import magrittr
#' @importFrom dplyr mutate
#' @importFrom stringr str_extract str_sub str_trim str_to_title
#'
#' @return A dataframe with a clean LOCATION_NAME column
#'
#' @examples
#' \dontrun{
#' eq_location_clean(eq_clean_data(raw_data))
#' }
#'
#' @export
eq_location_clean <- function(raw_data) {

  raw_data %>% dplyr::mutate(LOCATION_NAME = LOCATION_NAME %>%
                               stringr::str_extract(":.*") %>%
                               stringr::str_sub(2, -1) %>%
                               stringr::str_trim() %>%
                               stringr::str_to_title())

}

##### Module 2 #####

library(ggplot2)
library(grid)

###### Timeline ######

GeomTimeline <- ggproto("GeomTimeline", Geom,
                        required_aes = c("x"),
                        default_aes = aes(y = 0, alpha = 0.5, size = 1, color = "navyblue"),
                        draw_key = draw_key_point,
                        draw_panel = function(data, panel_scales, coord) {

                          ## Transform the data first
                          coords <- coord$transform(data, panel_scales)

                          # optional
                          if(sum(coords$y != 0) == 0) coords$y = ""

                          ## Construct a grid grob
                          pointsGrob(
                            x = coords$x,
                            y = coords$y,
                            size = unit(coords$size, "mm"),
                            pch = 21,
                            gp = grid::gpar(col = coords$color,
                                            alpha = coords$alpha)
                          )
                        })


#' geom_timeline
#'
#' This function uses GeomTimeline Prototype for plotting a timeline with the earthquakes of a given country
#'
#' @param mapping Mapping
#' @param data Data
#' @param stat Stat
#' @param position Position
#' @param na.rm  na.rm
#' @param show.legend Show legend
#' @param inherit.aes Inherit aes
#' @param ... other arguments
#'
#' @import ggplot2
#'
#' @return A timeline with all earthquakes between a set of dates for a set of countries.
#'
#' @examples
#' \dontrun{
#' eq_location_clean(eq_clean_data(raw_data)) %>%
#' dplyr::filter(datetime >= "1960-01-01" & datetime <="2011-01-01" & COUNTRY == c("CHILE","USA"))%>%
#' ggplot() +
#' geom_timeline(aes(x = datetime, size = EQ_MAG_ML, colour = DEATHS, fill = DEATHS))
#' }
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

###### Timeline Label ######

GeomTimelineLabel <- ggproto("GeomTimelineLabel", Geom,
                             required_aes = c("x", "label"),
                             default_aes = aes(),
                             draw_key = draw_key_point,
                             draw_panel = function(data, panel_scales, coord) {

                               coords <- coord$transform(data, panel_scales)

                               n_max <- ifelse(is.null(coords$n_max), nrow(coords), unique(coords$n_max))

                               coords_n_max <- coords %>% dplyr::group_by(y) %>% dplyr::top_n(n = n_max, wt = size)

                               text_HQ <- textGrob(label = coords_n_max$label,
                                                   x = coords_n_max$x,
                                                   y = coords_n_max$y * 1.1, vjust = 0, hjust = 0, rot = 45)

                               lines_HQ <- segmentsGrob(x0 =  coords_n_max$x,
                                                        y0 = coords_n_max$y,
                                                        x1 = coords_n_max$x,
                                                        y1 = coords_n_max$y * 1.1,
                                                        gp = grid::gpar(alpha = 0.6,
                                                                        lwd = 1,
                                                                        col = "grey60")
                               )

                               gTree(children = gList(text_HQ, lines_HQ))
                             })

#' geom_timeline_label
#'
#' This function adds location labels
#'
#' @param mapping Mapping
#' @param data Data
#' @param stat Stat
#' @param position Position
#' @param na.rm  na.rm
#' @param show.legend Show legend
#' @param inherit.aes Inherit aes
#' @param ... other arguments
#'
#' @import ggplot2
#'
#' @return Earthquake's labels
#'
#' @examples
#' \dontrun{
#' eq_location_clean(eq_clean_data(raw_data)) %>%
#' dplyr::filter(datetime >= "1960-01-01" & datetime <="2011-01-01" & COUNTRY == c("CHILE","USA"))%>%
#' ggplot() +
#' geom_timeline(aes(x = datetime, y = COUNTRY, size = EQ_MAG_ML, colour = DEATHS, fill = DEATHS)) +
#' geom_timeline_label(aes(x = datetime, y = COUNTRY, label = LOCATION_NAME, number = 3, max_aes = EQ_MAG_ML))
#'}
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

##### Module 3 #####

#' Interactive map with earthquakes data
#'
#' The function maps the epicenters (LATITUDE/LONGITUDE) and annotates each point with in
#' pop up window containing annotation data stored in a column of the data frame.
#'
#' @param clean_data The clean earthquake data.
#' @param annot_col Column in clean_data with the annotations
#'
#' @return An interactive map.
#'
#' @import leaflet
#'
#' @examples
#' \dontrun{
#' readr::read_delim("earthquakes.tsv.gz", delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#' eq_map(annot_col = "DATE")
#' }
#'
#' @export
eq_map <- function(clean_data, annot_col = "DATE"){

  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = clean_data,
                              radius = 2,
                              lng = ~ LONGITUDE,
                              lat = ~ LATITUDE,
                              popup = ~ paste(pull(clean_data, annot_col)))
}

#' Pop-up annotations in interactive map
#'
#' This function should put together a character string for each earthquake that will show the
#' cleaned location, the magnitude, and the total number of deaths (TOTAL_DEATHS),
#' with boldface labels for each ("Location", "Total deaths", and "Magnitude").
#'
#' @param clean_data The clean earthquake data
#'
#' @return A character vector containing the pop-up text to be used in a leaflet visualization.
#'
#' @examples
#' \dontrun{
#' readr::read_delim("earthquakes.tsv.gz", delim = "\t") %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")
#' }
#'
#' @export
eq_create_label <- function(clean_data){

  paste("<b> Location:</b>", clean_data$LOCATION_NAME, "<br />",
        "<b> Magnitude:</b>", clean_data$EQ_PRIMARY, "<br />",
        "<b> Total deaths:</b>", clean_data$TOTAL_DEATHS)

}
