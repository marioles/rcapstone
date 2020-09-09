library(testthat)

test_that("Leaflet", {

  file_name <- system.file("extdata", "signif.txt", package = "rcapstone")

  expect_that(file_name %>%
                readr::read_tsv() %>%
                rcapstone::eq_clean_data() %>%
                dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
                dplyr::mutate(popup_text = rcapstone::eq_create_label(.)) %>%
                rcapstone::eq_map(annot_col = "popup_text"), is_a("leaflet"))
})
