#' Assign colors to the modalities
#'
#' @param .data an msd based dataframe
#'
#' @return data frame of color encoded modalities
#'
#'
assign_mods_colors <- function(.data) {
  .data %>%
    dplyr::mutate(
      achv = ifelse(targets > 0, results / targets, NA_real_),
      indic_colors = dplyr::case_when(
        standardizeddisaggregate == "Offered" ~ "#877ec9",
        standardizeddisaggregate == "Accepted" ~ "#b5aaf9",
        standardizeddisaggregate == "Contacts" ~ "#005e7a",
        statushiv == "Known at Entry Positive" ~ scooter_med,
        statushiv == "Newly Identified Negative" ~ denim_light,
        statushiv == "Newly Identified Positive" ~ scooter,
        statushiv == " Documented Negative" ~ usaid_medblue
      ),
      cascade = dplyr::case_when(
        stringr::str_detect(indicator, "COM") ~ "Community",
        stringr::str_detect(indicator, "FAC") ~ "Facility",
        TRUE ~ "3rd 90"
      )
    )
}
