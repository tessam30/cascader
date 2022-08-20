#' Wrapper to return index cascade data frame
#'
#' @param .data msd data frame
#'
#' @return data frame of prepared index cascade
#'
return_index_cascade <- function(.data){

  df_index_cscd <-
    .data %>%
    fltr_mods(., agency = "USAID") %>%
    recode_mods() %>%
    sum_reshape_mods() %>%
    relevel_mods() %>%
    assign_mods_colors()

  return(df_index_cscd)

}


#' Filter index testing disaggregates
#'
#' @param .data msd data frame
#' @param agency funding agency (default is USAID)
#'
#' @return filtered data frame
#'
fltr_mods <- function(.data, agency = "USAID") {
  df_mods <-
    df %>%
    dplyr::filter(
      funding_agency == agency,
      indicator == "HTS_INDEX",
      standardizeddisaggregate %in% disag_mods
    )
  return(df_mods)
}


#' Recode disaggregates for plotting
#'
#' @param .data msd data frame
#'
#' @return data frame of recoded modalities
#'
recode_mods <- function(.data) {
  .data %>%
    dplyr::mutate(
      standardizeddisaggregate = dplyr::recode(standardizeddisaggregate,
                                               "1:Age/Sex/IndexCasesOffered" = "Offered",
                                               "2:Age/Sex/IndexCasesAccepted" = "Accepted",
                                               "3:Age Aggregated/Sex/Contacts" = "Contacts",
                                               "4:Age/Sex/Result" = "Results"
      ),
      statushiv = dplyr::case_when(
        is.na(otherdisaggregate) & statushiv == "Negative" ~ "Documented Negative",
        TRUE ~ statushiv
      ),
      otherdisaggregate = ifelse(statushiv == "Documented Negative", "", otherdisaggregate),
      statushiv = stringr::str_c(otherdisaggregate, statushiv, sep = " ")
    )
}


#' Reorder modalities
#'
#' @param .data msd data frame
#'
#' @return data frame with reordered modality factors for plotting
#'
relevel_mods <- function(.data) {
  .data %>%
    dplyr::mutate(
      standardizeddisaggregate = forcats::fct_relevel(standardizeddisaggregate, c(
        "Offered", "Accepted", "Contacts", "Results"
      )),
      statushiv = forcats::fct_relevel(statushiv, c(
        "Newly Identified Positive",
        "Newly Identified Negative",
        "Known at Entry Positive",
        "Documented Negative"
      ))
    )
}


# Reshapes the filtered df_mods data frame
#' Title
#'
#' @param .data msd data frame
#' @param ... for additional grouping
#'
#' @return data frame reshaped with current period index modalities
#'
sum_reshape_mods <- function(.data, ...) {
  df_mod_cascade <-
    .data %>%
    dplyr::filter(fiscal_year == curr_fy) %>%
    dplyr::filter(period == curr_pd) %>%
    dplyr::mutate(indicator = dplyr::case_when(
      indicator == "HTS_INDEX" & modality == "Index" ~ "HTS_INDEX_FAC",
      indicator == "HTS_INDEX" & modality == "IndexMod" ~ "HTS_INDEX_COM",
      TRUE ~ indicator
    )) %>%
    dplyr::group_by(
      funding_agency, indicator, standardizeddisaggregate,
      otherdisaggregate, statushiv, fiscal_year, ...
    ) %>%
    dplyr::summarise(dplyr::across(tidyselect::matches("targ|qtr"), sum, na.rm = T)) %>%
    dplyr::ungroup() %>%
    gophr::reshape_msd(direction = "quarters")

  return(df_mod_cascade)
}
