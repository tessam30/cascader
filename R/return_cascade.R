#' Fetches the correct version of the data from each cascade option
#' Returns a data frame of cascade indicators for a given population
#'
#' @param df MER structured data set for a given population and geography
#' @param cscd_num cascade selection from user
#'
#' @return a data frame of cascade indicators
#' @export
#'
#'
#'
return_cascade <- function(df, cscd_num) {

  # For Total Numerator all, female, male
  if (cscd_num %in% c(1, 2, 3)) {
    df_cscd <-
      df %>%
      youth_wrapper() %>%
      {
        if (cscd_num == 2) fltr_sex(., m_or_f = "Female") else .
      } %>%
      {
        if (cscd_num == 3) fltr_sex(., m_or_f = "Male") else .
      } %>%
      sum_reshape()
  }

  # Pediatric cascades all, female, male
  if (cscd_num %in% c(4, 5, 6)) {
    df_cscd <-
      df %>%
      youth_wrapper() %>%
      {
        if (cscd_num == 5) fltr_sex(., m_or_f = "Female") else .
      } %>%
      {
        if (cscd_num == 6) fltr_sex(., m_or_f = "Male") else .
      } %>%
      sum_reshape(trendscoarse) %>%
      dplyr::filter(trendscoarse == "<15")
  }

  # AYP cascades all, female, male
  if (cscd_num %in% c(7, 8, 9)) {
    df_cscd <-
      df %>%
      youth_wrapper() %>%
      {
        if (cscd_num == 8) fltr_sex(., m_or_f = "Female") else .
      } %>%
      {
        if (cscd_num == 9) fltr_sex(., m_or_f = "Male") else .
      } %>%
      fltr_ayp() %>%
      sum_reshape(trendscoarse) %>%
      dplyr::filter(trendscoarse == "AYP")
  }

  # Pediatric cascades all, female, male
  if (cscd_num %in% c(10, 11, 12)) {
    df_cscd <-
      df %>%
      youth_wrapper() %>%
      {
        if (cscd_num == 11) fltr_sex(., m_or_f = "Female") else .
      } %>%
      {
        if (cscd_num == 12) fltr_sex(., m_or_f = "Male") else .
      } %>%
      sum_reshape(trendscoarse) %>%
      dplyr::filter(trendscoarse == "15+")
  }

  # KP cascade (1 option)
  if (cscd_num == 13) {
    df_cscd <-
      df %>%
      fltr_cascade() %>%
      fltr_disag(pop_fltr = disag_kp) %>%
      sum_reshape()
  }

  return(df_cscd)
}







# Returns the cascade indicators from an msd
#' Cascade filter
#'
#' Filters the cascade variables to current fiscal year and
#' sets USAID as default funding agency
#'
#' @param .data MER structured data set
#' @param agency default is USAID
#'
#' @return passed through a filtered cascade data frame
#' @export
#'
#'
fltr_cascade <- function(.data, agency = "USAID") {
  .data %>%
    dplyr::filter(
      fiscal_year == curr_fy,
      funding_agency == agency,
      indicator %in% gophr::cascade_ind
    )
}


#' Filter disaggregate
#'
#' Filters the MSD to the appropriate population disaggregate
#'
#' @param .data  MER structured data set
#' @param pop_fltr population filter to be applied to disaggregate
#'
#' @return passes through a filtered cascade data frame
#' @export
#'
#'
fltr_disag <- function(.data, pop_fltr) {
  .data %>%
    dplyr::filter(standardizeddisaggregate %in% pop_fltr)
}


#
#' Filter for Adults and Young People
#'
#' Filters a whittled down MSD into 15-24 year olds or AYPs.
#'
#' @param .data MER structured data set whittled down
#'
#' @return passes through a filtered ayp dataframe
#'
#
fltr_ayp <- function(.data) {
  .data %>%
    dplyr::mutate(trendscoarse = ifelse(ageasentered %in% c("15-19", "20-24"), "AYP", "Non AYP"))
}


#' Filter sex
#'
#' Filters a whittled down MSD into sex categories, male or female.
#'
#' @param .data whittled down MSD
#' @param m_or_f Male or Female filter
#'
#' @return passes through a filtered sex data frame
#'
#'
fltr_sex <- function(.data, m_or_f) {
  .data %>%
    dplyr::filter(sex %in% m_or_f)
}




#' Wrapper for youth cascade
#'
#' Combines the cascade filter with the peds disag
#'
#' @param .data MSD data set
#'
#' @return passes through a filtered youth data frame
#'
#'
youth_wrapper <- function(.data) {
  .data %>%
    fltr_cascade() %>%
    fltr_disag(pop_fltr = disag_peds)
}



#' Aggregate targets and results
#'
#' Takes a whittled down MSD and cleans the indicators,
#' summarizes the targets and results, and reshapes everything
#' into a long data set with quarters and cumulative results.
#'
#' @param .data MSD data set
#' @param ... additional parameters to pass to `group_by`
#'
#' @return passes through a reshaped data frame
#'
#'
sum_reshape <- function(.data, ...) {
  .data %>%
    gophr::clean_indicator() %>%
    dplyr::group_by(funding_agency, indicator, fiscal_year, ...) %>%
    dplyr::summarise(dplyr::across(tidyselect::matches("targ|qtr"), sum, na.rm = T)) %>%
    gophr::reshape_msd(direction = "quarters") %>%
    dplyr::ungroup()
}
