#' Wrapper to return index cascade plot
#'
#' @param .data msd data frame
#'
#' @return ggplot2 object
#' @export
#'
#'
create_index_cascade <- function(.data){
  return_index_cascade() %>%
  plot_index_cascade()
}
