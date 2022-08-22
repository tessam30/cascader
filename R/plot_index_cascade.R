#' Plot the index testing modalities
#'
#' @param .data index testing prepared data frame
#'
#' @return ggplot object of index testing
#' @export
#'
plot_index_cascade <- function(.data) {
  .data %>%
    ggplot2::ggplot(aes(x = standardizeddisaggregate, fill = indic_colors)) +
    ggplot2::geom_col(aes(y = results, fill = "#005e7a"), width = 0.7, alpha = 0.3) +
    ggplot2::geom_col(aes(y = results), width = 0.7) +
    ggplot2::geom_text(
      data = . %>% dplyr::filter(standardizeddisaggregate == "Results"),
      aes(y = results, label = scales::comma(results, 1)),
      size = 12 / ggplot2::.pt,
      family = font_fam,
      position = position_stack(vjust = .9)
    ) +
    ggplot2::geom_text(
      data = . %>% dplyr::filter(standardizeddisaggregate != "Results"),
      aes(y = results, label = scales::comma(results, 1)),
      size = 12 / ggplot2::.pt,
      family = font_fam,
      vjust = -.125
    ) +
    ggplot2::facet_wrap(~cascade, scales = "free_y") +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_y_continuous(labels = scales::label_number()) +
    glitr::si_style_ygrid(text_scale = 1.25) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = glue::glue("INDEX TESTING CASCADE - {curr_pd}") %>% toupper(),
      subtitle = glue::glue("{curr_pd} results numbers listed above colored bar"),
      caption = data_source
    )
}
