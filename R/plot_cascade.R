#' Create cascade plot title
#'
#' `plot_title` converts the cascade selection number
#' into a title that is used in the ggplot object. The
#' options available are listed in the plot_name data.
#'
#' @param cscd_num Cascade number selected from user
#' @return cascade title
#' @export
#'
#'
plot_title <- function(cscd_num) {
  cscd_title <- glue::glue("{plot_name[cscd_num]}")
  return(cscd_title)
}



#' @export
#' @description results font color
#' @title font color for results
results_color <- "#414042"


#' @export
#' @description base font family
#' @title base font family
font_fam <- "Source Sans Pro"

#' @export
#' @description annotation font family
#' @title annotation font family
font_annot <- "Source Sans Pro SemiBold"

#' @export
#' @description targets fill color
#' @title targets fill color
targets_color <- "#939598"



#' Plot Cascade
#' `plot_cascade` is a ggplot2-based plot that arranges, annotates and
#' plots data from the 90-90-90 cascade. Four inputs are required to create
#' a valid plot.
#'
#' @param .data dataframe that is the result of `return_cascade` and `assign_cscd_colors`
#' @param df_annot annotation dataframe created from the `return_cascade_plot` function
#' @param cscd_num cascade number selected by the user
#' @param p_title plot title derived from the `cscd_num`
#'
#' @importFrom ggplot2 aes .pt position_nudge facet_wrap unit
#' @return a ggplot2 object
#'
plot_cascade <- function(.data, df_annot, cscd_num, p_title) {

  pnudge <- 0.1
  p <- .data %>%
    dplyr::filter(period == max(period)) %>%
    ggplot2::ggplot(aes(
      x = indicator,
      fill = indic_colors
    )) +
    ggplot2::geom_col(aes(y = targets),
      fill = "#e6e6e6",
      width = 0.5
    ) +
    ggplot2::geom_col(aes(y = results_cumulative),
      width = 0.5,
      position = position_nudge(x = pnudge)
    ) +
    ggplot2::geom_text(aes(
      y = results_cumulative,
      label = scales::comma(results_cumulative, 1)
    ),
    size = 12 / .pt, vjust = -0.45,
    family = font_fam,
    position = position_nudge(x = pnudge),
    color = results_color
    ) +
    ggplot2::geom_label(aes(
      y = results_cumulative,
      label = scales::percent(achv, 1)
    ),
    size = 9 / .pt, vjust = 1.2,
    family = font_fam,
    position = position_nudge(x = pnudge),
    fill = "white"
    ) +
    ggplot2::geom_text(
      data = . %>% dplyr::filter(indicator == "HTS_TST"),
      aes(y = targets, label = "FY22 Targets"),
      size = 12 / .pt,
      family = font_fam,
      hjust = 0.4,
      color = results_color
    ) +
    ggplot2::geom_text(
      data = df_annot %>%
        dplyr::filter(indicator == "HTS_TST"),
      aes(
        y = results_cumulative.y,
        label = paste(
          "Tests needed for\none positive:",
          scales::comma(results_cumulative.x),
          "\n(Positivity rate:",
          scales::percent(1 / results_cumulative.x),
          ")"
        ),
        x = indicator,
        fill = targets_color
      ),
      size = 12 / .pt,
      family = font_annot,
      hjust = 0,
      vjust = -1,
      color = results_color
    ) +
    ggplot2::geom_text(
      data = df_annot %>%
        dplyr::filter(indicator == "TX_NEW"),
      aes(
        y = results_cumulative.y,
        label = paste(
          "Linkage:",
          scales::percent(results_cumulative.x)
        ),
        x = indicator,
        fill = targets_color
      ),
      size = 12 / .pt,
      family = font_annot,
      hjust = 1,
      vjust = -10,
      color = results_color
    ) +
    ggplot2::geom_text(
      data = df_annot %>%
        dplyr::filter(indicator == "TX_PVLS_D"),
      aes(
        y = results_cumulative.y,
        label = paste(
          "VLC:",
          scales::percent(results_cumulative.x)
        ),
        x = indicator,
        fill = targets_color
      ),
      size = 12 / .pt,
      family = font_annot,
      vjust = -10,
      color = results_color
    ) +
    ggplot2::geom_text(
      data = df_annot %>%
        dplyr::filter(indicator == "TX_PVLS"),
      aes(
        y = results_cumulative.y,
        label = paste(
          "VLS:",
          scales::percent(results_cumulative.x)
        ),
        x = indicator,
        fill = targets_color
      ),
      size = 12 / .pt,
      family = font_annot,
      vjust = -10,
      color = results_color
    ) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_fill_identity() +
    ggplot2::facet_wrap(~cascade,
      scales = "free"
    ) +
    glitr::si_style_ygrid(text_scale = 1.25) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = glue::glue("{p_title} Cascade- {curr_pd} RESULTS TO TARGETS (GRAY BARS)") %>%
        toupper(),
      subtitle = glue::glue("{curr_pd} results listed above colored bar, achievement in box below"),
      caption = data_source
    )
  return(p)
}
