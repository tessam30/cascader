
#' Wrapper around the core return_index_plot functions. No readline input required.
#'
#' @param msd_df msd based data frame to be passed through cascade call
#' @param cscd_num number from the cascade plot see `plot_name` for a crosswalk
#' @param export logical to control saving of plot, TRUE by default
#' @param imgtype type of image to save (.png, .svg, .jpeg, ...)
#' @param imgpath path where images are saved ("Images" set as default)
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' plt <- batch_plot(df_sub, cscd_num = 1, export = T, imgtype = ".png", imgpath = "Images")}
#'
#'
batch_plot <- function(msd_df, cscd_num, export = TRUE, imgtype = ".png", imgpath = "Images"){

  cscd_num <- as.numeric(cscd_num)

  # Check the value entered is valid, if not return a useful error
  if (!cscd_num %in% seq(1:length(cascade::plot_name))) {
    stop(glue::glue_col("{cyan Please enter a valid selection between 1 and {length(plot_name)}}"))
  } else {
    message(glue::glue_col("{yellow You have selected the {plot_name[cscd_num]} Cascade.}"))
  }

  # Fetch the plot title
  p_title <- plot_title(cscd_num)

  # create cascade dataframe
  cscd_df <- return_cascade(msd_df, cscd_num)

  # create visualization dataframe
  df_viz <- cscd_df %>%
    assign_cscd_colors()

  # create the annotation dataframe (need the factor levels from df_viz)
  df_annot <- create_annotations(cscd_df, df_viz)

  # Plot the cascade
  suppressWarnings(p <- plot_cascade(df_viz, df_annot, cscd_num, p_title))

  if(export == TRUE){
    p_title <- gsub(" ", "_", p_title)
    glitr::si_save(glue::glue("{imgpath}/{p_title}_cascade{imgtype}") %>% stringr::str_to_lower(.), p, scale = 1.25)
  } else {
    return(p)
  }
}



#' Batch all 13 cascade plots and save them to the local Images folder
#'
#' @param msd_df msd based data frame
#' @param imgtype type of image to save (.png, .svg, .jpeg, ...)
#' @param imgpath path where images are saved ("Images" set as default)
#'
#' @return a list of plot objects, one for each cascade
#' @export
#'
#' @examples
#' \dontrun{
#' plt <- batch_cascade_plot(df_sub, imgtype = ".png", imgpath = "Images")}
#'
batch_cascade_plot <- function(msd_df, imgtype = ".jpeg", imgpath = "Images") {
  cscd_plts <- map(1:13, ~batch_plot(msd_df, .x,
    export = TRUE,
    imgtype = imgtype,
    imgpath = imgpath
  ))
  return(cscd_plts)
}


