% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/batch_cascade.R
\name{batch_cascade_plot}
\alias{batch_cascade_plot}
\title{Batch all 13 cascade plots and save them to the local Images folder}
\usage{
batch_cascade_plot(msd_df, imgtype = ".jpeg", imgpath = "Images")
}
\arguments{
\item{msd_df}{msd based data frame}

\item{imgtype}{type of image to save (.png, .svg, .jpeg, ...)}

\item{imgpath}{path where images are saved ("Images" set as default)}
}
\value{
a list of plot objects, one for each cascade
}
\description{
Batch all 13 cascade plots and save them to the local Images folder
}
\examples{
\dontrun{
plt <- batch_cascade_plot(df_sub, imgtype = ".png", imgpath = "Images")}

}
