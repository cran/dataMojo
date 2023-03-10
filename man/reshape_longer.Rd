% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/col_str_reshape.R
\name{reshape_longer}
\alias{reshape_longer}
\title{Reshape data frame to a longer format}
\usage{
reshape_longer(dt, keep_cols, label_cols, value_cols)
}
\arguments{
\item{dt}{input data}

\item{keep_cols}{columns to be kept}

\item{label_cols}{column name that contains the melted columns}

\item{value_cols}{column name that contains the value of melted columns}
}
\value{
data table in a longer format
}
\description{
Reshape data frame to a longer format
}
\examples{
data("dt_dates")
reshape_longer(dt_dates,
                        keep_cols = "Full_name",
                        label_cols = c("Date_Type"),
                        value_cols = "Exact_date")

}
