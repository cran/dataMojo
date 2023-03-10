% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rate_cal.R
\name{row_percent_convert}
\alias{row_percent_convert}
\title{Convert count to percentage}
\usage{
row_percent_convert(data, cols_rowsum)
}
\arguments{
\item{data}{data frame}

\item{cols_rowsum}{columns need to be converted to percentage}
}
\value{
data frame with calculated row percentage
}
\description{
Convert count to percentage
}
\examples{
test_df <- data.frame(
  Group = c("A", "B", "C"),
  Female = c(2,3,5),
  Male = c(10,11, 13)
)
dataMojo::row_percent_convert(test_df, cols_rowsum = c("Female", "Male"))
}
