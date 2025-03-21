% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_data.R
\name{read_lab_file}
\alias{read_lab_file}
\title{Reads the results sheet from a lab-protocol}
\usage{
read_lab_file(
  path,
  sheet = "results",
  .has_provid = TRUE,
  na_values = c("-99.99", "N/A", "-99.9"),
  remove_na = TRUE,
  pivot_longer = TRUE
)
}
\arguments{
\item{pivot_longer}{}
}
\description{
Reads the results sheet from a lab-protocol
}
