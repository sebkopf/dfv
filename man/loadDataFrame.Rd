% Generated by roxygen2 (4.0.0): do not edit by hand
\name{loadDataFrame}
\alias{loadDataFrame}
\title{Loads a data frame into a new tab of the dataNb notebook}
\usage{
loadDataFrame(module,
  df.name = module$getElements("dfTable")$getSelectedValues("Name"))
}
\arguments{
\item{module}{- the Module object that stores the relevant widgets}

\item{df.name}{- the name of the data frame to load (by default the one selected in the dfTable Element in the Module)}
}
\description{
Loads a data frame into a new tab of the dataNb notebook
}

