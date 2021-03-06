% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ops_files.R
\name{write_ops}
\alias{write_ops}
\title{Write ops file}
\usage{
write_ops(data, file)
}
\arguments{
\item{data}{the data for the ops file, typically the output from \code{read_ops()} (see details).}

\item{file}{the path to the ops file}
}
\value{

}
\description{
Write ops file
}
\details{
The \code{data} argument expects a list of two \code{data.frame}s:
\itemize{
\item dimensions: the plot dimensions (xmin, ymin, zmin, xmax, ymax);
\item plants: a \code{data.frame} with the format:
\tabular{lllllllllll}{
\emph{sceneId}  \tab \emph{plantId} \tab \emph{plantFileName}             \tab  x      \tab y      \tab z \tab scale \tab inclinationAzimut \tab inclinationAngle \tab stemTwist \tab Functional_group \cr
1          \tab   1       \tab opf/DA1_Average_MAP_12.opf  \tab  2.3025 \tab 3.988  \tab 0 \tab 1     \tab 0                 \tab 0                \tab 0         \tab two              \cr
1          \tab   2       \tab opf/DA1_Average_MAP_12.opf  \tab  6.9075 \tab 11.964 \tab 0 \tab 1     \tab 0                 \tab 0                \tab 0         \tab two
}
\item and optionaly a chaining line, e.g. -1 1 1.
}
}
\examples{
\dontrun{
library(tidyverse)
library(archimedR)

# Read the OPS:
OPS= read_ops("dummy.ops")
# Increase the position of the plants by 0.5 meter:
OPS$plants= OPS$plants%>%mutate(x= x+0.5)
# Write the new OPS:
write_ops(OPS, "dummy2.ops")
}
}
