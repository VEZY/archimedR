% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manage_voxels.R
\name{read_voxel_space}
\alias{read_voxel_space}
\title{Read voxel space}
\usage{
read_voxel_space(input)
}
\arguments{
\item{input}{File path (including file name). Can also be anything compatible with
\code{data.table::fread()}.}
}
\value{
The voxel space
}
\description{
Read the voxel space from a \code{.vox} file
}
\details{
This function uses \code{voxel_space()} to compute the voxel space from the data
}
\examples{
\dontrun{
read_voxel_space(input= "output/voxelized_scene.vox")
}
}
