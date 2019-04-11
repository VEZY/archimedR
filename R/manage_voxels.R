#' @title Voxel space
#'
#' @description Defines the voxel space using minimum and maximum coordinates
#'
#' @param coord_min Vector of the minimum 3D coordinates of the voxel space (point of origin, see details)
#' @param voxel_size Vector of the lengths of the voxel side in x, y and z.
#' @param coord_max Vector of the maximum 3D coordinates of the voxel space
#' @param n_voxel The number of voxels in c, y and z (optional, see details)
#'
#'
#' @details The 3D coordinates have the form of c(x,y,z). The user chooses to compute the voxel space from
#' a pair of arguments: either voxel_size and n_voxel, either voxel_size and coord_max, or n_voxel and
#' coord_max.
#'
#' @return A voxel space
#' @export
#'
#' @examples
#' voxel_space(voxel_size= c(1,1,1), n_voxel= c(10,10,20))
voxel_space= function(coord_min= c(0,0,0),voxel_size= NULL, n_voxel= NULL, coord_max= NULL){

  if(!is.null(coord_max)&(!is.null(voxel_size)&!is.null(n_voxel))){
    NULL
  }else{
    if(is.null(coord_max)&&(!is.null(voxel_size)&!is.null(n_voxel))){
      coord_max= coord_min+voxel_size*n_voxel
    }else if(is.null(voxel_size)&&(!is.null(coord_max)&!is.null(n_voxel))){
      voxel_size= (coord_max-coord_min)/n_voxel
    }else if(is.null(n_voxel)&&(!is.null(coord_max)&!is.null(voxel_size))){
      n_voxel= (coord_max-coord_min)/voxel_size
    }else{
      stop("Need at least two arguments")
    }
  }
  list(coord_min= coord_min, coord_max= coord_max, voxel_size= voxel_size, n_voxel= n_voxel)
}


#' Read voxel file
#'
#' @description Read the voxel file, either input or output from ARCHIMED
#'
#' @param input File path (including file name). Can also be anything compatible with
#' `data.table::fread()`.
#'
#' @details This function uses `read_voxel_space()` to find the voxel space and set it as an
#' attribute: `voxel_space`.
#'
#' @return The voxel data
#' @export
#'
#' @examples
#' \dontrun{
#' # Read a voxel file:
#' vox= read_vox(path= "output/voxelized_scene.vox")
#' # Find the voxel space:
#' attr(vox, "voxel_space")
#' }
#'
read_vox= function(input){
  vox= data.table::fread(input,skip = 5, data.table = FALSE)
  attr(vox, "voxel_space")= read_voxel_space(input)
  vox
}


#' Read voxel space
#'
#' @description Read the voxel space from a `.vox` file
#'
#' @param input File path (including file name). Can also be anything compatible with
#' `data.table::fread()`.
#'
#' @details This function uses `voxel_space()` to compute the voxel space from the data
#'
#' @return The voxel space
#' @export
#'
#' @examples
#' \dontrun{
#' read_voxel_space(input= "output/voxelized_scene.vox")
#' }
read_voxel_space= function(input){
  vox_space= scan(input, nlines = 5, what = "character")
  index_min= grep("#min_corner:", vox_space)
  coord_min= vox_space[(index_min+1):(index_min+3)]%>%as.numeric()
  index_max= grep("#max_corner:", vox_space)
  coord_max= vox_space[(index_max+1):(index_max+3)]%>%as.numeric()
  index_split= grep("#split:", vox_space)
  n_vox= vox_space[(index_split+1):(index_split+3)]%>%as.numeric()
  voxel_space(coord_min = coord_min, coord_max = coord_max, n_voxel = n_vox)
}
