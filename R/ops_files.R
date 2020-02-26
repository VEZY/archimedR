#' read ops file
#'
#' @param file the path to the ops file
#'
#' @return A list of two `data.frame`:
#' * dimensions: the plot dimensions (xmin, ymin, zmin, xmax, ymax);
#' * plants= the informations about the plants positions, path to the opf, ...
#'
#' @export
#'
#' @examples
#' \dontrun{
#' read_ops("path_to_the_ops/dummy.ops")
#' }
read_ops= function(file){
  config= readLines(file)

  config_archimed_index= grepl("#[Archimed]",config, fixed = TRUE)
  config_comments_index= grepl("#",config, fixed = TRUE)
  config_no_comments= config[(!config_comments_index|config_archimed_index) & (!config%in%"")]
  config_archimed_index= grep("#[Archimed]",config_no_comments, fixed = TRUE)

  if(config_no_comments[1]%>%substr(.,1,1)!="T"){
    stop(paste("Missing", crayon::red("plot boundaries"),"in OPS.",
               crayon::underline(crayon::red("Mandatory")),"for ARCHIMED simulations"))
  }
  plot_dim=
    config_no_comments[1]%>%gsub("T","",.)%>%strsplit(.," ")%>%unlist%>%
    .[!.%in%""]%>%t()%>%data.frame(stringsAsFactors = FALSE)%>%type.convert(.,as.is= TRUE)
  colnames(plot_dim)= c("xmin", "ymin", "zmin", "xmax", "ymax", "Terrain")
  plot_dim$area= (plot_dim$xmax-plot_dim$xmin)*(plot_dim$ymax-plot_dim$ymin)

  vals= list()
  for(i in 1:length(config_archimed_index)){
    last_vals= ifelse(i == length(config_archimed_index),length(config_no_comments),config_archimed_index[i+1]-1)
    i_vals= config_no_comments[(config_archimed_index[i]+1):last_vals]
    i_df=
      lapply(i_vals, function(x){
        x%>%gsub("\t"," ",.)%>%strsplit(.," ")%>%unlist()%>%t()%>%data.frame(stringsAsFactors = FALSE)
      })
    line_type= sapply(i_df, function(x){ifelse(length(x)>3,"scene","chaining")})
    vals_chaining= i_df[line_type=="chaining"]
    i_df= i_df[line_type=="scene"]%>%dplyr::bind_rows(.)%>%type.convert(.,as.is= TRUE)

    colnames(i_df)= c("sceneId", "plantId", "plantFileName", "x", "y", "z", "scale", "inclinationAzimut",
                      "inclinationAngle", "stemTwist")
    func_group= gsub("#[Archimed] ","",config_no_comments[config_archimed_index[i]], fixed = TRUE)
    vals[[i]]= i_df%>%dplyr::mutate(Functional_group= func_group)
  }
  vals= dplyr::bind_rows(vals)

  # No chaining:
  if(length(vals_chaining)==0){
    vals_chaining= NULL
  }

  list(dimensions= plot_dim, plants= vals, chaining= vals_chaining[[1]])
}

#' Write ops file
#'
#' @param data the data for the ops file, typically the output from `read_ops()` (see details).
#' @param file the path to the ops file
#'
#' @details The `data` argument expects a list of two `data.frame`s:
#' * dimensions: the plot dimensions (xmin, ymin, zmin, xmax, ymax);
#' * plants: a `data.frame` with the format:
#' \tabular{lllllllllll}{
#' *sceneId*  \tab *plantId* \tab *plantFileName*             \tab  x      \tab y      \tab z \tab scale \tab inclinationAzimut \tab inclinationAngle \tab stemTwist \tab Functional_group \cr
#' 1          \tab   1       \tab opf/DA1_Average_MAP_12.opf  \tab  2.3025 \tab 3.988  \tab 0 \tab 1     \tab 0                 \tab 0                \tab 0         \tab two              \cr
#' 1          \tab   2       \tab opf/DA1_Average_MAP_12.opf  \tab  6.9075 \tab 11.964 \tab 0 \tab 1     \tab 0                 \tab 0                \tab 0         \tab two
#' }
#' * and optionaly a chaining line, e.g. -1 1 1.
#'
#' @return
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(tidyverse)
#' library(archimedR)
#'
#' # Read the OPS:
#' OPS= read_ops("dummy.ops")
#' # Increase the position of the plants by 0.5 meter:
#' OPS$plants= OPS$plants%>%mutate(x= x+0.5)
#' # Write the new OPS:
#' write_ops(OPS, "dummy2.ops")
#' }
write_ops= function(data,file){

  data$dimensions= data$dimensions[,!grepl("area",colnames(data$dimensions))]
  ops_lines= c("# T xOrigin yOrigin zOrigin xSize ySize flat",
               paste(c("T", data$dimensions), collapse = " "))

  plants_groups= split(data$plants, data$plants$Functional_group)

  for(i in 1:length(plants_groups)){
    for(j in 1:nrow(plants_groups[[i]])){
      if(j==1){
        ops_lines= c(ops_lines,
                       paste("#[Archimed]",names(plants_groups)[i]),
                       paste("#sceneId plantId plantFileName x y z scale inclinationAzimut inclinationAngle stemTwist"))
      }

      ops_lines= c(ops_lines,paste(plants_groups[[i]][j,][,!grepl("Functional_group",colnames(plants_groups[[i]][j,]))],
                                   collapse = "\t"))
    }
  }

  if(!is.null(data$chaining)){
    ops_lines= c(ops_lines,
                 "#motherId sceneId date",
                 paste(data$chaining, collapse = "\t"))
  }

  write(ops_lines,file)

}

