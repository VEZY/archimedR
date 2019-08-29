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
  plot_dim= config_no_comments[1]%>%gsub("T|flat","",.)%>%strsplit(.," ")%>%unlist%>%
    .[!.%in%""]%>%t()%>%data.frame(stringsAsFactors = FALSE)%>%type.convert(.,as.is= TRUE)
  colnames(plot_dim)= c("xmin", "ymin", "zmin", "xmax", "ymax")

  config_archimed_index= grep("#[Archimed]",config_no_comments, fixed = TRUE)

  vals= list()
  for(i in 1:length(config_archimed_index)){
    last_vals= ifelse(i == length(config_archimed_index),length(config_no_comments),config_archimed_index[i+1]-1)
    i_vals= config_no_comments[(config_archimed_index[i]+1):last_vals]
    i_df=
      lapply(i_vals, function(x){
        x%>%gsub("\t"," ",.)%>%strsplit(.," ")%>%unlist()%>%t()%>%data.frame(stringsAsFactors = FALSE)
      })%>%
      dplyr::bind_rows(.)%>%type.convert(.,as.is= TRUE)

    colnames(i_df)= c("sceneId", "plantId", "plantFileName", "x", "y", "z", "scale", "inclinationAzimut",
                      "inclinationAngle", "stemTwist")
    func_group= gsub("#[Archimed] ","",config_no_comments[config_archimed_index[i]], fixed = TRUE)
    vals[[i]]= i_df%>%mutate(Functional_group= func_group)
  }
  vals= bind_rows(vals)

  list(dimensions= plot_dim, plants= vals)
}
