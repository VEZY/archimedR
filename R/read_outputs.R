#' Read plot-scale output
#'
#' @description Read the ARCHIMED output aggregated at plot-scale
#'
#' @param path  File path
#' @param sheet Desired Excel sheet
#'
#' @return The plot-scale ARCHIMED outputs
#' @export
#'
#' @examples
#' \dontrun{
#' read_out_plot(path= "output/2018.11.15_14-18-11_mirdata_46.xlsx", sheet= "Mir")
#' }
#'
read_out_plot= function(path, sheet= "Mir"){
  if(sheet== "Mir"){
    out= readxl::read_xlsx(path = path, sheet = sheet,skip= 2)[,1:12]
    colnames(out)= c("Step","Time","Rad_Inci_diff_Wm","Rad_Inci_dir_Wm","Rad_inci_glob_Wm",
                     "Rad_inci_glob_MJ","Rad_interc_veg_MJ","Rad_interc_veg_perc",
                     "Rad_interc_other_MJ","Rad_interc_other_perc",
                     "Rad_interc_tot_MJ","Rad_interc_tot_perc")
    times= stringr::str_split_fixed(out$Time," -> ",n= 2)%>%
      lubridate::parse_date_time2(orders= 'Y/m/d  H:M')
    hour_start= times[seq_along(times)%%2==1]
    hour_end= times[seq_along(times)%%2==0]
    out= data.frame(Step= out$Step, hour_start, hour_end, out[,-c(1,2)])
    return(out)
  }
}



#' Read node-scale output
#'
#' @description Read the ARCHIMED output at node-scale
#'
#' @param path      File path
#' @param duration  data.frame of two columns: the time-step, and its duration in seconds
#'
#' @details the duration data.frame must have two columns: step (step index) and timestep
#' (duration in seconds)
#' @return The ARCHIMED outputs for all nodes
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' read_out_node(path= "output/nodes_values.csv")
#' }
#'
read_out_node= function(path,duration=NULL){
  # if(is.null(duration)){
  #   import_meteo()
  # }
  data.table::fread(path,na.strings = c("null","NaN"),fill=TRUE, data.table = F)%>%
    dplyr::full_join(duration, by="step")%>%
    dplyr::mutate(globalIrradiation= .data$globalIrradiance*.data$timestep*10^-6, # MJ m-2[obj] timestep-1
                  Global_Intercepted= .data$globalIrradiation*.data$area,   # MJ obj-1 timestep-1
                  PAR_irradiance= .data$globalIrradiance*0.48*4.57,         # umol m-2[obj] s-1
                  PAR_tot= .data$PAR_irradiance+.data$PARscatteredIrr*0.48,       # umol m-2[obj] s-1
                  photosynthesis_rate= .data$photosynthesis/.data$timestep, # umol[C] m-2[obj] s-1 (check)
                  An_leaflet= .data$photosynthesis_rate*.data$area,               # umol[C] obj-1 s-1
                  transpiration= .data$transpiration*.data$area)            # mm obj-1 timestep-1
}


