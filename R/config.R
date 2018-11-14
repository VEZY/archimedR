#' Import ARCHIMED meteo file
#'
#' @param x The file path or connexion to the meteo file
#'
#' @details The function uses [data.table::fread()] under the hood, so the
#' `x` argument should match the input format as for this function.
#' @return
#' @export The meteo file in a more conveniant format for R usage
#'
#' @examples
#'  \dontrun{
#'  import_meteo("output/meteo.csv")
#'  }
#'
import_meteo= function(x){
  data.table::fread(x, data.table = F,na.strings = "")%>%
    mutate(date= lubridate::ymd(date),
           hour_start= lubridate::hms(hour_start),
           hour_end= lubridate::hms(hour_end),
           step= (1:n())-1,
           timestep= as.numeric(hour_end-hour_start))%>%
    tidyr::fill(date)
}


#' Read configuration
#'
#' @description Read the ARCHIMED configuration file parameters and
#'  return one, several or all parameters as a list or as a [tibble::tibble()]
#'
#' @param file      The path to the ARCHIMED configuration file
#' @param parameter A vector or list of parameter names. If `NULL`, returns all parameters.
#' @param format    The output format. Should be either "list" or "tibble".
#'
#' @details The parameter names are partially matched, so the user can retreive their values
#' without the need of perfectly knowing their names.
#' The configuration file is named "ArchimedConfiguration.properties".
#'
#' @return The ARCHIMED configuration parameters as a list or a [tibble::tibble()]
#'
#' @examples
#'  \dontrun{
#'  # Read two parameters, with partial matching (latit instead of latitude):
#'  read_config(file = "Archimed_July_2018/app_parameters/ArchimedConfiguration.properties",
#'              parameter = c("latit","altitude"))
#'  # read all parameters:
#'  read_config(file = "Archimed_July_2018/app_parameters/ArchimedConfiguration.properties")
#'
#'  }
#'
#' @export
read_config= function(file, parameter= NULL, format= c("list","tibble")){

  format= match.arg(format, c("list","tibble"))

  config=
    readLines(file)%>%
    .[grep("=",.)]%>%
    gsub("\t","",.)

  splitted= strsplit(config, "=")%>%unlist

  Out=
    tibble(param_names= splitted[seq_along(splitted)%%2==1],
           values= splitted[seq_along(splitted)%%2==0])%>%
    mutate(values_num= suppressWarnings(as.numeric(values)))%>%
    mutate(values= ifelse(!is.na(values_num),values_num,values))%>%
    select(-values_num)

  if(!is.null(parameter)){
    Out= Out[lapply(parameter, function(x){grep(x, Out$param_names)})%>%unlist,]
  }

  if(format=="list"){
    tmp= as.list(Out$values)
    names(tmp)= Out$param_names
    Out= tmp
  }

  return(Out)
}


#' Set the configuration
#'
#' @description Set a parameter value for the ARCHIMED configuration file
#'
#' @param file      The path to the ARCHIMED configuration file
#' @param parameter Parameter name
#' @param value     The new value of the parameter
#'
#' @details The configuration file is named "ArchimedConfiguration.properties".
#'
#' @return Nothing. Writes in the file directly.
#'
#' @export
set_config= function(file,parameter,value){
  config= readLines(file)
  param_index= grep(paste0(parameter,".{0,2}="),config)
  splitted= strsplit(config[param_index], "=")
  param= paste0(splitted%>%unlist()%>%.[1],"=",value)
  config[param_index]= param
  writeLines(config, con = file)
}








