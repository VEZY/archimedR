#' Import ARCHIMED meteo file
#'
#' @param x The file path or connexion to the meteo file
#'
#' @details The function uses [data.table::fread()] under the hood, so the
#' `x` argument should match the input format as for this function.
#' @return The meteo file in a more conveniant format for R usage
#'
#' @examples
#'  \dontrun{
#'  import_meteo("output/meteo.csv")
#'  }
#'
#' @export
import_meteo= function(x){
  hour_start= hour_end= n= NULL
  data.table::fread(x, data.table = F,na.strings = "")%>%
    dplyr::mutate(date= lubridate::ymd(date),
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
#' @details The `list` output format ensures that numeric parameters are treated as is. On
#' the contrary, the [tibble::tibble()] output will return all parameters as characters if
#' at least one is found to be character. This behavior is used to ensure a consistant expected
#' output.
#' The parameter names are partially matched, so the user can retreive their values
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
  values_num= values= NULL
  format= match.arg(format, c("list","tibble"))

  config=
    readLines(file)%>%
    .[grep("=",.)]%>%
    gsub("\t","",.)

  splitted= strsplit(config, "=")%>%unlist

  Out=
    tibble::tibble(param_names= splitted[seq_along(splitted)%%2==1],
                   values= splitted[seq_along(splitted)%%2==0])%>%
    dplyr::mutate(values_num= suppressWarnings(as.numeric(values)))

  if(!is.null(parameter)){
    Out=
      Out[lapply(parameter, function(x){grep(x, Out$param_names)})%>%unlist,]%>%
      dplyr::mutate(values= ifelse(!is.na(values_num),values_num,values))%>%
      dplyr::select(-values_num)
  }

  if(format=="list"){
    tmp=
      Out%>%dplyr::pull(values)%>%as.list()%>%
      lapply(., function(x){
        suppressWarnings(ifelse(is.na(as.numeric(x)),x,as.numeric(x)))
      })
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
  .= NULL
  config= readLines(file)
  param_index= grep(paste0(parameter,".{0,2}="),config)
  splitted= strsplit(config[param_index], "=")
  param= paste0(splitted%>%unlist()%>%.[1],"=", format(value, scientific= F))
  config[param_index]= param
  writeLines(config, con = file)
}








