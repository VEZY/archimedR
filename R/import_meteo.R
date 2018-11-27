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
                  timestep= as.numeric(hour_end)-as.numeric(hour_start))%>%
    tidyr::fill(date)
}
