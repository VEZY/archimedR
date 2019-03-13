#' Temperature dynamic
#'
#' @description Simulate a temperature dynamic over a time period
#'
#' @param Tmax Maximum air temperature
#' @param Tmin Minimum air temperature
#' @param per  The sine curve periodicity
#' @param shift Phase shift, or when does Tmin/Tmax does occur ?
#'
#' @return A simulated temperature dynamics over a period of time
#' @export
#'
#' @examples
#' # Temperature dynamic over a day:
#' temperature_dynamic(30,18,24,3*pi/2)
temperature_dynamic= function(Tmax,Tmin,per,shift= 3*pi/2){
  A= (Tmax-Tmin)/2 # amplitude
  B= (2*pi)/per
  A*sin(B*(1:per) + shift) + (Tmax+Tmin)/2
}
