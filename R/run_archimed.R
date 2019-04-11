#' Run ARCHIMED
#'
#' @param exe   Path to the ARCHIMED executable file including its name
#' @param memory The memory allocated to the JVM (Java Virtual Machine) in megabytes
#' @param config The configuration file. The function tries to find it if not provided. If the function
#' finds no configuration file, or several, it exits and returns `FALSE`.
#'
#'
#' @return The function prints the model outputs to the console and returns
#' `TRUE` if ARCHIMED ran successfully, or an error code if any problem occured
#' @export
#'
#' @note If you are using a version of ARCHIMED prior to 2018, you need to pass
#' `Archimed` in the configuration file, such as
#' `run_archimed(path= "archimed.jar", config= "Archimed app_parameters/ArchimedConfiguration.properties")`
#'
#' @examples
#' \dontrun{
#' run_archimed(path= "Archimed/archimed.jar", config= "app_parameters/config.properties")
#' }
run_archimed= function(exe= file.path(getwd(),'archimed-lib_florian-1.0.0.jar'),
                       memory= 4096, config= NULL){
  .= NULL
  wd= getwd()
  on.exit(setwd(wd))
  if(is.null(config)){
    config=
      list.files(getwd(),recursive = TRUE)%>%
      .[grep(".properties",.)]
    if(length(config)!=1){
      message(crayon::red("Failed")," to guess the configuration file, please set it as argument")
      return(FALSE)
    }
  }
  setwd(dirname(exe))
  out= system2(command = 'java',
               args = c(paste0('-Xmx',memory,'m'),'-jar',
                        basename(exe), config))
  if(out==0){
    TRUE
  }else{
    message("Error: ARCHIMED call ",crayon::red("failed"))
    return(out)
  }
}
