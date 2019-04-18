#' Run ARCHIMED
#'
#' @param exe   Path to the ARCHIMED executable file including its name
#' @param memory The memory allocated to the JVM (Java Virtual Machine) in megabytes (see details)
#' @param config The configuration file. The function tries to find it if not provided. If the function
#' finds no configuration file, or several, it exits and returns `FALSE`.
#'
#'
#' @details The `memory` actually used to run ARCHIMED is the minimum between the input `memory` and
#' the available memory on the system (see `get_free_ram()`)
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

  mem= min(memory,get_free_ram()-1000)
  if(mem<memory){
    warning(paste("Not enough memory available on the system. Requested:",crayon::red(memory),
                  "Mo, memory actually allocated to ARCHIMED:"),crayon::red(mem)," Mo")

  }
  if(mem<1024){stop("Not enough memory available on the system to run ARCHIMED")}
  out= system2(command = 'java',
               args = c(paste0('-Xmx',min(memory,get_free_ram()),'m'),'-jar',
                        basename(exe), config))
  if(out==0){
    TRUE
  }else{
    message("Error: ARCHIMED call ",crayon::red("failed"))
    return(out)
  }
}





#' Get available RAM
#'
#' @description Get the RAM available on the system
#'
#' @details The function calls wmic for windows and awk for Linux. It is not
#' compatible with Mac OS yet.
#'
#' @return The amount of RAM available on the system in megabytes
#' @export
#'
#' @examples
#' get_free_ram()
get_free_ram <- function(){
  gc()
  if(Sys.info()[["sysname"]] == "Windows"){
    x <- system2("wmic", args =  "OS get FreePhysicalMemory /Value", stdout = TRUE)
    x <- x[grepl("FreePhysicalMemory", x)]
    x <- gsub("FreePhysicalMemory=", "", x, fixed = TRUE)
    x <- gsub("\r", "", x, fixed = TRUE)
    as.integer(as.integer(x)*10^-3)
  } else if(Sys.info()[["sysname"]] == "Linux"){
    as.integer(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",
                                 intern=TRUE))*10^-3)
  }else{
    stop("Cannot guess free memory from this OS")
  }
}
