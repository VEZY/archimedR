#' Run ARCHIMED
#'
#' @param exe   Path to the ARCHIMED executable file including its name
#' @param memory The memory allocated to the JVM (Java Virtual Machine)
#'
#' @return The function prints the model outputs to the console and returns
#' `TRUE` if ARCHIMED ran successfully, or an error code if any problem occured
#' @export
#'
#' @examples
#' \dontrun{
#' run_archimed(path= "Archimed/archimed.jar")
#' }
run_archimed= function(exe= file.path(getwd(),'archimed-lib_florian-1.0.0.jar'),
                       memory= 4096){
  wd= getwd()
  on.exit(setwd(wd))
  setwd(dirname(exe))
  out= system2(command = 'java',
               args = c(paste0('-Xmx',memory,'m'),'-jar',
                        basename(exe),'Archimed'))
  if(out==0){
    TRUE
  }else{
    message("Error: ARCHIMED call ",crayon::red("failed"))
    return(out)
  }
}
