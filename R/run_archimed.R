#' Run ARCHIMED
#'
#' @param path   Path to the ARCHIMED root
#' @param memory The memory allocated to the JVM (Java Virtual Machine)
#'
#' @return The function prints the model outputs to the console and returns
#' `TRUE` if ARCHIMED ran successfully, or an error code if any problem occured
#' @export
#'
#' @examples
#' \dontrun{
#' run_archimed(path= "Archimed")
#' }
run_archimed= function(path= getwd(), memory= 4096){
  wd= getwd()
  setwd(path)
  out= system2(command = 'java',
               args = c(paste0('-Xmx',memory,'m'),'-jar',
                        'archimed-lib_florian-1.0.0.jar','Archimed'))
  setwd(wd)
  if(out==0){
    TRUE
  }else{
    message("Error: ARCHIMED call ",crayon::red("failed"))
    return(out)
  }
}
