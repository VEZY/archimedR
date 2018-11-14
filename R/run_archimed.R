#' Run ARCHIMED
#'
#' @param path Path to the ARCHIMED root
#'
#' @return The function prints the model outputs to the console and returns
#' `TRUE` if ARCHIMED ran successfully, or an error if any problem occured
#' @export
#'
#' @examples
#' \dontrun{
#' run_archimed(path= "Archimed")
#' }
run_archimed= function(path= getwd()){
  wd= getwd()
  setwd(path)
  out= system2('java', args = c('-Xmx1024m','-jar','archimed-lib_florian-1.0.0.jar','Archimed'))
  setwd(wd)
  if(out==0){
    TRUE
  }else{
    stop("ARCHIMED call ",crayon::red("failed"))
  }
}
