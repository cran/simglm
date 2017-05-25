#' Run Shiny Application Demo
#' 
#' Function runs Shiny Application Demo
#' 
#' This function does not take any arguments and will run the Shiny Application.
#' If running from RStudio, will open the application in the viewer, 
#' otherwise will use the default internet browser.
#' 
#' @export
run_shiny <- function() {
  appDir <- system.file("shiny_examples", "demo", package = "simglm")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `simglm`.", 
         call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}
