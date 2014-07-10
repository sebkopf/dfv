#' Data Frame Viewer
#' 
#' Simple user interface to facilitate getting started with using R for data processing. Illustrates how to import data from Excel, melt data frames into plottable format, add additional information to the data and plot it using ggplot. Provides an easy system to keep track of multiple plots and save them in PDF format. Always shows the actual code that is executed to process or plot the data so users can experiment with changing the code directly and copy it to make their own data processing pipeline independent of this GUI.
#' 
#' The Data Frame Viewer can be run without any configuration for interacting with the data frames in any workspace.
#' 
#' @name dfv-package
#' @aliases dfv
#' @docType package
#' @title dfv package
#' @author Sebastian Kopf
#' @seealso \code{\link{dfv.start}}
#' @examples
#' \dontrun{\code{dfv.start()}}
NULL



#' Launch the data frame viewer (DFV) user interface.
#' @param wsVariable the name of the global variable to use when running "Save DFV" from the Gui 
#' @param storeModule the name of a variable (e.g. \code{dfv.obj}) that, if set, will be assigned the dfv gui object for command line manipulation
#' @param modal whether to start window in active or modal mode (latter can be useful in command line script for example), default \code{FALSE}
#' @param silent whether to be silent about information (set to \code{FALSE} for helpful debug messages), default \code{TRUE}
#' @export
#' @examples
#' \dontrun{dfv.start() # should work out of the box like this for most applications}
dfv.start <- function(wsVariable = 'dfv_saved', storeModule = NULL, modal = FALSE, silent = TRUE) {
  # set Gui options
  options("guiToolkit"="RGtk2") # everything is written in RGtk2
  
  # messages
  DEBUG <<- !silent
  
  # generate test data frame
  dfv.test.df <<- data.frame(
    ID=1:500,
    x=rnorm(n=500, m=3, sd=1), 
    y=rnorm(n=500, m=3, sd=1),
    info=c("Banjo", "Turtle", "Jetpack", "Ferret", "Pizza"))
  
  # initialize new module for a data container bound to the Gui
  module <- GuiModule$new(gui = new("DataFrameViewerGui"))
  
  # set default settings
  module$setSettings(
    wsVariable = wsVariable,
    windowSize = c(1024, 720),
    windowTitle = paste("Data Frame Viewer version", packageVersion('dfv')),
    windowModal = modal,
    lrPane = 0.4, # left-right pane
    rtbPane = 0.8, # top-bottom pane on the left
    ltbPane = 0.5 # top-bottom pane on the right
  )
  
  # load if previous object exists and user wants to reload
  if (exists(wsVariable, envir=.GlobalEnv) && 
        gconfirm("A previous instance of the Data Frame Viewer is saved in this workspace. Would you like to load it?")) {
    obj <- get(wsVariable, envir=.GlobalEnv)
    module$setSettings(obj$settings, overwriteProtected = FALSE)  # when auto reloading settings, important to not overwrite protected settings!
    module$setData(obj$data)
  }
  
  # store module if requested
  if (!missing(storeModule))
    assign(storeModule, module, envir=.GlobalEnv)
  
  # launch module
  module$makeGui()
}

#' Development launch, stores the module in object \code{dfv_test} and has debug messages turned on.
dfv.dev <- function() {
  dfv.start(storeModule = 'dfv_test', silent = FALSE)
}

