# ' Launch data frame viewer user interface.
# ' @param wsVariable - the name of the global variable to use when "saving workspace" from the Gui
# ' @param storeModule - [optional] the name of the variable to save the module in for direct manipulation of the Gui
# ' @param modal - [default: FALSE] - whether to start window in active or modal mode (latter can be used in command line script)
# ' @export
dfv <- function(wsVariable = 'dfv_saved', storeModule = NULL, modal = FALSE) {
  # set Gui options
  options("guiToolkit"="RGtk2") # everything is written in RGtk2
  
  # initialize new module for a data container bound to the Gui
  module <- Module$new(gui = DataFrameViewerGui())
  
  # set default settings
  module$setSettings(
    wsVariable = wsVariable,
    windowSize = c(1024, 720),
    windowTitle = paste("Data Frame Viewer version", packageVersion('dfv')),
    windowModal = modal,
    lrPane = 0.2, # left-right pane
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

# ' Development launch, stores the module in object dfv_test
# ' (if you need to access the gui, it will be in dfv_test$gui)
dfv.dev <- function() {
  dfv(storeModule = 'dfv_test')
}

# ' Debug message function
DEBUG <- TRUE
dmsg <- function(...) {
  if (DEBUG)
    message(list(...))
}

# Test data frame
dfv.test.df <- data.frame(
  ID=1:500,
  x=rnorm(n=500, m=3, sd=1), 
  y=rnorm(n=500, m=3, sd=1),
  info=c("Banjo", "Turtle", "Jetpack", "Ferret", "Pizza"))
