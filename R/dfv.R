# ' Launch data frame viewer user interface.
# '
# ' @export
dfv <- function(wsVariable = 'dfv_saved', storeModule = NULL) {
  # initialize new module for a data container bound to the GUI
  module <- Module$new(gui = DataFrameViewerGUI())
  
  # set default settings
  module$setSettings(
      wsVariable = wsVariable,
      windowSize = c(1024, 720),
      windowTitle = paste("Data Frame Viewer version", packageVersion('dfv')),
      lrPane = 0.2
    )
  
   # load if previous object exists and user wants to reload
   if (exists(wsVariable, envir=.GlobalEnv) && 
      gconfirm("A previous instance of the Data Frame Viewer is saved in this workspace. Would you like to load it?")) {
      obj <- get(wsVariable, envir=.GlobalEnv)
      module$settings <- modifyList(module$settings, obj$settings) # FIXME: implement a function to pass a whole list to setting settings!!
   }
  
  # store module if requested
  if (!missing(storeModule))
    assign(storeModule, module, envir=.GlobalEnv)
  
  # launch module
  module$launch()
}

# ' Development launch, stores the module in object dfv_test
# ' (if you need to access the gui, it will be in dfv_test$gui)
dfv.dev <- function() {
  dfv(storeModule = 'dfv_test')
}
