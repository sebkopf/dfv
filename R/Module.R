#' A gui element data frame that has a whole user interface associated with it.
#' 
#' \code{Module$new()} initiates the module.
#' 
#' @method launch shows the user interface linked to this module
Module <- setRefClass(
  'Module',
  contains = 'GuiElementDataFrame',
  fields = list(
    gui = 'BaseGUI' # an S4 gui class
  ), 
  methods = list(
     initialize = function(...){
       callSuper(...)
       
       # so the GUI knows which module it belongs to (for multi module GUIs)
       gui@module <<- class(.self) 
       
       ### default setting for a module
       setSettings(
         windowSize = c(800, 600),
         windowTitle = "Module",
         windowModal = FALSE,
         protect = TRUE
       )
     },
    
     #' Get module (flexibility for multi module GUIs)
     getModule = function(name = 'Module') {
        return (.self) # standalone module always returns itself
     },
     
     #' Module make function
     makeGUI = function(...) {
       showGUI(gui, .self) # show module GUI
       # Note: the loadGUI() function is executed via the BaseGUI through the focusHandler to enable data loading in modal dialogs
     }
  )
)
