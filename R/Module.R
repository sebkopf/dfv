#' A module is a GuiElement that has a whole user interface associated with it.
#' 
#' \code{Module$new()} initiates the module.
#' 
#' @method launch shows the user interface linked to this module
Module <- setRefClass(
  'Module',
  contains = 'GuiElement',
  fields = list(
    gui = 'BaseGui' # an S4 gui class
  ), 
  methods = list(
     initialize = function(...){
       callSuper(...)
       
       # so the Gui knows which module it belongs to (for multi module Guis)
       gui@module <<- class(.self) 
       
       ### default setting for a module
       setSettings(
         windowSize = c(800, 600),
         windowTitle = "Module",
         windowModal = FALSE,
         protect = TRUE
       )
     },
    
     #' Get module (flexibility for multi module Guis)
     getModule = function(name = 'Module') {
        return (.self) # standalone module always returns itself
     },
     
     #' Module make function
     makeGui = function(...) {
       showGui(gui, .self) # show module Gui
       # Note: the loadGui() function is executed via the BaseGui through the focusHandler to enable data loading in modal dialogs
     },
     
     #' By default, Modal Dialog fetches data and settings from its GuiElements when saving the Gui
     saveGui = function(fetchData = TRUE, fetchSettings = TRUE) {
       callSuper(fetchData = fetchData, fetchSettings = fetchSettings)
     }
  )
)
