#' A data frame that has a user interface associated with it.
#' 
#' \code{Module$new()} initiates the module.
#' 
#' @method showGUI shows the user interface (requires a Hub object, unless running in standalone mode)
Module <- setRefClass(
  'Module',
  contains = 'DataFrame',
  fields = list(
    gui = 'BaseGUI', # an S4 gui class
    widgets = 'list' # list of all the widgets to keep track of
  ), 
  methods = list(
     initialize = function(...){
       callSuper(...)
       gui@module <<- class(.self) # so the GUI knows which module it belongs to (for multi module GUIs)
       
       ### default setting for a module
       setSettings(
         windowSize = c(800, 600),
         windowTitle = "Module"
       )
     },
    
     getWidget = function(id) {
       return (widgets[[id]])
     },
     
     getWidgets = function(ids) {
       if (missing(ids))
         return (widgets)
       return (widgets[ids])
     },
     
     setWidgets = function(...) {
       # FIXME: allow both ... and list(a=b) to be passed in!
       widgets <<- modifyList(widgets, list(...))
     },
     
     cleanWidgets = function() {
       widgets <<- list()
     },
    
     #' Module launch function
     launch = function(name) {
       showGUI(gui, .self) # show module GUI
     }
  )
)
