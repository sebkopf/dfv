#' Class representing a reference class DataFrame that holds both basic 
#' data information and settings information.
#' 
#' This is a reference class so can be passed around the easily GUI for storing information.
DataFrame <- setRefClass(
  'DataFrame', 
  fields = list(
    data = 'list',
    settings = 'list'
  ), 
  methods = list(
    initialize = function(...) {
      callSuper(...)
      
      ### default setting for a data frame
      setSettings(
        wsVariable = paste0(class(.self), "_saved")
      )
    },
    
    saveToWorkspace = function() {
      message("Saving data and settings to global variable ", getSetting('wsVariable'), ".")
      assign(getSetting('wsVariable'), list(
          data = data,
          settings = settings
        ), envir=.GlobalEnv)
      save.image()
    },
    
    getSetting = function(id) {
      return (settings[[id]])
    },
    
    getSettings = function(ids) {
      if (missing(ids))
        return (settings)
      return (settings[ids])
    },
    
    setSettings = function(...) {
      # FIXME: allow both ... and list(a=b) to be passed in!
      settings <<- modifyList(settings, list(...))
    },
    
    #' Get Data
    getData = function(id) {
      message("get Data not implemented yet")
    }
  )
)
