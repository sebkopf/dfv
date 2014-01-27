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
      message("Saving data and settings into global variable ", getSettings('wsVariable'), ".")
      assign(getSettings('wsVariable'), list(
          data = data,
          settings = settings
        ), envir=.GlobalEnv)
      save.image()
    },
    
    getSettings = function(ids) {
      if (missing(ids))
        return (settings)
      else if (length(ids) == 1)
        return (settings[[ids]]) 
      return (settings[ids])
    },
    
    # 'Pass in either named settings 
    # 'Code: setSettings(a=5, b='test', ...)
    # 'or pass in just a single list
    # 'Code: setSettings(list(a=5, b='test', ...))
    setSettings = function(...) {
      sets <- list(...)
      # If there is only one variable passed and that is an unnamed list, take that list directly to modify the settings
      if (length(sets) == 1 && class(sets[[1]]) == 'list' && is.null(names(sets)[1]))
        settings <<- modifyList(settings, sets[[1]]) 
      else
        settings <<- modifyList(settings, sets)
    },
    
    getData = function(ids) {
      if (missing(ids))
        return (data)
      else if (length(ids) == 1)
        return (data[[ids]])
      return (data[ids])
    },
    
    # 'Same as for the settings
    setData = function(...) {
      sets <- list(...)
      # If there is only one variable passed and that is an unnamed list, take that list directly to modify the settings
      if (length(sets) == 1 && class(sets[[1]]) == 'list' && is.null(names(sets)[1]))
        data <<- modifyList(data, sets[[1]]) 
      else
        data <<- modifyList(data, sets)
    }
  )
)
