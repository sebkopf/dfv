#' Class representing a reference class DataFrame that holds both basic 
#' data information and settings information.
#' 
#' This is a reference class so can be passed around the easily GUI for storing information.
DataFrame <- setRefClass(
  'DataFrame', 
  fields = list(
    data = 'list',
    settings = 'list',
    protected = 'character' # protected settings
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
    # '@param overwriteProtected (whether to overwrite protected settings or not)
    # '@param protectSettings (if TRUE, will protect all the settings newly set)
    setSettings = function(..., overwriteProtected = FALSE, protect = FALSE) {
      sets <- list(...)
      # If there is only one variable passed and that is an unnamed list, take that list directly to modify the settings
      if (length(sets) == 1 && class(sets[[1]]) == 'list' && is.null(names(sets)[1]))
        sets <- sets[[1]]
      
      # check for protected
      if (overwriteProtected == FALSE && length(protected) > 0 && length(skip <- intersect(names(sets), protected)) > 0) {
        dmsg("For class ", class(.self), " instance, NOT overwriting protected settings: ", paste(skip, collapse=", "))
        sets <- sets[names(sets)[which(!names(sets)%in%skip)]] # exclude protected settings
      }
      
      # update settings
      settings <<- modifyList(settings, sets)
      
      # protect what's required to protect
      if (protect && length(ids <- names(sets)) > 0) 
        protectSettings(ids)
    },
    
    # 'Protected settings are never overwritten by the setSettings method (e.g. by an autosave or such) unless it's specified (overwriteProtected = TRUE)
    # 'This is very useful for defining unchangable settings and should be used for all settings that are NOT user adjustable.
    # '@ids - ids of the settings to protected, if missing, all currently defined settings get protected
    protectSettings = function(ids) {
      if (missing(ids))
        ids <- names(settings)
      dmsg("For class ", class(.self), " instance, protecting the following settings: ", paste(ids, collapse=", "))
      protected <<- c(protected, ids)
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
