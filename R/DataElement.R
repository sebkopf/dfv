#' Class representing a reference class DataElement that holds both basic 
#' data information and settings information.
#' 
#' This is a reference class so can be passed around the easily GUI for storing information.
DataElement <- setRefClass(
  'DataElement', 
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
        wsVariable = paste0(class(.self), "_saved"), # global variable in workspace to save to
        wsFilePath = paste0(class(.self), "_saved.R") # file path in workspace to save to
      )
    },
    
    saveToWorkspace = function() {
      if (!is.null(getSettings('wsVariable'))) {
        message("Saving data and settings into global variable ", getSettings('wsVariable'), ".")
        assign(getSettings('wsVariable'), list(
            data = data,
            settings = settings
          ), envir=.GlobalEnv)
        save.image()
      }
      if (!is.null(getSettings('filePath'))) {
        message("Saving data and settings to file ", getSettings('filePath'), ".")
        save(data, settings, file = getSettings('filePath'))
      }
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
    #       by default this is TRUE, only auto-load does NOT overwrite protected settings
    # '@param protectSettings (if TRUE, will protect all the settings newly set)
    setSettings = function(..., overwriteProtected = TRUE, protect = FALSE) {
      sets <- list(...)
      
      # FIXME: implement this properly with using nargs() == 3 to check if there is only one variable passed in! can then access it via ..1
      
      # If there is only one variable passed and that is an unnamed list, take that list directly to modify the settings
      if (length(sets) == 1 && class(sets[[1]]) == 'list' && is.null(names(sets)[1]))
        sets <- sets[[1]]
      
      # protected settings
      if (overwriteProtected == TRUE && length(protected) > 0 && length(owrite <- intersect(names(sets), protected)) > 0) 
        dmsg("For class ", class(.self), " instance, overwriting protected settings: ", paste(owrite, collapse=", "))
      else if (overwriteProtected == FALSE)
        sets <- sets[setdiff(names(sets), protected)] # exclude protected settings
      
      # update settings
      settings <<- modifyList(settings, sets)
      
      # protect what's required to protect
      if (protect && length(ids <- names(sets)) > 0) 
        protectSettings(ids)
    },
    
    # 'Protected settings are never overwritten by an autosave
    # 'This is very useful for defining unchangable settings and should be used for all settings that are NOT user adjustable.
    # '@ids - ids of the settings to protected, if missing, all currently defined settings get protected
    protectSettings = function(ids) {
      if (missing(ids))
        ids <- names(settings)
      if ( length(pro <- setdiff(ids, protected)) > 0 ) { # which ones are newly protected?
        dmsg("For class ", class(.self), " instance, protecting the following settings: ", paste(pro, collapse=", "))
        protected <<- c(protected, pro)
      }
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
        sets <- sets[[1]] 
      
      # will overwrite all data entries (no selective modification like for the settings)
      # NOTE: this is a key different between a setting and data!
      # FIXME: this is only really necessary because of the limitiation that modifyList does not work if there are data.frames deeper in the list structure
      for (key in names(sets))
        data[[key]] <<- sets[[key]]
    }
  )
)
