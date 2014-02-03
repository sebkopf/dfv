#' A data frame that is linked to an element of the user interface (which can contain widgets and other GuiElements)
#' 
GuiElement <- setRefClass(
  'GuiElement',
  contains = 'DataElement',
  fields = list(
    widgets = 'list', # list of all the widgets to keep track of
    elements = 'list' # list of all the sub GuiElements to keep track of
  ), 
  methods = list(
    
    # 'Override to pull data and settings also from all sub Elements
    saveToWorkspace = function() {
      saveGui(fetchData = TRUE, fetchSettings = TRUE) # save the Gui (transfers information from the widgets to the settings and data lists)
      callSuper() # actually save to workspace
    },
    
    #' overload to get settings from all elements when calling getSettings without specifying fields
    getSettings = function(ids) {
      if (missing(ids)) { # get all settings
        allSettings <- c(list(), settings)
        for (ele in names(elements)) {
          # remove old settings for the element
          allSettings[[ele]] <- NULL
          
          # get new settings for the element
          s <- list()
          s[[ele]] <- elements[[ele]]$getSettings()
          
          # combine
          allSettings <- c(allSettings, s)
        }
        return(allSettings)
      } else
        return(callSuper(ids))
    },
    
    #' overload to get data from all elements when calling getData without specifying fields
    getData = function(ids) {
      if (missing(ids)) { # get all data
        allData <- c(list(), data)
        for (ele in names(elements)) {
          # remove old data for the element
          allData[[ele]] <- NULL
          
          # get new data
          d <- list()
          d[[ele]] <- elements[[ele]]$getData()
          
          # combine
          allData <- c(allData, d)
        }
        return(allData)
      } else
        return(callSuper(ids))
    },
    
    # 'Get the elements
    getElements = function(ids) {
      if (missing(ids))
        return (elements)
      else if (length(ids) == 1)
        return (elements[[ids]]) 
      return (elements[ids])
    },
    
    # 'Set sub elements into the element data frame
    # '@param: passSettings - whether to pass settings with the same id as the element to the element
    # '@param: overwriteProtectedSetting [default: FALSE] - when passing settings, whether to overwrite protected ones or not
    # '@param: passData - whether to pass data with the same id as the element to the element
    setElements = function(..., passSettings = TRUE, overwriteProtectedSettings = FALSE, passData = TRUE) {
      sets <- list(...)
      if (length(sets) == 1 && class(sets[[1]]) == 'list' && is.null(names(sets)[1])) 
        sets <- sets[[1]]
      elements <<- modifyList(elements, sets)
      
      # passing settings and data
      for (id in names(sets)) {
        if (passSettings && !is.null(s <- getSettings(id)) && class(s) == 'list')
          elements[[id]]$setSettings(s, overwriteProtected = overwriteProtectedSettings)
        if (passData && !is.null(d <- getData(id)) && class(d) == 'list')
          elements[[id]]$setData(d)
      }
    },
    
    # 'Remove sub elements from the GuiElement
    removeElements = function(ids, cleanData = TRUE, cleanSettings = TRUE) {
      for (id in ids) {
        elements[[id]]$destroyGui() # destroy widgets
        if (cleanData)
          data[[id]] <<- NULL # clean data
        if (cleanSettings)
          settings[[id]] <<- NULL # clean settings
        elements[[id]] <<- NULL # remove element
      }
    },
    
    getWidgets = function(ids) {
      if (missing(ids))
        return (widgets)
      else if (length(ids) == 1)
        return (widgets[[ids]])
      return (widgets[ids])
    },
    
    setWidgets = function(...) {
      sets <- list(...)
      # If there is only one variable passed and that is an unnamed list, take that list directly to modify the settings
      if (length(sets) == 1 && class(sets[[1]]) == 'list' && is.null(names(sets)[1]))
        sets <- sets[[1]]
        
      widgets <<- modifyList(widgets, sets) 
      if (length(sets) == 1)
        return (getWidgets(names(sets)[1])) # if setting a single widget, return it so it can be operated on right away
      else
        return (NULL)
    },
    
    # '@Param: value - if missing, take value from the field of the same id stored in $settings or $data (settings takes precedence)
    loadWidget = function(id, value) {
      # check if widget is set
      if (is.null(widgets[[id]])) {
        dmsg("trying to load widget '", id, "' but widget does not exist") 
        return(NULL)
      }
      
      # check if value is supplied or availabe in the data
      if (missing(value)) {
        if (is.null(value <- settings[[id]]) && is.null(value <- data[[id]])) { # settings first, then data
          message("WARNING: trying to load widget '", id, "' but no value supplied and no corresponding settings or data field found in this GuiElement")
          return(NULL)
        } 
      } else if (is.null(value)) {
        dmsg("trying to load widget '", id, "' but value supplied is NULL --> skip") #FIXME?
        return(NULL)
      }
      
      # set widget value
      if (class(widgets[[id]])[[1]]=="gTable") { # gtable style widgets
        if (class(value)=="list") # single record
          widgets[[id]][] <<- data.frame(value, stringsAsFactors=FALSE)  
        else # multiple records
          widgets[[id]][] <<- value  
      } else if (class(widgets[[id]])[[1]]=="gTree") {
        if (length(value) > 0) # accounts for svalue = integer(0)
          svalue(widgets[[id]], index=TRUE) <<- value
        # NOTE: not actively setting it to NULL because neither svalue(gtree) <- NULL nor svalue(gtree) <- integer(0) work without warning
      } else # all other widgets
        svalue(widgets[[id]]) <<- value
    },
    
    # 'Load widgets data (widgetids provided without a key are autloaded from data with the same name)
    # '@param ... : options
    #     - widgetid = value, widgetid2 = value, 'widgetid3',  ...  
    #     - list(widgetid = value, widgetid2 = value, 'widgetid3;)
    loadWidgets = function(...) {
      fields <- list(...)
      if (length(fields) == 1 && class(fields[[1]]) == 'list' && is.null(names(fields)[1])) 
        fields <- fields[[1]]
      
      for (i in 1:length(fields)) {
        if (is.null(names(fields)[i]) || names(fields)[i] == '')
          loadWidget(fields[[i]]) # load widget from corresponding data field
        else
          loadWidget(names(fields)[i], fields[[i]]) # load widget with passed in value
      }
    },
    
    # 'Load all widgets that have corresponding ids in the data field with the values from their settings / data counterparts.
    autoloadWidgets = function() {
      settingIds <- intersect(names(widgets), names(settings))
      dataIds <- intersect(names(widgets), names(data))
      
      # info messages
      dmsg("For ", class(.self), ", auto-loading widgets...")
      if (length(settingIds) > 0)
        dmsg("\tfrom settings: ", paste(settingIds, collapse=", "))
      if (length(dataIds) > 0) 
        dmsg("\tfrom data: ", paste(dataIds, collapse=", "))
      
      # load widgets
      for (id in union(settingIds, dataIds))
        loadWidget(id)
    },
    
    # ' get widget value (returns gTable always as data frame)
    getWidgetValue = function(id) {
      if (class(widgets[[id]])[[1]]=="gTable") { #in case it's a gTable(i.e. data frame), have to access info slightly differently
        if (class(widgets[[id]][])=="list") { # single record is returned as a list rather than a dataframe
          return (data.frame(widgets[[id]][], stringsAsFactors=FALSE))
        } else { # multiple records in table are returned properly as data frame
          return (widgets[[id]][]) 
        }
      } else if (class(widgets[[id]])[[1]]=="gTree") {
        return (svalue(widgets[[id]], index=TRUE)) # for gtree, return index
      } else
        return(svalue(widgets[[id]]))
    },
    
    # get widgets into list
    getWidgetValues = function(ids) {
      return (sapply(ids, function(id) {
        tryCatch(list(getWidgetValue(id)),
                 warning=function(w) { return(NA) } ) # coerce with NA values      
      }))
    },
    
    # get widgets into dataframe 
    # WARNING: will collapse any widgets that are tables down to NULL, if you want to preserve those, use getWidgetValues instead to get a list
    getWidgetValuesAsDF = function(ids) {
      #FIXME: actively kick out any entries that are from tables so this call doesn't crash if there are data.frames!
      return (data.frame(getWidgetValues(ids), stringsAsFactors=TRUE))
    },
    
    # save widget values to settings and data arrays if the fields are defined (settings takes presedence over data!)
    saveWidgets = function(ids) {
      if (missing(ids)) # save all widgets that have data/settings links
        ids <- names(widgets)
      dmsg("For ", class(.self), ", auto-saving widgets")
      settingIds <- intersect(ids, names(settings))
      dataIds <- intersect(setdiff(ids, settingIds), names(data))
      
      # save settings
      if (length(settingIds) > 0) {
        dmsg("\tto settings: ", paste(settingIds, collapse=", "))
        setSettings(getWidgetValues(settingIds))
      }
      
      # save rest to data if defined in the data arrays
      if (length(dataIds) > 0) {
        dmsg("\tto data: ", paste(dataIds, collapse=", "))
        setData(getWidgetValues(dataIds))
      }
    },
    
    # make gui
    makeGui = function(...) {
      message("\tMaking Gui Element for ", class(.self))
    },
    
    # 'Destroys all widgets in this GuiElement and all contained GuiElements
    # 'FIXME: does this require more implicit 'destroy' treatment for the widgets to unrealize them properly?
    destroyGui = function() {
      for (ele in elements) 
        ele$destroyGui()
      widgets <<- list()
    },
    
    # load (autoload by default, including all elements)
    loadGui = function() {
      autoloadWidgets()
      for (ele in elements) {
        if (length(ele$widgets) > 0) # only load if there are any widgets, i.e. gui is actualyl initialized
          ele$loadGui()
      }
    },
    
    # save (save all widgets by default, including all elements)
    #'@param fetchData/Settings - whether to retrieve all the data/settings in all sub elements and store them in this GuiElement's settings (use for top level save)
    #'        Note: default is FALSE for standard GuiElements, TRUE for Module and ModalDialogs
    saveGui = function(fetchData = FALSE, fetchSettings = FALSE) {
      saveWidgets()
      for (ele in elements) {
        if (length(ele$widgets) > 0) # only save if there are any widgets, i.e. gui is actualyl initialized
          ele$saveGui()
      }
      if (fetchData)
        data <<- getData() # fetch data from all widgets in all elements
      if (fetchSettings)
        settings <<- getSettings() # fetch settings from all widgets in all elements
    }
  )
)
