#' A data frame that is linked to an element of the user interface (which can contain widgets and other GuiElements)
#' 
GuiElementDataFrame <- setRefClass(
  'GuiElementDataFrame',
  contains = 'DataFrame',
  fields = list(
    widgets = 'list', # list of all the widgets to keep track of
    elements = 'list' # list of all the sub GuiElements to keep track of
  ), 
  methods = list(
    #' overwrite to get settings from all elements when calling getSettings without specifying fields
    getSettings = function(ids) {
      if (missing(ids)) { # get all settings
        allSettings <- modifyList(list(), settings)
        for (ele in names(elements)) {
          s <- list()
          s[[ele]] <- elements[[ele]]$getSettings()
          allSettings <- modifyList(allSettings, s)
        }
        return(allSettings)
      } else
        return(callSuper(ids))
    },
    
    #' overwrite to get data from all elements when calling getData without specifying fields
    getData = function(ids) {
      if (missing(ids)) { # get all data
        allData <- modifyList(list(), data)
        for (ele in names(elements)) {
          d <- list()
          d[[ele]] <- elements[[ele]]$getData()
          allData <- modifyList(allData, d)
        }
        return(allData)
      } else
        return(callSuper(ids))
    },
    
    getElements = function(ids) {
      if (missing(ids))
        return (elements)
      else if (length(ids) == 1)
        return (elements[[ids]]) 
      return (elements[ids])
    },
    
    # 'Set sub elements into the element data frame
    # '@param: passSettings - whether to pass settings with the same id as the element to the element
    # '@param: passData - whether to pass data with the same id as the element to the element
    setElements = function(passSettings = TRUE, passData = TRUE, ...) {
      sets <- list(...)
      if (length(sets) == 1 && class(sets[[1]]) == 'list' && is.null(names(sets)[1])) 
        sets <- sets[[1]]
      elements <<- modifyList(elements, sets)
      
      # passing settings and data
      for (id in names(sets)) {
        if (passSettings && !is.null(s <- getSettings(id)) && class(s) == 'list')
          elements[[id]]$setSettings(s)
        if (passData && !is.null(d <- getData(id)) && class(d) == 'list')
          elements[[id]]$setData(d)
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
        widgets <<- modifyList(widgets, sets[[1]]) 
      else
        widgets <<- modifyList(widgets, sets)
    },
    
    # '@Param: value - if missing, take value from the field of the same id stored in $data
    loadWidget = function(id, value) {
      # check if value is supplied or availabe in the data
      if (missing(value)) {
        if (is.null(data[[id]])) {
          message("WARNING: trying to load widget", id, "but no value supplied and no corresponding data field found in this GuiElementDataFrame")
          return(NULL)
        } else 
          value <- data[[id]]
      }
      
      # set widget value
      if (class(widgets[[id]])[[1]]=="gTable") { # gtable style widgets
        if (class(value)=="list") # single record
          widgets[[id]][] <<- data.frame(value, stringsAsFactors=FALSE)  
        else # multiple records
          widgets[[id]][] <<- value  
      } else # all other widgets
        svalue(widgets[[id]]) <<- value
    },
    
    # 'Load widgets data (widgetids provided without a key are autloaded from data with the same name)
    # '@code: widgetid = value, widgetid2 = value, 'widgetid3',  ...  
    loadWidgets = function(...) {
      # FIXME: allow both ... and list(a=b) to be passed in! (see setWigets for a fix)
      fields <- list(...)
      for (i in 1:length(fields)) {
        print(names(fields)[i])
        if (is.null(names(fields)[i]) || names(fields)[i] == '')
          loadWidget(fields[[i]]) # load widget from corresponding data field
        else
          loadWidget(names(fields)[i], fields[[i]]) # load widget with passed in value
      }
    },
    
    # 'Load all widgets that have corresponding ids in the data field with the values from their data counterparts.
    autoLoadWidgets = function() {
      for (id in intersect(names(widgets), names(data)))
        loadWidget(id)
    },
    
    # ' get widget value (returns gTable always as data frame)
    getWidgetValue = function(id) {
      if (class(widgets[[id]])[[1]]=="gTable") #in case it's a gTable(i.e. data frame), have to access info slightly differently
        if (class(widgets[[id]][])=="list") # single record is returned as a list rather than a dataframe
          return (data.frame(widgets[[id]][], stringsAsFactors=FALSE))
      else # multiple records in table are returned properly as data frame
        return (widgets[[id]][]) 
      else
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
    
    # save wiget values to data array
    saveWidgets = function(ids) {
      setData(getWidgetValues(ids))
    },
    
    # 'Cleans all widgets in this GuiElement and all contained GuiElements
    cleanWidgets = function() {
      for (element in elements) 
        element$cleanWidgets()
      widgets <<- list()
    },
    
    # make gui
    makeGUI = function(...) {
      message("\tMaking Gui Element for ", class(.self))
    }
  )
)
