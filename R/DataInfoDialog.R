DataInfoDialogGui <- setClass("DataInfoDialogGui", contains="ModalDialogGui")

setMethod("getToolbarXML", "DataInfoDialogGui", function(gui, module) {
  return (
    '<separator expand="true"/>
    <toolitem action="Copy"/>
    <separator expand="true"/>
    <toolitem action="Run"/>
    <separator expand="true"/>
    <toolitem action="Ok"/>
    <separator expand="true"/>
    </toolbar>
    <toolbar name="toolbar2">
    <separator expand="true"/>
    <toolitem action="Add"/>
    <separator expand="true"/>
    <toolitem action="Remove"/>
    <separator expand="true"/>')
})

setMethod("getMenuXML", "DataInfoDialogGui", function(gui, module) {
  return (
    '<menu name = "Map" action="Map">
    <menuitem action="Add"/>
    <menuitem action="Remove"/>
    </menu>
    <menu name = "Code" action="Code">
    <menuitem action="Copy"/>
    <menuitem action="Run"/>
    </menu>'
  )
})

setMethod("makeNavigation", "DataInfoDialogGui", function(gui, module) {
  uimanager <- callNextMethod()
  # top toolbar
  toolbarGrp <- getWidgets(gui, module, 'topToolbarGrp')
  getToolkitWidget(toolbarGrp)$packStart (uimanager$getWidget ( "/toolbar2" ), TRUE) # add toolbar
  return(uimanager)
})

setMethod("setNavigationActions", "DataInfoDialogGui", function(gui, module, actionGrp) {
  callNextMethod()
  nav.actions <-
    list(## name, icon, label , accelerator , tooltip , callback
      list ("Map", NULL , "_Map" , NULL, NULL, NULL),
      list ("Add", "gtk-add", "Add map entry", "<ctrl>A", "Add another entry to the mapping table", function(...) getModule(gui, module)$addMapEntry()),
      list ("Remove", "gtk-remove", "Remove map entry", "<ctrl>D", "Remove the selected entry from the mapping table", function(...) getModule(gui, module)$removeMapEntry()),
      list ("Code", NULL , "_Code" , NULL, NULL, NULL),
      list ("Run", "gtk-execute", "Run code", "<ctrl>R", "Execute code for tab", function(...) getModule(gui, module)$runCode(global = TRUE) ),
      list ("Copy", "gtk-copy", "Copy code", "<ctrl>C", "Copy code to clipboard", 
            function(...) {
              copyToClipboard(getModule(gui, module)$getWidgetValue('code'))
              showInfo(gui, module, msg="INFO: code copied to clipboard.", okButton = FALSE, timer = 2) 
            }))
  actionGrp$addActions(nav.actions)
})


setMethod("makeMainGui", "DataInfoDialogGui", function(gui, module) {
  setMenuGroup(gui, module, ggroup(horizontal=FALSE, cont=getWinGroup(gui, module), spacing=0))
  topGrp <- ggroup(horizontal=FALSE, spacing=0, expand=TRUE)
  bottomGrp <- ggroup(horizontal=FALSE, spacing=0, expand=TRUE)
  tbPane <- gpanedgroup(topGrp, bottomGrp, cont=getWinGroup(gui, module), expand=TRUE, horizontal=FALSE)
  
  # groups
  optionsGrp <- gframe("Select", container = topGrp, horizontal = TRUE, expand = FALSE)
  mapGrp <- gframe("Mapping Table", container = topGrp, expand = TRUE)
  setWidgets(gui, module, topToolbarGrp = ggroup(horizontal=FALSE, cont=topGrp, spacing=0))
  dataGrp <- gframe("Data")
  codeGrp <- gframe("Code", expand=TRUE, horizontal = FALSE)
  tbPane2 <- gpanedgroup(dataGrp, codeGrp, cont = bottomGrp, horizontal = FALSE, expand=TRUE)
  setWidgets(gui, module, tbPane = tbPane, tbPane2 = tbPane2, mapGrp = mapGrp, dataGrp = dataGrp)
  
  # options table
  glabel("  Data frame:", cont = optionsGrp)
  df.box <- gcombobox(sort(names(which(sapply(.GlobalEnv, is.data.frame)))), selected = 0, cont = optionsGrp, 
                      handler = function(h, ...) getModule(gui, module)$loadDataFrame())
  glabel("  Variable:", cont = optionsGrp)
  df.mod <- gedit("", cont = optionsGrp)
  setWidgets(gui, module, dframe = df.box, dframe.mod = df.mod)
  
  # columns table
  map <- DataTable$new()
  setElements(gui, module, mapTable = map)
  map$makeGui(mapGrp)
  map$setSettings(editableColumns = names(map$getData('frame')), resizable = TRUE)
  
  # data table
  dataT <- DataTable$new()
  setElements(gui, module, dataTable = dataT)
  dataT$setSettings(sortable = TRUE, resizable = TRUE)
  
  # code (attributes don't seem to work sadly)
  setWidgets(gui, module, code = gtext('', wrap=TRUE, font.attr = c(style="normal", weights="bold",sizes="medium"), container = codeGrp, expand = TRUE, height=50))
  setWidgets(gui, module, showData = gcheckbox("Automatically preview data (this can be very slow for large data frames)", handler = function(h, ...) getModule(gui, module)$generateCode(), cont = codeGrp))
})

DataInfoDialog <- setRefClass(
  'DataInfoDialog',
  contains = 'ModalDialog',
  methods = list(
    initialize = function(gui = DataInfoDialogGui(), ...){
      callSuper(gui = gui, ...)
      
      ### overwrite default setting for DataInfoDialog
      setSettings(
        windowSize = c(700, 700),
        windowTitle = "Map information to data frames",
        ok.label = "Done",
        ok.tooltip = "Close mapping window window",
        protect = TRUE
      )
      
      # new option (not protected, can be overwritten by user preference)
      setSettings(
        tbPane2 = 0.4,
        tbPane = 0.4
      ) 
      
      # default data for the map info dialog and its elements
      setData(
        dframe = sort(names(which(sapply(.GlobalEnv, is.data.frame))))[1], # data frame selected by default
        showData = TRUE, # whether to show data automatically
        mapTable = list(
          frame = data.frame(
            Lookup = factor(),
            Match = character(),
            Regexp = logical(),
            Property = character(),
            Value = character(),
            stringsAsFactors = F
          )),
        dataTable = list(
          frame = data.frame(Data = character(0), Frame = character(0)))
      )
    },
    
    # ' Load the layout of the mapping table for a new data frame
    loadDataFrame = function() {
      df.name <- getWidgetValue('dframe')
      loadWidgets(dframe.mod = paste0(df.name, '.mod'))
      
      # load map table
      map <- data.frame(
        Lookup = factor(levels = names(get(df.name, envir=.GlobalEnv))),
        Match = character(), Regexp = logical(), Property = character(), Value = character(), stringsAsFactors = F)
      mapTable <- getElements('mapTable')
      mapTable$destroyGui()
      mapTable$setData(frame = map)
      mapTable$makeGui(getWidgets('mapGrp'), changedHandler = function(...) generateCode())
      mapTable$loadGui()
      
      # reset code and destroy data table
      loadWidgets(code = '')
      getElements('dataTable')$destroyGui()
    },
    
    # ' Add an entry to the map table
    addMapEntry = function() {
      mapTable <- getElements('mapTable')
      if (!is.null(sel <- mapTable$getSelectedValues())) {
        sel$Match <- ''
        sel$Value <- ''
        row <- mapTable$addRow(sel)
      } else
        row <- mapTable$addRow(Match = '1, 3, 8, 15:20', Property = 'new', Value = 'value', Regexp = FALSE)
      mapTable$selectRows(row)
      generateCode()
    },
    
    # ' Remove an entry in the map table
    removeMapEntry = function() {
      mapTable <- getElements('mapTable')
      row <- mapTable$getSelectedRows()
      mapTable$removeRows(row)
      mapTable$selectRows(row)
      generateCode()
    },
    
    # ' Generate the code for excel import
    generateCode = function() {
      # data frame
      df.name <- getWidgetValue('dframe')
      df.mod.name <- getWidgetValue('dframe.mod')
      df <- get(df.name, envir=.GlobalEnv)
      
      # mapping table
      code <- ''
      map <- getElements('mapTable')$getTableData(drop = FALSE)
      
      if (nrow(map) > 0) {
        code <- paste0(
          "\n# Set up mapping table",
          "\nmap <- data.frame(\n\tlookupColumn = character(), lookupValue = character(), \n\tnewColumn = character(), newValue = character(), \n\tregexp = logical(), stringsAsFactors = F)")

        for (i in 1:nrow(map)) {
          if (map[i,'Regexp'] == TRUE)
            lookupValue <- map[i,'Match'] # regular expression takes it directly
          else if (mode(df[[map[i,'Lookup']]]) != 'numeric' && length(grep("^'.*'$", map[i,'Match'])) == 0)
            lookupValue <- paste0("c('", map[i,'Match'], "')") # column is not numeric and not in paranthesis -> add parenthesis
          else
            lookupValue <- paste0('c(', map[i,'Match'], ')')
          code <- paste0(code, sprintf('\nmap[nrow(map) + 1, ] <- list("%s", "%s", "%s", "%s", %s)',
                                       map[i,'Lookup'], lookupValue, map[i,'Property'], map[i,'Value'], map[i,'Regexp']))
        }
      
        # map transform function
        code <- paste0(code,
          '\n\n# Function to apply the map to the data\n',
          'mapTransform <- function (data, map) { \n',
          '    for (i in 1:nrow(map)) { \n',
          '      # search lookupColumn by lookupValue as regular expression (regexp = TRUE) \n',
          '      if (map$regexp[i]) rows <- grep(map$lookupValue[i], data[[map$lookupColumn[i]]], value = FALSE) \n',
          '      # evalute lookupValue and search directly in lookupColumn \n',
          '      else rows <- which(data[[map$lookupColumn[i]]] %in% eval(parse(text = map$lookupValue[i]))) \n',
          '      # create newColumn if it does not exist yet \n',
          '      if (is.null(data[[map$newColumn[i]]])) data[map$newColumn[i]] <- character() \n',
          '      # assign newValue \n',
          '      if (length(rows) > 0) data[rows, map$newColumn[i]] <- map$newValue[i] \n',
          '    } \n',
          '    return(data) \n',
          '}')
        
        # actually run the transformation
        code <- paste0(code,
          '\n\n# Apply mapping function',
          '\n', df.mod.name, ' <- mapTransform(', df.name, ', map)')
        
        # initialize factors
        if (nrow(map) > 0) {
          factors <- paste0(sapply(unique(map$Property), 
              function(i) paste0('\n', df.mod.name, "[['", i,"']] <- as.factor(", df.mod.name, "[['", i, "']])")), collapse = "")
          if (factors != "")
            code <- paste0(code, "\n\n# Convert factor columns", factors)
        }
      }

       # set code and run it
       loadWidgets(code = code)
       runCode(global = FALSE)
    },
    
    # Run the code
    # ' @param global (whether to run in the global environment - warning! if TRUE, can change variables in global scope!)
    runCode = function(global = FALSE) {
      # get code
      code <- getWidgetValue('code')
      
      if (code != '') {
      
        # modified data frame
        df.mod.name <- getWidgetValue('dframe.mod')
        
        # error function when there is trouble with the code
        errorFun<-function(e) {
          showInfo(gui, .self, msg=paste0("ERROR: There are problems running this code.\n", capture.output(print(e))), type="error", timer=NULL, okButton = FALSE) 
          stop(e)
        }
            
        # try to run import (locally / globally)
        tryCatch(eval(parse(text = code)), error = errorFun, warning = errorFun)
        
        # check what's in data frame
        df.mod <- get(df.mod.name)
        
        # show data frame in data table (need to convert dates first though)
        if (getWidgetValue('showData')) {
          showdf <- df.mod
          types <- sapply(showdf, function(col) class(col)[1])
          for (i in which(types == "Date")) 
            showdf[,i] <- format(showdf[,i], "%Y-%m-%d") 
          for (i in which(types == "POSIXct")) 
            showdf[,i] <- format(showdf[,i], "%Y-%m-%d %H:%M:%S")
          dataTable <- getElements('dataTable')
          dataTable$destroyGui()
          dataTable$setData(frame = showdf)
          dataTable$makeGui(getWidgets('dataGrp'))
          dataTable$loadGui()
        }
 
        # store in global variable and show success message
        if (global) {
          assign(df.mod.name, df.mod, envir=.GlobalEnv)
          showInfo(gui, .self, msg=paste0("SUCCESS! Data Frame '", df.mod.name, "' created."), timer = NULL, okButton = FALSE)
        } else
          showInfo(gui, .self, msg="INFO: All clear, code can be run.", okButton = FALSE, timer = NULL) 
      }
    }
  )
)

# Testing
#t <- DataInfoDialog$new()
#t$setSettings(windowModal = FALSE) # easier for testing purposes
#t$makeGui()
