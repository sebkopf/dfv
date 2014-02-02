DataMeltDialogGui <- setClass("DataMeltDialogGui", contains="ModalDialogGui")

setMethod("getToolbarXML", "DataMeltDialogGui", function(gui, module) {
  return (
    '<separator expand="true"/>
    <toolitem action="Copy"/>
    <separator expand="true"/>
    <toolitem action="Run"/>
    <separator expand="true"/>
    <toolitem action="Ok"/>
    <separator expand="true"/>')
})

setMethod("getMenuXML", "DataMeltDialogGui", function(gui, module) {
  return (
    '<menu name = "Code" action="Code">
    <menuitem action="Copy"/>
    <menuitem action="Run"/>
    </menu>'
  )
})

setMethod("setNavigationActions", "DataMeltDialogGui", function(gui, module, actionGrp) {
  callNextMethod()
  nav.actions <-
    list(## name, icon, label , accelerator , tooltip , callback
      list ("Code", NULL , "_Code" , NULL, NULL, NULL),
      list ("Run", "gtk-execute", "Run code", "<ctrl>R", "Execute code for tab", function(...) getModule(gui, module)$runCode(global = TRUE) ),
      list ("Copy", "gtk-copy", "Copy code", "<ctrl>C", "Copy code to clipboard", 
            function(...) {
              copyToClipboard(getModule(gui, module)$getWidgetValue('code'))
              showInfo(gui, module, msg="INFO: code copied to clipboard.", okButton = FALSE, timer = 2) 
            }))
  actionGrp$addActions(nav.actions)
})


setMethod("makeMainGui", "DataMeltDialogGui", function(gui, module) {
  setMenuGroup(gui, module, ggroup(horizontal=FALSE, cont=getWinGroup(gui, module), spacing=0))
  topGrp <- ggroup(horizontal=FALSE, spacing=0, expand=TRUE)
  bottomGrp <- ggroup(horizontal=FALSE, spacing=0, expand=TRUE)
  tbPane <- gpanedgroup(topGrp, bottomGrp, cont=getWinGroup(gui, module), expand=TRUE, horizontal=FALSE)
  
  # groups
  optionsGrp <- gframe("Options", container = topGrp, horizontal = TRUE, expand = FALSE)
  idsGrp <- gframe("ID Columns", container = topGrp, expand = TRUE)
  dataGrp <- gframe("Data")
  codeGrp <- gframe("Code", expand=TRUE, horizontal = FALSE)
  tbPane2 <- gpanedgroup(dataGrp, codeGrp, cont = bottomGrp, horizontal = FALSE, expand=TRUE)
  setWidgets(gui, module, tbPane = tbPane, tbPane2 = tbPane2, idsGrp = idsGrp, dataGrp = dataGrp)
  
  # options table
  optionsLayout <- glayout(cont = optionsGrp)
  optionsLayout[1, 1] <- "  Data frame:"
  optionsLayout[1, 2] <- (df.box <- gcombobox(sort(names(which(sapply(.GlobalEnv, is.data.frame)))), selected = 0, cont = optionsLayout, 
                                              handler = function(h, ...) getModule(gui, module)$loadDataFrame()))
  optionsLayout[2, 1] <- "  Output variable:"
  optionsLayout[2, 2] <- (df.melt <- gedit("", cont = optionsLayout))
  optionsLayout[3, 1] <- "  Melt variable:"
  optionsLayout[3, 2] <- (melt.var <- gedit("", cont = optionsLayout))
  optionsLayout[4, 1] <- "  Melt value:"
  optionsLayout[4, 2] <- (melt.val <- gedit("", cont = optionsLayout))
  addHandlerKeystroke(df.melt, handler = function(h, ...) getModule(gui, module)$generateCode())
  addHandlerKeystroke(melt.val, handler = function(h, ...) getModule(gui, module)$generateCode())
  addHandlerKeystroke(melt.var, handler = function(h, ...) getModule(gui, module)$generateCode())
  setWidgets(gui, module, dframe = df.box, dframe.melt = df.melt, melt.var = melt.var, melt.val = melt.val)
  
  # columns table
  ids <- DataTable$new()
  setElements(gui, module, idsTable = ids)
  ids$setSettings(editableColumns = 'ID?', resizable = TRUE)
  ids$makeGui(idsGrp, changedHandler = function(...) getModule(gui, module)$generateCode())
  
  # data table
  dataT <- DataTable$new()
  setElements(gui, module, dataTable = dataT)
  dataT$setSettings(sortable = TRUE, resizable = TRUE)
  
  # code (attributes don't seem to work sadly)
  setWidgets(gui, module, code = gtext('', wrap=TRUE, font.attr = c(style="normal", weights="bold",sizes="medium"), container = codeGrp, expand = TRUE, height=50))
  setWidgets(gui, module, showData = gcheckbox("Automatically preview data (this can be very slow for large data frames)", handler = function(h, ...) getModule(gui, module)$generateCode(), cont = codeGrp))
})

DataMeltDialog <- setRefClass(
  'DataMeltDialog',
  contains = 'ModalDialog',
  methods = list(
    initialize = function(gui = DataMeltDialogGui(), ...){
      callSuper(gui = gui, ...)
      
      ### overwrite default setting for DataMeltDialog
      setSettings(
        windowSize = c(350, 700),
        windowTitle = "Melt data frames into ggplot format",
        ok.label = "Done",
        ok.tooltip = "Close melting window",
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
        melt.var = 'Variable',
        melt.val = 'Value',
        showData = TRUE,
        idsTable = list(
          frame = data.frame(
            ID = logical(0),
            Name = character(0),
            Values = character(0),
            stringsAsFactors = F
          )),
        dataTable = list(
          frame = data.frame(Data = character(0), Frame = character(0)))
      )
      names(data$idsTable$frame)[c(1)] <<- 'ID?'
    },
    
    # ' Load the layout of the mapping table for a new data frame
    loadDataFrame = function() {
      df.name <- getWidgetValue('dframe')
      loadWidgets(dframe.melt = paste0(df.name, '.melt'))
      
      # load ids table
      df.name <- getWidgetValue('dframe')
      df <- get(df.name, envir=.GlobalEnv)
      idsDf <- data.frame(
        ID = c(TRUE, rep(FALSE, times = ncol(df) - 1)),
        Name = names(df), 
        Values = sapply(head(df, n=5), function(x) { paste0(paste(x, collapse=", "), ' ...') }),
        stringsAsFactors = F)
      names(idsDf)[c(1)] <- 'ID?'
      getElements('idsTable')$setData(frame = idsDf) # for initial loading
      getElements('idsTable')$setTableData(idsDf) # for later loading
      
      # generate code
      generateCode()
    },
    
    # ' Generate the code for excel import
    generateCode = function() {
      df.name <- getWidgetValue('dframe')
      df.melt.name <- getWidgetValue('dframe.melt')
      idsTable <- getElements('idsTable')$getTableData(drop = FALSE)
      code <- paste0(
        "\n# Melt data frame",
        "\n", df.melt.name, " <- melt(", df.name, ", variable.name = '", getWidgetValue('melt.var'), "', value.name = '", getWidgetValue('melt.val'), "', ",
        "\n\tid.vars = c('", paste0(idsTable$Name[idsTable[['ID?']] == TRUE], collapse = "', '"), "'))")

      # set code and run it
      loadWidgets(code = code)
      runCode(global = FALSE)
    },
    
    # Run the code
    # ' @param global (whether to run in the global environment - warning! if TRUE, can change variables in global scope!)
    runCode = function(global = FALSE) {
      # get code
      code <- getWidgetValue('code')
      
      # modified data frame
      df.melt.name <- getWidgetValue('dframe.melt')
      
      # error function when there is trouble with the code
      errorFun<-function(e) {
        showInfo(gui, .self, msg=paste0("ERROR: There are problems running this code.\n", capture.output(print(e))), type="error", timer=NULL, okButton = FALSE) 
        stop(e)
      }
      
      # try to run import (locally / globally)
      tryCatch(eval(parse(text = code)), error = errorFun, warning = errorFun)
      
      # check what's in data frame
      df.melt <- get(df.melt.name)
      
      # show data frame in data table (need to convert dates first though)     
      showdf <- df.melt
      if (getWidgetValue('showData') && nrow(showdf) > 0) {
        types <- sapply(showdf, function(col) class(col)[1])
        # FIXME: ideally this is taken care of automatically by the cell renderer!
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
        assign(df.melt.name, df.melt, envir=.GlobalEnv)
        showInfo(gui, .self, msg=paste0("SUCCESS! Data Frame '", df.melt.name, "' created."), timer = NULL, okButton = FALSE)
      } else
        showInfo(gui, .self, msg="INFO: All clear, code can be run.", okButton = FALSE, timer = NULL) 
      
    }
  )
)

# Testing
#t <- DataMeltDialog$new()
#t$setSettings(windowModal = FALSE) # easier for testing purposes
#t$makeGui()
