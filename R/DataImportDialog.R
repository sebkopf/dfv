DataImportDialogGui <- setClass("DataImportDialogGui", contains="ModalDialogGui")

setMethod("getToolbarXML", "DataImportDialogGui", function(gui, module) {
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
      <toolitem action="FromCb"/>
      <separator expand="true"/>
      <toolitem action="FromExcel"/>
      <separator expand="true"/>')
})

setMethod("getMenuXML", "DataImportDialogGui", function(gui, module) {
  return (
    '<menu name = "Import" action="Import">
      <menuitem action="FromCb"/>
      <menuitem action="FromExcel"/>
    </menu>
    <menu name = "Code" action="Code">
      <menuitem action="Copy"/>
      <menuitem action="Run"/>
    </menu>'
    )
})

setMethod("makeNavigation", "DataImportDialogGui", function(gui, module) {
  uimanager <- callNextMethod()
  # top toolbar
  toolbarGrp <- getWidgets(gui, module, 'topToolbarGrp')
  getToolkitWidget(toolbarGrp)$packStart (uimanager$getWidget ( "/toolbar2" ), TRUE) # add toolbar
  return(uimanager)
})

setMethod("setNavigationActions", "DataImportDialogGui", function(gui, module, actionGrp) {
  callNextMethod()
  nav.actions <-
    list(## name, icon, label , accelerator , tooltip , callback
      list ("Import", NULL , "_Import" , NULL, NULL, NULL),
      list ("FromCb", "gtk-convert", "From Clipboard", "<ctrl>P", "Paste data from clipboard", 
            function(...) {
              setSettings(gui, module, mode = 'clipboard')
              getElements(gui, module, 'optionsTable')$changeColumnVisibility(c(3,4), c(TRUE, FALSE))
              getModule(gui, module)$generateCode()
            } ),
      list ("FromExcel", "gtk-select-color", "From Excel", "<ctrl>E", "Import data from Excel file", 
            function(...) {
              f=gfile("Select Excel file to import.", type="open", cont = getWindow(gui, module),
                      filter = list("Excel Files" = list(patterns=c("*.xls", "*.xlsx")), "All files" = list(patterns = c("*"))))
              if (!is.na(f)){
                setData(gui, module, file = f)
                setSettings(gui, module, mode = 'excel')
                getElements(gui, module, 'optionsTable')$changeColumnVisibility(c(3,4), c(FALSE, TRUE))
                getModule(gui, module)$generateCode()
              }
            } ),
#       list ("FromCSV", "gtk-copy", "From Clipboard", "<ctrl>P", "Import data from CSV file", 
#             function(...) {
#               gmessage("sorry, work in progress...")
#             } ),
      list ("Code", NULL , "_Code" , NULL, NULL, NULL),
      list ("Run", "gtk-execute", "Run code", "<ctrl>R", "Execute code for tab", function(...) getModule(gui, module)$runCode(global = TRUE) ),
      list ("Copy", "gtk-copy", "Copy code", "<ctrl>C", "Copy code to clipboard", 
            function(...) {
              print("copy code")
            } ))
  actionGrp$addActions(nav.actions)
})


setMethod("makeMainGui", "DataImportDialogGui", function(gui, module) {
  setMenuGroup(gui, module, ggroup(horizontal=FALSE, cont=getWinGroup(gui, module), spacing=0))
  setWidgets(gui, module, topToolbarGrp = ggroup(horizontal=FALSE, cont=getWinGroup(gui, module), spacing=0))
  mainGrp <- ggroup(horizontal=FALSE, cont=getWinGroup(gui, module), spacing=0, expand=TRUE)
  
  # groups
  optionsGrp <- ggroup(container = mainGrp)
  columnsGrp <- gframe("Columns")
  dataGrp <- gframe("Data")
  codeGrp <- gframe("Code", expand=TRUE)
  tbPane <- gpanedgroup(dataGrp, codeGrp, expand=TRUE, horizontal=FALSE)
  tbPane2 <- gpanedgroup(columnsGrp, tbPane, container=mainGrp, expand=TRUE, horizontal=FALSE)
  setWidgets(gui, module, tbPane2 = tbPane2, tbPane = tbPane, dataGrp = dataGrp)
  
  # options table
  options <- DataTable$new()
  setElements(gui, module, optionsTable = options)  
  options$setSettings(editableColumns = names(options$getData('frame')))
  options$makeGui(optionsGrp, changedHandler = function(...) getModule(gui, module)$generateCode())
  options$changeColumnVisibility(c(3,4), xor(getSettings(gui, module, 'mode') == 'clipboard', c(FALSE, TRUE)))

  # columns table
  columns <- DataTable$new()
  setElements(gui, module, columnsTable = columns)
  columns$setSettings(editableColumns = c("Import", "Type"), resizable = TRUE)
  columns$makeGui(columnsGrp, changedHandler = function(...) getModule(gui, module)$generateCode())
  
  # data table
  setElements(gui, module, dataTable = DataTable$new())
  getElements(gui, module, 'dataTable')$setSettings(sortable = TRUE, resizable = TRUE)
  
  # code (attributes don't seem to work sadly)
  setWidgets(gui, module, code = gtext('', wrap=TRUE, font.attr = c(style="normal", weights="bold",sizes="medium"), container = codeGrp, expand = TRUE, height=50))
})

DataImportDialog <- setRefClass(
  'DataImportDialog',
  contains = 'ModalDialog',
  methods = list(
    initialize = function(gui = DataImportDialogGui(), ...){
      callSuper(gui = gui, ...)
      
      ### overwrite default setting for DataImportDialog
      setSettings(
        windowSize = c(450, 700),
        windowTitle = "Import data",
        ok.label = "Done",
        ok.tooltip = "Close import window",
        protect = TRUE
      )
      
      # new option (not protected, can be overwritten by user preference)
      setSettings(
        tbPane2 = 0.4,
        tbPane = 0.3, 
        mode = 'clipboard'
      ) 
      
      # default data for the data import dialog and all its elements
      setData(
        plotsPath = getwd(),
        plotsPathIndex = integer(0),
        filename = "",
        extension = ".pdf",
        width = 8,
        height = 6,
        optionsTable = list(
          frame = data.frame( # all the options for formats
            Variable = 'data',
            Headerrow = TRUE,
            Separator = factor("tab", levels = c(",", "tab", ";")),
            Sheet = 'Sheet1',
            Startrow = as.integer(1),
            stringsAsFactors = FALSE),
          selectedRows = 1
        ),
        columnsTable = list(
          frame = data.frame(
            Name = character(0),
            Import = logical(0),
            Type = factor(levels=c("Text", "Number", "Date", "Factor")),
            Values = character(0),
            stringsAsFactors = F
            )),
        dataTable = list(
          frame = data.frame())
      )
      names(data$optionsTable$frame)[c(2,4,5)] <<- c('Header row?', 'Excel sheet', 'Start row')
    },
    
    # ' make DataTable Element
    makeGui = function() {
      callSuper()
    },
    
    loadGui = function() {
      callSuper()
    }, 
    
    saveGui = function() {
      callSuper()
    },
    
    generateCode = function() {
      options <- getElements('optionsTable')$getTableData(rows = 1)
      variable <- getElements('optionsTable')$getTableData(1, 'Variable')
      if (getSettings('mode') == 'clipboard') {
        code <- paste0(
          "\n# Read data frame from clipboard\n",
          sprintf("%s <- read.clipboard (\n\theader = %s, sep = '%s', skip = %s, comment.char='', \n\trow.names = NULL, stringsAsFactors = FALSE", 
                        options[[1]], options[[2]], sub('tab', '\\\\t', options[[3]]), options[[5]] - 1))
        code.1 <- paste0(code, ", nrows=1)") # code for 1 line excerpt to find data types
      } else if (getSettings('mode') == 'excel') {
        code <- paste0(
          "\n# Read data frame from Excel file\n",
          sprintf("%s <- read.xlsx2(\n\tfile = '%s', \n\tsheetName = '%s',\n\theader = %s, stringsAsFactors = FALSE", 
                        options[[1]], getData('file'), options[[4]], options[[2]]))
        code.1 <- paste0(sub("read.xlsx2", "read.xlsx", code), ", rowIndex(", options[[5]], "))") # code for 1 line excerpt to find data types
        code <- paste0(code, ", startRow = ", options[[5]])
      }
      
      # check if there are columns defined yet
      defined <- nrow(getElements('columnsTable')$getTableData()) > 0
      if (defined) {
        types <- getElements('columnsTable')$getTableData(columns = 'Type')
        code <- paste0(code, ", \n\tcolClasses = c('", 
          paste(sapply(types, function(type) {
            switch(as.character(type),
                   'Date' = 'Date',
                   'Number' = 'numeric',
                   'character')
          }), collapse = "', '"), "')")
      } else {      
        # try to guess data types of the individual columns by running the script for the first column (silent if it doesn't work)
        tryCatch({
          eval(parse(text = code.1))
          df <- get(variable)
          code <- paste0(code, ", \n\tcolClasses = c('", paste(sapply(df, function(col) { if (class (col)[1] == 'POSIXct') "Date" else class (col)[1] }), collapse = "', '"), "')")
        }, error = function(e) {stop(e)}, warning = function(e) {stop(e)})
      }
      code <- paste0(code, ")")
      
      # initialize factors
      if (defined) {
        types <- getElements('columnsTable')$getTableData(columns = 'Type')
        factors <- (paste0(sapply(1:length(types), function(i) {
          if (as.character(types[i]) == 'Factor') 
            paste0('\n', variable, '[,', i,'] <- as.factor(', variable, '[,', i, '])') 
          else
            ''
        }), collapse = ""))
        if (factors != "")
          code <- paste0(code, "\n\n# Convert factor columns", factors)
      }
      
      # remove unwanted columns
      delColsCode <- ""
      if (defined) {
        import <- getElements('columnsTable')$getTableData(columns = 'Import')
        if (length(exclude <- which(!import)) > 0)
          delColsCode <- paste0("\n\n# Remove unwanted columns\n", variable, ' <- ', variable, '[, -c(', paste0(exclude, collapse=", "), ')]')
      }
      
      # set code and run it
      setData(delColsCode = delColsCode) # need to know what this is to execute it separately
      loadWidgets(code = paste0(code, delColsCode))
      runCode()
    },
    
    # Actually run the code
    # ' @param global (whether to run in the global environment - warning! if TRUE, can change variables in global scope!)
    runCode = function(global = FALSE) {
      code <- getWidgetValue('code')
      delColsCode <- getData('delColsCode')
      importCode <- if (delColsCode == "") code else gsub(delColsCode, "", code, fixed=TRUE)
     
      # error function when there is trouble with the code
      errorFun<-function(e) {
        err <- if (getSettings('mode') == 'clipboard') 'Make sure you have a data table copied to the clipboard.\n' else ''
        showInfo(gui, .self, msg=paste0("ERROR: There are problems running this code.\n", err, capture.output(print(e))), type="error", timer=NULL) 
        stop(e)
      }
      
      # try to run import (locally / globally)
      tryCatch(eval(parse(text = importCode)), error = errorFun, warning = errorFun)
      
      # check what's in data frame
      df <- get(getElements('optionsTable')$getTableData(1, 'Variable'))
      
      # update columns table if this is a different data frame
      if (!identical(names(df), getElements('columnsTable')$getTableData(columns = 'Name'))) {
        types <- sapply(df, function(x) { 
          switch(class(x)[1],
                 'integer' = 'Number',
                 'numeric' = 'Number',
                 'POSIXct' = 'Date',
                 'Date' = 'Date',
                 'Text')})
        getElements('columnsTable')$setTableData(
          data.frame(
            Name = names(df), 
            Import = TRUE, 
            Type = factor(types, levels=c("Text", "Number", "Date", "Factor")), 
            Values = sapply(head(df, n=3), function(x) { paste0(paste(x, collapse=", "), ' ...') }),
            stringsAsFactors = F))
      }
      
      # try to run delete code
      if (delColsCode != "")
        tryCatch(eval(parse(text = delColsCode)), error = errorFun, warning = errorFun)
      df <- get(getElements('optionsTable')$getTableData(1, 'Variable'))
      
      # show data frame in data table (need to convert dates first though)
      for (i in which(sapply(df, class) == "Date")) 
        df[,i] <- format(df[,i], "%Y-%m-%d") # FIXME: cannot handle time!!
      print(sapply(df, class))
      dataTable <- getElements('dataTable')
      dataTable$destroyGui()
      dataTable$setData(frame = head(df, n=10))
      dataTable$makeGui(getWidgets('dataGrp'))
      dataTable$loadGui()
      
      # success message
      if (!global)
        showInfo(gui, .self, msg="INFO: All clear, code can be run.", okButton = FALSE, timer = NULL)
      else
        showInfo(gui, .self, msg=paste0("INFO: Data Frame '", getElements('optionsTable')$getTableData(1, 'Variable'), '" created.'), timer = NULL, okButton = FALSE)
    }
  )
)

# Testing
t <- DataImportDialog$new()
t$setSettings(windowModal = FALSE, mode = 'excel') # easier for testing purposes
t$setData(file = '/Users/sk/Dropbox/Tools/software/r/dfv/Workbook1.xlsx')
t$makeGui()
#Sys.sleep(1)
#t$generateCode()


a <- function() {
  # variable name
  # header?
  # skip = 0
  # separator = tab/comma
  cp.pasteDF<-function(header=TRUE, sep="\t", skip=0, comment.char="", row.names=NULL, quote=""){
    return(read.clipboard(sep=sep, stringsAsFactors=FALSE, header=header, 
                          skip=skip, comment.char=comment.char, row.names=NULL, quote=quote))
  }
  
  excel.readDF <- function(file, sheet = 1, startRow = 1, stringsAsFactors=FALSE, trueColNames = TRUE) {
    df <- read.xlsx2(file, sheet, startRow=startRow, stringsAsFactors=stringsAsFactors, header=TRUE) 
    if (trueColNames) {
      dfcols <- read.xlsx(file, sheet, rowIndex=startRow, header=FALSE, stringsAsFactors=stringsAsFactors) 
      names(df) <- gsub("\\s*$", "", dfcols, perl=T) # trailing whitespaces removed
    }
    return(df)
  }
  
  errFun<-function(e) { gmessage(paste("ERROR: could not paste\n.", capture.output(print(e))), title="Could not paste from clipboard.", icon="error", parent=dfv$win); stop(e) }
  tryCatch( df<-cp.pasteDF(),  # try to paste
            warning = errFun,
            error = errFun)
  if (!is.empty(dfname<-ginput("Pasting succesful.\nWhat variable name would you like to save this as?"))) {
    assign(dfname, df, envir=.GlobalEnv)
    DFV.refreshDataFramesTable(dfv)
    DFV.loadDataFrame(dfv, dfname)
  }
}

