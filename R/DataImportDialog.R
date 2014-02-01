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
              setSettings(gui, module, mode = 'excel')
              getElements(gui, module, 'optionsTable')$changeColumnVisibility(c(3,4), c(FALSE, TRUE))
              getModule(gui, module)$generateCode()
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
  setWidgets(gui, module, tbPane = gpanedgroup(columnsGrp, dataGrp, container=mainGrp, expand=TRUE, horizontal=FALSE))
  codeGrp <- gframe("Code", cont = mainGrp, expand=TRUE)
  
  # options table
  options <- DataTable$new()
  setElements(gui, module, optionsTable = options)
  options$setSettings(editableColumns = names(options$getData('frame')))
  options$makeGui(optionsGrp, changedHandler = function(...) getModule(gui, module)$generateCode())
  options$changeColumnVisibility(c(3,4), xor(getSettings(gui, module, 'mode') == 'clipboard', c(FALSE, TRUE)))

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
        windowSize = c(450, 550),
        windowTitle = "Import data",
        ok.label = "Done",
        ok.tooltip = "Close import window",
        protect = TRUE
      )
      
      # new option (not protected, can be overwritten by user preference)
      setSettings(
        tbPane = 0.4, 
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
            Sheet = as.integer(1),
            Skiprow = as.integer(0),
            stringsAsFactors = FALSE),
          selectedRows = 1
        )
      )
      names(data$optionsTable$frame)[c(2,4,5)] <<- c('Header row?', 'Excel sheet #', '# rows to skip')
    },
    
    # ' make DataTable Element
    makeGui = function() {
      
      # variable name
      # header?
      # skip = 0
      # separator = tab/comma
      ImportDataTable <- setRefClass('ImportDataTable', contains = 'DataTable', methods = list(
          columnChanged = function(...) {
            callSuper(...)
            saveGui()
            generateCode()
          }
        ))
      callSuper()
    },
    
    loadGui = function() {
      # update dimensions column
      callSuper()
    }, 
    
    saveGui = function() {
      callSuper()
      # save width and height in options
    },
    
    generateCode = function() {
      options <- getElements('optionsTable')$getTableData(rows = 1)
      if (getSettings('mode') == 'clipboard')
        code <- paste0(
          "\n# Read data frame from clipboard\n",
          sprintf("%s <- read.clipboard (header = %s, sep = '%s', skip = %s, comment.char='', row.names = NULL, stringsAsFactors = FALSE)", 
                        options[[1]], options[[2]], sub('tab', '\\\\t', options[[3]]), options[[5]]))
      else if (getSettings('mode') == 'excel')
        code <- paste0(
          "\n# Read data frame from Excel file\n",
          sprintf("%s <- read.xlsx2('%s', %s, header = %s, startRow = %s, stringsAsFactors = FALSE)", 
                        options[[1]], 'FILE', options[[4]], options[[2]], options[[5]] + 1))
      
      loadWidgets(code = code)
      runCode()
    },
    
    # '@param global (whether to run in the global environment - warning! if TRUE, can change variables in global scope!)
    runCode = function(global = FALSE) {
      code <- getWidgetValue('code')
      print(code)
      
      #FIXME: continue here!
      
      errorFun<-function(e) {
        showInfo(gui, .self, msg=paste0("ERROR:\n", capture.output(print(e))), type="error", timer=NULL) 
        stop(e)
      }
      
      #if ( length(grep("^(\\s)*[gq]*plot\\(", code)) > 0) { # it's a plotting command, try to print plot
      tryCatch( eval(parse(text = code)), error = errorFun)
        showInfo(gui, .self, msg="INFO: Command(s) successfully executed.")
    }
  )
)

# Testing
t <- DataImportDialog$new()
t$setSettings(windowModal = FALSE) # easier for testing purposes
t$makeGui()


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

