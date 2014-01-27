SavePlotDialogGUI <- setClass("SavePlotDialogGUI", contains="BaseGUI")


setMethod("getToolbarXML", "SavePlotDialogGUI", function(gui, module) {
  return (
    nav <- '
      <separator expand="true"/>
      <toolitem action="Save"/>
      <separator expand="true"/>
      <toolitem action="Cancel"/>
      <separator expand="true"/>
      '
  )
})

setMethod("setNavigationActions", "SavePlotDialogGUI", function(gui, module, actionGrp) {
  nav.actions <-
    list(## name, icon, label , accelerator , tooltip , callback
      list ("Save" , "gtk-apply", "Save" , "<ctrl>S", "Save PDF(s)" , function(...) {
        setData(gui, module, save = TRUE)
        getModule(gui, module)$saveGUI()
        destroyGUI(gui, module)
      } ),
      list ("Cancel" , "gtk-cancel" ,"Cancel" , NULL, "Cancel saving.", function(...) { 
        destroyGUI(gui, module)
      }))
  actionGrp$addActions(nav.actions)
})

setMethod("makeMainGUI", "SavePlotDialogGUI", function(gui, module) {
  mainGrp <- ggroup(horizontal=FALSE, cont=getWinGroup(gui, module), spacing=0, expand=TRUE)
  setToolbarGroup(gui, module, ggroup(horizontal=TRUE, cont=getWinGroup(gui, module), spacing=0, expand=FALSE))
  
  # pictures directory selection
  fileBrowser.items <- function(path = NULL, user.data=NULL) {
    if (is.null(path)) 
      path <- getwd() 
    else
      path <- file.path(getwd(), do.call("file.path", args=as.list(path)))
    
    #setSettings(gui, module, plotsPath = path)
    showInfo(gui, module, paste("Folder:", path), timer=NULL, okButton=FALSE)
    
    folders <- subset(data.frame(
      Folder=dir(path, include.dirs=TRUE),
      Path=dir(path, include.dirs=TRUE, full.names=TRUE),
      file.info(dir(path, include.dirs=TRUE, full.names=TRUE))[,c(1:2)], 
      stringsAsFactors=FALSE), isdir==TRUE)
    
    # figure out number of subdirectories
    folders$Subdirs <- apply(folders, 1, function(x) length(which(file.info(dir(x[2], full.names=T))$isdir)))
    return(folders[c("Folder", "Subdirs", "Path")])
  }
  
  # check for subfolders
  fileBrowser.hasOffspring <- function(children, user.data=NULL, ...) return(children$Subdirs > 0) # which items have subdirectories
  fileBrowser.icons <- function(children,user.data=NULL, ...) return(rep("gtk-directory", length=nrow(children))) # FIXME: could implement some indicator which folders have already been used
  
  # tree
  tree <- gtree(fileBrowser.items, fileBrowser.hasOffspring, chosencol=3, icon.FUN = fileBrowser.icons, container=mainGrp, expand=TRUE)
  setWidgets(gui, module, plotsPathIndex = tree) # link tree to plotsPathIndex
  
  # tree click handler
  addHandlerClicked(tree, handler=function(h,...) {
    if (!is.null(val <- svalue(tree)))
      setSettings(gui, module, plotsPath = val)
    else
      setSettings(gui, module, plotsPath = getwd()) # set back to working directory
    showInfo(gui, module, paste("Folder: ", getSettings(gui, module, 'plotsPath')), timer=NULL, okButton=FALSE)
  })
})

SavePlotDialog <- setRefClass(
  'SavePlotDialog',
  contains = 'Module',
  methods = list(
    initialize = function(gui = SavePlotDialogGUI(), ...){
      callSuper(gui = gui, ...)
      
      ### default setting for SavePlotDialog
      setSettings(
        windowSize = c(400, 400),
        windowTitle = "Saving to PDF ...",
        windowModal = TRUE,
        plotsPath = getwd(),
        plotsPathIndex = integer(0)
      )
      
      # options available for plotting
      setSettings(
        options = data.frame(
          ID = 1,
          width.inches = 8,
          height.inches = 6,
          stringsAsFactors = FALSE)
      )
    }, 
    
    saveGUI = function() {
      callSuper()
    },
    
    loadGUI = function() {
      callSuper()
      setData(save = FALSE) # response from the modal dialog
    }
  )
)