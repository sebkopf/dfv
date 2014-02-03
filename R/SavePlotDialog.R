SavePlotDialogGui <- setClass("SavePlotDialogGui", contains="ModalDialogGui")

setMethod("makeMainGui", "SavePlotDialogGui", function(gui, module) {
  mainGrp <- ggroup(horizontal=FALSE, cont=getWinGroup(gui, module), spacing=0, expand=TRUE)
  treeGrp <- ggroup(horizontal=FALSE, expand=TRUE)
  optionsGrp <- gframe("Options", horizontal=FALSE)
  setWidgets(gui, module, tbPane = gpanedgroup(treeGrp, optionsGrp, container=mainGrp, expand=TRUE, horizontal=FALSE))
  
  # options group
  detailsGrp <- glayout(cont = optionsGrp, expand = TRUE)
  detailsGrpGTK<-getToolkitWidget(detailsGrp) # gtk object
  detailsGrpGTK['border-width']<-5 # border with
  detailsGrp[(i<-1), 1] <- "Filename:"
  detailsGrp[i, 2] <- setWidgets(gui, module, filename = gedit("", cont=detailsGrp))
  detailsGrp[i, 3] <- setWidgets(gui, module, extension = glabel("", cont=detailsGrp))
  detailsGrp[(i<-i+1), 1] <- "Saved formats:"
  detailsGrp[i, 2:3, expand=TRUE] <- (tableGrp <- ggroup(cont = detailsGrp, expand = TRUE))
  
  # options table
  getElements(gui, module, 'optionsTable')$makeGui(tableGrp, selectionHandler = function(...) {
    # get values of width and height in table and load the same name 'width' and 'height' text widgets with it
    getModule(gui, module)$loadWidgets(
      as.list(getElements(gui, module, 'optionsTable')$getSelectedValues(c('width', 'height'))) 
    )
  })
  
  detailsGrp[(i<-i+1), 1] <- "Height [inches]:"
  detailsGrp[i, 2] <- setWidgets(gui, module, height = gedit("", cont=detailsGrp, coerce.with=as.numeric))
  detailsGrp[(i<-i+1), 1] <- "Width [inches]:"
  detailsGrp[i, 2] <- setWidgets(gui, module, width = gedit("", cont=detailsGrp, coerce.with=as.numeric))
  
  # directory selection
  fileBrowser.items <- function(path = NULL, user.data=NULL) {
    if (is.null(path)) 
      path <- getwd() 
    else
      path <- file.path(getwd(), do.call("file.path", args=as.list(path)))
    
    showInfo(gui, module, paste("Folder:", path), timer=NULL, okButton=FALSE)
    
    folders <- subset(data.frame(
      Folder=dir(path, include.dirs=TRUE),
      Path=dir(path, include.dirs=TRUE, full.names=TRUE),
      file.info(dir(path, include.dirs=TRUE, full.names=TRUE))[,c(1:2)], 
      stringsAsFactors=FALSE), isdir==TRUE)
    
    # figure out number of subdirectories
    folders$Subdirs <- apply(folders, 1, function(x) length(which(file.info(dir(x[2], full.names=T))$isdir)))
    return(folders[c("Folder", "Path", "Subdirs")])
  }
  
  # check for subfolders
  fileBrowser.hasOffspring <- function(children, user.data=NULL, ...) return(children$Subdirs > 0) # which items have subdirectories
  fileBrowser.icons <- function(children,user.data=NULL, ...) return(rep("gtk-directory", length=nrow(children))) # FIXME: could implement some indicator which folders have already been used
  
  # tree
  tree <- gtree(fileBrowser.items, fileBrowser.hasOffspring, chosencol=2, icon.FUN = fileBrowser.icons, container=treeGrp, expand=TRUE)
  setWidgets(gui, module, plotsPathIndex = tree) # link tree to plotsPathIndex
  
  # tree click handler
  addHandlerClicked(tree, handler=function(h,...) {
    if (!is.null(val <- svalue(tree)))
      setData(gui, module, plotsPath = val)
    else
      setData(gui, module, plotsPath = getwd()) # set back to working directory
    showInfo(gui, module, paste("Folder: ", getData(gui, module, 'plotsPath')), timer=NULL, okButton=FALSE)
  })
})

SavePlotDialog <- setRefClass(
  'SavePlotDialog',
  contains = 'ModalDialog',
  methods = list(
    initialize = function(gui = SavePlotDialogGui(), ...){
      callSuper(gui = gui, ...)
      
      ### overwrite default setting for SavePlotDialog
      setSettings(
        windowSize = c(450, 550),
        windowTitle = "Saving to PDF ...",
        ok.icon = "gtk-save", # overwrite
        ok.label = "Save",
        ok.tooltip = "Save PDF(s).",
        protect = TRUE
      )
      
      # new option (not protected, can be overwritten by user preference)
      setSettings(tbPane = 0.4) 
      
      # data
      setData(
        plotsPath = getwd(),
        plotsPathIndex = integer(0),
        filename = "",
        extension = ".pdf",
        width = 8,
        height = 6
      )
    },
    
    # ' make DataTable Element
    makeGui = function() {
      options <- DataTable$new()
      options$setData(
        frame = data.frame( # all the options for formats
          width = c(4, 8, 16),
          height = c(4, 6, 12),
          Dimensions = '', 
          stringsAsFactors = FALSE),
        selectedRows = 2
      )
      options$setSettings(invisibleColumns = c('height', 'width'))
      setElements(optionsTable = options)
      callSuper()
    },
    
    loadGui = function() {
      # update dimensions column
      getElements('optionsTable')$setData(frame = 
        mutate(getElements('optionsTable')$getData('frame'), Dimensions = paste0(height, "x", width, " (height: ", height, " inches, width: ", width, " inches)")))
      callSuper()
    }, 
    
    saveGui = function() {
      callSuper()
      # save width and height in options
      if (nrow(subset(getElements('optionsTable')$getData('frame'), width == data$width & height == data$height)) == 0) {
        dmsg(class(.self), ": adding new widht x height option to table")
        getElements('optionsTable')$setData(
          selectedRows = nrow(getElements('optionsTable')$getData('frame')) + 1,
          frame = rbind(
            getElements('optionsTable')$getData('frame'),
            data.frame(width = data$width, height = data$height, Dimensions='', stringsAsFactors=F))
          )
      }
    }
  )
)

# Testing
#t <- SavePlotDialog$new()$makeGui()