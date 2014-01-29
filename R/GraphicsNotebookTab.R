#' Class implementing a fully flexible graphics notebook as a GuiElement
#' FIXME: consider implementing such that it doesn't have to be tied to a GraphicsNotebook (stand alone wrapper for graphics devices)
#' FIXME: implement multi graphics support here or in separate class (e.g. MultiGraphNotebookTab)
#'        for multigraphics, use definition scheme similar to layout in multiplot
GraphicsNotebookTab <- setRefClass(
  'GraphicsNotebookTab',
  contains = 'GuiElement', 
  fields = list(
    handlers = 'list' # handlers that the gui element needs to keep track of
  ),
  methods = list(
    initialize = function(...) {
      callSuper(...)
  
      ### default settings for a tab
      setSettings(
        editablePlotLabel = FALSE,
        plotHandlers = list(
            droptarget = FALSE,
            clicked = FALSE,
            changed = FALSE,
            rightclick = FALSE,
            mousemotion = FALSE,
            rightclickmenu = FALSE
          ),
        protect = TRUE # these cannot be overwritten by the user
      )
    },
  
    #' Event handlers - overwrite these
    droptargetHandler = function(...) dmsg("droptargethandler, params: ", paste(list(...), collapse=", ")),
    clickedHandler = function(...) dmsg("clickedhandler, params: ", paste(list(...), collapse=", ")),
    changedHandler = function(...) dmsg("changedhandler, params: ", paste(list(...), collapse=", ")),
    rightclickHandler = function(...) dmsg("rightlickhandler, params: ", paste(list(...), collapse=", ")),
    mousemotionHandler = function(...) dmsg("mousemotionhandler, params: ", paste(list(...), collapse=", ")),
    rightclickmenuHandler = function(...) dmsg("rightclickmenuhandler, params: ", paste(list(...), collapse=", ")),
    
    #' Expects nb = gnotebook ...
    makeGui = function(...) {
      nb <- list(...)$parent 
      grp <- ggroup(cont = nb, horizontal=FALSE, label = data$label)
      if (settings$editablePlotLabel) {
        widgets$label <<- gedit(data$label, cont = grp)
        addHandlerKeystroke(widgets$label, handler=function(h,...) {
          saveWidgets('label')
          names(nb)[svalue(nb)] <- getData('label')
        } ) # update notebook, data itself is later saved during a saveGui() event
      }
      widgets$gg <<- ggraphics(cont=grp)
      
      blockHandler(obj = widgets$gg) # disable automatic 2nd mouse button popup handler (for save and copy)
      # event handlers
      if (getSettings('plotHandlers')$droptarget)
        handlers$droptarget <<- adddroptarget(widgets$gg, targetType="object", handler = function(...) droptargetHandler(...))
      if (getSettings('plotHandlers')$clicked)
        handlers$clicked <<- addHandlerClicked(widgets$gg, handler = function(...) clickedHandler(...))
      if (getSettings('plotHandlers')$changed)
        handlers$changed <<- addHandlerChanged(widgets$gg, handler = function(...) changedHandler(...))
      if (getSettings('plotHandlers')$rightclick)
        handlers$rightclick <<- addHandlerRightclick(widgets$gg, handler = function(...) rightclickHandler(...))
      if (getSettings('plotHandlers')$mousemotion)
        handlers$mousemotion <<- addHandlerMouseMotion(widgets$gg, handler = function(...) mousemotionHandler(...))
      if (getSettings('plotHandlers')$rightclickmenu)
        handlers$rightclickmenu <<- add3rdMousePopupmenu(obj = widgets$gg, menulist = function(...) rightclickmenuHandler(...))
    },
    
    # activate graphics device in this plot
    activateGraphicsDevice = function() {
      gg <- getWidgets('gg')
      visible(gg) <- TRUE
    },
    
    # 'Print graphics device
    printGraphicsDevice = function(width = 8, height = 6) {
      activateGraphicsDevice() # make sure graphics device is active
      if (exists("win.print")) { #FIXME - find a better way to distinguish between windows and linux
        # on windows, go print
        win.print(width=width, height=height) # launches print interface
      } else {
        # use default CUPS printer (no dialog, just direct printing)
        # more information on CUPS printing here: http://localhost:631/help/options.html
        dev.copy2pdf(file = "|lp -o landscape -o", width = width, height = height, onefile = TRUE) # throws warning about onefile
        gmessage("Plot is printing on default printer.")
      }
      activateGraphicsDevice() # reactivate graphics device
    },
    
    # 'Save graphics device to pdf
    saveGraphicsDeviceToPDF = function(filename, width = 8, height = 6, ...) {
      activateGraphicsDevice()
      dev.copy2pdf(file=filename, width=as.numeric(width), height=as.numeric(height), ...) # copy graph
    }
    
  )
)
