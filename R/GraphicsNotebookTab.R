#' Class implementing a fully flexible graphics notebook as a GuiElement
#' FIXME: consider implementing such that it doesn't have to be tied to a GraphicsNotebook (stand alone wrapper for graphics devices)
#' FIXME: implement multi graphics support here or in separate class (e.g. MultiGraphNotebookTab)
#'        for multigraphics, use definition scheme similar to layout in multiplot
GraphicsNotebookTab <- setRefClass(
  'GraphicsNotebookTab',
  contains = 'GuiElement', 
  fields = list(
    handlers = 'list' # handler IDs that the gui element needs to keep track of
  ),
  methods = list(
    initialize = function(...) {
      callSuper(...)
  
      ### default settings for a tab
      setSettings(
        editablePlotLabel = FALSE, # whether the label has a text box for editing it
        showHandler = NULL, # handler for special events when the tab is shown
        droptargetHandler = NULL, # graphics device droptarget handler
        clickedHandler = NULL, # graphics device clicked handler
        changedHandler = NULL, # graphics device changed handler
        rightclickHandler = NULL, # graphics device right click handler
        mousemotionHandler = NULL, # graphics device mousemotion handler
        rightclickmenuHandler = NULL, # graphics deivce right click menu handler
        protect = TRUE # these are not overwritten by settings autoload
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
      if (!is.null(settings$droptargetHandler))
        handlers$droptarget <<- adddroptarget(widgets$gg, targetType="object", handler = settings$droptargetHandler)
      if (!is.null(settings$clickedHandler))
        handlers$clicked <<- addHandlerClicked(widgets$gg, handler = settings$clickedHandler)
      if (!is.null(settings$changedHandler))
        handlers$changed <<- addHandlerChanged(widgets$gg, handler = settings$changedHandler)
      if (!is.null(settings$rightlickHandler))
        handlers$rightclick <<- addHandlerRightclick(widgets$gg, handler = settings$rightlickHandler)
      if (!is.null(settings$mousemotionHandler))
        handlers$mousemotion <<- addHandlerMouseMotion(widgets$gg, handler = settings$mousemotionHandler)
      if (!is.null(settings$rightclickmenuHandler))
        handlers$rightclickmenu <<- add3rdMousePopupmenu(obj = widgets$gg, menulist = settings$rightclickmenuHandler)
    },
    
    #' Methods that is activated
    #' - activates the graphics device
    #' - runs the showHandler (if set) and passes itself to it as argument tab
    activateTab = function() {
      activateGraphicsDevice()
      if (!is.null(settings$showHandler))
        do.call(settings$showHandler, args = list(tab = .self))
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
