#' Class implementing a fully flexible graphics notebook as a GuiElement
#' FIXME: consider implementing such that it doesn't have to be tied to a GraphicsNotebook (stand alone wrapper for graphics devices)
#' FIXME: implement multi graphics support here or in separate class (e.g. MultiGraphNotebookTab)
#'        for multigraphics, use definition scheme similar to layout in multiplot
GraphicsNotebookTab <- setRefClass(
  'GraphicsNotebookTab',
  contains = 'GuiElementDataFrame', 
  fields = list(
    handlers = 'list' # handlers that the gui element needs to keep track of
  ),
  methods = list(
    initialize = function(...) {
      callSuper(...)
  
      ### default settings for a tab
      setSettings(
        editablePlotLabel = FALSE
      )
    },
    
    #' Expects nb = gnotebook ...
    makeGUI = function(...) {
      nb <- list(...)$nb
      grp <- ggroup(cont = nb, horizontal=FALSE, label = data$label)
      if (settings$editablePlotLabel) {
        widgets$label <<- gedit(data$label, cont=grp)
        addHandlerKeystroke(widgets$label, handler=function(h,...) names(nb)[svalue(nb)] <- svalue(h$obj) ) # update notebook, data itself is later saved during a saveGUI() event
      }
      widgets$gg <<- ggraphics(cont=grp)
      blockHandler(obj = widgets$gg) # disable automatic 2nd mouse button popup handler (for save and copy)
      # event handlers
      #       if (!is.null(eventHandlers$droptarget))
      #         adddroptarget(gg, targetType="object", handler=eventHandlers$droptarget)
      #       if (!is.null(eventHandlers$Clicked))
      #         addHandlerClicked(gg, handler=eventHandlers$Clicked)
      #       if (!is.null(eventHandlers$Changed))
      #         addHandlerChanged(gg, handler=eventHandlers$Changed)
      #       if (!is.null(eventHandlers$Rightclick))
      #         addHandlerRightclick(gg, handler=eventHandlers$Rightclick)
      #       if (!is.null(eventHandlers$MouseMotion))
      #         addHandlerMouseMotion(gg, handler=eventHandlers$MouseMotion)
      #       if (!is.null(eventHandlers$RightlickMousePopupmenu))
      #         add3rdMousePopupmenu(obj=gg, menulist=eventHandlers$RightlickMousePopupmenu)
      
      #       # make new object
      #       if (is.null(tabObj))
      #         tabObj<-list() # new object
      #       tabObj$gg<-gg # store the graphics object
      #       
      #       if (length(pn$plot.nb) == 1) 
      #         tag(pn$plot.nb, "tabs")<-list()
      #       tag(pn$plot.nb, "tabs")[[length(pn$plot.nb)]]<-tabObj # add new object
      #       
      #       # load
      #       if (!is.null(loadHandler))
      #         do.call(loadHandler, list(obj=tabObj))
    },
    
    # activate graphics device in this plot
    activateGraphicsDevice = function() {
      gg <- getWidgets('gg')
      visible(gg) <- TRUE
    }
  )
)
