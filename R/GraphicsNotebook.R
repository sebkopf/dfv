#' Class implementing a fully flexible graphics notebook as a GuiElement
GraphicsNotebook <- setRefClass(
  'GraphicsNotebook',
  contains = 'GuiElement', 
  fields = list(
    tab = 'GraphicsNotebookTab',
    icons = 'list', # keeps track of main icons this GuiElement uses
    handlers = 'list' # handlers that the gui element needs to keep track of
  ),
  methods = list(
    initialize = function(...) {
      callSuper(...)
      
      ### default icons
      icons <<- list(
        NEW.PLOT = 'gtk-page-setup',
        CLOSE.TAB = 'gtk-cancel',
        SAVE.PLOT = 'gtk-save-as',
        PRINT.PLOT = 'gtk-print',
        SAVE.ALL = 'gtk-harddisk')
      
      ### default settings for a graphics notebooke
      setSettings(
        defaultTabLabel = "Plot"
      )
      
      ### default data
      setData(
        nb = 0, # which index of the notebook is selected (none by default)
        tabIDs = NULL # which tab IDs exist (none by default)
      )
    },
    
    # 'Returns the currently active tab object (GraphicsNotebookTab)
    getActiveTab = function() return (getElements(paste0('tab', data$tabIDs[data$nb]))),
    # '(Re) activarts the currently open graphics device (e.g. after switching screens)
    activateGraphicsDevice = function() getActiveTab()$activateGraphicsDevice(),
    
    # 'Excepts a container in ...
    makeGui = function(...) {
      # save dialog
      setElements('savePlotDialog' = SavePlotDialog$new()) 
      
      # plots notebook
      widgets$nb <<- gnotebook(cont=list(...)$parent, expand=TRUE) 
      handlers$nb.changedHandler <<- addHandlerChanged(widgets$nb, handler=function(h,...) selectPlotTab(h$pageno))
      for (tabID in data$tabIDs)
        newPlotTab(tabID = tabID, select = FALSE)
    },
    
    # 'Creates the standard tab by default
    #'@param select - determines whether the tab will be selected after tab creation (default=TRUE, FALSE is used when tabs are recreated during makeGui)
    #'@param activate - determines whether the tab will be activated (e.g. its showHandler executed) after tab creation (default=TRUE)
    newPlotTab = function(tabID, newTab = tab$copy(), select = TRUE, activate = TRUE) {
      # block handlers
      blockHandler(widgets$nb, handlers$nb.changedHandler)

      # determine tab id
      if (missing(tabID)) {
        tabID <- (max(c(0, data$tabIDs)) + 1)
        data$tabIDs <<- c(data$tabIDs, tabID)
      }
      
      # add new tab to elements
      if (is.null(newTab$getData('label'))) # no label
        newTab$setData(label = paste(settings$defaultTabLabel, tabID))
      l <- list()
      l[[paste0('tab', tabID)]] <- newTab
      setElements(l, passSettings = FALSE) # Note: the tab settings can not be changed by the user, only tab data
      
      # make Gui for new plot tab
      dmsg("Adding GraphicsNotebook Tab with TABID: ", tabID)
      newTab$makeGui(parent = widgets$nb)
      
      # unblock handlers
      unblockHandler(widgets$nb, handlers$nb.changedHandler)      
      
      # select plot tab
      if (select)
        selectPlotTab(getWidgetValue('nb'), activate = TRUE)
    },

    # 'Close active tab
    closePlotTab = function() {
        if ( (index <- data$nb) > 0) {
          dmsg("Closing GraphicsNotebook tab index ", index, " TABID ", data$tabIDs[index])
          blockHandler(widgets$nb, handlers$nb.changedHandler)
          removeElements(paste0('tab', data$tabIDs[index])) # remove tab element
          dispose(widgets$nb) #remove tab
          data$tabIDs <<- data$tabIDs[-index] # remove ID entry
          unblockHandler(widgets$nb, handlers$nb.changedHandler)
          
          # reselect whichever tab is auto selected (default jumps back to 0)
          setData('nb' = 0)
          selectPlotTab(getWidgetValue('nb'))
        }
    },
    
    # 'Selects and loads the plot tab indetified by index
    selectPlotTab = function(index, activate = TRUE) {
      if (index > 0 && index <= length(widgets$nb)) { # check for legal indices
        dmsg("Selecting GraphicsNotebook tab index ", index, ", TABID ", data$tabIDs[index])
        
        # make sure the tab is selected (but don't refire changeHandler)
        blockHandler(widgets$nb, handlers$nb.changedHandler)
        setData('nb' = index)
        loadWidgets('nb')
        unblockHandler(widgets$nb, handlers$nb.changedHandler)
        
        # activate plot tab
        if (activate)
          activatePlotTab()
      }
    },
    
    #' load plot tab (always loads activate tab)
    activatePlotTab = function() {
      dmsg("\tLoading active tab with Tab ID ", data$tabIDs[data$nb])
      getActiveTab()$activateTab()
    },
    
    #' save active plot (ask for dimension info first)
    savePlot = function (saveAll = FALSE){
      if (data$nb > 0) {
        # setup save dialog
        if (!saveAll) { # save all
          getElements("savePlotDialog")$setSettings(windowTitle = "Save plot ...")
          getElements("savePlotDialog")$setData(filename =paste0(format(Sys.time(),format="%Y%m%d"),"_", names(widgets$nb)[data$nb]), extension = ".pdf")
        } else { # save single
          getElements("savePlotDialog")$setSettings(windowTitle = "Save all plots ...")
          getElements("savePlotDialog")$setData(filename = paste0(format(Sys.time(),format="%Y%m%d"), "_"), extension = "[tab title].pdf")
        }
          
        getElements("savePlotDialog")$makeGui() # this is a modal dialog so method will not continue until it is done
        if ( elements$savePlotDialog$dialogSaved() ) { # check if modal dialog was saved
          getElements("savePlotDialog")$saveGui()
          if (!saveAll) {
            filepath <- file.path(elements$savePlotDialog$data$plotsPath, paste0(elements$savePlotDialog$data$filename, ".pdf"))
            dmsg("Saving active plot to file ", filepath)
            getActiveTab()$saveGraphicsDeviceToPDF(filename = filepath, width = elements$savePlotDialog$data$width, height = elements$savePlotDialog$data$height)
          } else {
            for (tab in getElements(paste0("tab", data$tabIDs))) {
              filepath <- file.path(elements$savePlotDialog$data$plotsPath, paste0(elements$savePlotDialog$data$filename, tab$data$label, ".pdf"))
              dmsg("Saving plot to file ", filepath)
              tab$saveGraphicsDeviceToPDF(filename = filepath, width = elements$savePlotDialog$data$width, height = elements$savePlotDialog$data$height)
            }
          }
          
          return (TRUE) # file saved
        }
      }
      return (FALSE) # not saved
    },
    
    # 'Print active plot
    printPlot = function(width = 8, height = 6) {
      if (data$nb > 0) 
        getActiveTab()$printGraphicsDevice(width = width, height = height)
    }
  )
)
