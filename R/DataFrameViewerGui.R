# S4 class DataFrameViewerGui
DataFrameViewerGui <- setClass("DataFrameViewerGui", contains="BaseGui")

setMethod("getMenuXML", "DataFrameViewerGui", function(gui, module) {
  return (
    '<menu name = "DFV" action="DFV">
      <menuitem action="Reload"/>
      <menuitem action="SaveToWS"/>
      <menuitem action="Quit"/>
    </menu>
    <menu name = "Data" action="Data">
      <menuitem action="Import"/>
      <menuitem action="Melt"/>
      <menuitem action="AddInfo"/>
    </menu>
    <menu name = "Code" action="Code">
      <menuitem action="Run"/>
    </menu>
    <menu name = "Plots" action="Plots">
      <menuitem action="NewPlotTab"/>
      <menuitem action="ClosePlotTab"/>
      <menuitem action="SavePlot"/>
      <menuitem action="SaveAllPlots"/>
      <menuitem action="PrintPlot"/>
    </menu>
    <menu name = "Help" action="Help">
      <menuitem action="ggplot"/>
    </menu>')
})

setMethod("getToolbarXML", "DataFrameViewerGui", function(gui, module) {
  return (
    nav <- '
    <toolitem action="SaveToWS"/>
    <toolitem action="Import"/>
    <toolitem action="Melt"/>
    <toolitem action="AddInfo"/>
    <separator expand="true"/>
    <toolitem action="Run"/>
    <separator expand="true"/>
    <toolitem action="NewPlotTab"/>
    <toolitem action="ClosePlotTab"/>
    <toolitem action="SavePlot"/>
    <toolitem action="SaveAllPlots"/>
    <toolitem action="PrintPlot"/>
    '
  )
})

setMethod("setNavigationActions", "DataFrameViewerGui", function(gui, module, actionGrp) {
  gn <- getElements(gui, module, 'gn') # graphic notebook
  nav.actions <-
    list(## name, icon, label , accelerator , tooltip , callback
      list ("DFV" , NULL , "_DFV" , NULL , NULL , NULL ),
      list ("Reload" , "gtk-refresh" ,"Reload Screen" , NULL, "This reloads the screen and recreates it from the last save.", function(...) { 
          destroyGui(gui, module)
          getModule(gui, module)$makeGui()
        }), 
      list ("SaveToWS" , "gtk-home" , "Save DFV" , "<ctrl>H" ,"Save settings and data to workspace" , function(...) { 
        showInfo(gui, module, "Saving to workspace...", timer=1, okButton=FALSE)
        getModule(gui, module)$saveToWorkspace()
        showInfo(gui, module, "Data Frame Viewer settings and data succesfully saved to workspace.", timer=2, okButton=FALSE)
      }) , 
      list ("Quit", "gtk-quit", "Quit", "<ctrl>Q", "Quit program", function(...) destroyGui(gui, module) ),
      list ("Data", NULL , "_Data" , NULL, NULL, NULL),
      list ("Import", "gtk-select-color", "Import Data", "<ctrl>I", "Import data from the clipboard or from Excel", function(...) { 
        getElements(gui, module, "importDialog")$makeGui()
        if ( getElements(gui, module, "importDialog")$dialogSaved() ) 
          getElements(gui, module, "importDialog")$saveGui()
      } ),
      list ("Melt", "gtk-convert", "Melt Data", "<ctrl>M", "Melt existing data frames into a format that's easy to plot with ggplot", function(...) { 
        getElements(gui, module, "meltDialog")$makeGui()
        if ( getElements(gui, module, "meltDialog")$dialogSaved() )
          getElements(gui, module, "meltDialog")$saveGui()
      } ),
      list ("AddInfo", "gtk-info", "Add Info", NULL, "Add information to an existing data frame to inform your ggplots", function(...) { 
        getElements(gui, module, "infoDialog")$makeGui()
        if ( getElements(gui, module, "infoDialog")$dialogSaved() )
          getElements(gui, module, "infoDialog")$saveGui()
      } ),
      list ("Code", NULL , "_Code" , NULL, NULL, NULL),
      list ("Run", "gtk-execute", "Run code", "<ctrl>R", "Execute code for tab", function(...) { runCode(getModule(gui, module)) } ),
#      list ("Snippets", "gtk-find-and-replace", "Code Snippets", NULL, "Save/load code snippets", function(...) { gmessage("Sorry, not implemented yet.") } ),
      list ("Plots", NULL , "_Plots" , NULL, NULL, NULL),
      list ("NewPlotTab", gn$icons$NEW.PLOT, "New plot", "<ctrl>N", NULL, function(...) { gn$newPlotTab(activate = TRUE) } ),
      list ("ClosePlotTab", gn$icons$CLOSE.TAB, "Close plot", "<ctrl>X", NULL, function(...) { gn$closePlotTab() } ),
      list ("SavePlot", gn$icons$SAVE.PLOT, "Save plot", "<ctrl>S", NULL, function(...) { 
        if (gn$savePlot())
          showInfo(gui, module, "Plot saved to PDF.", timer=2, okButton=FALSE)
        } ),
      list ("SaveAllPlots", gn$icons$SAVE.ALL, "Save all plots", "<ctrl><shift>S", NULL, function(...) { 
        if (gn$savePlot(saveAll = TRUE))
          showInfo(gui, module, "All plots saved to PDF.", timer=2, okButton=FALSE)
        } ),
      list ("PrintPlot", gn$icons$PRINT.PLOT, "Print plot", NULL, NULL, function(...) { gn$printPlot() } ),
      list ("Help" , NULL , "_Help" , NULL , NULL , NULL ),
      list ("ggplot" , "gtk-info" ,"ggplot2" , NULL , NULL , function(...) browseURL("http://ggplot2.org/") )
      )
  
  actionGrp$addActions(nav.actions)
})

setMethod("makeMainGui", "DataFrameViewerGui", 
function(gui, module) {
  ### top level groups
  setMenuGroup(gui, module, ggroup(horizontal=FALSE, cont=getWinGroup(gui, module), spacing=0))
  setToolbarGroup(gui, module, ggroup(horizontal=TRUE, cont=getWinGroup(gui, module), spacing=0, expand=FALSE))
  left.tgrp <- ggroup(expand=TRUE, horizontal=FALSE)
  left.bgrp <- ggroup(expand=TRUE, horizontal=FALSE)
  right.tgrp <- ggroup(expand=TRUE, horizontal=FALSE)
  right.bgrp <- ggroup(expand=TRUE, horizontal=FALSE)
  ltbPane <- gpanedgroup(left.tgrp, left.bgrp, expand=TRUE, horizontal=FALSE)
  rtbPane <- gpanedgroup(right.tgrp, right.bgrp, expand=TRUE, horizontal=FALSE) 
  setWidgets(gui, module, 
             lrPane = gpanedgroup(ltbPane, rtbPane, horizontal=TRUE, cont=getWinGroup(gui, module), spacing=0, expand=TRUE),
             ltbPane = ltbPane,
             rtbPane = rtbPane)
  
  ### external dialogs
  setElements(gui, module, 'importDialog' = DataImportDialog$new()) 
  setElements(gui, module, 'meltDialog' = DataMeltDialog$new()) 
  setElements(gui, module, 'infoDialog' = DataInfoDialog$new()) 
  
  ### data frames table
  dfs.frame<-gframe("Available Data Frames", cont=left.tgrp, horizontal=FALSE, expand=TRUE)
  addHandlerIdle(dfs.frame, interval = 1000, handler = function(...) refreshDataFrames(getModule(gui, module)))
  dfTable <- DataTable$new()
  setElements(gui, module, dfTable = dfTable)
  dfTable$setSettings(resizable = TRUE, sortable = TRUE)
  dfTable$setData(frame = getDataFrames())
  dfTable$makeGui(dfs.frame, selectionHandler = function(...) loadDataFrame(getModule(gui, module)))
  
  ### plot constructor
  code.frame <- gframe("Plot Constructor", horizontal=FALSE, cont=left.tgrp)
  gtkFrame <- getToolkitWidget(code.frame)
  gtkFrame['border-width'] <- 5
  code.grp <- glayout(container = code.frame, spacing=10); i <- 0
  code.fields <- c("df", "x", "y", "color", "shape", "grid")
  
  # create 'labels'
  labels <- sapply(code.fields, function(x, grp) {
    b <- gbutton(action = gaction(paste0(x, ":"), tooltip = "Click to generate code", handler = function(...) { generateCode.ggplot(getModule(gui, module)) }), cont = grp)
    gtkButton <- getToolkitWidget(b)
    gtkButton['relief'] <- 'none' # change button relief
    list(b)
  }, grp = code.grp)
  
  # create 'values'
  droptargets <- sapply(code.fields, function (x, grp) {
    if (x == 'df') { # df
      b <- gbutton(action = 
            gaction("<Click to set>", tooltip = "Click to set to currently selected data frame. Click again to reset.", 
                    handler = function (...) svalue(b) <- getElements(gui, module, 'dfTable')$getSelectedValues('Name')), width=10, cont = grp)
    } else { # all other plot parameters
      b <- gbutton(action = 
            gaction("<Drop here>", tooltip = "Drag table tabs/columns here. Click to reset.", 
                    handler = function (...) svalue(b) <- "<Drop here>"), width=10, cont = grp)
      adddroptarget(b, targetType="object", handler = function(h, ...) svalue(h$obj) <- gWidgets::id(h$dropdata)) 
    }
    gtkButton <- getToolkitWidget(b)
    gtkButton['relief'] <- 'none' # change button relief
    list(b)
  }, grp = code.grp)
  
  # put it all into the layout
  for (field in code.fields) {
    code.grp[(i %% 3) + 1, floor(i/3)*2 + 1] <- labels[[field]]
    code.grp[(i %% 3) + 1, floor(i/3)*2 + 2] <- droptargets[[field]]
    i <- i + 1
  }
  
  # set widget
  setWidgets(gui, module, droptargets)

  ### Data Table
  df.frame <- gframe("Data", horizontal=FALSE, cont=left.bgrp, expand = TRUE)
  setWidgets(gui, module, dataNb = gnotebook(container = df.frame, expand=TRUE))  
  
  ### Graphics Notebook
  plot.grp <- gframe("Plots", cont=right.tgrp, expand=TRUE, horizontal=FALSE)
  tab <- GraphicsNotebookTab$new() # default tab
  tab$setSettings(
    editablePlotLabel = TRUE,
    showHandler = function(tab) getModule(gui, module)$loadWidgets(tab$getData()), # load widgets from tab into the main GUI
    droptargetHandler = function(h,...) {
      if (!is.null(h$dropdata) && !is.null(column <- gWidgets::id(h$dropdata))) 
        generateCode.singleColumnMultiplot(getModule(gui, module), column)
      })
  tab$setData(code = '')
  gn <- GraphicsNotebook$new(tab = tab)
  setElements(gui, module, gn = gn)
  gn$makeGui(parent = plot.grp)
  
  ### Code
  code.grp <- gframe("Code", expand=TRUE, horizontal = FALSE, cont=right.bgrp)
  setWidgets(gui, module, code = gtext('', wrap=TRUE, font.attr = c(style="normal", weights="bold",sizes="medium"), container = code.grp, expand = TRUE, height=50))
})

###################
# logic functions #
###################
# NOTE: consider putting into its own Module object
# (then it's hard to update dynamically while working on the gui)
# This could be easily achieved by copying these functions into its
# own Module object and replacing all referenes to module with .self (or just '')

# refresh data frames table and any changed data tables that are already loaded (triggered by an idleHandler)
refreshDataFrames <- function(module) {
  # check if there are new data frames and update data frames table and loaded data tabs if that's the case
  if (!is.null(module)) {
    dfTable <- module$getElements('dfTable')
    if (!identical(getDataFrames(), (oldDFs <- dfTable$getTableData(drop = FALSE)))) {
      dmsg("changed data frames detected, updating table")
      showInfo(module$gui, module, msg="Changed data frames detected, updating tables ...", timer = 2, okButton = FALSE)
      # update data frames tables
      dfTable$setTableData(getDataFrames())
      if (!is.null(selected <- dfTable$getSelectedValues('Name')))
        dfTable$selectRowsByValues(Name = selected, blockHandler = TRUE)
      
      # update loaded data frames
      dataNb <- module$getWidgets('dataNb')
      for (df.name in names(dataNb)) {
        if (!identical(oldDFs[which(oldDFs == df.name), , drop=FALSE], dfTable$getTableData(dfTable$getRowsByValues(Name = df.name), drop = FALSE))){
          dmsg("changed data frame '", df.name, "' is loaded --> reload ...")
          module$loadWidgets(dataNb = which(df.name == names(dataNb))[1])
          dispose(dataNb) # close tab
          gdf(get(df.name, env=.GlobalEnv), cont = dataNb, expand=TRUE, label = df.name) # reload tab
        }
      }
    }
  }
}

# load data frame
loadDataFrame <- function(module) {
  df.name <- module$getElements('dfTable')$getSelectedValues('Name')
  if (!is.null(df.name)) {
    df <- get(df.name, env=.GlobalEnv)
    if (is.na(select<-(which(names(module$getWidgets('dataNb')) == df.name)[1]))) 
      gdf(df, cont = module$getWidgets('dataNb'), expand=TRUE, label = df.name) # make new data table
    else
      module$loadWidgets(dataNb = select) # data frame alreay open, reselect the tab
  }
}

#' Generate the code for the ggplot
generateCode.ggplot <- function(module) {
  if (!is.null(module)) {
    if (is.null(module$getElements('gn')$getActiveTab())) 
        module$getElements('gn')$newPlotTab(select = TRUE, activate = FALSE) # make a plot tab if none exists
    tab <- module$getElements('gn')$getActiveTab()
    plotParams <- module$getWidgetValues(c('df', 'x', 'y', 'color', 'shape', 'grid'))
    tab$setData(plotParams) # store plot parameters in the plot tab

    # empty string
    emptyS<-"<Drop here>"
    
    # ggplot main
    code <- paste0(
      "\n#Generate ggplot\n",
      "p <- ggplot(", plotParams$df, 
      ",\n\taes(x = ", sub(emptyS, "", plotParams$x), ", y = ", sub(emptyS, "", plotParams$y))
    
    # color (does both color and fill)
    if (plotParams$color != emptyS)
      code <- paste0(code, ", fill = ", plotParams$color)
    
    # shape
    if (plotParams$shape != emptyS)
      code <- paste0(code, ", shape = ", plotParams$shape)
    
    code <- paste0(code, ")) + \n\t",
        "geom_point(colour = 'black', size = 5") # not runnin geom_line, it's more nuisance
    
    # fix shape for color defined but shape not
    if (plotParams$color!= emptyS && plotParams$shape==emptyS)
      code <- paste0(code,", shape = 21")
    code <- paste0(code, ")")
    
    # shape scale if shape is defined
    if (plotParams$shape!=emptyS)
      code <- paste0(code, " + \n\tscale_shape_manual(values=c(21,22,24,25,23))")
    
    # theme and title
    code <- paste0(code, " + \n\ttheme_bw() + \n\tlabs(title = '", 
                   plotParams$df, "', x = '", plotParams$x, "', y = '", plotParams$y, "')")
    
    # wrap (really grid but that's harder)
    if (plotParams$grid != emptyS)
      code <- paste0(code, " + \n\tfacet_wrap(~", plotParams$grid, ")")
    
    # print out
    code <- paste0(code, "\n\n#Render plot\nprint(p)")
    
    # set code and run it
    module$loadWidgets(code = code)
    runCode(module)
  }
}

#' Generate the code for a multiplot of a single column
generateCode.singleColumnMultiplot <- function(module, column) {
  if (!is.null(module)) {
    df.name <- module$getElements('dfTable')$getSelectedValues('Name')
    module$loadWidgets(code = paste0(
      "\n# Generate jitter, boxplot and violin plot of column '", column, "' in data frame '", df.name, "'\n",
      "p.jitter <- ggplot(", df.name, ", aes('', ", column, ")) + \n\tgeom_jitter() + coord_flip() + theme_bw() + labs(x='Jitter', y='", column, "')\n",
      "p.box <- ggplot(", df.name, ", aes('', ", column, ")) + \n\tgeom_boxplot() + coord_flip() + theme_bw() + labs(x='Boxplot', y='", column, "')\n",
      "p.violin <- ggplot(", df.name, ", aes('', ", column, ")) + \n\tgeom_violin(trim=FALSE) + theme_bw() + coord_flip() + labs(x='Violin', y='", column, "')\n",
      "\n# Combine plots",
      "grid.newpage()\n",
      "pushViewport(viewport(layout = grid.layout(3, 1)))\n",
      "print(p.jitter, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))\n",
      "print(p.box, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))\n",
      "print(p.violin, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))\n"))
    runCode(module)
  }
}

#' Run the code that is in the code field
runCode <- function(module) {
  code <- module$getWidgetValue('code')
  tab <- module$getElements('gn')$getActiveTab()
  tab$setData(code = code) # store code in tab
  
  # error function when there is trouble with the code
  errorFun<-function(e) {
    showInfo(module$gui, module, msg=paste0("ERROR: There are problems running this code.\n", capture.output(print(e))), type="error", timer=NULL, okButton = TRUE) 
    stop(e)
  }
  
  # try to run plot generation
  tryCatch(eval(parse(text = code)), error = errorFun, warning = errorFun)
  showInfo(module$gui, module, msg=paste0("Code successfully run"), timer = 2, okButton = FALSE)
}

#################
# utility funcs #
#################

#' Return a list of data frames
getDataFrames <- function() {
  dfs<-data.frame(Name=sort(names(which(sapply(.GlobalEnv, is.data.frame)))), stringsAsFactors=FALSE)
  dfs<-ddply(dfs, .(Name), mutate, 
             Rows=nrow(get(Name, envir=.GlobalEnv)),
             Columns=length(get(Name, envir=.GlobalEnv)), 
             Column.Names=paste(names(get(Name, envir=.GlobalEnv)), collapse=", "))
  return (dfs)
}
