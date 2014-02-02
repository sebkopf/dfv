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
      list ("Reload" , "gtk-refresh" ,"Reload Screen" , "<ctrl>R" , "This reloads the screen and recreates it from the last save.", function(...) { # FIXME: disable the keyboard shortcut! (enable code execution instead)
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
      list ("Run", "gtk-execute", "Run code", "<ctrl>R", "Execute code for tab", function(...) { dmsg("run") } ),
      list ("Snippets", "gtk-find-and-replace", "Code Snippets", NULL, "Save/load code snippets", function(...) { gmessage("Sorry, not implemented yet.") } ),
      list ("Plots", NULL , "_Plots" , NULL, NULL, NULL),
      list ("NewPlotTab", gn$icons$NEW.PLOT, "New plot", "<ctrl>N", NULL, function(...) { gn$newPlotTab() } ),
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

setMethod("makeMainGui", "DataFrameViewerGui", function(gui, module) {
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
  
  dfv <- list()
  
  
  #dfs.frame<-gframe("Available Data Frames", cont=left.tgrp, horizontal=FALSE)
  #size(dfs.frame)<-c(50, 200)
#  dfv$dfs.table<-gtable(DFV.getDataFrames(), cont=dfs.frame, expand=TRUE)
#  dfs.refresh<-gbutton(action=gaction("Refresh", icon="gtk-execute", handler=function(h,...) DFV.refreshDataFramesTable(dfv)), cont=(dfbuttons.grp<-ggroup(cont=dfs.frame)))
#  gbutton(action=gaction("Paste From\nExcel", icon="gtk-paste", handler=function(h,...) DFV.paste(dfv)), cont=dfbuttons.grp)
#  gbutton(action=gaction("Import CSV\nFile", icon="gtk-convert", handler=function(h,...) gmessage("Not implemented yet.", cont=dfv$win)), cont=dfbuttons.grp)
  code.frame<-gframe("Plot Constructor", horizontal=TRUE, cont=left.tgrp)
  code.leftgrp<-ggroup(cont=code.frame)
  code.grp<-glayout(container=code.leftgrp, spacing=10, expand=TRUE)
  code.grp[(i<-1),1]<-"df:"
  code.grp[i,2]<-(df.name <- glabel("", cont=code.grp, width=10))
  code.grp[(i<-i+1),1]<-"x:"
  code.grp[i,2]<-(dfv$plotParams$x<-glabel("", cont=code.grp, width=10))
  code.grp[(i<-i+1),1]<-"y:"
  code.grp[i,2]<-(dfv$plotParams$y<-glabel("", cont=code.grp, width=10))
  code.grp[(i<-i+1),1]<-"color:"
  code.grp[i,2]<-(dfv$plotParams$color<-glabel("", cont=code.grp, width=10))
  code.grp[(i<-i+1),1]<-"shape:"
  code.grp[i,2]<-(dfv$plotParams$shape<-glabel("", cont=code.grp, width=10))
  code.grp[(i<-i+1),1]<-"grid:"
  code.grp[i,2]<-(dfv$plotParams$grid<-glabel("", cont=code.grp, width=10))

  setWidgets(gui, module, df.name = df.name)

#  code.grp[(i<-i+1),1]<-gbutton(action=gaction("Clear", icon="gtk-clear", handler=function(h,...) DFV.clearPlotParams(dfv)), cont=code.grp)
#  code.grp[i, 2]<-gbutton(action=gaction("Create\nCode", icon="gtk-execute", handler=function(h,...) DFV.compileGGPlot(dfv)), cont=code.grp)
  #code.rightgrp<-ggroup(cont=code.frame, horizontal=FALSE, expand=TRUE)
  
  #gbutton(action=gaction("Run", icon="plot", tooltip="Press CTRL+R to execute code.", handler=function(h,...) DFV.execCode(dfv) ), cont=code.rightgrp.top)  

  ### Data Table
  df.frame <- gframe("Data", horizontal=FALSE, cont=left.bgrp, expand = TRUE)
  setWidgets(gui, module, dataNb = gnotebook(container = df.frame, expand=TRUE))  

  ### Graphics Notebook
  plot.grp <- gframe("Plots", cont=right.tgrp, expand=TRUE, horizontal=FALSE)
  tab <- GraphicsNotebookTab$new()
  tab$setSettings(editablePlotLabel = TRUE)
  gn <- GraphicsNotebook$new(tab = tab)
  setElements(gui, module, gn = gn)
  gn$makeGui(parent = plot.grp)
  
  ### Code
  code.grp <- gframe("Code", expand=TRUE, horizontal = FALSE, cont=right.bgrp)
  setWidgets(gui, module, code = gtext('', wrap=TRUE, font.attr = c(style="normal", weights="bold",sizes="medium"), container = code.grp, expand = TRUE, height=50))

  
  # handlers
#  addHandlerChanged(dfv$df.nb, handler=function(h,...) svalue(dfv$plotParams$df)<-names(h$obj)[h$pageno])
#  addHandlerClicked(dfv$dfs.table, handler=function(h,...) DFV.selectDataFrame(dfv))
#  addHandlerKeystroke(dfv$plotParams$code, handler=function(h,...) if (h$key=="\022") DFV.execCode(dfv) )
  
#  for (var in names(dfv$plotParams))
#    adddroptarget(dfv$plotParams[[var]], targetType="object", handler=
#                    function(h,...) svalue(h$obj)<-gWidgets::id(h$dropdata)) 
  
  # load
#  DFV.clearPlotParams(dfv)
  
  
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
    if (!identical(getDataFrames(), (oldDFs <- dfTable$getTableData()))) {
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
    module$loadWidgets(df.name = df.name)
    if (is.na(select<-(which(names(module$getWidgets('dataNb')) == df.name)[1]))) 
      gdf(df, cont = module$getWidgets('dataNb'), expand=TRUE, label = df.name) # make new data table
    else
      module$loadWidgets(dataNb = select) # data frame alreay open, reselect the tab
  }
}

# clear code parameters
DFV.clearPlotParams<-function(dfv) {
  for (var in c("x", "y", "color", "shape", "grid"))
    svalue(dfv$plotParams[[var]])<-"<Drag&Drop here>"
}

# excecute code
DFV.execCode<-function(dfv) {
  code=svalue(dfv$plotParams$code)
  errorFun<-function(e) {
    svalue(dfv$plotParams$errorMsg)<-strwrap(paste(c("ERROR", capture.output(print(e))), sep=""))
    pn.setSelectedPlotTabParam(dfv$pn, list(plotParams=widgets.getValuesAsDF(dfv$plotParams))) # save plot parameters
    stop(e)
  }
  
  if ( length(grep("^(\\s)*[gq]*plot\\(", code)) > 0) { # it's a plotting command, try to print plot
    tryCatch( (p<-eval(parse(text=code))),   # try to generate plot
              error = errorFun)
    tryCatch(print(p), error=errorFun)
    pn.setSelectedPlotTabParam(dfv$pn, list(plot=p)) # save plot
    svalue(dfv$plotParams$errorMsg)<-paste("INFO: Plot succesfuly generated.", sep="")
  } else  {# regular code
    tryCatch( eval(parse(text=code), envir=.GlobalEnv),  # just execute code
              error = errorFun)
    svalue(dfv$plotParams$errorMsg)<-paste("INFO: Command(s) successfully executed.", sep="")
  }
  pn.setSelectedPlotTabParam(dfv$pn, list(plotParams=widgets.getValuesAsDF(dfv$plotParams))) # save plot parameters
}

DFV.compileSingleDataPlot<-function(dfv, varName, dfName=NULL) {
  if (is.null(dfName))
    dfName<-svalue(dfv$plotParams$df)
  svalue(dfv$plotParams$code)<-paste(
    "multiplot(\n",
    "ggplot(", dfName, ", aes('', ", varName, ")) + geom_jitter() + coord_flip() + theme_bw() + labs(x='Jitter', y='", varName, "'),\n",
    "ggplot(", dfName, ", aes('', ", varName, ")) + geom_boxplot() + coord_flip() + theme_bw() + labs(x='Boxplot', y='", varName, "'),\n",
    "ggplot(", dfName, ", aes('', ", varName, ")) + geom_violin(trim=FALSE) + theme_bw() + coord_flip() + labs(x='Violin', y='", varName, "')\n",
    ")", sep="")
  DFV.execCode(dfv)
}

DFV.compileGGPlot<-function(dfv) {
  emptyS<-"<Drag&Drop here>"
  code<-paste("ggplot(",
              (df<-svalue(dfv$plotParams$df)), 
              ",\n aes(x=", (x<-svalue(dfv$plotParams$x)), ", y=", (y<-svalue(dfv$plotParams$y)), sep="")
  # color (does both color and fill)
  if ((color<-svalue(dfv$plotParams$color)) != emptyS)
    code<-paste(code, ", fill=", color, "", sep="")
  # shape
  if ((shape<-svalue(dfv$plotParams$shape)) != emptyS)
    code<-paste(code, ", shape=", shape, "", sep="")
  
  code<-paste(code, ")) + \ngeom_point(colour='black', size=5", sep="") # not runnin geom_line, it's more nuisance
  if (color!= emptyS && shape==emptyS)
    code<-paste(code,", shape=21", sep="")
  code<-paste(code, ") + \ntheme_bw() + labs(title='", 
              df, "', x='", x, "', y='", y, "')", sep="")
  if (shape!=emptyS)
    code<-paste(code, "+ \nscale_shape_manual(values=c(21,22,24,25,23))", sep="")
  
  # wrap (really grid but that's harder)
  if ((wrap<-svalue(dfv$plotParams$grid)) != emptyS)
    code<-paste(code, " + \nfacet_wrap(~", wrap, ")", sep="")
  
  svalue(dfv$plotParams$code)<-code
  DFV.execCode(dfv)
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
