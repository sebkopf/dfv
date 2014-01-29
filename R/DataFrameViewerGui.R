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
      <menuitem action="Paste"/>
      <menuitem action="ImportExcel"/>
    </menu>
    <menu name = "Code" action="Code">
      <menuitem action="Run"/>
      <menuitem action="Snippets"/>
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
    <toolitem action="Paste"/>
    <toolitem action="ImportExcel"/>
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
      list ("Paste", "gtk-copy", "Paste Data", NULL, "Paste data from the clipboard", function(...) { dmsg("paste") } ),
      list ("ImportExcel", "gtk-convert", "Import data", NULL, "Import data from excel spreadsheet", function(...) { dmsg("excel import") } ),
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
  # top level groups
  setMenuGroup(gui, module, ggroup(horizontal=FALSE, cont=getWinGroup(gui, module), spacing=0))
  mainGrp <- ggroup(horizontal=FALSE, cont=getWinGroup(gui, module), spacing=0, expand=TRUE)
  setToolbarGroup(gui, module, ggroup(horizontal=TRUE, cont=getWinGroup(gui, module), spacing=0, expand=FALSE))
  
  #setToolbarGroup(gui, module, ggroup(horizontal=FALSE, cont=getWinGroup(gui, module), spacing=0))
  
  dfv <- list()
  win.nb<-gnotebook(cont=getWinGroup(gui, module), expand=TRUE)
  # workspace variables browser
  gvarbrowser(cont=win.nb, label="Variables", expand=TRUE) # no handler yet
  # data frames browser
  dfs.tab<-ggroup(container=win.nb, horizontal=TRUE, expand=TRUE, label="Data Frames")
  left.grp<-ggroup(expand=TRUE, horizontal=FALSE)
  dfs.frame<-gframe("Available Data Frames", cont=left.grp, horizontal=FALSE)
  size(dfs.frame)<-c(50, 200)
#  dfv$dfs.table<-gtable(DFV.getDataFrames(), cont=dfs.frame, expand=TRUE)
#  dfs.refresh<-gbutton(action=gaction("Refresh", icon="gtk-execute", handler=function(h,...) DFV.refreshDataFramesTable(dfv)), cont=(dfbuttons.grp<-ggroup(cont=dfs.frame)))
#  gbutton(action=gaction("Paste From\nExcel", icon="gtk-paste", handler=function(h,...) DFV.paste(dfv)), cont=dfbuttons.grp)
#  gbutton(action=gaction("Import CSV\nFile", icon="gtk-convert", handler=function(h,...) gmessage("Not implemented yet.", cont=dfv$win)), cont=dfbuttons.grp)
  code.frame<-gframe("Plot Constructor", expand=TRUE, horizontal=TRUE)
  code.leftgrp<-ggroup(cont=code.frame)
  code.grp<-glayout(container=code.leftgrp, spacing=10, expand=TRUE)
  code.grp[(i<-1),1]<-"df:"
  code.grp[i,2]<-(dfv$plotParams$df<-glabel("", cont=code.grp, width=10))
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
#  code.grp[(i<-i+1),1]<-gbutton(action=gaction("Clear", icon="gtk-clear", handler=function(h,...) DFV.clearPlotParams(dfv)), cont=code.grp)
#  code.grp[i, 2]<-gbutton(action=gaction("Create\nCode", icon="gtk-execute", handler=function(h,...) DFV.compileGGPlot(dfv)), cont=code.grp)
  code.rightgrp<-ggroup(cont=code.frame, horizontal=FALSE, expand=TRUE)
  glabel("COMMAND LINE", cont=(code.rightgrp.top<-ggroup(cont=code.rightgrp, horizontal=TRUE)))
  addSpring(code.rightgrp.top)
  gbutton(action=gaction("Run", icon="plot", tooltip="Press CTRL+R to execute code.", handler=function(h,...) DFV.execCode(dfv) ), cont=code.rightgrp.top)  
  dfv$plotParams$code<-gtext(cont=code.rightgrp, expand=TRUE)  
  df.frame<-gframe("Data", horizontal=FALSE)
  size(df.frame)<-c(50,310)
  dfv$df.nb <- gnotebook(container = df.frame, expand=TRUE)
#  gbutton(action=gaction("Refresh", icon="gtk-refresh", handler=function(h, ...) DFV.refreshDataFrame(dfv)), cont=ggroup(cont=df.frame))
  right.grp<-ggroup(expand=TRUE)  
  plot.frame<-gframe("Plots", cont=right.grp, expand=TRUE, horizontal=FALSE)
  
  # set panes as widgets so they are user adjustable
  setWidgets(gui, module, tbPane = gpanedgroup(code.frame, df.frame, container=left.grp, expand=TRUE, horizontal=FALSE))
  setWidgets(gui, module, lrPane = gpanedgroup(left.grp,right.grp,container=dfs.tab, expand=TRUE))

  ### Graphics Notebook
  tab <- GraphicsNotebookTab$new()
  tab$setSettings(editablePlotLabel = TRUE, overwriteProtected = TRUE)
  gn <- GraphicsNotebook$new(tab = tab)
  setElements(gui, module, gn = gn)
  gn$makeGui(parent = plot.frame)
    
  
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
