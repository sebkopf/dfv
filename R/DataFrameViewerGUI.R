# S4 class DataFrameViewerGUI
DataFrameViewerGUI <- setClass("DataFrameViewerGUI", contains="BaseGUI")

setMethod("getMenuXML", "DataFrameViewerGUI", function(gui, module) {
  return (
    '<menu name = "DFV" action="DFV">
      <menuitem action="Reload"/>
      <menuitem action="SaveToWS"/>
      <menuitem action="Quit"/>
    </menu>
    <menu name = "Help" action="Help">
      <menuitem action="ggplot"/>
    </menu>')
})

setMethod("getToolbarXML", "DataFrameViewerGUI", function(gui, module) {
  return ('')
})

setMethod("setNavigationActions", "DataFrameViewerGUI", function(gui, module, actionGrp) {
  nav.actions <-
    list(## name, icon, label , accelerator , tooltip , callback
      list ("DFV" , NULL , "_DFV" , NULL , NULL , NULL ),
      list ("Reload" , "gtk-refresh" ,"Reload Screen" , "<ctrl>R" , NULL , function(...) remakeGUI(gui, module) ) , # FIXME: disable the keyboard shortcut!
      list ("SaveToWS" , "gtk-home" , "Save To Workspace" , "<ctrl>H" ,"Save settings and data to workspace" , function(...) { 
        setSettings(gui, module, lrPane = svalue(getWidget(gui, module, 'lrPane'))) # save paned group positions
        showInfo(gui, module, "Saving to workspace...", timer=1, okButton=FALSE)
        module$saveToWorkspace()
        showInfo(gui, module, "Data Frame Viewer settings and data succesfully saved to workspace.", timer=2, okButton=FALSE)
      }) , 
      list ("Quit", "gtk-quit", "Quit", "<ctrl>Q", "Quit program", function(...) destroyGUI(gui, module) ),
      list ("Help" , NULL , "_Help" , NULL , NULL , NULL ),
      list ("ggplot" , "gtk-info" ,"ggplot2" , NULL , NULL , function(...) browseURL("http://ggplot2.org/") )
      )
  
  actionGrp$addActions(nav.actions)
})

setMethod("makeMainGUI", "DataFrameViewerGUI", function(gui, module) {
  # top level groups
  setMenuGroup(gui, module, ggroup(horizontal=FALSE, cont=getWinGroup(gui, module), spacing=0))
  mainGrp <- ggroup(horizontal=FALSE, cont=getWinGroup(gui, module), spacing=0, expand=TRUE)
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
  gpanedgroup(code.frame, df.frame, container=left.grp, expand=TRUE, horizontal=FALSE)
  right.grp<-ggroup(expand=TRUE)  
  plot.frame<-gframe("Plots", cont=right.grp, expand=TRUE, horizontal=FALSE)
#  dfv$pn<-pn.GUI(plot.frame, dfv$win, 
#                 newPlotObj=DFV.newPlotTabObj(), newPlotObjLoadHandler=NULL, # specifically no new plotobj load handler to keep plot params from previous plot 
#                 plotObjLoadHandler=dfv.loadPlotHandler,
#                 plotEventHandlers=dfv.plotEventHandlers)
  gbutton(action=gaction("Save to\nWorkspace", icon="gtk-home", handler=function(h,...) DFV.saveToWorkspace(dfv), tooltip="Save to workspace to resume working on these plots at a later time."), cont=dfv$pn$buttons.grp)
  dfv$plotParams$errorMsg<-glabel("", cont=plot.frame)
  lrPane <- gpanedgroup(left.grp,right.grp,container=dfs.tab, expand=TRUE)
  setWidgets(gui, module, lrPane = lrPane)
  
  # handlers
#  addHandlerChanged(dfv$df.nb, handler=function(h,...) svalue(dfv$plotParams$df)<-names(h$obj)[h$pageno])
#  addHandlerClicked(dfv$dfs.table, handler=function(h,...) DFV.selectDataFrame(dfv))
#  addHandlerKeystroke(dfv$plotParams$code, handler=function(h,...) if (h$key=="\022") DFV.execCode(dfv) )
  
#  for (var in names(dfv$plotParams))
#    adddroptarget(dfv$plotParams[[var]], targetType="object", handler=
#                    function(h,...) svalue(h$obj)<-gWidgets::id(h$dropdata)) 
  
  # load
#  DFV.clearPlotParams(dfv)
    
  visHandler <- addHandlerFocus(getWindow(gui, module), handler=function(...) {
    # loading divider positions from settings
    svalue(lrPane) <- getSetting(gui, module, 'lrPane')
    # block Handler (only want it to fire once)  
    blockHandler(getWindow(gui, module), ID=visHandler)
  })
  
})
