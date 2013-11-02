# S4 class DataFrameViewerGUI
DataFrameViewerGUI <- setClass("DataFrameViewerGUI", contains="BaseGUI")

setMethod("getMenuXML", "DataFrameViewerGUI", function(gui, module) {
  return (
    '<menu name = "DFV" action="DFV">
    <menuitem action="Help"/>
    <menuitem action="Reload"/>
    <menuitem action="Quit" />
    </menu>')
})

setMethod("getToolbarXML", "DataFrameViewerGUI", function(gui, module) {
  return ('')
})

setMethod("setNavigationActions", "DataFrameViewerGUI", function(gui, module, actionGrp) {
  nav.actions <-
    list(## name, icon, label , accelerator , tooltip , callback
      list ("DFV" , NULL , "_DFV" , NULL , NULL , NULL ) ,
      list ("Help" , "gtk-info" ,"Help" , "<ctrl>H" , NULL , function(...) gmessage("sorry, not implemented yet") ) ,
      list ("Reload" , "gtk-refresh" ,"Reload Screen" , "<ctrl>R" , NULL , function(...) remakeGUI(gui, module) ) , # FIXME: disable the keyboard shortcut!
      list ("Quit", "gtk-quit", "Quit", "<ctrl>Q", "Quit program", function(...) destroyGUI(gui, module) ))
  
  actionGrp$addActions(nav.actions)
})

setMethod("makeMainGUI", "DataFrameViewerGUI", function(gui, module) {
  # top level groups
  setMenuGroup(gui, module, ggroup(horizontal=FALSE, cont=getWinGroup(gui, module), spacing=0))
  mainGrp <- ggroup(horizontal=FALSE, cont=getWinGroup(gui, module), spacing=0, expand=TRUE)
  #setToolbarGroup(gui, module, ggroup(horizontal=FALSE, cont=getWinGroup(gui, module), spacing=0))
})
