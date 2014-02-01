# ' This module is a basic implementation of a modal dialog and can be extended easily

ModalDialogGui <- setClass("ModalDialogGui", contains="BaseGui")

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

setMethod("getToolbarXML", "ModalDialogGui", function(gui, module) {
  return (
    nav <- '
    <separator expand="true"/>
    <toolitem action="Ok"/>
    <separator expand="true"/>
    <toolitem action="Cancel"/>
    <separator expand="true"/>
    '
  )
})

setMethod("setNavigationActions", "ModalDialogGui", function(gui, module, actionGrp) {
  nav.actions <-
    list(## name, icon, label , accelerator , tooltip , callback
      list ("Ok", getSettings(gui, module, "ok.icon"), getSettings(gui, module, "ok.label") , getSettings(gui, module, "ok.key"), getSettings(gui, module, "ok.tooltip"), 
            function(...) {
              setData(gui, module, saved = TRUE)
              getModule(gui, module)$saveGui()
              destroyGui(gui, module)
            } ),
      list ("Cancel" , getSettings(gui, module, "cancel.icon"), getSettings(gui, module, "cancel.label") , NULL, getSettings(gui, module, "cancel.tooltip"), 
            function(...) { 
              destroyGui(gui, module)
            }))
  actionGrp$addActions(nav.actions)
})

setMethod("makeNavigation", "ModalDialogGui", function(gui, module) {
  # set toolbar at the end of screen
  setToolbarGroup(gui, module, ggroup(horizontal=TRUE, cont=getWinGroup(gui, module), spacing=0, expand=FALSE))
  callNextMethod(gui, module)
})

ModalDialog <- setRefClass(
  'ModalDialog',
  contains = 'Module',
  methods = list(
    initialize = function(gui = ModalDialogGui(), ...){
      callSuper(gui = gui, ...)
      
      ### default setting for ModalDialog
      setSettings(
        windowModal = TRUE, # define window as modal
        ok.icon = "gtk-apply", # define button settings
        ok.label = "Ok",
        ok.tooltip = "Apply",
        ok.key = NULL,
        cancel.icon = "gtk-cancel",
        cancel.label = "Cancel",
        cancel.tooltip = "Cancel",
        protect = TRUE
      )
    }, 

    loadGui = function() {
      callSuper()
      setData(saved = FALSE) # records response from dialog
    },
    
    # 'Whether the dialog was saved (i.e. OK/Apply button pressed)
    dialogSaved = function() {
      return (getData('saved'))
    }
  )
)