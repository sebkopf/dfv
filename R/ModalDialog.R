# ' This module is a basic implementation of a modal dialog and can be extended easily

ModalDialogGui <- setClass("ModalDialogGui", contains="BaseGui")

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
    
    #' By default, Modal Dialog fetches data and settings from its GuiElements when saving the Gui
    saveGui = function(fetchData = TRUE, fetchSettings = TRUE) {
      callSuper(fetchData = fetchData, fetchSettings = fetchSettings)
    },
    
    # 'Whether the dialog was saved (i.e. OK/Apply button pressed)
    dialogSaved = function() {
      return (getData('saved'))
    }
  )
)