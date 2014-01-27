# ' This module is a basic implementation of a modal dialog and can be extended easily

ModalDialogGUI <- setClass("ModalDialogGUI", contains="BaseGUI")


setMethod("getToolbarXML", "ModalDialogGUI", function(gui, module) {
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

setMethod("setNavigationActions", "ModalDialogGUI", function(gui, module, actionGrp) {
  nav.actions <-
    list(## name, icon, label , accelerator , tooltip , callback
      list ("Ok", getSettings(gui, module, "ok.icon"), getSettings(gui, module, "ok.label") , NULL, getSettings(gui, module, "ok.tooltip"), 
            function(...) {
              setData(gui, module, saved = TRUE)
              getModule(gui, module)$saveGUI()
              destroyGUI(gui, module)
            } ),
      list ("Cancel" , getSettings(gui, module, "cancel.icon"), getSettings(gui, module, "cancel.label") , NULL, getSettings(gui, module, "cancel.tooltip"), 
            function(...) { 
              destroyGUI(gui, module)
            }))
  actionGrp$addActions(nav.actions)
})

setMethod("makeNavigation", "ModalDialogGUI", function(gui, module) {
  # set toolbar at the end of screen
  setToolbarGroup(gui, module, ggroup(horizontal=TRUE, cont=getWinGroup(gui, module), spacing=0, expand=FALSE))
  callNextMethod(gui, module)
})

ModalDialog <- setRefClass(
  'ModalDialog',
  contains = 'Module',
  methods = list(
    initialize = function(gui = ModalDialogGUI(), ...){
      callSuper(gui = gui, ...)
      
      ### default setting for ModalDialog
      setSettings(
        windowModal = TRUE, # define window as modal
        ok.icon = "gtk-apply", # define button settings
        ok.label = "Ok",
        ok.tooltip = "Apply",
        cancel.icon = "gtk-cancel",
        cancel.label = "Cancel",
        cancel.tooltip = "Cancel",
        overwriteProtected = TRUE,
        protect = TRUE
      )
    }, 

    loadGUI = function() {
      callSuper()
      setData(saved = FALSE) # records response from dialog
    },
    
    # 'Whether the dialog was saved (i.e. OK/Apply button pressed)
    dialogSaved = function() {
      return (getData('saved'))
    }
  )
)