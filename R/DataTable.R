#' Class implementing a data table as a GuiElement
#' Usage:
#' Define settings after initializing the class
#' Load data frame when makingGui 
#'  - the entire data frame will be displayed in the table but you can adjust the visibility afterwards to hide unwanted columns (or adjust the data frame before hand)
#'  - changing the order of the columns is not currently supported by the underlying model, instead just have the data frame in the right order when you pass it in
DataTable <- setRefClass(
  'DataTable',
  contains = 'GuiElement', 
  fields = list(
    icons = 'list', # keeps track of main icons this GuiElement uses 
    handlers = 'list', # handlers that the gui element needs to keep track of
    table = 'list' # holds the model (gtkTreeModel), view (gtkTreeView) and selection (gtkTreeSelection) objects
  ),
  methods = list(
    initialize = function(...) {
      callSuper(...)
      
      # FIXME: maybe define what the id columns are maybe in the table settings after all... (also order id maybe?)
      
      ### default setting for a data table
      setSettings(
        multiSelect = FALSE, # allow multiple selection?
        resizable = FALSE, # allow column resizing?
        sortable = FALSE, # allow sorting by clicking on column headers?
        editableColumns = c(), # any columns editable?
        invisibleColumns = c(), # any columns invisible?
        protect = TRUE
      )
      
      ### define data
      setData(
          frame = data.frame(empty = TRUE), # stores the data (make sure to initialize properly!)
          selectedRows = NULL # stores the selected rows
        )
    },
    
    # 'Makes the gtable based on the data frame
    # '@param parent (RGtkWidget) - some container like a ggroup
    # '@param selectionHandler - fired when row selection of the table changes
    # '@param changedHandler - fired when a column value in an editable table changes
    makeGui = function(parent, selectionHandler = NULL, changedHandler = NULL) {
      
      #INFO: for more information on this kind of data table layout, look at page 166+ of the R Gui guidebook!
      # topics include styling, how to have filters for columns (serachable --> look on page 172+), multiple selections, tooltips, signals, etc.
      # also, consider reading up more on gtkTreeView for its use as drag and drop source and destination (and potential reordering by drag and drop?)
      
      # gtk objects
      table$model <<- rGtkDataFrame(data$frame[0,]) 
      table$view <<- gtkTreeView (table$model)
      table$selection <<- table$view$getSelection()
      
      # styling
      table$view['enable-grid-lines'] <<- TRUE # grid lines between rows
      table$view['rules-hint'] <<- TRUE # nice alternating colors
      table$view['search-column'] <<- FALSE # ctrl-f controlled serach box
      
      ####### INTERNAL DRAG AND DROP ########
      # reordering rows (by drag and drop)
      table$view['reorderable'] <<- TRUE 
      # NOTE/FIXME: this doesn not work because the rGtkDtaFrame model deos not implement the gtkTreeViewDragSource and DragDest interfaces
      # solution: either reimplement drag and drop manually (find a way to block the underlying handlers implemented in C)
      # or: switch to using an original tree store (rather than the data.frame based model) although this would probably be slower --> read up in the R GUI book on how to use the iterators for doing this
      
      # attempt at internal drag and drop - throws error unless disableing the rows drag source and dest (because the default drag and drop handlers still want to fire)
      #table$view$enableModelDragSource('button1-mask', targetentries, c("default", "copy", "move")) #doesn't work because model doesn't have the interface implement
      #table$view$enableModelDragDest(targetentries, c("copy", "move", "link")) #if model had it implemented, this would not be necessary in the first place!
#       targetentries <- list(c("text/plain", "widget", 0))
#       gtkDragSourceSet(table$view, start.button.mask=c("button1-mask"), targets=targetentries, actions=c("move"))
#       gtkDragDestSet(table$view, flags="all", targets=targetentries, actions=c("move"))
#       view.onDragDataGet <- function(widget, context, selection, info, time, userdata) selection$setText("MOVE_ROW")
#       view.onDragDataReceived <- function (widget, context, x, y, selection, info, time, userdata) { 
#         if (identical(rawToChar(selection$getText()), "MOVE_ROW")) # passed on as raw
#           message("Dropping row to new index: ", gtkTreeViewGetDestRowAtPos(table$view, x, y)$path$getIndices()+1)
#       } 
#       # gObjectGetSignals(table$view) # doesn't give the underlying drag and drop handlers (implemented in c)
#       # gSignalConnect(table$view, "drag_drop", function(...) gSignalStopEmission(table$view, "drag_drop")) # doesn't work from R :(
#       gSignalConnect(table$view, "drag-data-get", view.onDragDataGet, after=TRUE)
#       gSignalConnect(table$view, "drag-data-received", view.onDragDataReceived, after=TRUE)
      ###### INTERNAL DRAG AND DROP #######
      
      
      # multi selection
      if (getSettings('multiSelect')) {
        table$selection$setMode ("multi") 
        table$view['rubber-banding'] <<- TRUE 
      } else
        table$selection$setMode ("single") 
      
      # selection handler
      if (!is.null(selectionHandler))
        handlers$selectionHandler <<- gSignalConnect (table$selection, "changed" , selectionHandler)
      
      # add columns
      #cell_renderer['background'] <- 'gray80' # can style columns if want to
      for (i in 1:ncol(data$frame))
        makeColumn(index = i, editable = names(data$frame)[i] %in% settings$editableColumns, type = class(data$frame[1,i]), changedHandler = changedHandler)
      
      # column resizability
      sapply ( seq_len(ncol (table$model)), function (i) table$view$getColumn(i - 1)$setResizable(getSettings('resizable')))
      
      # sortability
      if (getSettings('sortable'))
        sapply ( seq_len(ncol (table$model)), function (i) table$view$getColumn(i - 1)$setSortColumnId(i - 1))
     
      # add to parent
      scrolled_window <- gtkScrolledWindow ( ) 
      getToolkitWidget(parent)$add ( scrolled_window ) 
      scrolled_window$add ( table$view )
      scrolled_window$setPolicy ("automatic", "automatic")
      setWidgets(tableGroup = scrolled_window)
      
      # adjust visibility
      changeColumnVisibility(settings$invisibleColumns, FALSE) 

      # FIXME this is a hack
      # (this widget is only made so autoSave works, which requires at least 1 widget in the guielement but since DataTable saves the view in $table, need this)
      setWidgets(autosave = glabel('enable-autosave'))
    },
    
    # ' make a view column in the table for the column with index in the data frame used in the model
    # ' for editable cells, the standard cell change handler in this class is called (feel free to overwrite)
    # ' FIXME can't deal with date properly
    makeColumn = function(index, name = colnames(table$model)[index], editable = FALSE, type = c("numeric", "integer", "character", "logical", "factor", "icon"), changedHandler = NULL) {
      type = match.arg(type)
      
      ## define cell renderer
      if (editable)
        cr <- switch(type,
                     "logical" = gtkCellRendererToggle(),
                     "factor" = gtkCellRendererCombo(),
                     gtkCellRendererText())
      else
        cr <- gtkCellRendererText()
      
      # initialize column
      vc <- gtkTreeViewColumnNewWithAttributes(name, cr)
      #vc$setClickable(TRUE)
      table$view$InsertColumn(vc, -1)  # always add column at the end of treeView (-1) but could also specify here
      
      # link back to model (which is 0 index based) 
      if (editable && type == "logical") vc$addAttribute(cr, "active", index - 1) 
      else vc$addAttribute(cr, "text", index - 1)
      
      # styling
      if (type == "numeric") cr['xalign'] <- 1
      
      # editability
      if (editable) {
        # enable editability
        if (type == "logical") cr["activatable"] <- TRUE
        else cr["editable"] <- TRUE
        
        # combo box needs its own data store for editing (add levels of the factor to the combo box)
        if(type == "factor") { 
          cstore <- gtkListStore("gchararray")
          sapply(levels(getTableData(1, index)), function(lvl) cstore$setValue(cstore$append()$iter,column=0, as.character(lvl)) )
          cr['model'] <- cstore
          cr['text-column'] <- 0        
        }
        
        # connect callback
        gSignalConnect(cr, signal = if(type != "logical") "edited" else "toggled",
                             f = .self$columnChanged, 
                             data = list(column = colnames(table$model)[index]))
        
        # custom changed handler
        if (!is.null(changedHandler)) 
          gSignalConnect(cr, signal = if(type != "logical") "edited" else "toggled",
                         f = changedHandler, 
                         data = list(column = colnames(table$model)[index]))
      }
    },
    
    # ' default column data changed handler (for editable columns)
    # ' overwrite if need be
    columnChanged = function(cell, path, newValue, ...) {
      row <- as.numeric(path) + 1
      if(nargs() == 3) {
        userData <- newValue 
        newValue <- !table$model[row, userData$column] # invert value (toggle)
      } else 
        userData <- ..1
      l <- list()
      l[userData$column] <- newValue
      editRow(row, l)
      return(FALSE)
    },

    # ' destroy the GUI (finalize the tableGroup toplevel group)
    destroyGui = function() {
      if (!is.null(widgets$tableGroup) && !identical(class(widgets$tableGroup), "<invalid>")) 
        gtkWidgetDestroy(widgets$tableGroup) # make sure the table is properly finalized
      callSuper()
    },
    
    # ' load the user interface
    loadGui = function() {
      setTableData(data$frame)
      selectRows(data$selectedRows)
      callSuper()
    },
    
    # ' save gui
    saveGui = function() {
      setData(
        frame = getTableData(drop = FALSE),
        selectedRows = getSelectedRows()
      )
      callSuper()
    },
    
    
    # change the visibility on certain columns
    # @param visible - either a single value (TRUE/FALSE) that will be applied to all columns or a vector of same length as columnns
    changeColumnVisibility = function (columns, visible) {      
      # get columns and current visibility
      if (identical(class(columns), 'character'))
        columns <- which(colnames(table$model)%in%columns)
      visibility <- sapply( seq_len(ncol(table$model)), function(i) table$view$getColumn(i-1)$getVisible())
      
      # set visibility
      visibility[columns] <- visible
      mapply(function(col, vis) col$setVisible(vis), table$view$getColumns(), visibility) 
    },
        
    ###### GET DATA ######
    
    #' Get the whole data frame in the table (or parts of it)
    #' could get the data frame backing the model also via as.data.frame(table$model) pr by table$model[,] but
    #' might as well use this one for a little more flexbility
    #' @param rows only supports indices (not rownames, they mean nothing to GTK)
    #' @param columns can be either indices or column names
    #' @param drop - whether to drop to a list if there's only a single entry (default: TRUE)
    getTableData = function (rows, columns, drop = TRUE) {
      if (missing(rows))
        rows <- seq_len(nrow(table$model)) # return all rows by default
      if (missing(columns)) 
        columns <- seq_len(ncol(table$model)) # return all columns by default
      return(table$model[rows, columns, drop = drop])
    },
    
    # ' Get Row numbers from values in the specified column
    # 'FIXME: perhaps implement posibility to specifiy as many column conditions as you like
    # if needing a more specific output, can still use subset(getTableData(), ...) and do it manually
    # maybe: ss <- expression(x == "a")
    #        subset(d, eval(ss))
    getRowsByValues = function(...) {
      filter <- list(...) # currently only supporting the first condition, unfortuantely (FIXME)
      return(which(getTableData(columns = names(filter)[1])%in%filter[[1]]))
    },
    
    # 'Get the indices of the selected rows
    # 'returns NULL if nothing is selected
    getSelectedRows = function() {
      selected_rows <- table$selection$getSelectedRows()
      if ( length ( selected_rows$retval ) ) 
        return(sapply ( selected_rows$retval , gtkTreePathGetIndices ) + 1)
      return (NULL)
    },
    
    # 'Get the values of the selected rows
    # 'This is just a helper function for easier interaction with selected data
    getSelectedValues = function(columns){
      if ( !is.null(rows <- getSelectedRows()))
        return (getTableData(rows, columns))
      return (NULL)
    },
    
    #### SET DATA ####
    
    # ' Update data in data frame
    setTableData = function(data) {
      if (identical(lapply(data, mode),lapply(getTableData(), mode))) # make sure the data frames are identical in structure
        table$model$setFrame(data)
      else
        stop("Trying to overwrite the data frame in the DatTable with a data frame that has different struture from the original design. If this is really what is intended, you must first destroyGui() and then make it new with makeGui()")
    },
    
    # ' add row at a specific position
    # FIXME: implement that it does position properly
    # ' @param position: index where to add rows (0 = at the end)
    addRow = function(..., position = 0) {
      emptyRow <- table$model[0,] # copy structure
      emptyRow[1,1] <- NA # initialize
      # initialize factors
      for (col in which(sapply(emptyRow, class) == "factor")) 
        emptyRow[1,col] <- levels(table$model[,col])[1]
      # initialize logical
      for (col in which(sapply(emptyRow, class) == "logical")) 
        emptyRow[1,col] <- FALSE
      table$model$appendRows(emptyRow) # add empty row
      row <- nrow(table$model) # newly added row
      editRow(row, ...) # update row with proper data
      return(row) # return row index of the new row
    },
    
    # 'edits row
    # '@param row index of the row to edit
    # '@param ... -> can be 'data.frame' or 'list' or 'a = 4, b = "test", c = TRUE, ...'
    #                but must be the correct data type! (will try to coerce)
    # '@return - update succesful
    editRow = function(row, ...) {
      vals <- list(...)
      if (nargs() == 2)
        if (class(..1) == 'data.frame')
          vals <- as.list(..1) # single data frame passed in
        else if (class(..1) == 'list')
          vals <- ..1 # single list passed in     
      
      # update table row     
      for (col in names(vals)) {
        if (col%in%colnames(table$model)) { # update table field
         # cast to correct type
          newValue <- try(switch(class(table$model[,col]),
                   "integer" = as.integer(as.numeric(vals[[col]])),
                   "character" = as.character(vals[[col]]),
                   "numeric" = as.numeric(vals[[col]]),
                   "factor"  = as.character(vals[[col]]),
                   "logical" =  as.logical(vals[[col]])),
                silent=TRUE)
          
          # check if something went wrong
          if(inherits(newValue,"try-error")) 
            sprintf("Failed to coerce new value to type %s",userData$type)

          # check if there's trouble with a factor
          if(class(table$model[,col]) == "factor") {
            curLevels <- levels(table$model[,col])
            if(! newValue %in% curLevels) 
              message("Can't add level to a factor.")
          }
          
          # assign new value
          table$model[row, col] <<- newValue
        } else 
          dmsg("WARNING:\n\tTrying to update table column '", col, "' but column does not exist in table (", paste(colnames(table$model), collapse=", "), ")")  
      }
    },
    
    # 'Remove rows with the provided index
    # 'Combine this with getRowsByValues(...) to find the right indices
    removeRows = function(rows) {
      if (length(rows) > 0) {
        if ( length(selected <- setdiff(getSelectedRows(), rows)) > 0 ) # rows that will not get deleted and should remain selected
          selected <- sapply(selected, function(i) i - sum(rows < i)) # adjust index
        
        selectRows(NULL, blockHandler = TRUE) # deselect all 
        table$model$setFrame(getTableData()[-rows,]) # remove rows
        selectRows(selected, blockHandler = TRUE) # reselect what still exists
      }
    },
    
    ##### SELECT DATA #####
    
    # Select rows (can select multiple but if the table is in 'single' mode, only one will be possible)
    # FIXME: implement that it also focus on the right row rather than just selecting it!
    # ' @indices - which indices to select
    # ' @param - blockHandler (whether to block selection handler from triggering)
    selectRows = function(rows, blockHandler = FALSE) {
      # block handler if requested
      if (blockHandler)
        gSignalHandlerBlock(table$selection, handlers$selectionHandler)
      
      # make selection
      table$selection$unselectAll()
      if (length(rows) > 0) {
        for (index in rows)
          table$selection$selectPath(gtkTreePathNewFromIndices(index-1))
      }
      
      # unblock handler
      if (blockHandler)
        gSignalHandlerUnblock(table$selection, handlers$selectionHandler)
    },

    selectRowsByValues = function(..., blockHandler = FALSE) {
      selectRows(getRowsByValues(...), blockHandler = blockHandler)
    },
    
    # 'implement it with testthat package syntax and a modal dialog so user needs to click all buttons maybe?
    # 'TODO: figure out first exactly how testthat works
    test = function() {
      # FIXME: implement more tests for multiple selection data table
      # FIXME: run it as a modal dialog and read back out after user clicked the different buttons what should have happend
      # FIXME: can I make it click the buttons automatically by itself?

      options("guiToolkit"="RGtk2")
      win <- ggroup(horizontal = FALSE, cont=gwindow("DataTable test window"), expand=TRUE)
      bgrp <- ggroup(horizontal = TRUE, cont=win)
      bgrp2 <- ggroup(horizontal = TRUE, cont=win)
      bgrp3 <- ggroup(horizontal = TRUE, cont=win)
      tgrp <- ggroup(cont = win, expand=TRUE)
      
      # test implementations
      gbutton ("Select row ID=2", cont=bgrp, handler = function(...) test$selectRowsByValues(ID = 2))
      gbutton ("Hide column logical and factor", cont=bgrp, handler = function(...) test$changeColumnVisibility(c('logical', 'factor'), FALSE))
      gbutton ("Show column logical and factor again and hide ID", cont=bgrp, handler = function(...) test$changeColumnVisibility(c('ID', 'logical', 'factor'), c(FALSE, TRUE, TRUE)))
      gbutton ("Add new row", cont=bgrp, handler = function(...) {
        newID <- max(test$getTableData(columns = 'ID')) + 1
        row <- test$addRow(ID = newID, character = "new row", logical = FALSE, numeric = rnorm(1), factor="orange")
        test$selectRows(row)
      })
      gbutton ("Edit selected row", cont=bgrp, handler = function(...) {
        test$editRow(test$getSelectedRows(), character = 'new value', numeric = rnorm(1))
      })
      gbutton ("Remove row ID=2, ID=3", cont=bgrp2, handler = function(...) test$removeRows(test$getRowsByValues(ID = c(2,3))))
      gbutton ("Remove rows 1 and 3", cont=bgrp2, handler = function(...) test$removeRows(c(1,3)))
      gbutton ("Update data frame", cont=bgrp2, handler = function(...) test$setTableData(DF))
      gbutton ("Remake table\n(with sortable/resizable cols) and editable columns", cont=bgrp2, handler = function(...) {
        test$destroyGui()
        test$setSettings(sortable = TRUE, resizable = TRUE)
        test$setSettings(editableColumns = c("integer", "character", "logical", "factor", "numeric"))
        test$makeGui(tgrp, DF)
      })
      gbutton ("Save gui", cont=bgrp3, handler = function (...) {
        test$saveGui()
        print(test$getData('frame'))
        print(test$getData('selectedRows'))
      })
      
      # running the DataTable
      test <- DataTable$new()
      
      DF = data.frame(
        ID = 1:3,
        logical = c(TRUE, TRUE, FALSE),
        character = c("one","two","three"),
        factor = factor(c("banana", "apple", "orange")),
        integer = sample(3),
        numeric = rnorm(3),
        stringsAsFactors=FALSE)
      
      test$setData(frame = DF, selectedRows = 2)
      test$setSettings(sortable = TRUE, resizable = TRUE)
      test$setSettings(editableColumns = c("integer", "character", "logical", "factor", "numeric"))
      test$makeGui(tgrp, selectionHandler = function(...) print(data.frame(test$getSelectedValues())))
      
      test$loadGui()
      return (test)
    }
  )
)

# run test
# t <- DataTable$new()$test()