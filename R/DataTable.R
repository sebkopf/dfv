#' Class implementing a data table as a GuiElement
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
        protect = TRUE
      )
    },
    
    # 'Makes the gtable. If it already exists in the widgets, will delete the existing one first and then remake it.
    # '@param parent (RGtkWidget) - some container like a ggroup
    # '@param data (data.frame) - a data frame (can be just the definition without any rows but needs to be the correct row types to work!)
    makeGui = function(parent, data, changedHandler = NULL) {
      
      #INFO: for more information on this kind of data table layout, look at page 166+ of the R Gui guidebook!
      # topics include styling, how to have filters for columns (serachable --> look on page 172+), multiple selections, tooltips, signals, etc.
      # also, consider reading up more on gtkTreeView for its use as drag and drop source and destination (and potential reordering by drag and drop?)
      
      # store data
      setData(frame = data)
      
      # gtk objects
      table$model <<- rGtkDataFrame(data) 
      table$view <<- gtkTreeView (table$model)
      table$selection <<- table$view$getSelection()
      table$columnMap <<- seq_len(ncol(table$model)) # FIXME: implement proper tracking of what index in the model columns corersponds to what column in the view (so applying visible and such is done appropriately! - this is important even for the basic toggle table)
      
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
      if (!is.null(changedHandler))
        handlers$changedHandler <<- gSignalConnect (table$selection, "changed" , changedHandler)
      
      # add columns
      mapply(table$view$insertColumnWithAttributes,
               position = -1,
               title = colnames(table$model),
               cell = list(gtkCellRendererText()), text = seq_len(ncol(table$model)) - 1)
      
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
    },
    
    # 'Render a column as text (gtkCellRendererText)
    makeTextColumn = function (columns) {
      #IMPLEMENT ME
      # make text column and set resizability and sortability according to the settings
      # keep track of what data frame column index in the model corresponds to what index in the view columns via updating the columnMap field
    },
    
    # 'Render a column as a checkbox (gtkCellRendererToggle)
    makeCheckboxColumn = function (...) {
      
    },
    
    # 'Render a column as a combobox (gtkCellRendererCombo)
    makeComboColumn = function(...) {
      # for a good example of all of these, just look at the example editableDataFrame in the RGtk2 examples of the ProgGUIinR package
    },
    
    destroyGui = function() {
      if (!is.null(widgets$tableGroup) && !identical(class(widgets$tableGroup), "<invalid>")) 
        gtkWidgetDestroy(widgets$tableGroup) # make sure the table is properly finalized
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
    #' could get the data frame backing the model also via as.data.frame(table$model) but
    #' might as well use this one for a little more flexbility
    #' @param rows only supports indices (not rownames, they mean nothing to GTK)
    #' @param columns can be either indices or column names
    getTableData = function (rows, columns) {
      if (missing(rows))
        rows <- seq_len(nrow(table$model)) # return all rows by default
      if (missing(columns)) 
        columns <- seq_len(ncol(table$model)) # return all columns by default
      if (identical(class(columns), 'character'))
        columns <- which(colnames(table$model)%in%columns)
      
      if (length(columns) > 0) {
        df <- data.frame(table$model[rows, columns], stringsAsFactors=FALSE)
        names(df) <- colnames(table$model)[columns]
        return(df)
      } else
        return (NULL) # no columns!
    },
    
    # ' Get Row numbers from values in the specified column
    # 'FIXME: perhaps implement posibility to specifiy as many column conditions as you like
    # maybe: ss <- expression(x == "a")
    #        subset(d, eval(ss))
    getRowsByValues = function(...) {
      filter <- list(...) # currently only supporting the first condition, unfortuantely (FIXME)
      return(which(getTableData(columns = names(filter)[1])[[1]]%in%filter[[1]]))
    },
    
    # 'Get the indices of the selected rows
    # 'returns NULL if nothing is returned
    getSelectedRows = function() {
      selected_rows <- table$selection$getSelectedRows()
      if ( length ( selected_rows$retval ) ) 
        return(sapply ( selected_rows$retval , gtkTreePathGetIndices ) + 1)
      return (NULL)
    },
    
    # 'Get the values of the selected rows
    getSelectedValues = function(columns){
      if ( !is.null(rows <- getSelectedRows()))
        return (getTableData(rows, columns))
      return (NULL)
    },
    
    #### SET DATA ####
    
    # ' Update data in data frame
    setTableData = function(data) {
      if (identical(lapply(data, class),lapply(getTableData(), class))) # make sure the data frames are identical in structure
        table$model$setFrame(data)
      else
        stop("Trying to overwrite the data frame in the DatTable with a data frame that has different struture from the original design. If this is really what is intended, you must first destroyGui() and then make it new with makeGui()")
    },
    
    # ' add rows at a specific position
    # @param position: index where to add rows
    addRows = function(data, position = -1) {
      if (identical(lapply(data, class),lapply(getTableData(), class))) { # make sure the data frames are identical in structure
        # FIXME: implement that this works smoothly!
      } else
        stop("Trying to edit the data frame in the DataTable with a data frame that has different struture from the original design. If this is really what is intended, you must first destroyGui() and then make it new with makeGui()")
    },
    
    # ' edits rows
    # '@param rows indices of the rows to edit
    # '@param data (must be the same nrow as rows)
    editRows = function(rows, data) {
      # FIXME: test this!
      if (identical(lapply(data, class),lapply(getTableData(), class))) { # make sure the data frames are identical in structure
        if (length(rows) == nrow(data)) {
          df <- getTableData(rows) # get data
          df[rows, ] <- data # update data
          selected <- getSelectedRows() # save selection
          setTableData(df) # set data
          selectRows(selected, blockHandlers = TRUE) # reselect
        } else 
          stop("Rows to edit (", paste(rows, collapse=", "), ") are not the same number as data provided: ", nrow(data))
      } else
        stop("Trying to edit the data frame in the DataTable with a data frame that has different struture from the original design. If this is really what is intended, you must first destroyGui() and then make it new with makeGui()")
    },
    
    # 'Remove rows with the provided index
    removeRows = function(rows) {
      if (length(rows) > 0) {
        if ( length(selected <- setdiff(getSelectedRows(), rows)) > 0 ) # rows that will not get deleted and should remain selected
          selected <- sapply(selected, function(i) i - sum(rows < i)) # adjust index
        
        selectRows(NULL, blockHandler = TRUE) # deselect all 
        table$model$setFrame(getTableData()[-rows,]) # remove rows
        selectRows(selected, blockHandler = TRUE) # reselect what still exists
      }
    },
    
    # 'Remove rows by values of a column
    removeRowsByValues = function(...) {
      removeRows(getRowsByValues(...))
    },
    
    ##### SELECT DATA #####
    
    # ' @indices - which indices to select
    # ' @param - blockHandler (whether to block selection handler from triggering)
    selectRows = function(indices, blockHandler = FALSE) {
      # block handler if requested
      if (blockHandler)
        gSignalHandlerBlock(table$selection, handlers$changedHandler)
      
      # make selection
      table$selection$unselectAll()
      if (length(indices) > 0) {
        for (index in indices)
          table$selection$selectPath(gtkTreePathNewFromIndices(index-1))
      }
      
      # unblock handler
      if (blockHandler)
        gSignalHandlerUnblock(table$selection, handlers$changedHandler)
    },

    selectRowsByValues = function(..., blockHandler = FALSE) {
      selectRows(getRowsByValues(...), blockHandler = blockHandler)
    },
    
    # 'implement it with testthat package syntax and a modal dialog so user needs to click all buttons maybe?
    # 'TODO: figure out first exactly how testthat works
    test = function() {
      # FIXME: run it as a modal dialog and read back out after user clicked the different buttons what should have happend
      # FIXME: can I make it click the buttons automatically by itself?
      
      options("guiToolkit"="RGtk2")
      win <- ggroup(horizontal = FALSE, cont=gwindow("DataTable test window"), expand=TRUE)
      bgrp <- ggroup(horizontal = TRUE, cont=win)
      bgrp2 <- ggroup(horizontal = TRUE, cont=win)
      tgrp <- ggroup(cont = win, expand=TRUE)
      
      # test implementations
      gbutton ("Hide column a and c", cont=bgrp, handler = function(...) test$changeColumnVisibility(c('a', 'c'), FALSE))
      gbutton ("Show column a and c again and hide b", cont=bgrp, handler = function(...) test$changeColumnVisibility(c('a', 'b', 'c'), c(TRUE, FALSE, TRUE)))
      gbutton ("Select row a=4", cont=bgrp, handler = function(...) test$selectRowsByValues(a = 4))
      gbutton ("Remove row a=2, a=3", cont=bgrp2, handler = function(...) test$removeRowsByValues(a = c(2,3)))
      gbutton ("Remove rows 3 and 5", cont=bgrp2, handler = function(...) test$removeRows(c(3,5)))
      gbutton ("Update data frame", cont=bgrp2, handler = function(...) test$setTableData(data.frame(a=1:20, b='test', c='wurst')))
      gbutton ("Remake table\n(with sortable/resizable cols)", cont=bgrp2, handler = function(...) {
        test$destroyGui()
        test$setSettings(sortable = TRUE, resizable = TRUE, overwriteProtected = TRUE)
        test$makeGui(tgrp, data.frame(adiff=letters[10:1], bdiff=1:10))
      })
      
      # running the DataTable
      test <- DataTable$new()
      test$makeGui(tgrp, data.frame(a=1:5, b='test', c='wurst'), changedHandler = function(...) print(test$getSelectedValues()))
    }
  )
)

DataTable$new()$test()