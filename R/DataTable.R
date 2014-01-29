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
    },
    
    # 'Makes the gtable. If it already exists in the widgets, will delete the existing one first and then remake it.
    # '@param parent (RGtkWidget) - some container like a ggroup
    # '@param data (data.frame) - a data frame (will only use the definition, not the whole data)
    makeGui = function(parent, data, expand = TRUE, invisible = c(), changedHandler = NULL, ...) {
      
      #INFO: for more information on this kind of data table layout, look at page 166+ of the R Gui guidebook!
      # topics include styling, how to have filters for columns (serachable --> look on page 172+), multiple selections, tooltips, signals, etc.
      
      # store data
      data$frame <<- data
      
      table$model <<- rGtkDataFrame(data) 
      #test_model <- gtkTreeModelSortNewWithModel(table$model)
      table$view <<- gtkTreeView (table$model)
      
      # styling
      table$view['enable-grid-lines'] <<- TRUE # grid lines between rows
      table$view['rules-hint'] <<- TRUE # nice alternating colors
      table$view['search-column'] <<- FALSE # ctrl-f controlled serach box
      table$view['reorderable'] <<- TRUE # FIXME: this doesn't actually work properly, not sure why not (there's no drag/drop events really) - should allow vertical drag and drop...
      
      table$selection <<- table$view$getSelection()
      table$selection$setMode ("single") # could upgrade this to allow multiple selections!
      #table$view['rubber-banding'] <- TRUE # also for multiple selections: 
      
      
      # add columns
      mapply(table$view$insertColumnWithAttributes,
               position = -1,
               title = colnames(table$model),
               cell = list(gtkCellRendererText()), text = seq_len(ncol(table$model)) - 1)
      
      # enable column sorting by clicking on the header
      # sapply ( seq_len(ncol (table$model)), function (i) table$view$getColumn(i - 1)$setSortColumnId(i - 1))
      
      # set visibility
      mapply(function(col, vis) col$setVisible(vis), table$view$getColumns(), !colnames(table$model)%in%invisible) # NOTE: this is great! don't need to recreate table, with this can make columns visible/invisible as needed
      
      # selection handler
      if (!is.null(changedHandler))
        handlers$changedHandler <<- gSignalConnect (table$selection, "changed" , changedHandler)
      
      # add to parent
      scrolled_window <- gtkScrolledWindow ( ) 
      getToolkitWidget(parent)$add ( scrolled_window ) 
      scrolled_window$add ( table$view )
      scrolled_window$setPolicy ("automatic", "automatic")
      setWidgets(tableGroup = scrolled_window)
    },
    
    addRows = function() {
      #implement me
      #table$model$appendRows()
    },
    
    removeRows = function(rows) {
      # implement me - can only be done by replacing the data frame underlying the model!
      #table$model$setFrame(data.frame minus the rows)
    },
    
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
    
    # 'Get the indices of the selected rows
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

    selectRowsByValues = function(column, values, blockHandler = FALSE) {
      indices <- which(getTableData(columns = column)[[1]]%in%values)
      selectRows(indices, blockHandler = blockHandler)
    },
    
    # FIXME: implement destroy Gui properly in base classes
    destroyGui = function() {
      if (!is.null(widgets$tableGroup))
        gtkWidgetDestroy(widgets$tableGroup) # make sure the table is properly finalized
      #callSuper()
    },
    
    # 'implement it with testthat package syntax and a modal dialog so user needs to click all buttons maybe?
    # 'TODO: figure out first exactly how testthat works
    test = function() {
      # FIXME: run it as a modal dialog and read back out after user clicked the different buttons what should have happend
      # FIXME: can I make it click the buttons automatically by itself?
      
      options("guiToolkit"="RGtk2")
      win <- ggroup(horizontal = FALSE, cont=gwindow("DataTable test window"), expand=TRUE)
      bgrp <- ggroup(horizontal = TRUE, cont=win)
      tgrp <- ggroup(cont = win, expand=TRUE)
      
      # test implementations
      gbutton ("Hide column b", cont=bgrp, handler = function(...) print("test"))
      gbutton ("Select row a=4", cont=bgrp, handler = function(...) test$selectRowsByValues('a', 4))
      gbutton ("Remake table", cont=bgrp, handler = function(...) {
        test$destroyGui()
        test$makeGui(tgrp, data.frame(adiff='hello', bdiff=1:10))
      })
      
      # running the DataTable
      test <- DataTable$new()
      test$makeGui(tgrp, data.frame(a=1:5, b='test', c='wurst'), invisible = c(), changedHandler = function(...) print(test$getSelectedValues()))
    }
  )
)

#DataTable$new()$test()