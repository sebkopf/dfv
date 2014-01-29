#' Class implementing a data table as a GuiElement
DataTable <- setRefClass(
  'DataTable',
  contains = 'GuiElementDataFrame', 
  fields = list(
    icons = 'list', # keeps track of main icons this GuiElement uses
    handlers = 'list' # handlers that the gui element needs to keep track of
  ),
  methods = list(
    initialize = function(...) {
      callSuper(...)
    },
    
    # 'Makes the gtable. If it already exists in the widgets, will delete the existing one first and then remake it.
    # '@param parent (RGtkWidget) - some container like a ggroup
    # '@param data (data.frame) - a data frame (will only use the definition, not the whole data)
    makeGUI = function(parent, data, expand = TRUE, invisible = c(), changedHandler = NULL, ...) {
      
      #INFO: for more information on this kind of data table layout, look at page 166+ of the R GUI guidebook!
      # topics include styling, how to have filters for columns (serachable --> look on page 172+), multiple selections, tooltips, signals, etc.
      
      model <- rGtkDataFrame(data) 
      #test_model <- gtkTreeModelSortNewWithModel(model)
      view <- gtkTreeView (model)
      
      # styling
      view['enable-grid-lines'] <- TRUE # grid lines between rows
      view['rules-hint'] <- TRUE # nice alternating colors
      view['search-column'] <- FALSE # ctrl-f controlled serach box
      view['reorderable'] <- TRUE # FIXME: this doesn't actually work properly, not sure why not (there's no drag/drop events really) - should allow vertical drag and drop...
      
      selection <- view$getSelection()
      selection$setMode ("single") # could upgrade this to allow multiple selections!
      #view['rubber-banding'] <- TRUE # also for multiple selections: 
      
      # store objects
      setData(model = model)
      setWidgets(table = view)
      
      # add columns
      mapply(view$insertColumnWithAttributes,
               position = -1,
               title = colnames(model),
               cell = list(gtkCellRendererText()), text = seq_len(ncol(model)) - 1)
      
      # enable column sorting by clicking on the header
      # sapply ( seq_len(ncol (model)), function (i) view$getColumn(i - 1)$setSortColumnId(i - 1))
      
      # set visibility
      mapply(function(col, vis) col$setVisible(vis), view$getColumns(), !colnames(model)%in%invisible) # NOTE: this is great! don't need to recreate table, with this can make columns visible/invisible as needed
      
      # selection handler
      if (!is.null(changedHandler))
        handlers$changedHandler <<- gSignalConnect (selection, "changed" , changedHandler)
      
      # window (here use the group instead)
      window <- gtkWindow ( )
      window$setTitle ( "Tabular view of data frame" ) 
      scrolled_window <- gtkScrolledWindow ( ) 
      window$add ( scrolled_window ) 
      scrolled_window$add ( view )
      scrolled_window$setPolicy ("automatic", "automatic")
      
      mapply(function(col, vis) col$setVisible(vis), view$getColumns(), !colnames(model)%in%invisible)
    },
    
    addRows = function() {
      #implement me
      #data$model$appendRows()
    },
    
    removeRows = function(rows) {
      # implement me - can only be done by replacing the data frame underlying the model!
      #data$model$setFrame(data.frame minus the rows)
    },
    
    #' Get the whole data frame in the table (or parts of it)
    #' could get the data frame backing the model also via as.data.frame(data$model) but
    #' might as well use this one for a little more flexbility
    #' @param rows only supports indices (not rownames, they mean nothing to GTK)
    #' @param columns can be either indices or column names
    getDataFrame = function (rows, columns) {
      if (missing(rows))
        rows <- seq_len(nrow(data$model)) # return all rows by default
      if (missing(columns)) 
        columns <- seq_len(ncol(data$model)) # return all columns by default
      if (identical(class(columns), 'character'))
        columns <- which(colnames(data$model)%in%columns)
      
      df <- data.frame(data$model[rows, columns], stringsAsFactors=FALSE)
      names(df) <- colnames(data$model)[columns]
      return(df)
    },
    
    # 'Get the indices of the selected rows
    getSelectedRows = function() {
      selected_rows <- widgets$table$getSelection()$getSelectedRows()
      if ( length ( selected_rows$retval ) ) 
        return(sapply ( selected_rows$retval , gtkTreePathGetIndices ) + 1)
      return (NULL)
    },
    
    # 'Get the values of the selected rows
    getSelectedValues = function(columns){
      if ( !is.null(rows <- getSelectedRows()))
        return (getDataFrame(rows, columns))
      return (NULL)
    },
    
    # ' @indices - which indices to select
    # ' @values - list 
    selectRows = function(indices, blockHandler = FALSE) {
      # block handler if requested
      if (blockHandler)
        gSignalHandlerBlock(widgets$table$getSelection(), handlers$changedHandler)
      
      # make selection
      widgets$table$getSelection()$unselectAll()
      for (index in indices)
          widgets$table$getSelection()$selectPath(gtkTreePathNewFromIndices(index-1))
      
      # unblock handler
      if (blockHandler)
        gSignalHandlerUnblock(widgets$table$getSelection(), handlers$changedHandler)
    },

    selectRowsByValues = function(column, values, blockHandler = FALSE) {
      indices <- which(getDataFrame(columns = column)[[1]]%in%values)
      selectRows(indices, blockHandler = blockHandler)
    }
    
    # FIXME: implement cleanwidgets properly
  )
)

test <- DataTable$new()
test$makeGUI(NULL, data.frame(a=1:5, b='test', c='wurst'), invisible = c(), changedHandler = function(...) print(test$getSelectedValues()))
