#' Copies the variable to the clipboard
#' For data frames, writes as table (with separator \t), everything else directly.
copyToClipboard <- function(variable) {
  if (exists("writeClipboard")) # windows
    clipboard <- "clipboard"
  else # unix/MacOS
    clipboard <- pipe("pbcopy", "w")
  
  if (class(variable) == 'data.frame')
    write.table(variable, file=clipboard, sep="\t", row.names=FALSE)
  else
    cat(variable, file=clipboard)
  
  if (!exists("writeClipboard")) # unix
    close(clipboard)
}