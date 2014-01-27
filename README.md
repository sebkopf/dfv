dfv
===

Data Frame Viewer for getting started with plotting in R and specifically ggplot



Installing GTK+ (the engine used to generate the user interface)
===
Install GTK+ depending on your R version (it won't work with the wrong version and R does not automatically always install the correct one):

For R < 3.0
For Mac: http://r.research.att.com/libs/GTK_2.18.5-X11.pkg (~60Mb)
For Windows: R will automatically install the right one when first starting up
For Linux:

For R > 3.0
For Mac: http://r.research.att.com/libs/GTK_2.24.17-X11.pkg (~40Mb)

Follow the regular installation instructions.

Note on GTK+ troubles
===
R version 3 requires the GTK+ 2.24 framework which does not work properly yet on Mac OS X throwing a malloc error when running the following simple example:
```R
> library(gWidgets)
> options("guiToolkit"="RGtk2")
> win <- gwindow("test")
> glabel("test label", container = win)

> gedit("test text field", container = win)
guiWidget of type: gEditRGtk for toolkit: guiWidgetsToolkitRGtk2 
> R(9523,0x7fff76be1960) malloc: *** error for object 0x4024000000000000: pointer being freed was not allocated *** set a breakpoint in malloc_error_break to debug
Abort trap: 6
```

The problem is documented on stack overflow in some detail here: http://stackoverflow.com/questions/15868860/r-3-0-and-gtk-rgtk2-error and there is a work-around (installing gtk2 via macports or homebrew and installing RGtk2 and cairoDevice from source), for example, as detailed here: https://dl.dropboxusercontent.com/u/515592/README-mac-gtk.md (link from the stack overflow discussion).

I'm sure this will be resolved eventually in the normal installation, for the time being I recommend just installing the older GTK+ 2.18 framework instead (it's no problem having both installed, just make sure the 'Current' symlink in '/Library/Frameworks/GTK+.framework/Versions/' points to 2.18.X11) and using R 2.15.3. For those want to run both R 2.15.3 (http://r.research.att.com/R-2.15-branch-leopard.pkg - works fine with OS X Lion) and R >3.0 on Mac OS, RSwitch provides a simples means for switching back and forth easily: http://r.research.att.com/RSwitch-1.2.dmg

Dependencies
===
```R
install.packages('devtools', depen=T) # development tools
install.packages('ggplot2', depen=T) # for plotting purposes
install.packages('psych', depen=T) # for implementation of copy & paste
install.packages('gWidgets', depen=T) # the generic widgets interface
install.packages('RGtk2', depen=T) # for specific GTK toolkit 
```

