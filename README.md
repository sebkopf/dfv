Data Frame Viewer (dfv)
===

This package provides a simple user interface to facilitate getting started with using R for data processing. The GUI illustrates how to import data from Excel, melt data frames into plottable format, add additional information to the data and plot it using ggplot. Provides an easy system to keep track of multiple plots and save them in PDF format. Always shows the actual code that is executed to process or plot the data so users can experiment with changing the code directly and copy it to make their own data processing pipeline independent of this GUI.

The user interface in this package is generated using GTK+, a cross-platform toolkit for graphical user interfaces (http://www.gtk.org/). GTK needs to be installed prior to using this package but the installation is fairly straight forward (there is a known problem with the newest version of GTK on Mac OS, see details below). If R and GTK are already installed and running on your system, you can go straight to [installing the dfv package](#install-dfv-package).

##Install R and GTK+

###Windows
If you don't have R yet, install the newest version from http://www.r-project.org/ . Additionally, I highly recommend RStudio (http://www.rstudio.com/) for working with R regularly (but the basic command line will work just fine for the Data Frame Viewer). Once R is installed, you can directly [install the dfv package](#install-dfv-package) and R will walk you through installing GTK+ the first time you start the dfv (or any other GTK based GUI). So in brief:

 - install R from http://www.r-project.org/
 - [optional but recommended: install R-Studio from http://www.rstudio.com/]
 - [install the dfv package](#install-dfv-package)

###MacOS
R version 3 requires the GTK+ 2.24 framework which does not work properly yet on Mac OS X ([details on the problem below](#details-on-gtk-224-trouble)). I'm sure this will be resolved eventually in a normal installation of R > 3.0 and GTK+ 2.24 but for the time being I recommend just installing the older GTK+ 2.18 framework (http://r.research.att.com/libs/GTK_2.18.5-X11.pkg) instead and using R 2.15 (http://r.research.att.com/R-2.15-branch-leopard.pkg - works fine with OS X Lion). If you already have R > 3.0 installed and want to run both version, no problem, the little helper program RSwitch (http://r.research.att.com/RSwitch-1.2.dmg) provides a simple means for switching back and forth easily. Also, having both GTK+ 2.24 and GTK+ 2.18 installed is no problem either, just make sure the 'Current' symlink in '/Library/Frameworks/GTK+.framework/Versions/' points to 2.18.X11. So in brief:

 - install R 2.15 from http://r.research.att.com/R-2.15-branch-leopard.pkg
 - [optional but recommended: install R-Studio from http://www.rstudio.com/]
 - install GTK 2.18 from http://r.research.att.com/libs/GTK_2.18.5-X11.pkg
 - [install the dfv package](#install-dfv-package)

#####Details on GTK+ 2.24 trouble
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

##Install dfv package

###Install dependencies
The package has a few dependencies required for performing various tasks, make sure these are installed. You can do this quickly by running the following code in an R command line:
```R
install.packages('devtools', depen=T) # development tools
install.packages('ggplot2', depen=T) # for plotting purposes
install.packages('psych', depen=T) # for implementation of copy & paste
install.packages('gWidgets', depen=T) # the generic widgets interface
install.packages('RGtk2', depen=T) # for specific GTK toolkit 
install.packages('xlsx', depen=T) # for reading excel files
```

###Get dfv from GitHub
Install the latest version of the Data Frame Viewer directly from GitHub by running the following code:
```R
library(devtools)
install_github('dfv', 'sebkopf', ref = 'master')
```

###Run dfv
Once installed, you can now run the Data Frame Viewer in any R workspace (terminal, RStudio, iPython Rmagic, etc.):
```R
library(dfv)
dfv.start()
```
