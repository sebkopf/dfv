Data Frame Viewer (dfv)
===

This package provides a simple user interface to facilitate getting started with using R for data processing. The GUI illustrates how to import data from Excel, melt data frames into plottable format, add additional information to the data and plot it using ggplot. Provides an easy system to keep track of multiple plots and save them in PDF format. Always shows the actual code that is executed to process or plot the data so users can experiment with changing the code directly and copy it to make their own data processing pipeline independent of this GUI.

The user interface in this package is generated using GTK+, a cross-platform toolkit for graphical user interfaces (http://www.gtk.org/). GTK needs to be installed prior to using this package but the installation is fairly straight forward (there is a known problem with the newest version of GTK on Mac OS, see details below). If R and GTK are already installed and running on your system, you can go straight to [installing the dfv package](#install-dfv-package).

![Screenshot of the Data Frame Viewer](/doc/screenshot.png?raw=true)

##Install R and GTK+

###Windows
If you don't have R yet, install the newest version from http://www.r-project.org/ . Additionally, I highly recommend RStudio (http://www.rstudio.com/) for working with R regularly (but the basic command line will work just fine for the Data Frame Viewer). Once R is installed, you can directly [install the dfv package](#install-dfv-package) and R will walk you through installing GTK+ the first time you start the dfv (or any other GTK based GUI). So in brief:

 - install R from http://www.r-project.org/
 - [optional but recommended: install R-Studio from http://www.rstudio.com/]
 - [install the dfv package](#install-dfv-package)

###MacOS
R version 3 requires the GTK+ 2.24 framework which does not work properly out of the box yet on Mac OS X ([details on the problem and instructions on how to solve it below if you want to use it in R 3](#details-on-gtk-224-trouble)). I'm sure this will be resolved eventually in a normal installation of R >= 3.0 and GTK+ 2.24 but for the time being I recommend just installing the older GTK+ 2.18 framework (http://r.research.att.com/libs/GTK_2.18.5-X11.pkg) instead and using R 2.15 (http://r.research.att.com/R-2.15-branch-leopard.pkg - works fine with OS X Lion). If you already have R > 3.0 installed and want to run both version, no problem, the little helper program RSwitch (http://r.research.att.com/RSwitch-1.2.dmg) provides a simple means for switching back and forth easily. Also, having both GTK+ 2.24 and GTK+ 2.18 installed is no problem either, just make sure the 'Current' symlink in '/Library/Frameworks/GTK+.framework/Versions/' points to 2.18.X11. So in brief:

 - install R 2.15 from http://r.research.att.com/R-2.15-branch-leopard.pkg
 - [optional but recommended: install R-Studio from http://www.rstudio.com/]
 - install GTK 2.18 from http://r.research.att.com/libs/GTK_2.18.5-X11.pkg
 - [install the dfv package](#install-dfv-package)

If really you want to run GTK+ with R >= 3.0, please see the more detailed description of the problem and instructions on how to solve it below.

#####Details on GTK+ 2.24 trouble
R version 3 requires the GTK+ 2.24 framework which does not work properly yet out of the box on Mac OS X throwing a malloc error when running the following simple example:
```
> library(gWidgets)
> options("guiToolkit"="RGtk2")
> win <- gwindow("test")
> glabel("test label", container = win)

> gedit("test text field", container = win)
guiWidget of type: gEditRGtk for toolkit: guiWidgetsToolkitRGtk2 
> R(9523,0x7fff76be1960) malloc: *** error for object 0x4024000000000000: pointer being freed was not allocated *** set a breakpoint in malloc_error_break to debug
Abort trap: 6
```

The problem is documented on stack overflow in [some detail](#http://stackoverflow.com/questions/15868860/r-3-0-and-gtk-rgtk2-error). There is a work-around but it requires installing gtk2 via macports or homebrew and installing the R packages RGtk2 and cairoDevice from source. The approach listed below is inspired by suggestions on stack overflow (especially this [contribution from John Verzani](https://dl.dropboxusercontent.com/u/515592/README-mac-gtk.md) and worked well for me and others):

 - install macports (http://www.macports.org/install.php)
  - macports requires Apple's Xcode command line tools (free but you need an AppleID to download), which include all the necessary compilers for macports
  - you can install the full version of Xcode (~2Gb) or just the command line tools (in either case, make sure to install the latest version for your MacOS version)
  - for the full version, after installation go to the XCode preferences and *install the command line tools*
  - for the command line tools only, search for *command line tools* on https://developer.apple.com/downloads/ (the standalone command line tools are not recognized by the MacPorts installers but the warnings can be safely ignored)
 - open a terminal and run the following commands to install GTK (you will be asked for your password)
  - ```sudo port selfupdate```
  - ```sudo port install gtk2 +x11```
  - ```exportPATH=/opt/local/bin:/opt/local/sbin:$PATH```
 - then download the *RGtk2* and *cairoDevice* package source from CRAN (there might newer versions but these were current as of 2/20/14):
  - http://cran.r-project.org/src/contrib/RGtk2_2.20.25.tar.gz
  - http://cran.r-project.org/src/contrib/cairoDevice_2.19.tar.gz
 - install the two packages by running the following commands on the terminal (adjust for the correct path to the downloads)
  - ```R CMD INSTALL ~/Downloads/RGtk2_2.20.25.tar.gz```
  - ```R CMD INSTALL ~/Downloads/cairoDevice_2.19.tar.gz```

##Install dfv package

The **devtools** package provides a super convenient way of installing the **dfv** package directly from GitHub. To install **devtools**, run the following from the R command line:
```
install.packages('devtools', depen=T) # development tools
```

Then simply install the latest version of the Data Frame Viewer directly from GitHub by running the following code (if it is the first time you install the **dfv** package, all missing dependencies will be automatically installed as well -> **ggplot2, plyr, psych, scales, grid, gWidgets, RGtk2**, and **xlsx** as well as their respective dependencies, which might take a few minutes):
```
library(devtools)
install_github('dfv', 'sebkopf')
```
Note: if you don't like a version, you can always go back to your favorite by installing it instead:
``` install_github('dfv', 'sebkopf', ref = 'v1.0') ```

#####Troubleshooting
The **xlsx** package uses **rJava** to access the Java API for Excel. This requires Java to be installed on your computer. If there is a problem, it might throw an error something like the one below and usually launch an installer program for Java. If this happens, please go directly to http://java.com/en/download/index.jsp to install the newest version of the Java runtime environment (JRE) for your operating system and then restart RStudio and try again.
```
No Java runtime present, requesting install.
Error: Command failed (97)
```


##Run dfv
Once installed, you can now run the Data Frame Viewer in any R workspace (command line R, RStudio, iPython Rmagic, etc.):
```
library(dfv)
dfv.start()
```

Or directly from the terminal via Rscript (important to run with ```modal=TRUE```)
```
> Rscript -e 'library(dfv); dfv.start(modal=TRUE)'
```

