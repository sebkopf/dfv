dfv
===

This package provides a simple user interface to facilitate getting started with using R for data processing. The GUI illustrates how to import data from Excel, melt data frames into plottable format, add additional information to the data and plot it using ggplot. Provides an easy system to keep track of multiple plots and save them in PDF format. Always shows the actual code that is executed to process or plot the data so users can experiment with changing the code directly and copy it to make their own data processing pipeline independent of this GUI. The data frame viewer can also save all settings and plotting code/tabs and reload them in the next R session to make it easy to continue were left off.

The user interface in this package is generated using GTK+, a cross-platform toolkit for graphical user interfaces (http://www.gtk.org/). GTK needs to be installed prior to using this package but the installation is fairly straight forward (there is a known problem with the newest version of GTK on Mac OS, details in the [installation instructions](https://gist.github.com/sebkopf/9405675)). If R and GTK are already installed and running on your system, you can go straight to [installing the dfv package](#install-dfv-package).

![Screenshot of the Data Frame Viewer](/doc/screenshot.png?raw=true)

##Install R with GTK+

Please follow this [link](https://gist.github.com/sebkopf/9405675) for information on installing R with GTK+.

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

####Troubleshooting

##### Java
The **xlsx** package uses **rJava** to access the Java API for Excel. This requires Java to be installed on your computer. If there is a problem, it might throw an error something like the one below and usually launch an installer program for Java. If this happens, please go directly to http://java.com/en/download/index.jsp to install the newest version of the Java runtime environment (JRE) for your operating system and then restart RStudio and try again.
```
No Java runtime present, requesting install.
Error: Command failed (97)
```

##### Dependencies
The dependency packages have many dependencies of their own and sometimes not all of them get installed properly all at once. If this is the case, try rerunning the dfv installation (now that fewer dependencies need to be installed) or install the failed packages manually by running ```install.packages("PACKAGE NAME", depen=T)``` from the R command line.


##Run dfv
Once installed, you can now run the Data Frame Viewer in any R workspace (command line R, RStudio, iPython Rmagic, etc.):
```
library(dfv)
dfv.start()
```

Or directly from the terminal via Rscript (important to run with ```modal=TRUE```)
```
Rscript -e 'library(dfv); dfv.start(modal=TRUE)'
```

