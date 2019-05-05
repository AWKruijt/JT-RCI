# JTRCI: Jacobson & Truax reliable change indices

obtain and plot Jacobson and Truax reliable change indices

devtools::install_github("AWKruijt/JT-RCI")
library(JTRCI)

JTRCI(x.pre = NA, x.post = NA, reliability = NA, ppid = NA, indextype = "JT", plot = T, table = T, higherIsBetter = F, JTcrit = "auto", normM = NA, normSD = NA, facetplot = F)

screenshots:

<img width="450" src="https://github.com/AWKruijt/JT-RCI/blob/master/screenshots/example%20one%20group%20JT%20plot.png">
  
<img width="450" src="https://github.com/AWKruijt/JT-RCI/blob/master/screenshots/example%20JT%20table.png">

<img width="450" src="https://github.com/AWKruijt/JT-RCI/blob/master/screenshots/example%20two%20groups%20JT%20panelplot.png">
  
<img width="450" src="https://github.com/AWKruijt/JT-RCI/blob/master/screenshots/example%20two%20groups%20RCI%20plot.png">
