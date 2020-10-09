# VisualSHIELD

VisualSHIELD is a shiny app module.
A shiny app module is a self-contained UI with it's own logic that can be easily integrated in any other custom shiny app. 

The VisualSHIELD module allows to seamlessly analyze multiple remote datasets in parallel hosted on  [Opal](https://www.obiba.org/pages/products/opal/) and optionally provides also a facility to load [dbNP](https://www.dbnp.org/) data into Opal. 
The analysis is performed through the privacy-aware [DataSHIELD](https://www.datashield.ac.uk/) analysis package, and allows to easily perform:
* histograms
* contour plots
* heatmaps
* linear models (lm)
* generalized linear models (glm)

VisualSHIELD module is exposed through the VisualSHIELDUI and the VisualSHIELDServer functions. 