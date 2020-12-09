# VisualSHIELD

<img align="right" height="100" src="https://dashin.cosbi.eu/img/dash-in_logo.png">

VisualSHIELD allows to create reactive visual web UIs to seamlessly analyze multiple remote datasets in parallel hosted on [Opal](https://www.obiba.org/pages/products/opal/) and optionally provides also a facility to load [dbNP](https://dashin.eu/interventionstudies/) data into Opal. The [DASH-IN interactive federated analysis system](https://dashin.cosbi.eu/) is a unifying visual federated analytical framework of observational and interventional studies powered by VisualSHIELD and contribuited to the [ENPADASI](https://www.dtls.nl/wp-content/uploads/2016/05/ENPADASI_Bouwman_250516_FAIR.pdf#page=7) project of 51 partners in 9 European countries.

This repository contains the reference implementation for VisualSHIELD. You may freely use this work in your research and activities under the non-commercial [COSBI-SSLA license](https://www.cosbi.eu/research/prototypes/licence_terms).

For more information and guided hands-on tutorials on how everything can be systematically glued together, as in the ENPADASI project, check out the [ENPADASI Hackaton](https://agenda.infn.it/event/11522/) or get in touch with the Cosbi Bioinformatics lab, led by lombardo@cosbi.eu. We'll be happy to help!

# Table of contents

- [VisualSHIELD](#visualshield)
- [Installation](#installation)
- [Usage](#usage)


# Installation

from R console type

```R
install.packages('VisualSHIELD_1.0.tar.gz')
```
# Usage

VisualSHIELD is a shiny app module.
A shiny app module is a self-contained UI with it's own logic that can be easily integrated in any other custom shiny app. 

The analysis is performed through the privacy-aware [DataSHIELD](https://www.datashield.ac.uk/) analysis package, and allows to easily perform:
* histograms
* contour plots
* heatmaps
* linear models (lm)
* generalized linear models (glm)

VisualSHIELD module is exposed through the VisualSHIELDUI and the VisualSHIELDServer functions. 

## Demo

To run the demo, after installation go to the [example](example) folder, run the R console and type in

```R
shiny::runApp() 
```

## Further instructions

Look at the vignette for details on how to incorporate VisualSHIELD in your shiny app.
After installing VisualSHIELD, from the R console run

```R
vignette('VisualSHIELD')
```
or look at the [PDF version](doc/VisualSHIELD-vignette.pdf) 
