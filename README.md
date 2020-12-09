# VisualSHIELD

VisualSHIELD is a shiny app module.
A shiny app module is a self-contained UI with it's own logic that can be easily integrated in any other custom shiny app. 

<img align="right" height="100" src="https://dashin.cosbi.eu/img/dash-in_logo.png">

VisualSHIELD allows to create reactive visual web UIs to seamlessly analyze multiple remote datasets in parallel hosted on [Opal](https://www.obiba.org/pages/products/opal/) and optionally provides also a facility to load [dbNP](https://dashin.eu/interventionstudies/) data into Opal. The [DASH-IN interactive federated analysis system](https://dashin.cosbi.eu/) is a unifying visual federated analytical framework of observational and intervetional studies powered by VisualSHILED and contribuited to the [ENPADASI](https://www.dtls.nl/wp-content/uploads/2016/05/ENPADASI_Bouwman_250516_FAIR.pdf#page=7) project of 51 partners in 9 European countries.

This repository contains the reference implementation for VisualSHIELD and XXXX two real-world examples described in the paper XXXX. You may freely use this work in your research and activities under the non-commercial [COSBI-SSLA license](https://www.cosbi.eu/research/prototypes/licence_terms).

For more information on ongoing work in biomedical knowledge extraction you may want to visit the [COSBI knowledge extraction page](https://www.cosbi.eu/research/prototypes/biomedical_knowledge_extraction) or get in touch with the Cosbi Bioinformatics lab led by lombardo@cosbi.eu. We'll be happy to help!

## Table of contents

- [Installation](#installation)
- [Usage](#usage)
  + [Real-world examples](#real-world-examples)
- [What's next (making sense with Bio NLP)](#whats-next-or-how-to-make-sense-with-nlp)


# Installation

from R console type

```R
install.packages('VisualSHIELD_1.0.tar.gz')
```
# Usage

The analysis is performed through the privacy-aware [DataSHIELD](https://www.datashield.ac.uk/) analysis package, and allows to easily perform:
* histograms
* contour plots
* heatmaps
* linear models (lm)
* generalized linear models (glm)

VisualSHIELD module is exposed through the VisualSHIELDUI and the VisualSHIELDServer functions. 

To run the demo, after installation go to the [example](example) folder, run the R console and type in

```R
shiny::runApp() 
```

Look at the vignette for details on how to incorporate VisualSHIELD in your shiny app.

```R
vignette('embed-in-custom-shiny-app')
```
