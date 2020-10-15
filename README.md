# robustSL

## Presentation

This package implements the clustering algorithm proposed in Klutchnikoff, Poterie, Rouvière (2020).
Based on a new interpretation of the dendrogram produced by the AHC (in term of minimal distance) it can be viewed as a robust version of the single linkage algorithm.
(see arxiv://xxxxx for more details.)

## Install

How do you install the latest version of `robustSL` available on GitHub?

```{r}
if (!require("devtools")) install.packages("devtools")
devtools::install_github("klutchnikoff/robustSL")
```
