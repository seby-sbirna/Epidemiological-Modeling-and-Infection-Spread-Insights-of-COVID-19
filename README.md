## Epidemiological Modeling and Infection Spread Insights of COVID-19
### _By Sebastian Sbirna, Sule Altintas and Stanley Frederiksen_
---

![](./img/project_flowchart.png)

## Project Description

Our project is based on the analysis of a COVID-19 data set containing daily numbers of confirmed cases, deaths, and recoveries from the whole world, as well as patient data including symptoms. The fully wrangled data can be found under the folder: `data\_augmented`.

## Dataset Dictionary

The raw data was obtained from the following Kaggle project, and is a customized version of the John Hopkins University dataset:
https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset

This data has been augmented, for our analysis purposes, with world population data from the UN:
https://population.un.org/wpp/Download/Standard/CSV/

## How to reproduce the research?
By running the script R/00_do.R, the full data analysis can be carried out. This is especially useful if updating the data in `data/_raw` with the new daily numbers, as new data will be incorporated into all resulting plots.

## Dependencies
The following dependencies are necessary to run our project:
- [R](https://cran.r-project.org/bin/windows/base/) >= 4.0.0, and the following additional packages:
  * __tidyverse__
  * __lubridate__
  * __broom__
  * __leaflet__
  * __leaflet.extras__
  * __ggpubr__
  * __rpart__
  * __rpart.plot__
  * __factoextra__
  * __caret__
  * __gridExtra__
  * __deSolve__
