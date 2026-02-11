# DigitAF: Biodiversity assessment in agroforestry systems

<p xmlns:cc="http://creativecommons.org/ns#" >This work is licensed under <a href="http://creativecommons.org/licenses/by/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">CC BY 4.0 <img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1"><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1"></a></p>

### Here we present the code and datasets used within the DigitAF project for biodiversity assessment in agroforestry systems, with meta-analysis modelling through existing case-studies.
For more information we kindly refer to the publications below:
1) Deliverable 3.3: <a href="https://doi.org/10.5281/zenodo.13834598." target="_blank">DOI: 10.5281/zenodo.13834598.</a>
2) <a href="https://doi.org/10.1002/2688-8319.70133" target="_blank">DOI: 10.1002/2688-8319.70133</a>
3) Deliverable 3.4: 
4) The database: 

The current tool is available via <a href="https://biodivaf.shinyapps.io/BiodivAF/." target="_blank">https://biodivaf.shinyapps.io/BiodivAF/</a>

Data inputs that are needed of this repository for tool employment are: 
* BiodivAF-app --> *Shiny App*
* BiodivAF --> *Script for running the Shiny App*
* model_outputs --> *folder with model outputs used in the shiny app*
* www --> *folder with icons used in the shiny app*

#### Building the dataset
*code (.rmd/.R)*
* Database building
* Finetuning_initial_database
* GridCoords
* LandCompl_SoilData

*Cleaned data (.csv)*
* Folder data_per_study: contains biodiversity outputs per case-study

#### Automated model updates
*code (.rmd/.R)*
* Model_updates
* Model_updates_Full_Tool
* Model_updates_Full_Tool_replicates_separetely --> *To split loops for running replicates simultaneously instead of consecutively*
* BiodivAF-check

#### Shiny App
*code (.rmd/.R)*
* BiodivAF-app
* BiodivAF

*Metadata*
* www --> folder with icons used in the shiny app
* model_outputs --> folder with model outputs used in the shiny app
* 10-fold predictions --> folder with results for 10-fold-validation loop for current tool version

#### Jari Vandendriessche
##### Forest and Nature Lab
##### Ghent University

Refer to as: 
Vandendriessche, J., Landuyt, D., Eeraerts, M., Verheyen, K., & De Frenne, P. (2025). An evidence-based modeling framework to assess biodiversity change through agroforestry implementation in European context. Zenodo. https://doi.org/10.5281/zenodo.17900792

This work was supported by the DigitAF project (Grant Agreement N° 101059794), co-funded by the European Commission, European Research Agency, within the Horizon Europe programme. 