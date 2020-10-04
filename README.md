# DsFeatFreqDistFit - Dataset Feature-Frequency Distributions Fitting
### A Research Compedium of "Gaining Insights in Datasets in the Shade of “Garbage In, Garbage Out” Rationale: Feature-Space Distribution Fitting"

[![Last-changedate](https://img.shields.io/badge/last%20change-2020--09--25-brightgreen.svg)](https://github.com/gurol/dsfeatfreqcomp) [![License: AGPL v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)  [![ORCiD](https://img.shields.io/badge/ORCiD-0000--0002--9337--097X-green.svg)](https://orcid.org/0000-0002-9337-097X)

This respository is a research compedium of our academic publication. It provides ready-to-run open-source R scripts, example data sets, and materials (e.g. tabular data and graphics) for quantifying and categorizing machine learning data sets by the methodology proposed in my article.

**Note**: Please, cite the study if you would like to use and/or adapt the code, datasets, methodology, and other materials provided and [let me know](mailto:gurol@canbek.com). Thank you for your interest.

> [Gürol Canbek](http://gurol.canbek.com), Gaining Insights in Datasets in the Shade of “Garbage In, Garbage Out” Rationale: Feature-Space Distribution Fitting. In *TBD*, Submitted, 2020.

## Contents
1. Dataset Distribution Analysis R Scripts
2. Sample Academic Datasets on Android Mobile Benign/Malign Applications (i.e. Goodware/Malware)
3. Extra Materials in an Open Office Spreadsheet (best viewed in [LibreOffice](https://www.libreoffice.org/download/download/))

### File Contents

```
/ [root]
├── code
│   ├── dsfeatfreqdist.R : Script calculating data set distributions and generating all the related figures (more than the article)
│   ├── utils.R : Common utility script used by other scripts
│   ├── LICENSE : License file
│   ├── README.md : This help file
├── data
│   ├── datasets.ods : Extra materials in an Open Office Spreadsheet (best viewed in LibreOffice)
│   ├── ds_dist_benign.RData : Feature distribution data for benign data sets in R format
│   └── ds_dist_malign.RData : Feature distribution data for malign data sets in R format
│   └── csv
│       └── FatureFrequencyDistribution_BenignDatasets.csv : Benign data sets frequency distributions in comma seperated values in decreasing order
│       └── FatureFrequencyDistribution_MalignDatasets.csv : Malign data sets frequency distributions in comma seperated values in decreasing order
│       └── DatasetSizesPerClass.csv : Benign and malign data set (sample) sizes
└── results
    └── dist_analysis_all.csv : Complete dump of data set distribution analysis for all the benign and malign data sets
    └── fit_unfit.csv : The list of features fit/unfit to the 4 statistical distributions (power law, log-normal, exponential, and Poisson)
    └── Benign or Malign
        └── DSi
            └── fig(Benign/Malign)_DSi_fit(Ex/Ln/Pl)_vs_All.png : One of the Exponential, Log-normal, and Power law fits against others fitting DSi feature distribution graph
            └── fig(Benign/Malign)_DSi_Likelihood_[fit](Pl/Ln)_vs_[fit](Ex/Ln).png : Vuong's test showing colored log-likelihood plots for the two distributions compared (according to one of the distribution's fit)
            └── fig(Benign/Malign)_DSi_Vuong_[fit](Pl/Ln/Ex/Po)_vs_[fit](Pl/Ln/Ex/Po).png : Vuong's test showing log-likelihood plots for the two distributions compared (according to one of the distribution's fit)
```
## Sample Outputs
![](https://raw.githubusercontent.com/gurol/dsanalysis/master/temp/figBenign_DS0_fitLn_vs_All.png)

*Figure 1. Benign data set plausibility of log-normal distribution fit and others*

![](https://raw.githubusercontent.com/gurol/dsanalysis/master/temp/figBenign_DS0_fitPl_vs_All.png)

*Figure 2. Benign data set plausibility of power law distribution fit and others*

![](https://raw.githubusercontent.com/gurol/dsanalysis/master/temp/figMalign_DS3_fitEx_vs_All.png)

*Figure 3. Malign (malware) data set plausibility of exponential distribution fit and others*

## Dataset Distribution Analysis R Scripts (standalone file for testing purposes, refer to the package for the full up-to-date code)
1. `dsfeatfreqdist.R` Comprehensive set of distribution testing to fit power law, log-normal, exponential, and Poisson statistical distribution into the feature frequency distributions (i.e. the truth)
2. Data set size categorization (**will be provided after publication of the manuscript submitted**)

## Sample Academic Datasets on Android Mobile Benign/Malign Applications (i.e. Goodware/Malware)
Hassle free, ready-to-run data frames for data set frequency distributions to run the R script (see **Data** pane)
1. `ds_dist_benign.RData`
2. `ds_dist_benign.RData`

## Extra Materials in an Open Office Spreadsheet (best viewed in LibreOffice)
Extra materials:
1. Frequency of permission features per data set per class as a tabular data
2. Data set quantification chart
3. Feature frequency distribution along with the plausible statistical distribution fits with the parameters
4. A comprehensive visualy enhanced table of distribution fit hypothesis tests on power law, log-normal, exponential, and Poisson
5. The tabulated complete dump of dsdist.R script run
6. Fitted/nonfitted feature list per best fit statistical distributions

