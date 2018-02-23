# Data Set Analysis: A Research Compedium of "A New Approach to Quantifying and Categorizing Data Sets"
This developing respository is a research compedium of our academic publication.
It provides ready-to-run open-source R scripts, example data sets, and materials (e.g. tabular data and graphics) for quantifying and categorizing machine learning data sets by the methodology proposed in our article.

**Note**: Please, cite our study if you would like to use and/or adapt the code, datasets, methodology, and other materials provided and [let us know](mailto:gurol@canbek.com). Thank you for your interest.

> Gürol Canbek, Seref Sagiroglu, and Tugba Taskaya Temizel. A New Approach to Quantifying and Categorizing Data Sets. In *[Journal of Machine Learning Research (JMLR)](http://www.jmlr.org/)*, Submitted, 2018.

Code updates at [https://github.com/gurol/dsanalysis](https://github.com/gurol/dsanalysis)
Methodology updates at [http://canbek.com/dataset](http://canbek.com/dataset)

## Contents
1. Dataset Distribution Analysis R Scripts
2. Sample Academic Datasets on Android Mobile Benign/Malign Applications (i.e. Goodware/Malware)
3. Extra Materials in an Open Office Spreadsheet (best viewed in [LibreOffice](https://www.libreoffice.org/download/download/))

### File Contents

```
/ [root]
├── code
│   ├── dsdist.R : Script calculating data set distributions and generating all the related figures (more than the article)
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

## Dataset Distribution Analysis R Scripts
1. `dsdist.R` Comprehensive set of distribution testing to fit power law, log-normal, exponential, and Poisson statistical distribution into the feature frequency distributions (i.e. the truth)
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

