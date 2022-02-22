# DDU predictor

This repository provides scripts to prepare input data and run drug propertied predictor for DDU compounds, using a large training data (provided). 

## Usage

The scripts in this repository require an R package [drupr](https://github.com/bartongroup/drupr/tree/main), which contains all relevant code.

One of the provided training sets can be used.

 - `lombardo.csv` - to find volume of distribution, Vd
 - `master_set.csv` - to find solubility, permeability, etc.
 
Each of the training sets is accompanied by a file with information about columns that are used as id, response and rejected.
 
A test data set should be a CSV file with the same columns as in the training set. Predictor is an R script, so it can be called as in this example
 
 ```
rfpred.R -t training_data/master_set.csv.gz -i training_data/master_set.info.csv -s test_data/Inem_18_mastersheet.csv -o results_master -m 1500
```

The `-m` parameter is important and needs to be specify. It tells the predictor to ignore variables (columns) with less than m non-missing values.
