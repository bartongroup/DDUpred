# DDU predictor

Random forest based predictor for DDU compounds. Using a large training data (provided) it can predict properties of new compounds.

## Usage

One of the provided training sets can be used.

 - `lombardo.csv` - to find volume of distribution, Vd
 - `master_set.csv` - to find solubility, premeability, etc.
 
 Each of the training sets is accompanied by a file with information about columns that are used as id, response and rejected.
 
 A test data set should be a CSV file with the same columns as in the training set. Predictor is an R script, so it can be called as in this example
 
 ```
Rscript rfpred.R -t training_data/master_set.csv.gz -i training_data/master_set.info.csv -s test_data/Inem_18_mastersheet.csv -o results_master -m 1500
```

The `-m` parameter is important and needs to be specify. It tells the predictor to ignore variables (columns) with less than m non-missing values.
