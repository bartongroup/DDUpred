# DDU predictor

Author: Marek Gierlinski (M.Gierlinski@dundee.ac.uk)

This repository provides scripts to prepare input data and run drug propertied predictor for DDU compounds, using a large training data (provided). 

## Usage

The scripts in this repository require an R package [drupr](https://github.com/bartongroup/drupr/tree/main), which contains all relevant code.

One of the provided training sets can be used (in training_data directory).

 - `lombardo.csv` - to find volume of distribution, Vd
 - `master.csv` - to find solubility, permeability, etc.
 
Each of the training sets is accompanied by an info file with information about columns that are used as id, response and rejected.

### Merge and convert Excel files

There should be three Excel files as input: with pH7.4 descriptors (main file), with pH2 descriptors and Moka designations. In the first step these files are merged into one CSV file using `mergeph`. For example:

```
bin/mergeph.R -d excel_files/647/647_structures2_export_descriptors.xlsx -p excel_files/647/647_structures2_export_pH2.xlsx -o test_data/647_set.csv
```

creates a file `test_data/647_set.csv` which can be used for predictions.

### Run predictor
 
Predictor is called as in this example:
 
 ```
rfpred.R -t training_data/master.csv.gz -i training_data/master.info.csv -s test_data/647_set.csv -o results/647 -m 500
```

The `-m` parameter is important and needs to be specify. It tells the predictor to ignore variables (columns) with less than m non-missing values.
