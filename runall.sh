#!/bin/bash

bin/mergeph.R \
  -d excel_files/647/647_structures2_export_descriptors.xlsx \
  -p excel_files/647/647_structures2_export_pH2.xlsx \
  -o test_data/647_set.csv
bin/rnlombardo.R \
  -i test_data/647_set.csv \
  -r test_data/lombardo_rename.csv \
  -o test_data/647_set_lombardo.csv
bin/rfpred.R \
  -t training_data/master_set.csv.gz \
  -i training_data/master_set.info.csv \
  -s test_data/647_set.csv \
  -o results/647_master \
  -m 500
bin/rfpred.R \
  -t training_data/lombardo.csv.gz \
  -i training_data/lombardo.info.csv \
  -s test_data/647_set_lombardo.csv \
  -o results/647_lombardo \
  -m 500

bin/mergeph.R \
   -d excel_files/1202/1202_structures_export_pH74.xlsx \
   -p excel_files/1202/1202_structures_export_pH2.xls \
   -m excel_files/1202/1202_Moka_incl_designation.xlsx \
   -o test_data/1202_set.csv
bin/rnlombardo.R \
  -i test_data/1202_set.csv \
  -r test_data/lombardo_rename.csv \
  -o test_data/1202_set_lombardo.csv
bin/rfpred.R \
  -t training_data/master_set.csv.gz \
  -i training_data/master_set.info.csv \
  -s test_data/1202_set.csv \
  -o results/1202_master \
  -m 1000
bin/rfpred.R \
  -t training_data/lombardo.csv.gz \
  -i training_data/lombardo.info.csv \
  -s test_data/1202_set_lombardo.csv \
  -o results/1202_lombardo \
  -m 1000
  

bin/mergeph.R \
   -d excel_files/5000/5000_pH74.xlsx \
   -p excel_files/5000/5000_structures_only_export_pH2.xlsx \
   -m excel_files/5000/5000_Moka_incl_designation.xlsx \
   -o test_data/5000_set.csv
bin/rnlombardo.R \
  -i test_data/5000_set.csv \
  -r test_data/lombardo_rename.csv \
  -o test_data/5000_set_lombardo.csv
bin/rfpred.R \
  -t training_data/master_set.csv.gz \
  -i training_data/master_set.info.csv \
  -s test_data/5000_set.csv \
  -o results/5000_master \
  -m 1000
bin/rfpred.R \
  -t training_data/lombardo.csv.gz \
  -i training_data/lombardo.info.csv \
  -s test_data/5000_set_lombardo.csv \
  -o results/5000_lombardo \
  -m 1000
  
