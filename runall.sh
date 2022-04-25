#!/bin/bash

bin/rfpred.R \
  -t training_data/master.csv.gz \
  -i training_data/master.info.csv \
  -s test_data/18_set.csv \
  -o results/18_master \
  -m 15
bin/rfpred.R \
  -t training_data/lombardo_new.csv.gz \
  -i training_data/lombardo.info.csv \
  -s test_data/18_set.csv \
  -o results/18_lombardo \
  -m 15


bin/mergeph.R \
  -d excel_files/647/647_structures2_export_descriptors.xlsx \
  -p excel_files/647/647_structures2_export_pH2.xlsx \
  -m excel_files/647/647_Moka_incl_designation.xlsx \
  -o test_data/647_set.csv
bin/rfpred.R \
  -t training_data/master.csv.gz \
  -i training_data/master.info.csv \
  -s test_data/647_set.csv \
  -o results/647_master \
  -m 500
bin/rfpred.R \
  -t training_data/lombardo_new.csv.gz \
  -i training_data/lombardo.info.csv \
  -s test_data/647_set.csv \
  -o results/647_lombardo \
  -m 500

bin/mergeph.R \
   -d excel_files/1202/1202_structures_export_pH74.xlsx \
   -p excel_files/1202/1202_structures_export_pH2.xls \
   -m excel_files/1202/1202_Moka_incl_designation.xlsx \
   -o test_data/1202_set.csv
bin/rfpred.R \
  -t training_data/master.csv.gz \
  -i training_data/master.info.csv \
  -s test_data/1202_set.csv \
  -o results/1202_master \
  -m 1000
bin/rfpred.R \
  -t training_data/lombardo_new.csv.gz \
  -i training_data/lombardo.info.csv \
  -s test_data/1202_set.csv \
  -o results/1202_lombardo \
  -m 1000
  

bin/mergeph.R \
   -d excel_files/5000/5000_pH74.xlsx \
   -p excel_files/5000/5000_structures_only_export_pH2.xlsx \
   -m excel_files/5000/5000_Moka_incl_designation.xlsx \
   -o test_data/5000_set.csv
bin/rfpred.R \
  -t training_data/master.csv.gz \
  -i training_data/master.info.csv \
  -s test_data/5000_set.csv \
  -o results/5000_master \
  -m 1000
bin/rfpred.R \
  -t training_data/lombardo_new.csv.gz \
  -i training_data/lombardo.info.csv \
  -s test_data/5000_set.csv \
  -o results/5000_lombardo \
  -m 1000
  
