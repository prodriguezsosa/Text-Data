#!/bin/bash
set -e

for i in $(seq 1 ${1})
do
cat ScraperHPC.txt | sed "s/SEGMENT/$i/g" > ScraperHPC.$i.txt

sbatch ScraperHPC.$i.txt

rm ScraperHPC.$i.txt
done
