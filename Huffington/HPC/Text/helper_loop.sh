#!/bin/bash
set -e

for i in $(seq 1 ${1})
do
cat TextScraperHPC.txt | sed "s/SEGMENT/$i/g" > TextScraperHPC.$i.txt

sbatch TextScraperHPC.$i.txt

rm TextScraperHPC.$i.txt
done
