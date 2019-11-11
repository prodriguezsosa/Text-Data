#!/bin/bash
set -e

for i in $(seq 1 ${1})
do
cat LinkScraper.txt | sed "s/SEGMENT/$i/g" > LinkScraper.$i.txt

sbatch LinkScraper.$i.txt

rm LinkScraper.$i.txt
done
