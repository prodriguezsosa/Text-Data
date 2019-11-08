#!/bin/bash
set -e

cat ScraperHPC.txt | sed "s/SEGMENT/$1/g" > ScraperHPC.${1}.txt

sbatch ScraperHPC.${1}.txt

rm ScraperHPC.${1}.txt
