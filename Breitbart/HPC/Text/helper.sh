#!/bin/bash
set -e

cat TextScraperHPC.txt | sed "s/SEGMENT/$1/g" > TextScraperHPC.${1}.txt

sbatch TextScraperHPC.${1}.txt

rm TextScraperHPC.${1}.txt
