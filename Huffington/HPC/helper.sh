#!/bin/bash
set -e

cat LinkScraper.txt | sed "s/SEGMENT/$1/g" > LinkScraper.${1}.txt

sbatch LinkScraper.${1}.txt

rm LinkScraper.${1}.txt
