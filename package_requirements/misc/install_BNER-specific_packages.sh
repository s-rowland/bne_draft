#!/bin/bash
# IMPORTANT NOTE: activate the BNER conda environment BEFORE RUNNING THIS SCRIPT!!!
# the line below is for safety but you should explicitly run it BEFORE running this script, 
# as this for whatever reason doesn't always run
conda activate BNER

echo "installing packages from the conda-forge channel now..."

# install 1-by-1 the packages (& their implicit dependencies) located in the conda-forge.txt file from the 'conda-forge' channel
while read requirement; do conda install --yes -c conda-forge $requirement; done < requirements_BNER_conda-forge.txt
