--------------------------------------------------------------------------------

File: README.txt for bne/package_requirements directory
Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
Date: 01/21/2022

Table of Contents:

    (1) FILE DESCRIPTIONS of all files in the directory.

    (2) SETTING UP BNER environment for running BNE-related scripts. 

    (3) ADDING PACKAGES TO BNER environment, & guidelines to keep 
        the .yml file up to date.

    (4) UPDATING BNER environment w/ packages others have added.

    (5) WARNINGS: packages that we still need to run BNER but are not available
        on conda, whether that be due to availability or dependency issues.

    (6) FOR LAWRENCE ONLY: creating BNER.yml file from R_env.

** Note: throughout this document, commands prefaced with '**' MUST be run 
inside the directory: /data0/shr/bne/package_requirements. 
Other commands can be run from anywhere.

--------------------------------------------------------------------------------

1. FILE DESCRIPTIONS

    (a) BNER.yml: A YAML file containing the package information for the BNER 
        conda environment. Instructions on how to use it to create / update the
        BNER conda environment are in sections 2, 3, and 4.

    (b) requirements.txt: A plain text file containing all packages (python,
        R, etc.) in the BNER conda environment, along with their versions.

    (c) pacs_R.csv: A csv file containing the R packages & their versions in the 
        BNER conda environment. Created from requirements.txt by get_pacs_R.py.

    (d) get_pacs_R.py: A python script to print only the R packages and versions
        from requirements.txt in a more readable format (saved as pacs_R.csv).

    (e) CHEATSHEET.txt: A plain text file abbreviating the info in this README.

    (f) misc: a directory for Lawrence's use only, containing:

            (i) R_env.yml: A YAML file containing the package info for the R_env
            conda environment. It's only real use is to easily see the 
            differences b/w R_env & BNER, using "diff BNER.yml misc/R_env.yml"

            (ii) install_BNER-specific_packages.sh: A bash shell script used to 
            create the BNER conda environment from R_env (from scratch).

            (iii) requirements_BNER_conda-forge.txt: A plain text file 
            containing the packages (excluding dependencies) that must be added
            to the basic R_env conda environment in order to create the BNER 
            conda environment. Needed for the shell script.

            (iv) requirements_original_local.txt: A plain text file containing 
            the packages (excluding dependencies & base R packages) that have 
            been used in all BNE scripts on Sebastian / Lawrence's LOCAL copies 
            as of 1/20/22. Meant to be used as a reference / for posterity.

--------------------------------------------------------------------------------

2. SETTING UP YOUR OWN BNER CONDA ENVIRONMENT FOR THE FIRST TIME

    (Step 0): Ensure you don't already have the BNER environment set up by
              running the following line:
                  
                    conda env list | grep 'BNER' | wc -l

              If a '1' is returned, the environment is already installed and
              you can skip to Step 2. If a '0' is returned, you need to set up 
              the environment, so continue to Step 1.

    (Step 1): Run the following command** to create the BNER conda environment:
                  
                    conda env create --file BNER.yml

              If the file BNER.yml cannot be found, see section 6. Otherwise, 
              the command has run correctly, and rerunning the command in Step 0
              above, you should see a '1' output. Proceed to Step 2 below.

    (Step 2): Now you can activate your environment simply by running the
              following line anywhere on the server:
                  
                    conda activate BNER

              Depending on how your shell prompt is configured, you should see
              (BNER) to the far left of the shell prompt. When you are finished 
              working with the environment, run the following line from 
              anywhere on the server:
                  
                    conda deactivate BNER

              Now that the BNER conda environment is set up, in future
              server sessions you simply activate it with:

                    conda activate BNER

              i.e. there is no need to set up the environment again in step 1.

--------------------------------------------------------------------------------

3. ADDING PACKAGES TO THE BNER CONDA ENVIRONMENT

    (Step 0): New packages can be searched for at https://anaconda.org OR via
              either of the following commands:

                    conda search PACKAGE-NAME # this one searches on currently 
                                              # configured channels

                    anaconda search PACKAGE-NAME # this one searches on
                                                 # all channels

    (Step 1): Activate the BNER conda environment with:
                    
                    conda activate BNER

    (Step 2): Install PACKAGE-NAME from the conda-forge channel (try to only
              use the conda-forge channel to install packages, for consistency's 
              sake) with the following command:
                    
                    conda install -c conda-forge PACKAGE-NAME

              You can specify a specific package version number with e.g.:
                    
                    conda install -c conda-forge PACKAGE-NAME==1.0.4

    (Step 3): Once the package is installed, you need to document its
              installation in three places:
              
              <1> Update requirements.txt by overwriting it with**:
                    
                    conda list --explicit > requirements.txt

              <2> Update BNER.yml by overwriting it with**:
                    
                    conda env export --name BNER | grep -v "^prefix: " > BNER.yml

              <3> Update pacs_R.csv by overwriting it with**:

                    python get_pacs_R.py

    (Step 4): Push changes to GitHub with**:
                    
                    git add BNER.yml pacs_R.csv requirements.txt
                    git commit -m "Updated BNER env w/ PKGNAME1, PKGNAME2, ..."
                    git push

              NOTE: Before pushing changes to these files, check in with the
              group to ensure there aren't major conflicts.

--------------------------------------------------------------------------------

4. UPDATING BNER WITH PACKAGES OTHERS HAVE ADDED

    (Step 1): Pull in any changes to the BNER.yml file from GitHub with**:
                
                    git pull

    (Step 2): Update the conda environment with**:
                    
                    conda env update --name BNER --file BNER.yml --prune

              The '--prune' flag uninstall dependencies which were removed
              from BNER.yml. 

--------------------------------------------------------------------------------

5. WARNINGS: PACKAGES NEEDED BUT NOT CURRENTLY INSTALLED:

nngeo: unavailable on conda. need to ask Gus for a workaround or clone its
source repository...

--------------------------------------------------------------------------------

6. (FOR LAWRENCE ONLY) INSTRUCTIONS TO CREATE THE BNER.yml FILE FROM R_env

Note: These instructions are for Lawrence's use only, unless the BNER.yml
      file, the requirements_BNER.txt file, and the BNER environment have all 
      been deleted / corrupted accidentally. Doing these instructions will
      essentially be performing a hard reset of the BNER environment to its
      state as of 01/21/22.

    (Step 1): Clone R_env to initialize the BNER environment with:
                    
                    conda create --clone R_env --name BNER

    (Step 2): Activate BNER environment:

                    conda activate BNER

    (Step 3): Navigate to misc directory:
                    
                    cd /data0/shr/bne/package_requirements/misc

    (Step 4): Run the bash shell script to install necessary packages for BNER:

                    source install_BNER-specific_packages.sh

    (Step 5): Export .yml and .txt files from the newly created BNER env:
                    
                    cd ../
                    conda env export --name BNER | grep -v "^prefix: " > BNER.yml
                    conda list --explicit > requirements.txt

    (Step 6): Push changes to GitHub:

                    git add BNER.yml requirements.txt
                    git commit -m "Added new YAML and txt files."
                    git push

--------------------------------------------------------------------------------

END OF README.txt

--------------------------------------------------------------------------------
