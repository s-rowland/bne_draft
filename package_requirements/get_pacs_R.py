# File: get_pacs_R.py
# Author: Lawrence Chillrud <lgc2139@cumc.columbia.edu>
# Date: 1/24/22

import re
import pandas as pd

pacs = []
versions = []
with open('requirements.txt', 'r') as f:
    lines = f.readlines()
    for line in lines:
        pac_version = line.split('/')[-1]
        if pac_version[:2] == "r-":
            info = pac_version.split('-')
            pacs.append("r-" + info[1])
            versions.append(info[2])

df = pd.DataFrame(list(zip(pacs, versions)), columns = ["Package", "Version"]).sort_values(by = "Package")
df.to_csv('pacs_R.csv', index = False)
