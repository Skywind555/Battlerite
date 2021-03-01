### **Overview**

This project used to pull data from a video game (Battlerite) API while it was still active (permanently dead now). 

With the limited data that I collected, I built a complex R Shiny dashboard which you can only view now if you download the repo and run it on your local machine.

Dashboard deployed on AWS viewable at http://shiny.battlerite.site/Personal-Projects/
***Currently not deployed due to expensive cost***

**Libraries used for Python:**

requests

pandas

numpy

json

datetime

time

os

shutil

math

glob

pathlib

pygame

**Libraries used for R:**

shiny
dplyr
readr
forcats
tidyr
stringr
readxl
schoolmath
ggplot2
DT
fastDummies
leaflet


### **File Description:**

**Get_Data.py / Get_Data.ipynb** - Version 1 data collection script from the API that corresponds to the 'Data' folder.

For better rendering on the jupyter notebook, use this [link](https://nbviewer.jupyter.org/github/Skywind555/Personal-Projects/blob/master/Get_Data.ipynb)

**Get_Reference_Files.py / Get_Reference_Files.ipynb** - Version 1 of collecting a few reference tables used in the
Get_Data script. Corresponds to the 'Reference Files' folder.

For better rendering on the jupyter notebook, use this [link](https://nbviewer.jupyter.org/github/Skywind555/Personal-Projects/blob/master/Get_Reference_Files.ipynb)

**Get_Data_V2.py** - Version 2 data collection script from the API that corresponds to the 'Data2' folder. More indepth
metrics are gathered such as EX1, EX2, R, and the number of ultimates used.

**Get_Reference_Files_V2.py** - Version 2 of collecting a few reference tables used in the Get_Data_V2 script. Corresponds to
the 'Reference Files' folder.

**Image Conversion.ipynb** - Script to convert files in the 'Image' folder to convert all images to a png format and resizes them
to 130 x 130 pixels. Corresponds to the 'ImageConverted' folder.

For better rendering on the jupyter notebook, use this [link](https://nbviewer.jupyter.org/github/Skywind555/Personal-Projects/blob/master/Image%20Conversion.ipynb)

**Process_Data.R** - R script used to prepare the data used in the R shiny dashboard. Corresponds to data in the 'Dashboard Data...'
folder.

**Battlerite_API_Dashboard.R** - R shiny dashboard script for local usage.

**app.R** - R shiny dashboard script to be put on a virtual box on AWS.

**User_Guide.md** - User guide for the R shiny dashboard.

**Features_To_add.md** - Additional features to add to the R shiny dashboard (abandoned)

### **Data Analysis Folder**

This folder contains another project using the data in the 'Data2' folder. It's linked to the [blog post](https://skywind555.github.io/Answering-Your-Battlerite-Questions-With-Pro-Player-Skywind555/) that I created.