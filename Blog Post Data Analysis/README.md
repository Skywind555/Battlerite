### **Introduction**

This project was started and completed as part of the Data Scientist Nanodegree program on Udacity. I decided to
choose this dataset because I had already collected it and always wanted to answer more questions about it, but never
commited time and effort into it.

A [blog post](https://skywind555.github.io/Answering-Your-Battlerite-Questions-With-Pro-Player-Skywind555/) was created to summarize results to a non-technical audience that is already familiar with Battlerite.

**Python libraries used:**

os

glob

pandas

numpy

math

collections

glob

matplotlib.pyplot

seaborn

statsmodels

scipy

openpyxl

warnings

pylab

sklearn

### **File Description**

**BR Data Processing.ipynb** - Contains the initial data processing and transformation of the data located in the ../Data2 folder
to a format used in the analysis.

For better rendering on the jupyter notebook, use this [link](https://nbviewer.jupyter.org/github/Skywind555/Battlerite/blob/master/Blog%20Post%20Data%20Analysis/BR%20Data%20Processing.ipynb)

**BR Data Analysis.ipynb** - Contains more data processing and analysis to answer two indepth questions about Battlerite.

For better rendering on the jupyter notebook, use this [link](https://nbviewer.jupyter.org/github/Skywind555/Battlerite/blob/master/Blog%20Post%20Data%20Analysis/BR%20Data%20Analysis.ipynb)

**BR Data Analysis2.ipynb** - Contains a statistical modeling script and analysis done for the third question.

For better rendering on the jupyter notebook, use this [link](https://nbviewer.jupyter.org/github/Skywind555/Battlerite/blob/master/Blog%20Post%20Data%20Analysis/BR%20Data%20Analysis%202.ipynb)

**Processed_Data_#.csv** - The split data from BR Data Processing.ipynb

**SoloQueue_Data_#.csv** - The split data result after further data processing in the BR Data Analysis.ipynb for the solo queue group.

**Modeling_Data...** - Various data output files from BR Data Analysis.ipynb to be used in BR Data Analysis2.ipynb

**Model_Summary_3v3_V1 / Model_Summary_3v3_V2 / Model_Summary_2v2_V1 / Model_Summary_2v2_V2** - Excel files that summarizes each
step of the logistic regression model building process for 3v3 games, 2v2 games split by a different set of predictors fed into
the model.

**png image files** - Output image files from BR Data Analysis.ipynb to be used in the blog post. Note that the tables used in the
blog post are manual screenshots taken with Gyazo in the [blog template repository](https://github.com/Skywind555/Skywind555.github.io) in the images folder.