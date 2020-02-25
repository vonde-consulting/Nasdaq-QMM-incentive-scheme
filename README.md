# Nasdaq-QMM-incentive-scheme
This project explores the effectiveness of Nasdaq’s Qualified Market Maker (QMM) program, an incentive scheme meant to encourage liquidity provision

## Set up and work with the Github repository

This section is for the coauthors.

1. Download the git client software from [here](https://git-scm.com/downloads), if you have not got it installed before;
2. Clone the repository
```bash
cd /path/to/your/work/directory
git clone https://github.com/vonde-consulting/Nasdaq-QMM-incentive-scheme.git
```
3. Create a branch
```bash
cd Nasdaq-QMM-incentive-scheme
git checkout -b <your_branch_name>
```
4. Work on the branch
5. Commit your work
```bash
git status
git commit -am "My commit comments"

```
6. Push the new branch to the repository
```bash
git push -u origin <your_branch_name>
```
Note that since the second time, you push the change to the repository without **-u**, i.e.
```bash
git push origin <your_branch_name>
```
7. Request to merge your change to the master branch

  - Log in Github 
  - Get to the repository **vonde-consulting/Nasdaq-QMM-incentive-scheme**
  - Under the **Code** tab, click **branches**
  - Click **New pull request** button next to your branch

8. Once your pull request has been overviewed and approved by another collaborator, your branch will be able to merge into **master** branch -- the release of our paper and codes. 

## Introduction to Load Data by R (Not Recommended)
Though I believe that in Cluster, R would work well with spark, in local mode using **sparklyr** is painful . The JVM keeps running out of memory, when I am trying to run a slightly big dataset. 
* Install packages in R console
```R
> install.packages(c("sparklyr", "dplyr", "DBI"))
```
* Install spark
```R
> library(sparklyr)
> spark_install(version = "2.4")
```
* Make sure your Dropbox has been synchronized
* Run **Codes/Clean Data/S01_loadData.R**
* Note that after ``spark_connect(...)`` line, you can monitor spark by a browser at the address ``http://localhost:4040``

## Introduction to Load Data by Python (Recommanded)
Python runs smoothly in local mode.
* Install pyspark by following the steps at <https://spark.apache.org/downloads.html> 
* Install Pandas
* Install Jupyter
* Start jupyter-notebook by
```bash
$ cd Nasdaq-QMM-incentive-scheme/PythonCode
$ jupyter-notebook
```
* Open **lobsterOnSpark.ipynb** on jupyter start page 




