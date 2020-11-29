# CEU_DA2_Assignment_1

This repository contains Bruno Helmeczy's 1st Assignment for the Data Analysis 2 - Regressions & Coding 1 - Data Analysis & Management with R courses at the Central European Universitys' MSc in Business Analytics curriculum.

The assignment investigated the pattern of association between registered covid-19 cases and registered number of deaths due to covid-19 across countries, as of 20th October, 2020.

The repository contains the following:

Data folder: 
The raw data set is obtained from: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data, originating from the 2019 Novel Coronavirus Visual Dashboard, operated by Johns Hopkins Universitys' Centre for Systems Science and Engineering (JHU CSSE).

Population in 2019 from listed countries is also included in this data set. It is downloaded from the World Bank- World Development Indicators. The number of population is in ten thousands. link: https://data.worldbank.org/indicator/SP.POP.TOTL

The cleaned data set contains countries covid19 reports as of October 20, 2020, including confirmed-,  death-, recovered-, & active cases from 182 listed countries.
Confirmed- & Death cases also include probable cases, if such data is reported in a country. Active cases are calculated as the total confirmed cases minus deaths, minus recovered cases.

Codes folder:The codes folder includes the following:
covid_get_data.R: this is the R script that collects all the data used in this analysis.

covid_clean_data.R: this is the R script that contains the steps of cleaning the data set before doing analysis.

covid_analysis_data.R: this is the R script with all the analysis part of this project.

covid_analysis_data.Rmd: same as covid_analysis _data.R but in .Rmd. You should be able to replicate my results by running this Rmd file.

Joint_Assignment_COVID.Rproj: Project file is also included so you should be able to open all the R scripts and Rmd file in this project.

Docs folder: it contains both .html and .pdf generated from .rmd.

Output folder: in this folder, you can find the code generated model summary statistics table in the html format, and also a screenshot of the html result.
