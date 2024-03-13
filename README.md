# Case Study - US Heart Disease Mortality (2010-2019)

**Author**: Clint Barnard-El <br />
**Email**: barnard.clint@yahoo.com <br />
**LinkedIn**: https://www.linkedin.com/in/clintbarnardel/  <br />
<br />
<br />

## Introduction
This repository provides the results from an analysis of heart disease mortality rates in the US during the period of 2010-2019. Because [heart disease](https://www.cdc.gov/heartdisease/facts.htm) is the leading cause of mortality in the US, stakeholders such as healthcare providers and administrators could use analysis such as this to assess the impact of heart disease on healthcare systems, forecast future healthcare needs, and optimize resource allocation.

The data was provided by the Centers for *Disease Control & Prevention* [(CDC)](https://www.cdc.gov/) and exported from the *National Vital Statistics System* [(NVSS)](https://data.cdc.gov/Heart-Disease-Stroke-Prevention/National-Vital-Statistics-System-NVSS-National-Car/kztq-p2jf/about_data). 
<br />

## Objective

Identify what specific groups were most/least impacted by heart disease (HD) mortality, the most prevalent types of HD, and regions within the US during 2010-2019.
<br />

## Applications

- RStudio
- Google Chrome

## Language 
- R

## R Analysis

R Analysis [(Markdown File)](https://github.com/clintbel/case-study-heart-disease-2010-2019/blob/main/heart_disease_case_study_20240312.md) 
<br />


## Skills Demonstrated

- Data Cleaning
- Descriptive Analysis
- [Data Visualization](https://github.com/clintbel/case-study-heart-disease-2010-2019/tree/main/docs)
<br />

## Summary

The US national average of heart disease mortality rates was reduced by 13.69% (103 to 88.9 per 100K) during the period of 2010-2019. The mortality rate for both genders decreased (16.62% for women and 9.4% for men),  however men had a higher mortality rate then women (111 per 100K compared to 85.5). All racial groups showed a reduction in mortality rates, however African-Americans (Black) had nearly double the mortality rates (123 per 100K) compared to Hispanic-Americans with the lowest mortality rates (66 per 100K). 
The four main types of heart disease specified were: Coronary Heart Disease, Stroke, Heart Attack, and Heart Failure. Despite Coronary Heart Disease rates dropping by 21%, it was still the most reported type (3x more than the other 3 types). All types decreased during this period except for heart attacks that increased by over 10% (23.5 to 26 per 100). 

**Potential Stakeholders:**
- Healthcare providers 
- Government health agencies
- Insurance companies
<br />

**Insights:**
- The southern region of the US was most impacted with *Mississippi* experiencing the highest rates (134.7 per 100K), followed by *Arkansas, West Virginia, Oklahoma, and South Dakota* (119.2-129.9 range). 
- States in the *West* and *Northeast* region were the least impacted with *Massachusetts* experiencing the lowest rates (68.1 per 100K), followed by *Colorado, Minnesota, Washington, and Virginia* (71.8-75.7 range).
- The racial group most impacted were *African-Americans*; the least were *Hispanic-Americans*.
- The gender most impacted were American *males*.
<br />


**Notes:**

1.	*Dataset included data supplied by healthcare professionals/facilities.*
2.	*For fairer group comparison, “age-standardized” data was used.* 
3.	*Racial group “other/unknown” included Asian, Native Hawaiian or Other Pacific Islander, American Indian or Alaska Native, and/or unspecified. Not all states adopted the updated [federal standards](https://www.cdc.gov/nchs/hus/sources-definitions/race.htm) set in 1997 and 2003.*
4.	*Summation for heart disease types exclude generic/unspecified designations such as “Major Cardiovascular Disease” and “Diseases of the Heart”.*
<br />


## Dataset
The dataset was exported from the [NVSS](https://data.cdc.gov/Heart-Disease-Stroke-Prevention/National-Vital-Statistics-System-NVSS-National-Car/kztq-p2jf/about_data) on 3/4/24 in the form of a [.csv](https://github.com/clintbel/case-study-heart-disease-2010-2019/blob/main/docs/nvss_hd_2010_2020_raw.csv) file. 
