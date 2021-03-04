
# Development of Quality Monitoring System based on Common Data Model with Application to Clinical Quality in Emergency Department

---

## Background
It is necessary to manage the clinical quality indices, especially in emergency department (ED), to establish the better clinical environment and offer patients to high quality healthcare service. However, there have been problems related to efficiency and accuracy because most institutions have managed their indices by hand. Also, it has been difficult to improve the clinical quality continuously and instantly because quality assessment from authorities has been operated on a regular and long-term basis. Therefore, our study aims to develop the monitoring system for clinical quality indices based on common data model (CDM) to offer national regulatory agencies to manage quality indices efficiently with an automatic dashboard under standardized framework at any time.

---

## Goal
•	Development of the algorithm to improve the accuracy of quality indices and efficiency of calculating the indices

•	Development of <Monitoring system of clinical quality indices > to manage the indices at the national level anytime

---

## Procedure

Below figure indicates the pipeline from the data extraction in an institution to development of a dashboard.
![process](https://user-images.githubusercontent.com/28096343/109112997-db136c80-777e-11eb-8c4e-90652f598026.png)

1) Data extraction from the data base (DB) in an institution and data preprocessing
2) Data Mapping based on OMOP CDM Table ([OMOP CDM v5.3.1](https://ohdsi.github.io/CommonDataModel/cdm531.html#omop_cdm_v531)) -> [__1_CDM Mapping.R__](https://github.com/hong-sj/Digital_Health/blob/main/CDM/Dash_eng/1_CDM_Mapping.R)
3) Building the algorithm for calculation of clinical quality indices -> [__2_Quality Indices.R__](https://github.com/hong-sj/Digital_Health/blob/main/CDM/Dash_eng/2_Quality%20Indices.R)
4) Development of the integrated clinical quality dashboard for self-monitoring in the institution. -> [__Dashboard__](https://monitoring-amia.herokuapp.com/)

---

## Example index

__Bed occupancy index__ in ED according to the criteria for evaluation of Korean emergency medical institutions in 2020.

Bed occupancy index = {the sum of staying time form all visiting patients ÷ (the number of beds in criteria * the number of days in each month * 24)} * 100


1. The preparation of data: the data structure has to be same with __sample.csv__

2. CDM Mapping: the mapping procedure is executed with the OMOP CDM table (__1_CDM_Mapping.R__)

3. The calculation of the quality index: the bed occupancy index is calculated with above the equation by using the data which is mapped by CDM (__2_Quality Indices.R__)

4. Visualization in the dashboard. (This part is preparing. Python script will be distributed in the future)

---

## Dashboard

![대시보드화면](https://user-images.githubusercontent.com/28096343/109990747-07b22000-7d4d-11eb-9cb7-74992ff3e6ff.jpg)

---

## Expectation

It is expected to improve the clinical environment by developing the monitoring system. It is possible to reduce the clinician’s burden to calculate clinical indices one by one. The accuracy and usability would also increase considerably. In addition, it could be evoked about the need for monitoring system for standardized indices through the establishment of a pipeline that develops monitoring system having same structures.


