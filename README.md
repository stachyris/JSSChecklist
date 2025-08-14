# JSSChecklist
Scripts used for the short note on the checklists of birds found in the JSS College of Arts Commerce and Science, Mysuru, campus

## Overview

This repo contains, data, script for analysis and output related to the short note on the avi-faunal diversity of JSS College of Arts Commerce and Science College, Mysuru. While doing my master's at JSS, I kept a regular 'eBird Checklist' and coallated them as a [hotspot](https://ebird.org/hotspot/L5915465). 

## Repo Struture

- **data** : This directory has raw input for the analysis. 
	- ``ebd_IN-KA-MY_201704_201905_unv_smp_relJun-2025`` : eBird ebd file for Mysuru between April 2017 to May 2019
	- ``JSS_Sp_list.csv`` : Cummulative species observed in the campus between April 2017 to May 2019. 
	- ``eBird-Clements-v2024-integrated-checklist-October-2024-rev.csv`` : Integrated eBird/Clements taxonomy to match and extract family and order information. 
	- ``SOIB23.csv`` : State of India's Birds 2023 assessment data file (Redundant) - using [skimmr](https://github.com/rikudoukarthik/skimmr) to extract same information. 
	- ``annotated_species.csv`` : Final, annoated species list with higher taxonomy and conservation priority status information. 
- **figures** : Final figures as they appears in the short note. 
- **scripts** : R script used to import, integrate and analyse the data. 

## Data Availability
All raw and intermediate data are available in the ``data`` directory. Check [eBird hotspot](https://ebird.org/hotspot/L5915465) for latest species counts and tally. 

## Attribution
To be updated

## Contact Information
Please contact the following in case of interest.
Vinay K L [vkl1@lsu.edu](mailto:vkl1@lsu.edu)  
PhD Student, Louisiana State University. 
