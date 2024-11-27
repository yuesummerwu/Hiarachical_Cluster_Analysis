---
title: "Midterm_HCAHeatmap_YueSummerWu"
author: "Summer"
date: "2024-10-29"
output: 
  html_document:
    keep_md: true
bibliography: references.bib
---

# 1. Importing Libraries

## 1.1 For data preparation


``` r
# Working with URLs and HTTP requests
library(httr)
# Reading excel sheet
library(readxl)
# Connectting with online resources
library(httr) 
# Working with compressed ZIP files in R
library(zip)
```

```
## 
## Attaching package: 'zip'
```

```
## The following objects are masked from 'package:utils':
## 
##     unzip, zip
```

``` r
# Cleaning dataset 
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
## ✔ purrr     1.0.2
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
# Merging multiple datasets
library(purrr)
# Standardizing column names 
library(janitor)
```

```
## 
## Attaching package: 'janitor'
## 
## The following objects are masked from 'package:stats':
## 
##     chisq.test, fisher.test
```

``` r
# Annotations
library(ggrepel)
```

```
## Warning: package 'ggrepel' was built under R version 4.3.3
```

``` r
# References 
library(grateful)
```

```
## Warning: package 'grateful' was built under R version 4.3.3
```

## 1.2 For visualizations


``` r
# Loading our best friend 
library(ggplot2)
# Adding text 
library(ggtext)
```

## 1.3 For HCA heatmap


``` r
# Creating complex heatmap visualizations
library(ComplexHeatmap)
```

```
## Loading required package: grid
```

```
## ========================================
## ComplexHeatmap version 2.21.1
## Bioconductor page: http://bioconductor.org/packages/ComplexHeatmap/
## Github page: https://github.com/jokergoo/ComplexHeatmap
## Documentation: http://jokergoo.github.io/ComplexHeatmap-reference
## 
## If you use it in published research, please cite either one:
## - Gu, Z. Complex Heatmap Visualization. iMeta 2022.
## - Gu, Z. Complex heatmaps reveal patterns and correlations in multidimensional 
##     genomic data. Bioinformatics 2016.
## 
## 
## The new InteractiveComplexHeatmap package can directly export static 
## complex heatmaps into an interactive Shiny app with zero effort. Have a try!
## 
## This message can be suppressed by:
##   suppressPackageStartupMessages(library(ComplexHeatmap))
## ========================================
```

``` r
# Supporting ComplexHeatmap by offering color functions and helping to manage complex layout structures within heatmaps
library(circlize)
```

```
## ========================================
## circlize version 0.4.16
## CRAN page: https://cran.r-project.org/package=circlize
## Github page: https://github.com/jokergoo/circlize
## Documentation: https://jokergoo.github.io/circlize_book/book/
## 
## If you use it in published research, please cite:
## Gu, Z. circlize implements and enhances circular visualization
##   in R. Bioinformatics 2014.
## 
## This message can be suppressed by:
##   suppressPackageStartupMessages(library(circlize))
## ========================================
```

``` r
# Allowing us to perform hierarchical clustering with a unique approach that groups data into clusters while ordering them in a meaningful way
library(hopach)
```

```
## Loading required package: cluster
```

```
## Loading required package: Biobase
```

```
## Loading required package: BiocGenerics
```

```
## 
## Attaching package: 'BiocGenerics'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
```

```
## The following objects are masked from 'package:dplyr':
## 
##     combine, intersect, setdiff, union
```

```
## The following objects are masked from 'package:stats':
## 
##     IQR, mad, sd, var, xtabs
```

```
## The following objects are masked from 'package:base':
## 
##     anyDuplicated, aperm, append, as.data.frame, basename, cbind,
##     colnames, dirname, do.call, duplicated, eval, evalq, Filter, Find,
##     get, grep, grepl, intersect, is.unsorted, lapply, Map, mapply,
##     match, mget, order, paste, pmax, pmax.int, pmin, pmin.int,
##     Position, rank, rbind, Reduce, rownames, sapply, setdiff, sort,
##     table, tapply, union, unique, unsplit, which.max, which.min
```

```
## Welcome to Bioconductor
## 
##     Vignettes contain introductory material; view with
##     'browseVignettes()'. To cite Bioconductor, see
##     'citation("Biobase")', and for packages 'citation("pkgname")'.
```

```
## 
## Attaching package: 'Biobase'
```

```
## The following object is masked from 'package:httr':
## 
##     content
```

# 2. Dataset Construction


``` r
# Suppressing column specification message
options(readr.show_col_types = FALSE)
```

## 2.1 Loading Data

### 2.1.1 CRDC


``` r
# Define a temporary file for the zip
temp_crdc <- tempfile(fileext = ".zip")
crdc_zip_link <- "https://civilrightsdata.ed.gov/assets/ocr/docs/2020-21-crdc-data.zip"

# Download the zip file
GET(crdc_zip_link, write_disk(temp_crdc, overwrite = TRUE))
```

```
## Response [https://civilrightsdata.ed.gov/assets/ocr/docs/2020-21-crdc-data.zip]
##   Date: 2024-11-27 22:36
##   Status: 200
##   Content-Type: application/x-zip-compressed
##   Size: 81.6 MB
## <ON DISK>  /var/folders/9b/7s3y6tb91md6y_vlqq56g41c0000gn/T//RtmprAV6Ri/filefb4856ae6a47.zip
```

``` r
# Define an output directory to unzip files
folders_dir <- tempdir()

# Unzip the file into the output directory
unzip(temp_crdc, exdir = folders_dir)

# Define the School folder path based on folder structure after unzipping
school_folder <- file.path(folders_dir, "CRDC", "School")

# Check if the school folder exists and list the files
if (dir.exists(school_folder)) {
  school_files <- list.files(school_folder, full.names = TRUE)
  
  # Display the list of files and folders
  print(basename(school_files))
  
  # Load all files in the School folder into a list
  crdc_data_list <- lapply(school_files, function(file) {
    read.csv(file)
  })
  
  # Assign names to the list based on file names for easy access
  names(crdc_data_list) <- basename(school_files)
} else {
  print("The School folder does not exist. Please check the folder structure after unzipping.")
}
```

```
##  [1] "Advanced Mathematics.csv"         "Advanced Placement.csv"          
##  [3] "Algebra I.csv"                    "Algebra II.csv"                  
##  [5] "Biology.csv"                      "Calculus.csv"                    
##  [7] "Chemistry.csv"                    "Computer Science.csv"            
##  [9] "Corporal Punishment.csv"          "COVID Directional Indicators.csv"
## [11] "Dual Enrollment.csv"              "Enrollment.csv"                  
## [13] "Expulsions.csv"                   "Geometry.csv"                    
## [15] "Gifted and Talented.csv"          "Harassment and Bullying.csv"     
## [17] "International Baccalaureate.csv"  "Internet Access and Devices.csv" 
## [19] "Justice Facilities.csv"           "Offenses.csv"                    
## [21] "Physics.csv"                      "Referrals and Arrests.csv"       
## [23] "Restraint and Seclusion.csv"      "Retention.csv"                   
## [25] "SAT and ACT.csv"                  "School Characteristics.csv"      
## [27] "School Support.csv"               "Single sex Athletics.csv"        
## [29] "Single sex Classes.csv"           "Suspensions.csv"                 
## [31] "Transfers.csv"
```

### 2.1.2 NC DPI


``` r
# Define a temporary file for the zip
temp_ncdpi <- tempfile(fileext = ".zip")
ncdpi_zip_link <- "https://www.dpi.nc.gov/documents/src-datasets-2023/open"

# Download the zip file
GET(ncdpi_zip_link, write_disk(temp_ncdpi, overwrite = TRUE))
```

```
## Response [https://www.dpi.nc.gov/documents/src-datasets-2023/open]
##   Date: 2024-11-27 22:37
##   Status: 200
##   Content-Type: application/zip
##   Size: 212 MB
## <ON DISK>  /var/folders/9b/7s3y6tb91md6y_vlqq56g41c0000gn/T//RtmprAV6Ri/filefb4837ffc4e.zip
```

``` r
# Define an output directory to unzip files
output_dir_ncdpi <- tempdir()

# Unzip the file into the output directory
unzip(temp_ncdpi, exdir = output_dir_ncdpi)

# List all files in the unzipped directory
ncdpi_files <- list.files(output_dir_ncdpi, recursive = TRUE, full.names = TRUE)

# Display the list of files
print(basename(ncdpi_files))
```

```
##   [1] "Distance Education.csv"              
##   [2] "High School Equivalency.csv"         
##   [3] "LEA Characteristics.csv"             
##   [4] "Advanced Mathematics.csv"            
##   [5] "Advanced Placement.csv"              
##   [6] "Algebra I.csv"                       
##   [7] "Algebra II.csv"                      
##   [8] "Biology.csv"                         
##   [9] "Calculus.csv"                        
##  [10] "Chemistry.csv"                       
##  [11] "Computer Science.csv"                
##  [12] "Corporal Punishment.csv"             
##  [13] "COVID Directional Indicators.csv"    
##  [14] "Dual Enrollment.csv"                 
##  [15] "Enrollment.csv"                      
##  [16] "Expulsions.csv"                      
##  [17] "Geometry.csv"                        
##  [18] "Gifted and Talented.csv"             
##  [19] "Harassment and Bullying.csv"         
##  [20] "International Baccalaureate.csv"     
##  [21] "Internet Access and Devices.csv"     
##  [22] "Justice Facilities.csv"              
##  [23] "Offenses.csv"                        
##  [24] "Physics.csv"                         
##  [25] "Referrals and Arrests.csv"           
##  [26] "Restraint and Seclusion.csv"         
##  [27] "Retention.csv"                       
##  [28] "SAT and ACT.csv"                     
##  [29] "School Characteristics.csv"          
##  [30] "School Support.csv"                  
##  [31] "Single sex Athletics.csv"            
##  [32] "Single sex Classes.csv"              
##  [33] "Suspensions.csv"                     
##  [34] "Transfers.csv"                       
##  [35] "ID 814 SCH - Chronic Absenteeism.csv"
##  [36] "filefb4837ffc4e.zip"                 
##  [37] "filefb4856ae6a47.zip"                
##  [38] "rcd_161.xlsx"                        
##  [39] "rcd_acc_aapart.xlsx"                 
##  [40] "rcd_acc_act.xlsx"                    
##  [41] "rcd_acc_awa.xlsx"                    
##  [42] "rcd_acc_cgr.xlsx"                    
##  [43] "rcd_acc_eds.xlsx"                    
##  [44] "rcd_acc_eg.xlsx"                     
##  [45] "rcd_acc_elp.xlsx"                    
##  [46] "rcd_acc_essa_desig.xlsx"             
##  [47] "rcd_acc_gp.xlsx"                     
##  [48] "rcd_acc_irm.xlsx"                    
##  [49] "rcd_acc_lowperf.xlsx"                
##  [50] "rcd_acc_ltg_detail.xlsx"             
##  [51] "rcd_acc_ltg.xlsx"                    
##  [52] "rcd_acc_mcr.xlsx"                    
##  [53] "rcd_acc_part_detail.xlsx"            
##  [54] "rcd_acc_part.xlsx"                   
##  [55] "rcd_acc_pc.txt"                      
##  [56] "rcd_acc_rta.xlsx"                    
##  [57] "rcd_acc_spg1.xlsx"                   
##  [58] "rcd_acc_spg2.xlsx"                   
##  [59] "rcd_acc_wk.xlsx"                     
##  [60] "rcd_adm.xlsx"                        
##  [61] "rcd_ap_crs_list.xlsx"                
##  [62] "rcd_ap.xlsx"                         
##  [63] "rcd_arts.xlsx"                       
##  [64] "rcd_arts2.xlsx"                      
##  [65] "rcd_att.xlsx"                        
##  [66] "rcd_charter.xlsx"                    
##  [67] "rcd_chronic_absent.xlsx"             
##  [68] "rcd_cie.xlsx"                        
##  [69] "rcd_code_desc.xlsx"                  
##  [70] "rcd_college.xlsx"                    
##  [71] "rcd_course2.xlsx"                    
##  [72] "rcd_courses1.xlsx"                   
##  [73] "rcd_courses2.xlsx"                   
##  [74] "rcd_cte_concentrators.xlsx"          
##  [75] "rcd_cte_credentials.xlsx"            
##  [76] "rcd_cte_endorsement.xlsx"            
##  [77] "rcd_cte_endorsements.xlsx"           
##  [78] "rcd_cte_enrollment_cluster.xlsx"     
##  [79] "rcd_cte_enrollment.xlsx"             
##  [80] "rcd_dlmi.xlsx"                       
##  [81] "rcd_effectiveness.xlsx"              
##  [82] "rcd_effectiveness3.xlsx"             
##  [83] "rcd_eq.xlsx"                         
##  [84] "rcd_esea_att.xlsx"                   
##  [85] "rcd_experience.xlsx"                 
##  [86] "rcd_funds.xlsx"                      
##  [87] "rcd_funds2.xlsx"                     
##  [88] "rcd_hqt.xlsx"                        
##  [89] "rcd_ib.xlsx"                         
##  [90] "rcd_improvement.xlsx"                
##  [91] "rcd_improvement2.xlsx"               
##  [92] "rcd_inc1.xlsx"                       
##  [93] "rcd_inc2.xlsx"                       
##  [94] "rcd_licenses.xlsx"                   
##  [95] "rcd_location.xlsx"                   
##  [96] "rcd_naep.xlsx"                       
##  [97] "rcd_nbpts.xlsx"                      
##  [98] "rcd_nbpts2.xlsx"                     
##  [99] "rcd_pk_enroll.xlsx"                  
## [100] "rcd_prin_demo.xlsx"                  
## [101] "rcd_readiness.xlsx"                  
## [102] "rcd_sar.xlsx"                        
## [103] "rcd_sat.xlsx"                        
## [104] "rcd_school_recognitions.xlsx"
```

``` r
# Load all .xlsx files into a list
ncdpi_data_list <- suppressWarnings(lapply(ncdpi_files[grepl("\\.xlsx$", ncdpi_files)], read_excel))

# Assign names to each dataset based on file names for easy access
names(ncdpi_data_list) <- basename(ncdpi_files[grepl("\\.xlsx$", ncdpi_files)])
```

## 2.2 Data Cleaning

CRDC data contains all schools in the US, and since we only care about NC schools in this project, we want to reduce the data to contain only NC high schools. We can identify those schools using one of the datasets called `School Characteristics.csv`.


``` r
high_schools <- crdc_data_list[["School Characteristics.csv"]] |>
  # Selecting NC schools 
  filter(LEA_STATE == "NC") |>
  # Selecting NC high schools 
  filter(SCH_GRADE_G09 == "Yes",
         SCH_GRADE_G10 == "Yes",
         SCH_GRADE_G11 == "Yes",
         SCH_GRADE_G12 == "Yes") |>
  # Only selecting metadata about schools and some school characteristics  
  select(COMBOKEY, LEA_NAME, SCH_NAME, SCH_STATUS_SPED, SCH_STATUS_MAGNET, SCH_STATUS_CHARTER, SCH_STATUS_ALT)|>
  # Renaming the columns for clearer annotations in heatmap 
  rename(`Special Education School` = SCH_STATUS_SPED,
         `Magnet School` = SCH_STATUS_MAGNET,
         `Charter School` = SCH_STATUS_CHARTER,
         `Alternative School` = SCH_STATUS_ALT)
```

School report card datasets from NCDPI only contain school code, but we need school name to merge them with the CRDC data since CRDC and NCDPI use different school ID. Therefore, to prepare for merging data later, we need to extract the school name.

*Note:* *Here, we haven't filter any school, so this dataset contains all schools in NC and some are not high school. We can achieve that in later merge with CRDC data.*


``` r
nc_school_location <- ncdpi_data_list[["rcd_location.xlsx"]] |>
  # Relying on the most recent data 
  filter(year == 2023,
         # Only school info are needed
         agency_level == "SCH") |>
  # Selecting agency_code and name only
  select(agency_code, name)
```

## 2.3 Feature Extraction & Engineering

Since `COMBOKEY` will be used for final merging of CRDC dataset, we can identify them here for later use.


``` r
# Extracting combokeys of high schools
high_schools_combokey <- high_schools |> select(COMBOKEY)
```

### **School Enrollment**

Most of the course enrollment data captures the raw number of students enrolled rather than the percentage, which can make the distribution heavily influenced by school size. To adjust for this, we need overall school enrollment data to calculate the percentage of students enrolled in each course. This will allow for a more accurate comparison across schools of different sizes.


``` r
enrollment <- crdc_data_list[["Enrollment.csv"]] |>
  # Filtering the dataset by high school 
  right_join(high_schools_combokey, by = "COMBOKEY") |>
  # Getting total enrollment 
  mutate(total_enrollment = TOT_ENR_M + TOT_ENR_F) |>
  # Selecing variables needed
  select(COMBOKEY, total_enrollment)
```

### College-Preparatory Courses

#### Math-Related Courses


``` r
# Advanced Math 
adv_math <- crdc_data_list[["Advanced Mathematics.csv"]] |>
  # Filtering the dataset by high school 
  right_join(enrollment, by = "COMBOKEY") |>
  # Changing neg values to NA 
  mutate(across(where(is.numeric), ~ ifelse(.x < 0, NA, .x))) |>
  # Total number of enrollment in advanced math 
  mutate(num_enr_adv_math = TOT_MATHENR_ADVM_M + TOT_MATHENR_ADVM_F,
         # Percentage of students enrolled in advanced math 
         pct_enr_adv_math = num_enr_adv_math / total_enrollment * 100) |>
  # Select columns of interest
  select(COMBOKEY, SCH_MATHCLASSES_ADVM, pct_enr_adv_math) |>
  # Renaming column names for clarity 
  rename(num_classes_adv_math = SCH_MATHCLASSES_ADVM) 
```


``` r
# Algebra II
algebra2 <- crdc_data_list[["Algebra II.csv"]] |>
  # Filtering the dataset by high school 
  right_join(enrollment, by = "COMBOKEY") |>
  # Changing neg values to NA 
  mutate(across(where(is.numeric), ~ ifelse(.x < 0, NA, .x))) |>
  # Total number of enrollment in algebra
  mutate(num_enr_algebra2 = TOT_MATHENR_ALG2_M + TOT_MATHENR_ALG2_F,
         pct_enr_algebra2 = num_enr_algebra2 / total_enrollment * 100) |>
  # Select columns of interest
  select(COMBOKEY, SCH_MATHCLASSES_ALG2, pct_enr_algebra2) |>
   # Renaming column names for clarity 
  rename(num_classes_algebra2 = SCH_MATHCLASSES_ALG2) 
```


``` r
# Calculus 
calculus <- crdc_data_list[["Calculus.csv"]] |>
  # Filtering the dataset by high school 
  right_join(enrollment, by = "COMBOKEY") |>
  # Changing neg values to NA 
  mutate(across(where(is.numeric), ~ ifelse(.x < 0, NA, .x))) |>
  # Total number of enrollment in calculus
  mutate(num_enr_calculus = TOT_MATHENR_CALC_M + TOT_MATHENR_CALC_F,
         # Percentage of students enrolled in calculus
         pct_enr_calculus = num_enr_calculus / total_enrollment * 100) |>
  # Select columns of interest
  select(COMBOKEY, SCH_MATHCLASSES_CALC, pct_enr_calculus) |>
  # Renaming column names for clarity 
  rename(num_classes_calculus = SCH_MATHCLASSES_CALC) 
```


``` r
# Geometry
geometry <- crdc_data_list[["Geometry.csv"]] |>
  # Filtering the dataset by high school 
  right_join(enrollment, by = "COMBOKEY") |>
  # Changing neg values to NA 
  mutate(across(where(is.numeric), ~ ifelse(.x < 0, NA, .x))) |>
  # Total number of enrollment in geometry
  mutate(num_enr_geometry = TOT_MATHENR_GEOM_M + TOT_MATHENR_GEOM_F,
         # Percentage of students enrolled in geometry
         pct_enr_geometry = num_enr_geometry / total_enrollment * 100) |>
  # Select columns of interest
  select(COMBOKEY, SCH_MATHCLASSES_GEOM, pct_enr_geometry) |>
  # Renaming column names for clarity 
  rename(num_classes_geometry = SCH_MATHCLASSES_GEOM) 
```

#### Science-Related Courses


``` r
# Biology
biology <- crdc_data_list[["Biology.csv"]] |>
  # Filtering the dataset by high school 
  right_join(enrollment, by = "COMBOKEY") |>
  # Changing neg values to NA 
  mutate(across(where(is.numeric), ~ ifelse(.x < 0, NA, .x))) |>
  # Total number of enrollment in biology
  mutate(num_enr_biology = TOT_SCIENR_BIOL_M + TOT_SCIENR_BIOL_F,
         # Percentage of students enrolled in biology
         pct_enr_biology = num_enr_biology / total_enrollment * 100) |>
  # Select columns of interest
  select(COMBOKEY, SCH_SCICLASSES_BIOL, pct_enr_biology) |>
  # Renaming column names for clarity 
  rename(num_classes_biology = SCH_SCICLASSES_BIOL) 
```


``` r
# Chemistry
chemistry <- crdc_data_list[["Chemistry.csv"]] |>
  # Filtering the dataset by high school 
  right_join(enrollment, by = "COMBOKEY") |>
  # Changing neg values to NA 
  mutate(across(where(is.numeric), ~ ifelse(.x < 0, NA, .x))) |>
  # Total number of enrollment in chemistry
  mutate(num_enr_chemistry = TOT_SCIENR_CHEM_M + TOT_SCIENR_CHEM_F,
         # Percentage of students enrolled in calculus
         pct_enr_chemistry = num_enr_chemistry / total_enrollment * 100) |>
  # Select columns of interest
  select(COMBOKEY, SCH_SCICLASSES_CHEM, pct_enr_chemistry) |>
  # Renaming column names for clarity 
  rename(num_classes_chemistry = SCH_SCICLASSES_CHEM) 
```


``` r
# Physics 
physics <- crdc_data_list[["Physics.csv"]] |>
  # Filtering the dataset by high school 
  right_join(enrollment, by = "COMBOKEY") |>
  # Changing neg values to NA 
  mutate(across(where(is.numeric), ~ ifelse(.x < 0, NA, .x))) |>
  # Total number of enrollment in physics
  mutate(num_enr_physics = TOT_SCIENR_PHYS_M + TOT_SCIENR_PHYS_F,
         # Percentage of students enrolled in physics
         pct_enr_physics = num_enr_physics / total_enrollment * 100) |>
  # Select columns of interest
  select(COMBOKEY, SCH_SCICLASSES_PHYS, pct_enr_physics) |>
  # Renaming column names for clarity 
  rename(num_classes_physics = SCH_SCICLASSES_PHYS) 
```

#### **Computer-Related Courses**


``` r
# Computer Science 
computer_science <- crdc_data_list[["Computer Science.csv"]] |>
  # Filtering the dataset by high school 
  right_join(enrollment, by = "COMBOKEY") |>
  # Changing neg values to NA 
  mutate(across(where(is.numeric), ~ ifelse(.x < 0, NA, .x))) |>
  # Total number of enrollment in cs
  mutate(num_enr_cs = TOT_COMPENR_CSCI_M + TOT_COMPENR_CSCI_F,
         # Percentage of students enrolled in cs
         pct_enr_cs = num_enr_cs / total_enrollment * 100) |>
  # Select columns of interest
  select(COMBOKEY, SCH_COMPCLASSES_CSCI, pct_enr_cs) |>
  # Renaming column names for clarity 
  rename(num_classes_cs = SCH_COMPCLASSES_CSCI) 
```

### College-Level Courses

#### **Advanced Placement** (AP)


``` r
# Advanced Placement
ap_courses <- crdc_data_list[["Advanced Placement.csv"]] |>
  # Filtering the dataset by high school 
  right_join(enrollment, by = "COMBOKEY") |>
  # Changing neg values to NA 
  mutate(across(where(is.numeric), ~ ifelse(.x < 0, NA, .x))) |>
  # Percentage of students enrolled in AP courses (any, math, science, computer science)
  mutate(pct_enr_ap = (TOT_APENR_M + TOT_APENR_F) / total_enrollment * 100,
         pct_enr_ap_math = (TOT_APMATHENR_M + TOT_APMATHENR_F) / total_enrollment * 100,
         pct_enr_ap_science = (TOT_APSCIENR_M + TOT_APSCIENR_F) / total_enrollment * 100,
         pct_enr_ap_cs = (TOT_APCOMPENR_M + TOT_APCOMPENR_F) / total_enrollment * 100) |>
  # Select columns of interest
  select(COMBOKEY,
         pct_enr_ap, pct_enr_ap_math, pct_enr_ap_science, pct_enr_ap_cs)
```

#### **International Baccalaureate (IB)**


``` r
# International Baccalaureate
ib_programme <- crdc_data_list[["International Baccalaureate.csv"]] |>
  # Filtering the dataset by high school 
  right_join(enrollment, by = "COMBOKEY") |>
  # Changing neg values to NA 
  mutate(across(where(is.numeric), ~ ifelse(.x < 0, NA, .x))) |>
  # Percentage of students in IB programme
  mutate(pct_enr_ib = (TOT_IBENR_M + TOT_IBENR_F) / total_enrollment * 100) |>
  # Select columns of interest
  select(COMBOKEY, pct_enr_ib)
```

#### **Dual Enrollment**


``` r
dual_enrollment <- crdc_data_list[["Dual Enrollment.csv"]] |>
  # Filtering the dataset by high school 
  right_join(enrollment, by = "COMBOKEY") |>
  # Changing neg values to NA 
  mutate(across(where(is.numeric), ~ ifelse(.x < 0, NA, .x))) |>
  # Percentage of students in dual enrollment program
  mutate(pct_enr_dual = (TOT_DUAL_M + TOT_DUAL_F) / total_enrollment * 100) |>
  # Select columns of interest
  select(COMBOKEY, pct_enr_dual)
```

### **Test Participation and Performance for College Readiness**

Before deriving the features, we need to define a function that safely calculate the mean statistics of cohort AY 2021-2022 and AY 2022-2023, that is ignore the null value and replace `NaN` with `NA`.


``` r
# Define a custom mean function that returns NA if result is NaN
safe_mean <- function(x) {
  result <- mean(x, na.rm = TRUE)
  if (is.nan(result)) NA else result
}
```

#### Scholastic Aptitude Test (SAT)


``` r
# SAT participation and performance 
sat_participation_performance <- ncdpi_data_list[["rcd_sat.xlsx"]] |>
  # Corhot matching 
  filter(year %in% c(2021, 2022, 2023)) |>
  # Adding school name to the dataset 
  inner_join(nc_school_location, by = "agency_code") |>
  # Mean participation rate and mean sat score of the two cohorts 
  group_by(agency_code, name) |>
  summarize(sat_participation = safe_mean(pct_sat_participation),
            sat_performance = safe_mean(avg_sat_score),
            .groups = "drop")
```

#### Advanced Placement (AP)


``` r
# AP participation and performance 
ap_participation_performance <- ncdpi_data_list[["rcd_ap.xlsx"]] |>
  # Corhot matching 
  filter(year %in% c(2021, 2022, 2023)) |>
  # Adding school name to the dataset 
  inner_join(nc_school_location, by = "agency_code") |>
  # Mean participation rate and mean passing rate of the two cohorts 
  group_by(agency_code, name) |>
  summarize(ap_participation = safe_mean(pct_ap_participation),
            ap_performance = safe_mean(pct_ap_pass),
            .groups = "drop")
```

#### International Baccalaureate (IB)


``` r
# IB participation and performance 
ib_participation_performance <- ncdpi_data_list[["rcd_ib.xlsx"]] |>
  # Corhot matching 
  filter(year %in% c(2021, 2022, 2023)) |>
  # Adding school name to the dataset 
  inner_join(nc_school_location, by = "agency_code") |>
  # Mean participation rate and mean passing rate of the two cohorts 
  group_by(agency_code, name) |>
  summarize(ib_participation = safe_mean(pct_ib_participation),
            ib_performance = safe_mean(pct_ib_pass),
            .groups = "drop")
```

### Educational Attainment

#### **Cohort Graduation Rate**


``` r
# Four-year graduation rate
cgr_4yr <- ncdpi_data_list[["rcd_acc_cgr.xlsx"]] |>
  # Taking a subgroup 
  filter(year %in% c(2021, 2022, 2023),
         subgroup == "ALL",
         cgr_type == "STD") |>
  # Adding school name to the dataset 
  inner_join(nc_school_location, by = "agency_code") |>
  # Mean 4-yr graduation rate of the two cohorts 
  group_by(agency_code, name) |>
  summarize(graduation_rate_4yr = safe_mean(pct),
            .groups = "drop")
```


``` r
# Five-year graduation rate
cgr_5yr <- ncdpi_data_list[["rcd_acc_cgr.xlsx"]] |>
  # Taking a subgroup 
  filter(year %in% c(2021, 2022, 2023),
         subgroup == "ALL",
         cgr_type == "EXT") |>
  # Adding school name to the dataset 
  inner_join(nc_school_location, by = "agency_code") |>
  # Mean 5-yr graduation rate of the two cohorts 
  group_by(agency_code, name) |>
  summarize(graduation_rate_5yr = safe_mean(pct), 
            .groups = "drop") 
```


``` r
# Combine the two cgr, which I should achive using pivot_wider 
corhot_graduation_rate <- merge(cgr_4yr, cgr_5yr,
                                by = c("agency_code", "name"), all = TRUE)
```

#### College Enrollment Rate


``` r
college_enrollment <-  ncdpi_data_list[["rcd_college.xlsx"]] |>
  # Taking a subgroup 
  filter(year %in% c(2021, 2022, 2023),
         subgroup == "All",
         Status == "ENROLL") |>
  # Adding school name to the dataset 
  inner_join(nc_school_location, by = "agency_code") |>
  # Mean college enrollment rate of the two cohorts 
  group_by(agency_code, name) |>
  summarize(pct_college_enrollment = safe_mean(pct_enrolled) * 100,
            .groups = "drop")
```

### Economically Disadvantaged Students

At the same time, NC DPI also have data about percentage of economically disadvantaged students, which can be utilized as an annotation variable.


``` r
eds <- ncdpi_data_list[["rcd_acc_eds.xlsx"]] |> 
  filter(year %in% c(2021, 2022, 2023)) |>
  # Adding school name to the dataset 
  inner_join(nc_school_location, by = "agency_code") |>
  group_by(agency_code, name) |>
  summarize(pct_eds_students = safe_mean(pct_eds),
            .groups = "drop") 
```

## 2.4 Dataset for HCA Heatmap

### Dataset for CRDC Features


``` r
# All CRDC datasets with engineered features 
## ib_programme has two many missing values 
crdc_datasets <- list(adv_math, algebra2, calculus, biology, chemistry, geometry, computer_science, physics, ap_courses, dual_enrollment)

# Combining these datasets 
crdc_features <- reduce(crdc_datasets, function(x, y) left_join(x, y, by = "COMBOKEY"), .init = high_schools)
```

### Dataset for NCDPI Features


``` r
# All NCDPI datasets with engineered features 
## 
ncdpi_datasets <- list(sat_participation_performance, ap_participation_performance, corhot_graduation_rate, college_enrollment, eds)

# Combining these datasets 
ncdpi_features <- reduce(ncdpi_datasets, function(x, y) full_join(x, y, by = c("agency_code", "name")))
```

### Merging CRDC and NCDPI Datasets

Merging CRDC and NCDPI data requires some efforts, since school name is the only shared key. But some schools may have the same name but are from different county, we need to deal with those schools with duplicated names separately.


``` r
# Identifying schools with the same school name in CRDC features
crdc_same_name <- crdc_features |>
  group_by(SCH_NAME) |>
  filter(n() > 1) |>
  ungroup() 

# We need to deal with those schools one by one 
crdc <- crdc_features |>
    mutate(SCH_NAME = case_when(
    COMBOKEY == "370011202551" ~ "Union Academy Charter School",
    COMBOKEY == "370033002224" ~ "Northside High (Beaufort)",
    COMBOKEY == "370345002598" ~ "Northside High (Onslow)",
    COMBOKEY == "370053002975" ~ "Performance Learning Center (Cabarrus)",
    COMBOKEY == "370297002842" ~ "Performance Learning Center (Charlotte-Mecklenburg)",
    COMBOKEY == "370090002708" ~ "Turning Point Academy (Cleveland)",
    COMBOKEY == "370297000871" ~ "Turning Point Academy (Charlotte-Mecklenburg))",
    COMBOKEY == "370126002292" ~ "Riverside High (Durham))",
    COMBOKEY == "370288001169" ~ "Riverside High (Martin))",
    TRUE ~ SCH_NAME
  )) |>
  mutate(SCH_NAME = str_trim(str_to_title(as.character(SCH_NAME)))) 
```


``` r
# Identifying schools with the same school name in NCDPI features
ncdpi_same_name <- ncdpi_features |>
  group_by(name) |>
  filter(n() > 1) |>
  ungroup() 

# We need to deal with those schools one by one
ncdpi <- ncdpi_features |>
  mutate(name = case_when(
    agency_code == "070330" ~ "Northside High (Beaufort)",
    agency_code == "670333" ~ "Northside High (Onslow)",
    agency_code == "320365" ~ "Riverside High (Durham)",
    agency_code == "580368" ~ "Riverside High (Martin)",
    TRUE ~ name
  )) |>
  mutate(name = str_trim(str_to_title(as.character(name)))) |>
  filter(if_all(where(is.numeric), ~ !is.na(.)))
```


``` r
# Merging datasets
merged_dataset <- inner_join(crdc, ncdpi, by = c("SCH_NAME" = "name")) |>
  # Only keeping the features for heatmap 
  select(-COMBOKEY, -LEA_NAME, -agency_code) |>
  # Making school name as row names 
  column_to_rownames(var = "SCH_NAME") |>
  # Styling column names 
  clean_names() |>
  relocate(pct_eds_students, .before = 1)
```

### Splitting and Scaling Variables


``` r
# Extracting the categorical variables 
categorical_variables <- merged_dataset[, 2:5]

# Extracting all continuous variables 
continuous_variables <- merged_dataset[, 6:33]
# Reordring the continous variables 
continuous_variables <- continuous_variables |>
   relocate(names(continuous_variables)[c(3, 5, 7, 9, 11, 13, 15)], .after = 1)

# Scaling continuous variables
scaled_continuous <- scale(continuous_variables)
# EDS variable
economically_disadvantaged <- merged_dataset[, 1] 
```

In order to better annotate variables related to advanced courses vs. post-secondary outcomes, two additional data frame has been made.


``` r
# For access variables 
access_variables <- scaled_continuous[, 1:21]

# For outcome variables 
outcome_variables <- scaled_continuous[, 22:28]
```

# 3. HCA Heatmap


``` r
# Defining a function for uncertered correlation using cosangle 
function_uncertered_correlation <- function(matrix) {
  as.dist(as.matrix(distancematrix(matrix, d = "cosangle")))
}
```


``` r
# Customizing distance metric and clustering metric for both rows and columns
r_cluster <- hclust(function_uncertered_correlation(scaled_continuous), method = "ave")
# Note that we need to transpose the matrix to cluster the columns 
c_cluster <- hclust(function_uncertered_correlation(t(scaled_continuous)), method = "ave")
```


``` r
# Creating a temporary heatmap to get the clustered row order
temp_heatmap <- Heatmap(
  scaled_continuous,
  name = "access vs outcome",
  cluster_rows = r_cluster,
  cluster_columns = FALSE,
  show_row_names = FALSE)

# Creating row order 
ht_temp <- draw(temp_heatmap)
```

![](ORLA6541_Midterm_HCAHeatmap_YueSummerWu_files/figure-html/unnamed-chunk-40-1.png)<!-- -->

``` r
custom_row_order <- row_order(ht_temp)

custom_column_order <- column_order(ht_temp)
custom_column_order <- custom_column_order[1:(length(custom_column_order) - 7)]
```


``` r
column_split <- factor(
  c(rep("Course\nAvailability", 8),
    rep("Course\nEnrollment", 8),
    rep("College\nCourse Enrollment", 5)),
  levels = c("Course\nAvailability", "Course\nEnrollment", "College\nCourse\nEnrollment") # Define order explicitly
)

label_colors <- c(
  "Course\nAvailability" = NA,
  "Course\nEnrollment" = NA,
  "College\nCourse Enrollment" = NA
)

column_annotation <- HeatmapAnnotation(
  Category = anno_block(
    gp = gpar(fill = label_colors[c("Course\nAvailability", "Course\nEnrollment", "College\nCourse\nEnrollment")]),  
    labels = c("Course\nAvailability", "Course\nEnrollment", "College\nCourse\nEnrollment"),                     
    labels_gp = gpar(fontsize = 8, col = "#1E88E5")  # Text color
  )
)

access_heatmap <- Heatmap(
  access_variables,
  name = "Advanced Coursework",
  cluster_rows = r_cluster, 
  cluster_columns = FALSE,
  row_order = custom_row_order,
  show_row_names = FALSE,
  row_split = 4,
  column_title = "Advanced\nCoursework",
  column_title_gp = gpar(fontsize = 10, fontface = "bold", col = "#1E88E5", just = "center"),
  column_split = column_split,
  bottom_annotation = column_annotation
)

access_heatmap
```

![](ORLA6541_Midterm_HCAHeatmap_YueSummerWu_files/figure-html/unnamed-chunk-41-1.png)<!-- -->


``` r
outcome_heatmap <- Heatmap(outcome_variables,
                          name = "Educational Attainment",
                          cluster_rows = FALSE, 
                          cluster_columns = FALSE,
                          row_order = custom_row_order,
                          show_row_names = FALSE,
                          column_title = "Educational\nAttainment",
                          column_title_gp = gpar(fontsize = 10, 
                                                 fontface = "bold", 
                                                 col = "#D81B60",
                                                 just = "center"))
```


``` r
main_heatmap <- access_heatmap + outcome_heatmap
```


``` r
categorical_color = c("No" = "white", "Yes" = "black")

create_annotations <- function(data, name) {
  return(
    Heatmap(data, 
            name = name,
            col = categorical_color,
            heatmap_legend_param = list(at = c("No", "Yes"), labels = c("No", "Yes")),
            width = unit(0.297, "cm"))
  )
}

# Apply the function to each variable and combine the heatmaps
annotations <- 
  create_annotations(categorical_variables[, 2], "Magnet School") +
  create_annotations(categorical_variables[, 3], "Charter School") 

combined_plot <- main_heatmap + annotations
```


``` r
annotation_eds <- Heatmap(economically_disadvantaged,
                          name = "% Eco Disadv Students",
                          cluster_rows = FALSE)

final_plot <- combined_plot + annotation_eds
```


``` r
heatmap_grob <- grid.grabExpr(draw(final_plot, 
                                   heatmap_legend_side = "right",
                                   annotation_legend_side = "right", 
                                   merge_legend = TRUE))

# Convert the grid object to a plot-friendly format
ggplot_heatmap <- ggplot() +
  annotation_custom(heatmap_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  theme_void()  # Remove ggplot background for a clean look

# Save the heatmap using ggsave
ggsave("/Users/summerwu/Desktop/final_heatmap_midterm.png", 
       plot = ggplot_heatmap, 
       width = 7,    # Width in inches, suitable for Word
       height = 9,   # Height in inches, suitable for Word
       dpi = 300)    # High resolution for clarity

ggplot_heatmap
```

![](ORLA6541_Midterm_HCAHeatmap_YueSummerWu_files/figure-html/unnamed-chunk-46-1.png)<!-- -->

# 4. Exploratory Data Analysis

## Descriptive Statistics


``` r
summary_df <- data.frame(
  Percent_NA = round(sapply(merged_dataset, function(x) mean(is.na(x)) * 100), 2),
  Min = sapply(merged_dataset, function(x) if(is.numeric(x)) round(min(x, na.rm = TRUE), 2) else NA),
  Max = sapply(merged_dataset, function(x) if(is.numeric(x)) round(max(x, na.rm = TRUE), 2) else NA),
  Mean = sapply(merged_dataset, function(x) if(is.numeric(x)) round(mean(x, na.rm = TRUE), 2) else NA),
  SD = sapply(merged_dataset, function(x) if(is.numeric(x)) round(sd(x, na.rm = TRUE), 2) else NA)
)

summary_df <- summary_df[-c(2, 5), ]

knitr::kable(summary_df, format = "markdown")
```



|                       | Percent_NA|    Min|     Max|    Mean|    SD|
|:----------------------|----------:|------:|-------:|-------:|-----:|
|pct_eds_students       |       0.00|   5.00|   67.10|   35.96| 14.04|
|magnet_school          |       0.00|     NA|      NA|      NA|    NA|
|charter_school         |       0.00|     NA|      NA|      NA|    NA|
|num_classes_adv_math   |       0.42|   0.00|   87.00|    7.65|  9.39|
|pct_enr_adv_math       |       3.35|   0.46|   30.97|    8.47|  5.68|
|num_classes_algebra2   |       0.42|   0.00|   68.00|   15.73| 11.09|
|pct_enr_algebra2       |       1.26|   3.01|   33.06|   20.36|  5.73|
|num_classes_calculus   |       0.42|   0.00|   24.00|    2.72|  3.51|
|pct_enr_calculus       |      20.50|   0.10|   17.28|    2.31|  2.18|
|num_classes_biology    |       0.00|   0.00|   98.00|   20.10| 15.37|
|pct_enr_biology        |       1.67|   3.34|   38.36|   23.60|  7.05|
|num_classes_chemistry  |       0.00|   0.00|   63.00|    9.73|  9.65|
|pct_enr_chemistry      |       1.26|   1.53|   33.51|   11.67|  5.87|
|num_classes_geometry   |       0.42|   0.00|   73.00|   16.82| 12.12|
|pct_enr_geometry       |       0.84|   4.65|  100.00|   22.80|  8.08|
|num_classes_cs         |       0.00|   0.00|   47.00|    4.82|  6.95|
|pct_enr_cs             |      26.36|   0.05|   42.58|    4.77|  5.37|
|num_classes_physics    |       0.00|   0.00|   31.00|    2.28|  3.69|
|pct_enr_physics        |      35.15|   0.05|   18.75|    2.65|  3.52|
|pct_enr_ap             |       2.09|   0.18|   56.87|   15.26| 10.85|
|pct_enr_ap_math        |      15.06|   0.13|   21.87|    3.67|  3.39|
|pct_enr_ap_science     |      23.01|   0.09|   35.80|    2.84|  3.83|
|pct_enr_ap_cs          |      49.79|   0.05|   19.75|    2.02|  3.02|
|pct_enr_dual           |      19.67|   0.07|   99.51|   10.69| 11.56|
|sat_participation      |       0.00|   0.05|    0.96|    0.23|  0.16|
|sat_performance        |       0.00| 838.50| 1328.67| 1095.63| 76.56|
|ap_participation       |       0.00|   0.01|    0.57|    0.16|  0.11|
|ap_performance         |       0.00|   0.05|    0.86|    0.46|  0.18|
|graduation_rate_4yr    |       0.00|  54.27|   95.00|   87.27|  6.26|
|graduation_rate_5yr    |       0.00|  56.67|   95.00|   88.57|  5.63|
|pct_college_enrollment |       0.00|  25.44|   93.91|   62.66| 13.34|

## Visualization


``` r
vis_data <- cbind(continuous_variables, charter_school = categorical_variables$charter_school, economically_disadvantaged)

vis_data_filtered <- vis_data |>
  select(charter_school, economically_disadvantaged, pct_enr_ap, pct_college_enrollment)|>
  rownames_to_column(var = "school_name")

clusters <- cutree(r_cluster, k = 4) 
vis_data_filtered$clusters <- as.factor(clusters) 
table(vis_data_filtered$clusters)
```

```
## 
##   1   2   3   4 
## 125  31  47  36
```

``` r
mean_pct_college_enrollment <- safe_mean(vis_data_filtered$pct_college_enrollment)
mean_pct_enr_ap <- safe_mean(vis_data_filtered$pct_enr_ap)

vis_data_cleaned <- vis_data_filtered|>
  mutate(clusters = case_when(
    clusters == 1 ~ "Cluster 4",
    clusters == 2 ~ "Cluster 1",
    clusters == 3 ~ "Cluster 2",
    clusters == 4 ~ "Cluster 3",
    TRUE ~ NA
  )) |>
  mutate(annotation = ifelse(economically_disadvantaged >= 40 & clusters %in% c("Cluster 1", "Cluster 3"), school_name, NA))
```


``` r
plot <- ggplot(vis_data_cleaned, aes(x = pct_enr_ap, 
                             y = pct_college_enrollment, 
                              color = clusters,
                              size = economically_disadvantaged)) +
  geom_vline(xintercept = mean_pct_enr_ap, color = "#FFC107", size = 1) +
  geom_hline(yintercept = mean_pct_college_enrollment, color = "#FFC107", size = 1) +
  
  geom_point(alpha=0.5) +
  scale_color_manual(labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"),
                     values = c("#D81B60", "gray", "#1E88E5", "gray")) +
  geom_text_repel(
    aes(label = annotation), # Annotate labels
    na.rm = TRUE, # Automatically removes NA labels
    size = 3,
    arrow = arrow(length = unit(0.03, "npc"), type = "closed", angle = 30),
    box.padding = 0.5, # Space between label and arrow
    point.padding = 0.3
  ) +
  theme_test()  +
  labs(x = "% AP Enrollment (At Least One AP Course)",
       y = "% College Enrollment",
       size = "% Economically Disadvantaged",
       title = "Equal Access, Unequal Outcome",
       subtitle = "Economically disadvantaged schools have lower college enrollment rates, even with similar\nAP course enrollment levels",
       caption = "NC DPI: schools with at least 40% ecomonicaly disadvantaged students are eligible for Community Eligibility Provision") +
  theme(legend.position = "top") +
  guides(color = "none") +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  geom_richtext(aes(x = 85, y = 85, 
                    label = "<span style='color:#1E88E5;'>Cluster 3</span> vs <span style='color:#D81B60;'>Cluster 1</span>"),
                size = 5, 
                fill = NA, 
                label.color = NA) +
  annotate("text", x = 30, y = 15, label = "Average AP Enrollment", size = 3, color = "#FFC107") +
  annotate("text", x = 90, y = 60, label = "Average College Enrollment", size = 3, color = "#FFC107") +
  annotate("text", x = 5, y = 100, label = "Low Access, High Outome", size = 2, color = "black") +
  annotate("text", x = 5, y = 0, label = "Low Access, Low Outome", size = 2, color = "black") +
  annotate("text", x = 90, y = 100, label = "High Access, High Outome", size = 2, color = "black") +
  annotate("text", x = 90, y = 0, label = "High Access, Low Outome", size = 2, color = "black") 
```

```
## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
## ℹ Please use `linewidth` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

``` r
plot
```

```
## Warning: Removed 5 rows containing missing values or values outside the scale range
## (`geom_point()`).
```

![](ORLA6541_Midterm_HCAHeatmap_YueSummerWu_files/figure-html/unnamed-chunk-49-1.png)<!-- -->

``` r
ggsave(plot = plot, "EDA_midterm.png")
```

```
## Saving 7 x 5.5 in image
```

```
## Warning: Removed 5 rows containing missing values or values outside the scale range
## (`geom_point()`).
```

# 5. R Pacakages


``` r
pkgs <- cite_packages(output = "table",pkgs = "Session", out.dir = ".")
knitr::kable(pkgs)
```



|Package        |Version |Citation     |
|:--------------|:-------|:------------|
|base           |4.3.2   |@base        |
|Biobase        |2.62.0  |@Biobase     |
|BiocGenerics   |0.48.1  |@BiocGen.... |
|circlize       |0.4.16  |@circlize    |
|cluster        |2.1.6   |@cluster     |
|ComplexHeatmap |2.21.1  |@Complex.... |
|ggrepel        |0.9.6   |@ggrepel     |
|ggtext         |0.1.2   |@ggtext      |
|hopach         |2.62.0  |@hopach      |
|janitor        |2.2.0   |@janitor     |
|tidyverse      |2.0.0   |@tidyverse   |
|zip            |2.3.1   |@zip         |
