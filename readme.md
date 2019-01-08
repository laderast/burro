# `burro`: A Shiny Data Explorer for Teaching Exploratory Data Analysis

## Why `burro`?

Exploratory Data Analysis (EDA) is highly visual and can be a motivating entry point into data science and the analysis of the data. `burro` attempts to make EDA accessible to a larger audience by exposing datasets as a simple Shiny App that can be shared via `shinyapps.io` or other Shiny hosts. 

We use `burro` as an introductory tool for EDA by using it in "data scavenger hunts", where groups of students are given specific questions to answer about the data, and then have to show their fellow students the answer and how the discovered it.

By concentrating on the data visualzation rather than the coding, `burro` lets us have conversations about the data first, and hopefully motivate the students to learn more tools of EDA such as `ggplot`, `visdat`, and `skimr`.

## Installing `burro`

```{r}
devtools::install_github("laderast/burro")
```

## Running `burro` on `NHANES` Data



You can see the `burro` app for the `NHANES` data here: https://tladeras.shinyapps.io/nhanes_explore/

```
library(burro)
data(NHANES)

##specify outcome variable here
outcome_var <- c("Depressed")
## specify covariates here (including outcome variable)
covariates <- c("Gender", "Age", "SurveyYr", "Race1", "Race3" ,"MaritalStatus",
                "BMI", "HHIncome", "Education",
                "BMI_WHO", "BPSysAve", "TotChol", "Depressed", "LittleInterest",
                "SleepHrsNight", "SleepTrouble", "TVHrsDay", "AlcoholDay",
                "Marijuana", "RegularMarij", "HardDrugs")
                
explore_data(NHANES, covariates, outcome_var)
```
## Acknowledgements

`burro` uses many wonderful packages developed by Nicholas Tierney, rOpenSci, and others: `visdat`, `naniar`, and `skimr`, among others. 
