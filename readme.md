# `burro`: A Shiny Data Explorer for Teaching Exploratory Data Analysis

![burro-hex](img/burro3.png)

## Why `burro`(w) into your data?

Exploratory Data Analysis (EDA) is highly visual and can be a motivating entry point into data science and analysis. `burro` attempts to make EDA accessible to a larger audience by exposing datasets as a simple Shiny App that can be shared via `shinyapps.io` or other Shiny hosts. You can see an example here: https://tladeras.shinyapps.io/nhanes_explore/

We use `burro` as an introductory tool for EDA by using it in "data scavenger hunts", where groups of students are given specific questions to answer about the data, and then have to show their fellow students the answer and how the discovered it. Looking at the data together is vital to building understanding of the data together. 

By concentrating on the data visualization first, `burro` apps let us have conversations about the data, and hopefully motivate students to learn more tools of EDA such as `ggplot`, `visdat`, and `skimr`.

## Installing `burro`

```{r}
install.packages("devtools")
devtools::install_github("laderast/burro")
```

## Dataset requirements

`burro` expects a dataset as a `data.frame` or `data.table`. The dataset should have at least 2 numeric variables and two categorical variables. 

`burro` requires an *outcome variable*, which should be categorical/factor.
It's on my list of things to do to make `burro` adaptive to the data passed into it, but it currently is pretty inflexible about these two things. 

An optional (though helpful) requirement is to have a data dictionary which has a column called `variableNames` that defines each variable in the dataset.

## Running `burro` on `NHANES` Data

Here we make a `burro` app using the `explore_data` option for the NHANES (National Health and Nutrition Examination Survey) data. We specify our covariates, and our outcome of interest (`Depressed`, the number of depressive episodes). 

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

## Running `burro` on `biopics` data from `fivethirtyeight`

We can examine the `biopics` dataset using `burro`. We specify our `outcome_var` to be `subject_sex`, so we can examine everything through the facet of gender.

```{r}
library(burro)
library(fivethirtyeight)
data(biopics)
explore_data(biopics, outcome_var = "subject_sex")

```

## Run `burro` on `diamonds`

```{r}
library(ggplot)
data(diamonds)
burro::explore_data(diamonds, outcome_var="Cut")
```

## Acknowledgements

`burro` uses many wonderful packages developed by Nicholas Tierney, rOpenSci, and others: `visdat`, `naniar`, and `skimr`, among others. 

`burro` was partially developed with funding from Big Data to Knowledge (BD2K) and a National Library of Medicine T15 Training Grant supplement for the development of data science curricula.

The `burro` hex sticker uses clipart [designed by Freepik](https://www.freepik.com/free-vector/donkey-head_763369.htm).

## Related Packages

The `esquisse` package (https://github.com/dreamRs/esquisse) is a wonderful start to making a drag and drop visualization tool in R. Check it out!

## License

`burro` is released under an Apache 2.0 license.
