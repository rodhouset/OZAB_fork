# OZAB Data Specifications

Each of the models contained within this package assume that the data are presented in long format with each observational unit represented by a different row of the dataset. In other words, each row should represent the smallest unit of observation observed (a single species, a single location, within a single timepoint). All covariates associated with that observation should be included as part of the row. In an example below, we transform a wide table containing three species across 2 years of observation with associated covariates into a long format table capable of being used by OZAB package modeling functions.

| Year | Location | SpeciesA | SpeciesB | Species C | Topography | Fire |
|------|----------|----------|----------|-----------|------------|------|
| 2011 |    1     |     3    |    2     |    5      |     0.25   | No   |
| 2011 |    2     |     2    |    2     |    4      |     0.27   | Yes  |
| 2012 |    1     |     2    |    1     |    4      |     0.25   | Yes  |
| 2012 |    2     |     1    |    1     |    3      |     0.27   | Yes  |

In this example table, we have three species in wide format observed at multiple locations across multiple years. In order to use OZAB Package functionality, we must transform this table to have the following format:

| Year | Location |  Topography | Fire | Species | Cover Class |
|------|----------|-------------|------|---------|-------------|
| 2011 |    1     |   0.25      |  No  |    A    |      3      |
| 2011 |    1     |   0.25      |  No  |    B    |      2      |
| 2011 |    1     |   0.25      |  No  |    C    |      5      |
| 2011 |    2     |   0.27      |  Yes |    A    |      2      |
| 2011 |    2     |   0.27      |  Yes |    B    |      2      |
| 2011 |    2     |   0.27      |  Yes |    C    |      4      |
| 2012 |    1     |   0.25      |  Yes |    A    |      2      |
| 2012 |    1     |   0.25      |  Yes |    B    |      1      |
| 2012 |    1     |   0.25      |  Yes |    C    |      4      |
| 2012 |    2     |   0.27      |  Yes |    A    |      1      |
| 2012 |    2     |   0.27      |  Yes |    B    |      1      |
| 2012 |    2     |   0.27      |  Yes |    C    |      3      |

# Other Important Notes

* When preparing data, both Cover Class and Presence should be present as columns within the dataframe. Assuming that a "0" cover class measurement, the `add_presence` function within the OZAB package will detect and, using this "0" cover class as a default, append a logical column indicating presence or absence named `Presence` to the dataframe. 

* The `Cover Class` column must be a `factor`.

* While other column names can be used and functionality is provided for this, we encourage use of the package notation of `Cover Class` and `Presence` throughout. Especially if you find it necessary to post an issue to our Github page. 

# Example Code

Below, I have implemented the example wide-format table above manually as a dataframe. What follows is a set of example code to show how to appropriately transform the dataset into an OZAB-ready format.

```{r}
library(tidyverse)
library(OZAB)

wide_format <- tibble(
  Year = c(2011, 2011, 2012, 2012),
  Location = c(1, 2, 1, 2),
  SpeciesA = c(3, 2, 2, 1),
  SpeciesB = c(2, 2, 1, 1),
  SpeciesC = c(5, 4, 4, 3),
  Topography = c(0.25, 0.27, 0.25, 0.27),
  Fire = c('No', 'Yes', 'Yes', 'Yes')
)

long_format <-
  wide_format %>%
  pivot_longer(c(SpeciesA, SpeciesB, SpeciesC), names_to = 'Species', values_to = 'Cover Class') %>%
  mutate(`Cover Class` = as_factor(`Cover Class`)) %>%
  add_presence()

## Abbreviated version using the `pivot_nps_function`
wide_format %>%
  pivot_nps_data(c(Year, Location, Topography, Fire)) %>%
  add_presence()
```
