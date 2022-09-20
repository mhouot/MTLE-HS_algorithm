# MTLE-HS_algorithm
Algorithm from the paper *"Early identification of seizure freedom with medical treatment in patients with mesial temporal lobe epilepsy and hippocampal sclerosis (MTLE HS)"*


## Installation

First, you need to install R on your computer from https://cran.r-project.org      
Then, you need to install the **shiny** package in R:

```R
if (!require('shiny')) install.packages("shiny")
```

## Getting Started

To run the application, you have to load the library and use the function `runGitHub()`:
```R
library(shiny)
shiny::runGitHub("MTLE-HS_algorithm", "mhouot")
```

Choose between the two tabs:    

 1. *Select one patient's data*: to obtain the probability of being medically seizure-free by entering patient data one by one
 2. *Upload a file of the data patients*: to obtain the probability of being medically seizure-free of several patient at a time. See the data_test.xlsx file in the `datas` folder for example.


## License

The shiny package as a whole is licensed under the GPLv3. See the [LICENSE](LICENSE) file for more details.
