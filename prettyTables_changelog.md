## 1.0 	First version
## 1.01 	
* New function: table_two_by_two; Summarise one variable by another with the option of a stratification variable.
* New function: roundWzero; An extension of the sigif function. This converts results to strings to preserve right trailing zeros after the decimal point. This has replaced signif in all the type methods used by table_values and table_two_by_two, This also sets missing numerical results to -.
* New function: prettifyPValue; This applies roundWZero and replaces really small values with an inequality sign. There is also an option to star significant values.
* Replaced the round function with a significant figures utility function. This preserve right trailing zeros by returning a string with the correct number of zeros.
*  Bugfix: Added code the the application to prevent it from crashing when the strata variable has missing values in it. This does not save to the log file because these really need to be correct before you start creating tables.
* Bugfix: Added functionality to the strata names input. This wasn't there in the original version for some reason.
* Bugfix: Instead of crashing when a string asks for numeric results the type methods now issues a warning instead and skip that variable.
* Issues: Aware of an issue with read.csv and read.stata13 when these files contain non-ascii characters. R can only handle standard ascii characters. As a result it fails to read the csv files completely and the stata (.dta) files are read without the labels. This problem is beyond the scope of this package and may be beyond the scope of R in general. As a work around ensure that all of the input file are ascii characters to begin with.