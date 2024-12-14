# source("00-run-CRICscripts-bat.R")

# Input: `00runCRICscripts-bat-template.txt`
# Output:
# generates and executes `00runCRICscripts-temp.bat`

prj_name       = "CRIC_prj"
scriptBaseName = "30-cric-cv.coxnet"
anl_name       = "test"  # test, lin1

tvar_ids =  "01 02 03 10 31 32"  # Time var ids selected

# Define the string to be replaced in BAT template
value_to_replace <- "!replace!"

## Libraries

library(DT)
library(ggplot2)
library(xfun)
library(dm)
library(dplyr)
library(skimr)
library(ggplot2)
library(knitr)
library(tidyr)
library(stringr)
library(readr)

# Read the content from the bat template
template_content <- read_lines("00runCRICscripts-bat-template.txt")

# Apply str_glue to each line of the template content
modified_content <- sapply(template_content, str_glue)

# Write the modified content to a new file
write_lines(modified_content, "00runCRICscripts-temp.bat")

# Execute batch file
ff = "00runCRICscripts-temp.bat"
#shell.exec(ff)
#unlink(ff)


