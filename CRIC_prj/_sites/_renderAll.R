# source("_renderAll.R")

# Input: _site_template.yml
# Output: _site.yml

# Auxiliary code
options(width = 100)

rm(list = ls())

analysis_name = "test" # Subfolder with analysis results

tvar_ids_vec =  c("01", "03") #, "32")  # Vector with time var ids selected

# Define the number of replications


# Define the value to be replaced in yml template
value_to_replace <- "!replace!"


## Libraries

library(DT)
library(glmnet)
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
library(openxlsx)

# Cleanup
unlink("_site.yml")
unlink(paste0("a_", analysis_name), recursive = TRUE)
# Using tvar_ids_vec generate the vector of formatted strings

replacement_vector <- sapply(1:length(tvar_ids_vec), function(i) {
  text = tvar_ids_vec[i]
  ## text <- sprintf("%02d", i) # Format the number with leading zeros
  cx1 = str_glue('    - text: "{text}"') 
  cx2 = str_glue('      href: {text}.html')
  c(cx1, cx2)
})
dim(replacement_vector) = NULL 

# Print the vector
# cat(paste(replacement_vector, collapse = "\n"))



# Read the content from the yml template
template_content <- read_lines("_site_template_yml.txt")

# Apply str_glue to each line of the template content
modified_content <- sapply(template_content, str_glue)


# Determine the position of the value to be replaced
replace_position <- which(modified_content == value_to_replace)

# Error handling: Ensure the value is found in the existing vector
if (length(replace_position) == 0) {
  stop(paste("Value ", value_to_replace, " not found in the existing vector."))
}

# Combine the vectors by replacing the element at the determined position with the new vector
final_content<- c(modified_content[1:(replace_position - 1)], 
                     replacement_vector, 
                     modified_content[(replace_position + 1):length(modified_content)])

# Write the modified yml content to a new file
write_lines(final_content, "_site.yml")
# cat(paste(final_content, collapse = "\n"))

parms = list(anl_name =analysis_name) 
rmarkdown::render("00Info.Rmd", "all", params=parms)


##=== xx.Rmd rendered
#parms = list(anl_name =analysis_name, t_varsid = "01") 
#rmarkdown::render("xx.Rmd", "all", output_file="01", params=parms)

repl_vector <- sapply(1:length(tvar_ids_vec), function(i) {
  txt = tvar_ids_vec[i]
  cx1 = str_glue('parms = list(anl_name =analysis_name, t_varsid ="{txt}")') 
  cx2 = str_glue('rmarkdown::render("xx.Rmd", "all", output_file ="{txt}", params=parms)')
  c(cx1, cx2)
})
dim(repl_vector) = NULL 
eval(parse(text=repl_vector))


rmarkdown::render_site()
unlink("*.html")
#rmarkdown::clean_site(preview =FALSE)


