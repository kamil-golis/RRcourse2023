
# Sets the path to the parent directory of RR classes
setwd("Z:\\File folders\\Teaching\\Reproducible Research\\2023\\Repository\\RRcourse2023\\6. Coding and documentation")


#   Import data from the O*NET database, at ISCO-08 occupation level.
# The original data uses a version of SOC classification, but the data we load here
# are already cross-walked to ISCO-08 using: https://ibs.org.pl/en/resources/occupation-classifications-crosswalks-from-onet-soc-to-isco/

# The O*NET database contains information for occupations in the USA, including
# the tasks and activities typically associated with a specific occupation.

task_data = read.csv("Data\\onet_tasks.csv")
# isco08 variable is for occupation codes
# the t_* variables are specific tasks conducted on the job

# read employment data from Eurostat
# These datasets include quarterly information on the number of workers in specific
# 1-digit ISCO occupation categories. (Check here for details: https://www.ilo.org/public/english/bureau/stat/isco/isco08/)
library(readxl)                     
library(readr)

# first take list of excel sheets - we exclude sheet "total"
sheets <- excel_sheets("Data\\Eurostat_employment_isco.xlsx")[-1]

# create an empty data frame to store the data from excel sheets in
isco_data <- data.frame(TIME=character(), 
                        `European Union - 28 countries (2013-2020)`=numeric(),
                        Belgium=numeric(),
                        Czechia=numeric(), 
                        Denmark=numeric(),
                        Spain=numeric(),
                        Italy=numeric(),
                        Lithuania=numeric(),
                        Poland=numeric(),
                        Finland=numeric(),
                        Sweden=numeric(),
                        ISCO=numeric())

# collect the data for all of the countries in one data frame
for (sheet in sheets) {
  
  data_tmp <- read_excel("Data\\Eurostat_employment_isco.xlsx", sheet = sheet)
  data_tmp$ISCO <- parse_number(sheet)
  
  isco_data <- rbind(isco_data, data_tmp)
  
}

#let's get rid of the column concerning whole EU - not needed later
isco_data <- subset(isco_data, select = -`European Union - 28 countries (2013-2020)`)


# calculate worker totals in each country and add them as additional columns to our dataset
# this approach saves the next step - we do not have to replicate values 9 times for each sector
# next, the loop adds a column with the worker share
library(dplyr)

# retrieve list of countries to iterate over in the next step
countries <- colnames(isco_data)[-c(1,11)]

for (country in countries) {
  
  # calculate total for a given country for a given period (sum values for all sectors)
  total_tmp <- as.data.frame(aggregate(isco_data[,country], by=list(isco_data$TIME),
                                       FUN=sum, na.rm=TRUE))
  
  # add the new column with a total for a given country
  isco_data <- left_join(isco_data, total_tmp, by = c("TIME" = "Group.1"), suffix = c("", "_total"))
  
  # add the new column with share of workers in a given sector
  isco_data$share_to_be <- isco_data[,country] / isco_data[, paste0(country, "_total")]
  # change the column name to inform about the share of a given country
  colnames(isco_data)[ncol(isco_data)] <- paste0("share_",country)
  
}

# Now let's look at the task data. We want the first digit of the ISCO variable only
library(stringr)

task_data$isco08_1dig <- str_sub(task_data$isco08, 1, 1) %>% as.numeric()

# And we'll calculate the mean task values at a 1-digit level 
# (more on what these tasks are below)

aggdata <-aggregate(task_data, by=list(task_data$isco08_1dig),
                    FUN=mean, na.rm=TRUE)
aggdata$isco08 <- NULL

# We'll be interested in tracking the intensity of Non-routine cognitive analytical tasks
# Using a framework reminiscent of the work by David Autor.

#These are the ones we're interested in:
# Non-routine cognitive analytical
# 4.A.2.a.4 Analyzing Data or Information
# 4.A.2.b.2	Thinking Creatively
# 4.A.4.a.1	Interpreting the Meaning of Information for Others

#Let's combine the data.
combined <- left_join(isco_data, aggdata, by = c("ISCO" = "isco08_1dig"))

# Traditionally, the first step is to standardise the task values using weights 
# defined by share of occupations in the labour force. This should be done separately
# for each country. Standardisation -> getting the mean to 0 and std. dev. to 1.
# Let's do this for each of the variables that interests us:

#install.packages("Hmisc")
library(Hmisc)

data <- combined
country <- "Spain"
tasks <- c("t_4A2a4", "t_4A2b2", "t_4A4a1")

# In order to make the last step concise, we are going to wrap it in a function. It enables to calculate and plot the tasks share in time:
calc_plot_task <- function(country, tasks, data=combined){
  
  # create an empty column to dataframe to store the sum of standardized tasks
  data$general_task_sum <- 0
  
  # iterate over task items
  for (task in tasks) {
    
    # store data needed in a dedicated object
    temp_data <- data[, c(task, paste0("share_",country))]
    colnames(temp_data) <- c("task", "share_country")
    
    temp_mean <- wtd.mean(temp_data$task, temp_data$share_country)
    temp_sd <- wtd.var(temp_data$task, temp_data$share_country) %>% sqrt()
    
    data$std_to_be <- (temp_data$task-temp_mean)/temp_sd
    # change the column name to inform about the share of a given country
    colnames(data)[ncol(data)] <- paste0("std_",country,"-",task)
    
    # The next step is to calculate the `classic` task content intensity, here by summing the chosen tasks iteratively
    data$general_task_sum <- data$general_task_sum + data[, ncol(data)]
  }
  
  # And we standardise the general task (NRCA) in a similar way.
  temp_data <- data[, c("general_task_sum", paste0("share_",country))]
  colnames(temp_data)[2] <- "share_country"
  
  temp_mean <- wtd.mean(temp_data$general_task_sum, temp_data$share_country)
  temp_sd <- wtd.var(temp_data$general_task_sum, temp_data$share_country) %>% sqrt()
  data$std_general_task <- (temp_data$general_task_sum-temp_mean)/temp_sd
  
  # Finally, to track the changes over time, we have to calculate a country-level mean
  # Step 1: multiply the value by the share of such workers.
  data$multip_country_general <- (data$std_general_task * data[, paste0("share_",country)])
  
  # Step 2: sum it up (it basically becomes another weighted mean)
  agg_country <- aggregate(data$multip_country_general, by=list(data$TIME),
                           FUN=sum, na.rm=TRUE)
  colnames(agg_country)[2] <- "x"
  
  # We can plot it now! - add title with country
  plot(agg_country$x, xaxt="n", main=country)
  axis(1, at=seq(1, 40, 3), labels=agg_country$Group.1[seq(1, 40, 3)])
  
}

# now we can test our function!

# firstly, let's reproduce the results from the original file
calc_plot_task("Poland",c("t_4A2a4", "t_4A2b2", "t_4A4a1"))
calc_plot_task("Spain",c("t_4A2a4", "t_4A2b2", "t_4A4a1"))
calc_plot_task("Belgium",c("t_4A2a4", "t_4A2b2", "t_4A4a1"))

# now - modifications
# 1. let's try with other countries
calc_plot_task("Czechia",c("t_4A2a4", "t_4A2b2", "t_4A4a1"))
calc_plot_task("Denmark",c("t_4A2a4", "t_4A2b2", "t_4A4a1"))

# 2. change the tasks to be plotted - randomly selected here
calc_plot_task("Italy",c("t_1A1f1", "t_1A2a2", "t_2B1a"))
calc_plot_task("Lithuania",c("t_4C3b4", "t_4C3b7", "t_4C3d3"))

# And finally the tasks proposed in the assignment:
# Routine manual
# 4.A.3.a.3	Controlling Machines and Processes
# 4.C.2.d.1.i	Spend Time Making Repetitive Motions
# 4.C.3.d.3	Pace Determined by Speed of Equipment
calc_plot_task("Finland",c("t_4A3a3", "t_4C2d1i", "t_4C3d3"))

