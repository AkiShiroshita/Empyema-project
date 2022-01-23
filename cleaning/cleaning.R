
# Set-up ------------------------------------------------------------------

library("readr")
library("readxl")
library("tidyverse")
library("lubridate")
library("tidylog")
library("ggplot2")
library("ggthemes")
library("ggplotgui")
library("arsenal")

getwd()
dir.exists("cleaning")
#dir.create("cleaning")
setwd("C:/Users/akihi/Downloads/empyema_analysis")
rm(list=ls())

# Kumamoto ----------------------------------------------------------------

file.edit("cleaning/kumamoto.R")
rm(list=ls())

# Ichinomiya --------------------------------------------------------------

file.edit("cleaning/ichinomiya.R")
rm(list=ls())

# Kobe --------------------------------------------------------------------

file.edit("cleaning/kobe.R")
rm(list=ls())

# Yokohama ----------------------------------------------------------------

file.edit("cleaning/yokohama.R")
rm(list=ls())
