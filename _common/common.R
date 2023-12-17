
library(flextable)      # make HTML tables 
library(officer)        # helper functions for tables
library(tidyverse)      # data management, summary, and visualization
library(readxl)         # read excel data
library(openxlsx)
library(knitr)
library(kableExtra)
library(purrr)
library(tibble)
library(data.table)
library(lubridate)
library(janitor)
library(corrplot)
library(scales)
library(showtext)
library(arsenal)
library(arrow)
#library(gamlss)

# Nanum Pen Script 는 선택
font_add_google("Nanum Pen Script", "gl")
showtext_auto()

not_all_na <- function(x) any(!is.na(x))
not_any_na <- function(x) all(!is.na(x))
