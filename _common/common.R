
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
library(lubridate)
library(rlang)
#library(gamlss)

# Nanum Pen Script 는 선택
font_add_google("Nanum Pen Script", "gl")
showtext_auto()

not_all_na <- function(x) any(!is.na(x))
not_any_na <- function(x) all(!is.na(x))

## var_info

var_paps_info <- read_excel(here("data","PAPS_col_definition.xlsx"))
var_outlier_info <- read_excel(here("data","PAPS_col_outlier_treat.xlsx"))
sido_info <- read_excel(here("data","sido_code.xlsx"))

filepath <- here("data", "physical100_item_20240127.csv")
phy100_colnames  <- read_csv(filepath)

var_change_vector_PAPS <- deframe(var_paps_info[,c(4,2)])
var_change_vector_SIDO <- deframe(sido_info[,c(2,1)])
var_change_vector_phy100<- toupper(deframe(phy100_colnames[,c(3,1)]))

# 범주형 레벨

school_levels <- c("초등학교", "중학교", "고등학교")
grade_levels <- c("초등학교_4", "초등학교_5", "초등학교_6", "중학교_1", "중학교_2", "중학교_3", "고등학교_1", "고등학교_2", "고등학교_3")
sido_levels <- c(names(var_change_vector_SIDO))
test_levels <- c("심폐지구력", "유연성","근지구력","순발력" )
bmi_levels <- c("마름", "정상", "과체중", "경도비만", "고도비만")

test_item_levels <- var_paps_info$var_ko_s[c(11,10,9,14,15,20,19,18,23,24,5,6,27)]

# 워드 화일 페이지 바꾸는 문장 
wordnewpage <-
  '```{=openxml}
<w:p><w:r><w:br w:type="page"/></w:r></w:p>
```'
