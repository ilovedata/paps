#
#
#
#
#
knitr::opts_chunk$set(echo = FALSE, warning=FALSE, message=FALSE)
library(here)           # file pathways
source(here::here("_common", "common.R"))
source(here::here("_common", "preprocess.R"))
#
#
#
#
#
#
#| waring : FALSE
#| message : FALSE
aa <- summ_by_item_1var(df_e_01, BMI_IDEX, "초등학교", "체질량지수")
bb <- summ_by_item_1var(df_m_01, BMI_IDEX, "중학교", "체질량지수")
cc <- summ_by_item_1var(df_h_01, BMI_IDEX, "고등학교", "체질량지수")
pp <- combine_summ_by_item_1var(aa[[1]], bb[[1]], cc[[1]], "체질량지수")
k <- barplot_by_item_1var_year(BMI_IDEX, "체질량지수")
```
#
#
#
pp[[3]]
#
#
#
if(isword) cat("\n", wordnewpage, "\n")
#
#
#
#
pp[[1]]
#
#
#
#
if(isword) cat("\n", wordnewpage, "\n")
#
#
#
#
#
pp[[4]]
#
#
#
#
if(isword) cat("\n", wordnewpage, "\n")
#
#
#
#
pp[[2]]
#
#
#
#
if(isword) cat("\n", wordnewpage, "\n")
#
#
#
#
k
#
#
#
#
#
#
