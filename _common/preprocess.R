## =============== 전처리 단계 - 데이터 작업 

# 자료읽기
#load(here("data","Rdata","paps.RData"))


## read arrow data
dirpath <- here("data","feather","E_final.feather")
df_e_01 <- read_feather(dirpath, col_select = NULL, as_data_frame = TRUE, mmap = TRUE)
dirpath <- here("data","feather","M_final.feather")
df_m_01 <- read_feather(dirpath, col_select = NULL, as_data_frame = TRUE, mmap = TRUE)
dirpath <- here("data","feather","H_final.feather")
df_h_01 <- read_feather(dirpath, col_select = NULL, as_data_frame = TRUE, mmap = TRUE)

df_e_01 <- df_e_01[,1:31]
df_m_01 <- df_m_01[,1:31]
df_h_01 <- df_h_01[,1:31]

## var_info

var_paps_info <- read_excel(here("data","PAPS_col_definition.xlsx"))
sido_info <- read_excel(here("data","sido_code.xlsx"))

var_change_vector_PAPS <- deframe(var_paps_info[,c(4,2)])
var_change_vector_SIDO <- deframe(sido_info[,c(2,1)])

# To rename column names in an R data frame 
# using another data frame that contains old and new names
make_rename_col_df <- function(df, info_vec){
  df %>%   rename( any_of(info_vec))
}

# To rename one column's data based on matching with named vector 
# using another data frame that contains old and new names
make_rename_row_df <- function(df, varname, info_vec){
  df0 <- df
  x <- df0 %>% dplyr::pull({{varname}})
  res <- df0 %>% dplyr::mutate( {{varname}} := names(info_vec)[match(x, info_vec)])
  return(res)
}

# 워드 화일 페이지 바꾸는 문장 
wordnewpage <-
  '```{=openxml}
<w:p><w:r><w:br w:type="page"/></w:r></w:p>
```'

# flextable option 설정
set_flextable_defaults(
  font.size = 8, 
  padding.bottom = 3, 
  padding.top = 3,
  padding.left = 3,
  padding.right = 3,
  big.mark = " ", 
  font.color = "#666666",
  border.color = "#666666"
#  post_process_html = function(x){
#    theme_vanilla(x) %>%
#      set_table_properties(layout = "fixed") %>%
#      autofit()
#  },
# post_process_pdf = function(x){
#    theme_vanilla(x) %>%
#      set_table_properties(layout = "fixed") %>%
#      autofit()
#  },
#  post_process_docx = function(x){
#    theme_vanilla(x) %>%
#      set_table_properties(layout = "fixed") %>%
#      autofit()
#  }
)


# flextable 을 만들어 주는 함수
# loc_center: 헤더(header) 컬럼 중에서 label 을 중앙에 위치하게 하는 컬럼위치 
### 가중치 없는 결과 테이블

# loc_center 값
#### table 1
#center_col_1 <- 3:8
#### table 2
#center_col_2 <- 2:11
### 가중치 있는 결과 테이블
#### table 3
#center_col_3 <- 3:7
#### table 4
#center_col_4 <- 2:15

make_table_1 <- function(df, loc_center, filename) {
  
  fname <- paste(filename,".xlsx", sep="")
  write.xlsx(df, file = here("data","outputs",fname))
  
  ft <- df |> 
    flextable() |> 
    separate_header() |> 
    autofit()  |>
    theme_booktabs(bold_header = TRUE) |>
    align(align = "center", part = "header", j = loc_center) |> 
    colformat_num(big.mark = "", decimal.mark = ".") |>
    colformat_double(digits = 1) |> 
    fix_border_issues()
  
  ft
}

# flextable 을 만들어 주는 함수
# loc_center: 헤더(header) 컬럼 중에서 label 을 중앙에 위치하게 하는 컬럼위치 

make_table_2 <- function(df, filename) {
  
  table_col <- colnames(df) 
  
  fname <- paste(filename,".xlsx", sep="")
  write.xlsx(df, file = here("data","outputs",fname))
  
  ft <- df |> dplyr::group_by(`학교`) |>
    dplyr::mutate(is_last_val_in_group = row_number() == max(row_number())) |>
    flextable(col_keys = table_col ) |> 
    separate_header() |> 
    merge_v( j = 1 ) |> 
    valign(j = 1, valign = "top") |>
    #autofit()  |>
    theme_booktabs(bold_header = TRUE) |>
    align(align = "center", part = "header", j =3:12) |> 
    colformat_double(digits = 1) |> 
    colformat_num(big.mark = "", decimal.mark = ".") |>
    hline(i = ~is_last_val_in_group == TRUE, border = fp_border()) |>
    width(j = 3:12, 1, unit = "in") |>
    fix_border_issues()
  
  ft
}



# 현재 프로젝트 폴더를 기준으로 폴더명을 정의  

#path_csv <- "csv"       # 요약 통계량이 저장된 csv 화일이 있는 폴더명 
#path_doc <- "doc"       # csv 화일이 있는 폴더명 
#path_common <- "common" #라이브러리, 함수 등이 정의된 R 프로그램이 있는 폴더 

# 출력 결과를 탐지하는 함수
ishtml <- knitr::is_html_output()
ispdf <- knitr::is_latex_output()
isword <- !ishtml & !ispdf

# arsenal options 
mycontrols  <- tableby.control(numeric.stats=c("N", "meansd", "median",  "min", "q1q3", "max"),
                               stats.labels=list(N = "학생수", meansd='평균(표준편차)', median='중앙값', min='최소', q1q3='25%,75%' , max='최대'  ))


mycontrols2  <- tableby.control(numeric.stats=c( "mean", "sd"),stats.labels=list( mean='평균', sd="표준편차"))

# function to make count table by year and grade
make_count_table <- function(df){
  df %>% 
    dplyr::group_by(AYR, GRD) %>% 
    dplyr::summarise(n = n()) %>%
    tidyr::pivot_wider(names_from = GRD, values_from = n) #%>% 
  #dplyr::mutate(total = rowSums(.[2:ncol(.)]))
}

# function to print data frame with HTML forma bu using kableExtra
print_df <- function(df, caption_text){
  df %>% 
    kable("html", align = "c", caption = caption_text , booktabs = T) %>%
    kable_styling(bootstrap_options = c("striped"),  position = "center")
}

# function to list summary statistics such as mean, std, some qunatiles by grade
print_summary <- function(df, caption_text){
  df %>% 
    tableby( ~ GRD + SXDS_SC_CD, data = ., controls = mycontrols) %>% 
    kable("html", align = "c", caption = caption_text , booktabs = T) %>%
    kable_styling(bootstrap_options = c("striped"), full_width = F, position = "center")
}


school_levels <- c("초등학교", "중학교", "고등학교", NA)
grade_levels <- c("초등학교_4", "초등학교_5", "초등학교_6", "중학교_1", "중학교_2", "중학교_3", "고등학교_1", "고등학교_2", "고등학교_3", NA)
sido_levels <- c(names(var_change_vector_SIDO), NA)
test_levels <- c("심폐지구력", "유연성","근지구력","순발력", NA )

## summarize by school


summ_by_school_3var <- function(df, var1, var2, var3, school_char, test_char ) {

  df_1 <- df %>% dplyr::select(SHL_CD_NM, region, AYR, GRD, SXDS_SC_CD, {{var1}}, {{var2}}, {{var3}}) %>%
    group_by(SHL_CD_NM, region, AYR, GRD, SXDS_SC_CD) %>% 
    dplyr::summarize( N = n(), {{var1}} := mean({{var1}}, na.rm = TRUE), 
                    {{var2}} := mean({{var2}}, na.rm = TRUE),
                    {{var3}} := mean({{var3}}, na.rm = TRUE) )  


  df_2 <- df %>% dplyr::select(SHL_CD_NM, region, AYR, GRD, SXDS_SC_CD, {{var1}}, {{var2}}, {{var3}}) %>%
    group_by(SHL_CD_NM, region, AYR, GRD, SXDS_SC_CD) %>% 
    dplyr::summarize(    {{var1}} := sum(!is.na({{var1}})),
                       {{var2}} := sum(!is.na({{var2}})),
                       {{var3}} := sum(!is.na({{var3}})) )  %>%
    dplyr::rename( a = {{var1}}, b = {{var2}}, c = {{var3}}) 


  df_3 <- df_2 %>% dplyr::select(SHL_CD_NM, region, AYR, GRD, SXDS_SC_CD,  a, b, c) %>%
    dplyr::rename( {{var1}} := a, {{var2}} := b, {{var3}} := c)  %>%
    pivot_longer(cols = -(1:5), names_to = "item", values_to = "value") %>%
    group_by(SHL_CD_NM, region, AYR, GRD, SXDS_SC_CD) %>%
    summarise(select_1_item = item[which.max(value)], select_1_n = max(value))  

  df_4 <- left_join(df_1, df_3, by = c("SHL_CD_NM", "region", "AYR", "GRD", "SXDS_SC_CD")) %>%
    dplyr::select(SHL_CD_NM, region, AYR, GRD, SXDS_SC_CD, select_1_item,  {{var1}}, {{var2}}, {{var3}}) %>%
    pivot_longer(cols = -(1:6), names_to = "item", values_to = "value") %>%
    dplyr:: filter(item == select_1_item) %>%
    dplyr:: select(-item) %>% 
    dplyr:: select(-select_1_item) %>% 
    dplyr:: rename(select_1_mean = value)      

  df_f <- left_join(df_3 , df_4, by = c("SHL_CD_NM", "region", "AYR", "GRD", "SXDS_SC_CD")) %>% 
    dplyr::mutate(school_type = school_char) %>%
    dplyr::relocate(school_type) %>%
    dplyr::mutate(test_type = test_char) %>%
    dplyr::relocate(test_type, .after = SXDS_SC_CD) %>%
    dplyr::mutate( SCH_GRD = paste(school_type, GRD, sep = "_") ) %>%
    dplyr::relocate(SCH_GRD, .before = GRD) %>%
    dplyr::ungroup()
  
  df_f <- make_rename_row_df(df_f, region, var_change_vector_SIDO)
  df_f <- make_rename_row_df(df_f, select_1_item, var_change_vector_PAPS)
  df_f$region <- factor(df_f$region, levels = sido_levels)
  df_f$SCH_GRD <- factor(df_f$SCH_GRD, levels = grade_levels)
  df_f$school_type <- factor(df_f$school_type, levels = school_levels)
  df_f$test_type <- factor(df_f$test_type, levels = test_levels)
  
  df_f <- make_rename_col_df(df_f, var_change_vector_PAPS)
  
  return(df_f)
   
}


summ_by_school_2var <- function(df, var1, var2, school_char, test_char ) {
  
  df_1 <- df %>% dplyr::select(SHL_CD_NM, region, AYR, GRD, SXDS_SC_CD, {{var1}}, {{var2}}) %>%
    group_by(SHL_CD_NM, region, AYR, GRD, SXDS_SC_CD) %>% 
    dplyr::summarize( N = n(), {{var1}} := mean({{var1}}, na.rm = TRUE), 
                      {{var2}} := mean({{var2}}, na.rm = TRUE))  
  
  
  df_2 <- df %>% dplyr::select(SHL_CD_NM, region, AYR, GRD, SXDS_SC_CD, {{var1}}, {{var2}}) %>%
    group_by(SHL_CD_NM, region, AYR, GRD, SXDS_SC_CD) %>% 
    dplyr::summarize(    {{var1}} := sum(!is.na({{var1}})),
                         {{var2}} := sum(!is.na({{var2}})) )  %>%
    dplyr::rename( a = {{var1}}, b = {{var2}}) 
  
  
  df_3 <- df_2 %>% dplyr::select(SHL_CD_NM, region, AYR, GRD, SXDS_SC_CD,  a, b) %>%
    dplyr::rename( {{var1}} := a, {{var2}} := b)  %>%
    pivot_longer(cols = -(1:5), names_to = "item", values_to = "value") %>%
    group_by(SHL_CD_NM, region, AYR, GRD, SXDS_SC_CD) %>%
    summarise(select_1_item = item[which.max(value)], select_1_n = max(value))  
  
  df_4 <- left_join(df_1, df_3, by = c("SHL_CD_NM", "region", "AYR", "GRD", "SXDS_SC_CD")) %>%
    dplyr::select(SHL_CD_NM, region, AYR, GRD, SXDS_SC_CD, select_1_item,  {{var1}}, {{var2}}) %>%
    pivot_longer(cols = -(1:6), names_to = "item", values_to = "value") %>%
    dplyr:: filter(item == select_1_item) %>%
    dplyr:: select(-item) %>% 
    dplyr:: select(-select_1_item) %>% 
    dplyr:: rename(select_1_mean = value)      
  
  df_f <- left_join(df_3 , df_4, by = c("SHL_CD_NM", "region", "AYR", "GRD", "SXDS_SC_CD")) %>% 
    dplyr::mutate(school_type = school_char) %>%
    dplyr::relocate(school_type) %>%
    dplyr::mutate(test_type = test_char) %>%
    dplyr::relocate(test_type, .after = SXDS_SC_CD) %>%
    dplyr::mutate( SCH_GRD = paste(school_type, GRD, sep = "_") ) %>%
    dplyr::relocate(SCH_GRD, .before = GRD) %>%
    dplyr::ungroup()
  
  df_f <- make_rename_row_df(df_f, region, var_change_vector_SIDO)
  df_f <- make_rename_row_df(df_f, select_1_item, var_change_vector_PAPS)
  df_f$region <- factor(df_f$region, levels = sido_levels)
  df_f$SCH_GRD <- factor(df_f$SCH_GRD, levels = grade_levels)
  df_f$school_type <- factor(df_f$school_type, levels = school_levels)
  df_f$test_type <- factor(df_f$test_type, levels = test_levels)
  
  df_f <- make_rename_col_df(df_f, var_change_vector_PAPS)
  
  return(df_f)
  
}

# calculate percent and freq by year and school

cal_percent_by_2 <- function(df) { 

  a <-  df %>% dplyr::select(`년도`, `시도`, `학교`,`체력요인`, `선택종목`,`학교명`,`성별`, `학교_학년`) %>%
    dplyr::filter(`년도` >= 2011) %>%
    dplyr::arrange(`년도`, `시도`, `학교`,`체력요인`, `선택종목`,`학교명`,`성별`, `학교_학년` ) %>%
    group_by(`년도`, `시도`, `학교`, `체력요인`, `선택종목`,`학교명`) %>% 
    slice_head(n = 1) %>% 
    ungroup() %>%
    group_by(`년도`, `학교`,`체력요인`, `선택종목`) %>%
    summarise( 학교수 =n())   %>%   
    dplyr::mutate(비율 = round(`학교수` *100 / sum(`학교수`),2))

  df_n <- a %>% dplyr::select(-`비율`)  %>%
    dplyr::arrange(`학교`,`년도`) %>%
    pivot_wider(names_from = c( `체력요인`, `선택종목`), values_from = `학교수`) %>%
    dplyr::relocate(`학교`)

  df_p <- a %>% dplyr::select(-`학교수`)  %>%
    dplyr::arrange(`학교`,`년도`) %>%
    pivot_wider(names_from = c(`체력요인`, `선택종목`), values_from = `비율`)  %>%
    dplyr::relocate(`학교`)
  
  return(list(df_n, df_p))
}
