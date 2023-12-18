## =============== 전처리 단계 - 데이터 작업 

# 자료읽기
#load(here("data","Rdata","paps.RData"))


## read arrow data
#dirpath <- here("data","feather","E_final.feather")
#df_e_01 <- read_feather(dirpath, col_select = NULL, as_data_frame = TRUE, mmap = TRUE)
#dirpath <- here("data","feather","M_final.feather")
#df_m_01 <- read_feather(dirpath, col_select = NULL, as_data_frame = TRUE, mmap = TRUE)
#dirpath <- here("data","feather","H_final.feather")
#df_h_01 <- read_feather(dirpath, col_select = NULL, as_data_frame = TRUE, mmap = TRUE)

#df_e_01 <- df_e_01[,1:31]
#df_m_01 <- df_m_01[,1:31]
#df_h_01 <- df_h_01[,1:31]


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


####--- 학교별 편균자료를 받나서 연도벼르 학교_학년별 종목의 전체 평균을 구하고 데이믈과 그림 출럭 및 저장 

summ_stat_plot_itemwise_sex_all <- function(df, item_name, sex, filename ) {
  
  mytitle <- paste(item_name,"-", sex,"- 전국 평균값", sep = "")
  
  df_item_1 <- df %>% dplyr::filter(`선택종목`  == item_name) %>%
    dplyr::filter(`성별` == sex ) %>%
    group_by(`년도`, `학교_학년`) %>%
    summarize( 학교수 =n(), 전국평균 = weighted.mean(`평균`,`학생수`)) %>%
    ungroup() 
  
  df_item_1_tbl <- df_item_1  %>% 
    dplyr::select(`년도`, `학교_학년`, 전국평균) %>%
    pivot_wider(names_from = `년도`, values_from = 전국평균, names_prefix = "년도_")
  
  g <- df_item_1 %>% ggplot(aes(x = `년도`, y = 전국평균, group = `학교_학년`, color = `학교_학년`, shape= `학교_학년`)) +
    geom_line() +
    geom_point(size=2) +
    scale_shape_manual(values = c(15,16,15,16,17,15,16,17)) + 
    labs(title = mytitle, x = "년도", y = "평균") +
    theme_bw() +
    theme(legend.position = "bottom")
  
  ft <- df_item_1_tbl |> 
    flextable() |> 
    separate_header() |> 
    autofit()  |>
    theme_booktabs(bold_header = TRUE) |>
    align(align = "center", part = "header", j = 2:13) |> 
    colformat_num(big.mark = "", decimal.mark = ".") |>
    colformat_double(digits = 2) |>
    set_caption( mytitle) |>
    fix_border_issues()
  
  fname_1 <- paste(filename,"-", item_name,"-", sex,".xlsx", sep="")
  fname_2 <- paste(filename,"-", item_name,"-", sex,".png", sep="")
  write.xlsx(df_item_1_tbl, file = here("data","outputs",fname_1))
  ggsave(here("data","outputs",fname_2), plot = g, width = 5, height = 4, units = "in")
  
  list(ft, g)
}

# 주어진 등급 변수(cd_code) 에 대하여 등급에 해당하는 학생수와 비율을 구하는 함수

summ_CD_all <-function(df, cd_code, cd_code_str, school_char){
  
  res <- df %>% dplyr::select(SHL_CD_NM, AYR, GRD, SXDS_SC_CD, {{cd_code}}) %>%
    dplyr::filter(AYR >= 2011) %>%
    group_by(AYR, GRD, SXDS_SC_CD, {{cd_code}}) %>% 
    dplyr::summarize( 학생수 = n()) %>%
    dplyr::mutate(비율 = round(`학생수` *100 / sum(`학생수`),2)) %>%
    dplyr::filter( !is.na(SXDS_SC_CD )) %>% 
    dplyr::mutate(school_type = school_char) %>%
    dplyr::relocate(school_type) %>%
    dplyr::mutate( SCH_GRD = paste(school_type, GRD, sep = "_") ) %>%
    dplyr::relocate(SCH_GRD, .before = GRD) %>%
    dplyr::mutate( SCH_GRD = paste(school_type, GRD, sep = "_") ) %>%
    dplyr::relocate(SCH_GRD, .before = GRD) %>%
    dplyr::mutate( cd_type = cd_code_str) %>%
    dplyr::relocate(cd_type, .after = SXDS_SC_CD) %>%
    dplyr::rename( CD =  {{cd_code}} ) %>%
    dplyr::mutate( CD = as.character(CD)) 
  
  return(res)
}

####--- 남여별, 분야별 등급의 요약 데이블과 그림 출럭 및 저장 


summ_cd_plot_table_sex_all <- function( df, cd_name, sex, filename ) {
  
  df_1 <- df %>% dplyr::filter(`등급유형` == cd_name) %>% dplyr::filter(`성별` == sex) %>% 
    dplyr::filter(!is.na(`등급`))
  
  if (cd_name == "BDFAT_CLA_SCR_NM"){
    
    df_1$`등급` <- factor(df_1$`등급`, levels = bmi_levels)
  }
  
  g <-df_1 %>%  ggplot( aes(fill=`등급`, x=factor(`년도`), y=`학생수`)) +
    geom_bar(position="fill", stat="identity") +
    xlab("연도") +
    ylab("등급비율") +
    theme(legend.position = "bottom")  +
    facet_wrap(~`학교_학년`, ncol=3)
  
  a <- df_1 %>% dplyr::select(`년도`, `학교_학년`, `등급`, `학생수`)
  
  df_summ_n <- a %>% pivot_wider(names_from = `등급`, values_from = `학생수`, names_prefix="등급인원수_") %>%
    dplyr::arrange(`년도`, `학교_학년`)
  
  a <- df_1 %>%     dplyr::select(`년도`, `학교_학년`, `등급`, `비율`)
  
  df_summ_p <- a %>% pivot_wider(names_from = `등급`, values_from = `비율`, names_prefix="등급비율_") %>%
    dplyr::arrange(`년도`, `학교_학년`)
  
  df_summ <- left_join(df_summ_n, df_summ_p, by=c("년도", "학교_학년"))    
  
  
  table_col <- colnames(df_summ_n) 
  
  ft1 <- df_summ_n %>% dplyr::group_by(`년도`) |>
    dplyr::mutate(is_last_val_in_group = row_number() == max(row_number())) |>
    flextable(col_keys = table_col ) |> 
    separate_header() |> 
    merge_v( j = 1 ) |> 
    valign(j = 1, valign = "top") |>
    #autofit()  |>
    theme_booktabs(bold_header = TRUE) |>
    align(align = "center", part = "header", j =2:6) |> 
    colformat_double(digits = 1) |> 
    colformat_num(big.mark = "", decimal.mark = ".") |>
    hline(i = ~is_last_val_in_group == TRUE, border = fp_border()) |>
    width(j = 2:6, 1, unit = "in") |>
    fix_border_issues()
  
  table_col <- colnames(df_summ_p) 
  
  ft2 <- df_summ_p %>% dplyr::group_by(`년도`) |>
    dplyr::mutate(is_last_val_in_group = row_number() == max(row_number())) |>
    flextable(col_keys = table_col ) |> 
    separate_header() |> 
    merge_v( j = 1 ) |> 
    valign(j = 1, valign = "top") |>
    #autofit()  |>
    theme_booktabs(bold_header = TRUE) |>
    align(align = "center", part = "header", j =2:6) |> 
    colformat_double(digits = 1) |> 
    colformat_num(big.mark = "", decimal.mark = ".") |>
    hline(i = ~is_last_val_in_group == TRUE, border = fp_border()) |>
    width(j = 2:6, 1, unit = "in") |>
    fix_border_issues()
  
  fname_1 <- paste(filename,"-", cd_name,"-", sex,".xlsx", sep="")
  fname_2 <- paste(filename,"-", cd_name,"-", sex,".png", sep="")
  write.xlsx(df_summ, file = here("data","outputs",fname_1))
  ggsave(here("data","outputs",fname_2), plot = g, width = 5, height = 5, units = "in")
  
  list(g, ft1, ft2)
}

