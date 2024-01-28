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

dirpath <- here("data","feather","physical100.feather")
df_100 <- read_feather(dirpath, col_select = NULL, as_data_frame = TRUE, mmap = TRUE)

#df_e_01 <- df_e_01[,1:31]
#df_m_01 <- df_m_01[,1:31]
#df_h_01 <- df_h_01[,1:31]

## read csv file 1

auto_csv_reader1 <- function(fname) {
  read.csv(fname, fileEncoding="CP949", encoding="UTF-8",header = T)
}

## read csv file 2


auto_csv_reader1 <- function(fname, sep0=",", header0=TRUE) {
  reader <- function(encoding, fname, sep0, header0) {
    read.csv(fname, fileEncoding = encoding, header = header0, sep = sep0)
  }
  quiet_reader <- quietly(reader)
  a <- quiet_reader("utf-8", fname,sep0, header0)
  b <- quiet_reader("CP949", fname,sep0, header0)
  
  if ( identical(a$warnings, character(0)) ) return(a$result)
  else return(b$result)
}  


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

# function for convert floating number with real number where integer is minute and fraction is digit to second 
# 6.30(6분 30초)를 6*60 + 30 = 390초로 변환
convert_real_to_sec <- function(x) {
  return(ifelse(is.na(x), NA, floor(x) * 60 + (x %% 1) * 100))
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

# calculate percent and freq by year and school

cal_percent_by_2 <- function(df) { 
  
  a <-  df %>% dplyr::select(`년도`, `시도`, `학교`,`체력요인`, `선택종목`,`학교명`,`성별`, `학교_학년`) %>%
    dplyr::filter(`년도` >= 2009) %>%
    dplyr::arrange(`년도`, `시도`, `학교`,`체력요인`, `선택종목`,`학교명`,`성별`, `학교_학년` ) %>%
    group_by(`년도`, `시도`, `학교`, `체력요인`, `선택종목`,`학교명`) %>% 
    slice_head(n = 1) %>% 
    ungroup() %>%
    group_by(`년도`, `학교`,`체력요인`, `선택종목`) %>%
    summarise( 학교수 =n() ,.groups = 'drop')   %>%   
    group_by(`년도`, `학교`,`체력요인`) %>%
    dplyr::mutate(비율 = round(`학교수` *100 / sum(`학교수`),2)) %>%
    dplyr::filter(!is.na(`체력요인`)) %>% 
    dplyr::ungroup() 
  
  df_n <- a %>% dplyr::select(-`비율`)  %>%
    dplyr::arrange(`학교`,`년도`) %>%
    pivot_wider(names_from = c( `체력요인`, `선택종목`), values_from = `학교수`) %>%
    dplyr::relocate(`학교`)
  
  df_p <- a %>% dplyr::select(-`학교수`)  %>%
    dplyr::arrange(`학교`,`년도`) %>%
    pivot_wider(names_from = c(`체력요인`, `선택종목`), values_from = `비율`)  %>%
    dplyr::relocate(`학교`)
  
  return(list(df_n, df_p, a))
  return(a)
}


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

make_table_2_plot <- function(df, sch, item ) {

pl <- df %>% dplyr::filter(`학교` ==  sch & `체력요인` == item ) %>%
  ggplot(aes(x = `년도`, y = `비율`, group = `선택종목`, color = `선택종목`, shape= `선택종목`)) +
  geom_line(alpha = 1) +
  geom_point(size=2) + 
  ylim(0,100) +
  labs(title = paste(sch,"-", item), y="비율(%)" ) +
  theme(plot.title = element_text(size=30),
        axis.text=element_text(size=40),
        axis.title=element_text(size=40),
        strip.text.x = element_text(size = 40,face="bold"),
        legend.text=element_text(size=40,face="bold"),
        legend.title=element_text(size=40,face="bold")) 

 fname_1 <- paste("체력요인선택그림-",sch,"-",item,".png", sep="")
 ggsave(here("data","outputs",fname_1), plot = pl, width = 10, height = 5, units = "in")

  return(pl)

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

## summarize by item

### 계산할 기초통계량을 정의량

my_summ_func <- list(
  학생수 = ~sum(!is.na(.x)),
  결측개수 = ~sum(is.na(.x)),
  평균 = ~mean(., na.rm = TRUE),
  표준편차 = ~sd(., na.rm = TRUE),
  최소값 = ~min(.x, na.rm = TRUE),
  백분위25 = ~quantile(., probs = 0.25, na.rm = TRUE),
  중앙값 = ~median(., na.rm = TRUE),
  배분위75 = ~quantile(., probs = 0.75, na.rm = TRUE),
  최대값 = ~max(.x, na.rm = TRUE)
)

### 학교별로 학년별로 학생수, 평균, 표준편차, 최소값, 25%, 75%, 최대값을 계산하는 함수
### 두 개의 결과 :
### 1. 학교별, 학년별로 계산된 통계량
### 2. 시도별, 학교별, 학년별로 계산된 통계량


summ_by_item_1var <- function(df, var1,  school_char, test_char ) {
  
  if (school_char == "초등학교") {
    df <- df %>% dplyr::filter( GRD >=5) 
  } 
  
  res1 <- df %>%  
        dplyr::select( SXDS_SC_CD, GRD, AYR,  {{var1}}) %>%
        group_by(  SXDS_SC_CD, GRD, AYR) %>% summarise(across(everything(), my_summ_func, .names = "{.fn}"), .groups="drop") %>%
        dplyr::filter( !is.na(SXDS_SC_CD) & SXDS_SC_CD != " ") 
  
  res2 <- df %>%  
    dplyr::select( SXDS_SC_CD,  GRD, AYR, region, {{var1}}) %>%
    group_by(  SXDS_SC_CD, GRD, AYR, region) %>% summarise(across(everything(), my_summ_func, .names = "{.fn}"), .groups="drop") %>%
    dplyr::filter( !is.na(SXDS_SC_CD) & SXDS_SC_CD != " ") 
  
  res2 <- make_rename_row_df(res2, region, var_change_vector_SIDO)
  
  res1 <- make_rename_col_df(res1, var_change_vector_PAPS)
  res2 <- make_rename_col_df(res2, var_change_vector_PAPS)
  
  res1 <- res1 %>%
    dplyr::mutate(학교 = school_char, 종목 = test_char) %>%
    dplyr::mutate(학교_학년 = paste(학교,학년,sep = '_')) %>%
    dplyr::relocate(종목, 성별, 학교_학년, 학교, 학년)
  
  res2 <- res2 %>%
    dplyr::mutate(학교 = school_char, 종목 = test_char) %>%
    dplyr::mutate(학교_학년 = paste(학교,학년,sep = '_')) %>%
    dplyr::relocate(종목, 성별, 학교_학년, 학교, 학년, 년도, 시도)

  res1[is.na(res1)] <- NA
  res2[is.na(res2)] <- NA
  
  res1[sapply(res1, is.infinite)] <- NA
  res2[sapply(res2, is.infinite)] <- NA
  
  return(list(res1,res2))
}

### 남녀, 연도별, 학교별로 학년별로 상자그림을 그리는 함수 
barplot_by_item_1var_year <- function(var1, test_char) {
  
  df_e <- df_e_01 %>%
     dplyr::filter( GRD >=5) %>%
     dplyr::filter( AYR %in% c(2011, 2015, 2019:2022 )) %>%
     dplyr::select( SXDS_SC_CD, GRD, AYR, {{var1}}) %>%
     dplyr::mutate(`학교_학년` = paste("초등학교",GRD,sep = '_')) %>%
     dplyr::relocate(`학교_학년`) 

  df_m <- df_m_01 %>%
    dplyr::filter( AYR  %in% c(2011, 2015, 2019:2022 )) %>%
    dplyr::select( SXDS_SC_CD, GRD, AYR, {{var1}}) %>%
    dplyr::mutate(`학교_학년` = paste("중학교",GRD,sep = '_')) %>%
    dplyr::relocate(`학교_학년`) 
  
  df_h <- df_h_01 %>%
    dplyr::filter( AYR  %in% c(2011, 2015, 2019:2022 )) %>%
    dplyr::select( SXDS_SC_CD, GRD, AYR, {{var1}}) %>%
    dplyr::mutate(`학교_학년` = paste("고등학교",GRD,sep = '_')) %>%
    dplyr::relocate(`학교_학년`) 
  
  df <- rbind(df_e,df_m,df_h)
  
  df <- df %>% dplyr::filter(!is.na({{var1}}))
  
  df <- make_rename_col_df(df, var_change_vector_PAPS)
  
  df$`학교_학년` <- factor(df$`학교_학년`, levels = grade_levels)

  df <- df %>%
    dplyr::filter( !is.na(`성별`) & `성별` != " ") 
  
  g <- ggplot(df, aes(x = `학교_학년`, y = !!sym(test_char) , fill = `성별`)) + 
    geom_boxplot(alpha = 0.3) +
    theme(axis.text=element_text(size=40),
          axis.title=element_text(size=50),
          strip.text.x = element_text(size = 50,face="bold"),
          legend.text=element_text(size=50,face="bold"),
          legend.title=element_text(size=40,face="bold")) + 
    facet_wrap(~`년도`, ncol = 1)  
    
  
  fname_1 <- paste("종목별분석-",test_char,"-boxplot",".png", sep="")
  ggsave(here("data","outputs",fname_1), plot = g, width = 10, height = 20, units = "in")
  
  return(g)
}

  
### 초중고 요약자료를 합쳐서 남여별로 그림과 표를 만드는 함수  
  
combine_summ_by_item_1var <- function(df1, df2, df3, test_char) { 
  
  df <- rbind(df1,df2,df3)

  
  df$`학교_학년` <- factor(df$`학교_학년`, levels = grade_levels)
  df$`학교` <- factor(df$`학교`, levels = school_levels)
  df$`년도` <- factor(df$`년도`, levels = 2009:2022)
  
  df <- df %>% dplyr::arrange(종목,성별,학교_학년,학교,학년, 년도)
  
  g_m <- df %>% dplyr::filter(성별 == '남성') %>%
      ggplot(aes(x = `년도`, y = `평균`, group = `학교_학년`, color = `학교_학년`, shape= `학교_학년`)) +
      geom_line(alpha = 0.5) +
      geom_point(size=1.5) +
     scale_shape_manual(values = c(15,16,15,16,17,15,16,17)) + 
     labs(title = paste(test_char,"-남성"),  x = "년도", y = "평균") +
     theme_bw() +
     theme(legend.position = "bottom",
           axis.text=element_text(size=18),
          axis.title=element_text(size=18),
          legend.text=element_text(size=18,face="bold"),
          legend.title=element_text(size=18,face="bold"))  
  
  g_f <- df %>% dplyr::filter(성별 == '여성') %>%
    ggplot(aes(x = `년도`, y = `평균`, group = `학교_학년`, color = `학교_학년`, shape= `학교_학년`)) +
    geom_line(alpha = 0.5) +
    geom_point(size=1.5) +
    scale_shape_manual(values = c(15,16,15,16,17,15,16,17)) + 
    labs(title = paste(test_char,"- 여성"),  x = "년도", y = "평균") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text=element_text(size=18),
          axis.title=element_text(size=18),          
          legend.text=element_text(size=18,face="bold"),
          legend.title=element_text(size=18,face="bold"))  
  
  df_t <- df %>% dplyr::select(-c(`학교`,`학년`))
  
  df_t_m <- df_t %>% dplyr::filter(성별 == '남성') %>% dplyr::select(-`성별`) 
  df_t_f <- df_t %>% dplyr::filter(성별 == '여성') %>% dplyr::select(-`성별`) 
  
  col_names <- colnames(df_t_m)
  
  ft1_m <- df_t_m %>%  dplyr::select(-`종목`) %>%   dplyr::group_by(`학교_학년`) %>%
    dplyr::mutate(is_last_val_in_group = row_number() == max(row_number())) |>
    flextable(col_keys = col_names ) |> 
    merge_v( j = 1 ) |> 
    valign(j = 1, valign = "top") |>
    autofit()  |>
    theme_booktabs(bold_header = TRUE) |>
    align(align = "center", part = "header", j =2:11) |> 
    colformat_double(digits = 2) |> 
    colformat_num(big.mark = "", decimal.mark = ".") |>
    hline(i = ~is_last_val_in_group == TRUE, border = fp_border()) |>
    fix_border_issues()
  
  col_names <- colnames(df_t_f)
  
  ft1_f <- df_t_f %>%  dplyr::select(-`종목`)  %>% dplyr::group_by(`학교_학년`) %>%
    dplyr::mutate(is_last_val_in_group = row_number() == max(row_number())) |>
    flextable(col_keys = col_names ) |> 
    merge_v( j = 1) |> 
    valign(j = 1, valign = "top") |>
    autofit()  |>
    theme_booktabs(bold_header = TRUE) |>
    align(align = "center", part = "header", j =2:11) |> 
    colformat_double(digits = 2) |> 
    colformat_num(big.mark = "", decimal.mark = ".") |>
    hline(i = ~is_last_val_in_group == TRUE, border = fp_border()) |>
    fix_border_issues()

  
  fname_1 <- paste("종목별분석-",test_char,"-남녀모두",".xlsx", sep="")

  fname_2m <- paste("종목별분석-",test_char,"-남자",".png", sep="")
  fname_2f <- paste("종목별분석-",test_char,"-여자",".png", sep="")
  
  write.xlsx(df_t, file = here("data","outputs",fname_1))
  ggsave(here("data","outputs",fname_2m), plot = g_m, width = 5, height = 4, units = "in")
  ggsave(here("data","outputs",fname_2f), plot = g_f, width = 5, height = 4, units = "in")
  
  return(list(g_m, g_f, ft1_m, ft1_f))
}


## summarize by school


summ_by_school_3var <- function(df, var1, var2, var3, school_char, test_char ) {

  df_1 <- df %>% dplyr::select(SHL_CD_NM, region, AYR, GRD, SXDS_SC_CD, {{var1}}, {{var2}}, {{var3}}) %>%
    group_by(SHL_CD_NM, region, AYR, GRD, SXDS_SC_CD) %>% 
    dplyr::summarize( N = n(), {{var1}} := mean({{var1}}, na.rm = TRUE), 
                    {{var2}} := mean({{var2}}, na.rm = TRUE),
                    {{var3}} := mean({{var3}}, na.rm = TRUE), .groups = 'drop')  


  df_2 <- df %>% dplyr::select(SHL_CD_NM, region, AYR, GRD, SXDS_SC_CD, {{var1}}, {{var2}}, {{var3}}) %>%
    group_by(SHL_CD_NM, region, AYR, GRD, SXDS_SC_CD) %>% 
    dplyr::summarize(    {{var1}} := sum(!is.na({{var1}})),
                       {{var2}} := sum(!is.na({{var2}})),
                       {{var3}} := sum(!is.na({{var3}})), .groups = 'drop' )  %>%
    dplyr::rename( a = {{var1}}, b = {{var2}}, c = {{var3}}) 


  df_3 <- df_2 %>% dplyr::select(SHL_CD_NM, region, AYR, GRD, SXDS_SC_CD,  a, b, c) %>%
    dplyr::rename( {{var1}} := a, {{var2}} := b, {{var3}} := c)  %>%
    pivot_longer(cols = -(1:5), names_to = "item", values_to = "value") %>%
    group_by(SHL_CD_NM, region, AYR, GRD, SXDS_SC_CD) %>%
    summarise(select_1_item = item[which.max(value)], select_1_n = max(value) ,.groups = 'drop')  

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
                      {{var2}} := mean({{var2}}, na.rm = TRUE), .groups = 'drop')  
  
  
  df_2 <- df %>% dplyr::select(SHL_CD_NM, region, AYR, GRD, SXDS_SC_CD, {{var1}}, {{var2}}) %>%
    group_by(SHL_CD_NM, region, AYR, GRD, SXDS_SC_CD) %>% 
    dplyr::summarize(    {{var1}} := sum(!is.na({{var1}})),
                         {{var2}} := sum(!is.na({{var2}})) ,.groups = 'drop')  %>%
    dplyr::rename( a = {{var1}}, b = {{var2}}) 
  
  
  df_3 <- df_2 %>% dplyr::select(SHL_CD_NM, region, AYR, GRD, SXDS_SC_CD,  a, b) %>%
    dplyr::rename( {{var1}} := a, {{var2}} := b)  %>%
    pivot_longer(cols = -(1:5), names_to = "item", values_to = "value") %>%
    group_by(SHL_CD_NM, region, AYR, GRD, SXDS_SC_CD) %>%
    summarise(select_1_item = item[which.max(value)], select_1_n = max(value) ,.groups = 'drop')  
  
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
    dplyr::filter(AYR >= 2009) %>%
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
  
  df_1 <- df %>% 
    dplyr::filter(`등급유형` == cd_name) %>%  
    dplyr::filter(`성별` == sex) %>% 
    dplyr::filter(!is.na(`등급`)) %>%
    dplyr::filter(`등급` != "0") 
  
  if (cd_name == "BDFAT_CLA_SCR_NM"){
    
    df_1$`등급` <- factor(df_1$`등급`, levels = bmi_levels)
  } else {
    df_1$`등급` <- factor(df_1$`등급`, levels = c("1","2","3","4","5"))
  }
  
  
  g <- list()
  
  for (i in 1:length(grade_levels)){
    
   g[[i]] <- df_1 %>%  
     dplyr::filter(`학교_학년` == grade_levels[i]) %>%
     ggplot( aes(fill=`등급`, x=factor(`년도`), y=`학생수`)) +
    geom_bar(position="fill", stat="identity") +
     scale_y_continuous(labels = scales::percent_format()) +  
    xlab("연도") +
    ylab("등급비율") +
     theme(legend.position = "bottom",
           plot.title = element_text(size=40),
           axis.text=element_text(size=20),
           axis.title=element_text(size=25),
           legend.text=element_text(size=25),
           legend.title=element_text(size=25)) +
    labs(title = paste(grade_levels[i], "_", sex, sep = "")) 
   
   fname_2 <- paste(filename,"-", sex,"-",i,"-",grade_levels[i],".png", sep="")
   ggsave(here("data","outputs",fname_2), plot = g[[i]], width = 7, height = 5, units = "in")
   
  }
#    facet_wrap(~`학교_학년`, ncol=1)

  if (cd_name == "BDFAT_CLA_SCR_NM") {
    
  a <- df_1 %>% dplyr::select(`년도`, `학교_학년`, `등급`, `학생수`) 
  
  a$`학교_학년` <- factor(a$`학교_학년`, levels = grade_levels)
  
  a <- a %>% 
    dplyr::filter(!is.na(`학교_학년`)) %>% as.data.frame()

    
  df_summ_n <- a %>% pivot_wider(names_from = `등급`, values_from = `학생수`, names_prefix="등급인원수_") %>%
    dplyr::arrange(`년도`, `학교_학년`) %>%
    dplyr::filter(!is.na(`학교_학년`))  %>%
    dplyr::relocate(`년도`, `학교_학년`, `등급인원수_마름`, `등급인원수_정상`, `등급인원수_과체중`, `등급인원수_경도비만`, `등급인원수_고도비만` ) 
  
  a <- df_1 %>%     dplyr::select(`년도`, `학교_학년`, `등급`, `비율`)
  
  df_summ_p <- a %>% pivot_wider(names_from = `등급`, values_from = `비율`, names_prefix="등급비율_") %>%
    dplyr::arrange(`년도`, `학교_학년`) %>%
    dplyr::filter(!is.na(`학교_학년`)) %>%
    dplyr::relocate(`년도`, `학교_학년`, `등급비율_마름`, `등급비율_정상`, `등급비율_과체중`, `등급비율_경도비만`, `등급비율_고도비만` ) 
  
  df_summ <- left_join(df_summ_n, df_summ_p, by=c("년도", "학교_학년"))  
  
  }  else {
    
    a <- df_1 %>% dplyr::select(`년도`, `학교_학년`, `등급`, `학생수`) 
    
    a$`학교_학년` <- factor(a$`학교_학년`, levels = grade_levels)
    
    a <- a %>% 
      dplyr::filter(!is.na(`학교_학년`)) %>% as.data.frame()
    
    
    df_summ_n <- a %>% pivot_wider(names_from = `등급`, values_from = `학생수`, names_prefix="등급인원수_") %>%
      dplyr::arrange(`년도`, `학교_학년`) %>%
      dplyr::filter(!is.na(`학교_학년`))  %>%
      dplyr::relocate(`년도`, `학교_학년`, `등급인원수_1`, `등급인원수_2`, `등급인원수_3`, `등급인원수_4`, `등급인원수_5` ) 
    
    a <- df_1 %>%     dplyr::select(`년도`, `학교_학년`, `등급`, `비율`)
    
    df_summ_p <- a %>% pivot_wider(names_from = `등급`, values_from = `비율`, names_prefix="등급비율_") %>%
      dplyr::arrange(`년도`, `학교_학년`) %>%
      dplyr::filter(!is.na(`학교_학년`)) %>%
      dplyr::relocate(`년도`, `학교_학년`, `등급비율_1`, `등급비율_2`, `등급비율_3`, `등급비율_4`, `등급비율_5` ) 
    
    df_summ <- left_join(df_summ_n, df_summ_p, by=c("년도", "학교_학년"))  
    
    
  }

  
  table_col <- colnames(df_summ_n) 
  
  ft1 <- df_summ_n  %>% as.data.frame()  %>% dplyr::group_by(`년도`) |>
    dplyr::mutate(is_last_val_in_group = row_number() == max(row_number())) |>
    flextable(col_keys = table_col ) |> 
    separate_header() |> 
    merge_v( j = 1 ) |> 
    valign(j = 1, valign = "top") |>
    autofit()  |>
    theme_booktabs(bold_header = TRUE) |>
    align(align = "center", part = "header", j =2:6) |> 
    colformat_double(digits = 1) |> 
    colformat_num(big.mark = "", decimal.mark = ".") |>
    hline(i = ~is_last_val_in_group == TRUE, border = fp_border()) |>
    width(j = 2:6, 1, unit = "in") |>
    fix_border_issues()
  
  table_col <- colnames(df_summ_p) 
  
  ft2 <- df_summ_p  %>% as.data.frame()  %>% dplyr::group_by(`년도`) |>
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
  
  fname_1 <- paste(filename,"-", sex,".xlsx", sep="")
  
  write.xlsx(df_summ, file = here("data","outputs",fname_1))
  
  
  list(g, ft1, ft2)
}


## 이상치 제거 함수 
## PAPS_col_outlier_treat.xlsx 가 필요함 

delete_outlier <- function(df, school_type, outlier_info){
  
  if (school_type == "초등"){
    df_new <- df %>% 
      dplyr::mutate(SHLR_NTS = case_when( 
        SHLR_NTS >= as.numeric(var_outlier_info[1,"min"]) & 
          SHLR_NTS <= as.numeric(var_outlier_info[1,"max"]) ~ SHLR_NTS,
        is.na(SHLR_NTS) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(MIN_SEC_LRW_HR = case_when( 
        MIN_SEC_LRW_HR >= as.numeric(var_outlier_info[3,"min"]) & 
          MIN_SEC_LRW_HR <= as.numeric(var_outlier_info[3,"max"]) ~ MIN_SEC_LRW_HR,
        is.na(MIN_SEC_LRW_HR) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(PEI_NMVL = case_when( 
        PEI_NMVL >= as.numeric(var_outlier_info[6,"min"]) & 
          PEI_NMVL <= as.numeric(var_outlier_info[6,"max"]) ~ PEI_NMVL,
        is.na(PEI_NMVL) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(BKRCH_NMVL = case_when( 
        BKRCH_NMVL >= as.numeric(var_outlier_info[7,"min"]) & 
          BKRCH_NMVL <= as.numeric(var_outlier_info[7,"max"]) ~ BKRCH_NMVL,
        is.na(BKRCH_NMVL) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(GNRLZ_FLX_NMVL = case_when( 
        GNRLZ_FLX_NMVL >= as.numeric(var_outlier_info[8,"min"]) & 
          GNRLZ_FLX_NMVL <= as.numeric(var_outlier_info[8,"max"]) ~ GNRLZ_FLX_NMVL,
        is.na(GNRLZ_FLX_NMVL) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(PSP_NTS = case_when( 
        PSP_NTS >= as.numeric(var_outlier_info[9,"min"]) & 
          PSP_NTS <= as.numeric(var_outlier_info[9,"max"]) &
          SXDS_SC_CD == '여성' ~ PSP_NTS,
        PSP_NTS >= as.numeric(var_outlier_info[10,"min"]) & 
          PSP_NTS <= as.numeric(var_outlier_info[10,"max"]) &
          SXDS_SC_CD == '남성' ~ PSP_NTS,
        is.na(PSP_NTS) ~ NA ,
        .default = NA))  %>%
      dplyr::mutate(CLUP_NTS = case_when( 
        CLUP_NTS >= as.numeric(var_outlier_info[11,"min"]) & 
          CLUP_NTS <= as.numeric(var_outlier_info[11,"max"]) ~ CLUP_NTS,
        is.na(CLUP_NTS) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(GRIP_NMVL = case_when( 
        GRIP_NMVL >= as.numeric(var_outlier_info[12,"min"]) & 
          GRIP_NMVL <= as.numeric(var_outlier_info[12,"max"]) ~ GRIP_NMVL,
        is.na(GRIP_NMVL) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(R50M_REC_HR = case_when( 
        R50M_REC_HR >= as.numeric(var_outlier_info[13,"min"]) & 
          R50M_REC_HR <= as.numeric(var_outlier_info[13,"max"]) ~ R50M_REC_HR,
        is.na(R50M_REC_HR) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(SLJ_NMVL = case_when( 
        SLJ_NMVL >= as.numeric(var_outlier_info[14,"min"]) & 
          SLJ_NMVL <= as.numeric(var_outlier_info[14,"max"]) ~ SLJ_NMVL,
        is.na(SLJ_NMVL) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(BMI_IDEX = case_when( 
        BMI_IDEX >= as.numeric(var_outlier_info[15,"min"]) & 
          BMI_IDEX <= as.numeric(var_outlier_info[15,"max"]) ~ BMI_IDEX,
        is.na(BMI_IDEX) ~ NA ,
        .default = NA )) %>%
      dplyr::mutate(STU_HGHT = case_when(
        STU_HGHT >= as.numeric(var_outlier_info[16,"min"]) & 
          STU_HGHT <= as.numeric(var_outlier_info[16,"max"]) ~ STU_HGHT, 
        is.na(STU_HGHT) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(STU_BWGHT = case_when( 
        STU_BWGHT >= as.numeric(var_outlier_info[17,"min"]) & 
          STU_BWGHT <= as.numeric(var_outlier_info[17,"max"]) ~ STU_BWGHT,
        is.na(STU_BWGHT) ~ NA ,
        .default = NA)) 
  }
  else
  { 
    df_new <- df %>% 
      dplyr::mutate(SHLR_NTS = case_when( 
        SHLR_NTS >= as.numeric(var_outlier_info[2,"min"]) & 
          SHLR_NTS <= as.numeric(var_outlier_info[2,"max"]) ~ SHLR_NTS,
        is.na(SHLR_NTS) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(MIN_SEC_LRW_HR = case_when( 
        MIN_SEC_LRW_HR >= as.numeric(var_outlier_info[4,"min"]) & 
          MIN_SEC_LRW_HR <= as.numeric(var_outlier_info[4,"max"]) &
          SXDS_SC_CD == '여성' ~ MIN_SEC_LRW_HR,
        MIN_SEC_LRW_HR >= as.numeric(var_outlier_info[5,"min"]) & 
          MIN_SEC_LRW_HR <= as.numeric(var_outlier_info[5,"max"]) &
          SXDS_SC_CD == '남성' ~ MIN_SEC_LRW_HR,
        is.na(MIN_SEC_LRW_HR) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(PEI_NMVL = case_when( 
        PEI_NMVL >= as.numeric(var_outlier_info[6,"min"]) & 
          PEI_NMVL <= as.numeric(var_outlier_info[6,"max"]) ~ PEI_NMVL,
        is.na(PEI_NMVL) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(BKRCH_NMVL = case_when( 
        BKRCH_NMVL >= as.numeric(var_outlier_info[7,"min"]) & 
          BKRCH_NMVL <= as.numeric(var_outlier_info[7,"max"]) ~ BKRCH_NMVL,
        is.na(BKRCH_NMVL) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(GNRLZ_FLX_NMVL = case_when( 
        GNRLZ_FLX_NMVL >= as.numeric(var_outlier_info[8,"min"]) & 
          GNRLZ_FLX_NMVL <= as.numeric(var_outlier_info[8,"max"]) ~ GNRLZ_FLX_NMVL,
        is.na(GNRLZ_FLX_NMVL) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(PSP_NTS = case_when( 
        PSP_NTS >= as.numeric(var_outlier_info[9,"min"]) & 
          PSP_NTS <= as.numeric(var_outlier_info[9,"max"]) &
          SXDS_SC_CD == '여성' ~ PSP_NTS,
        PSP_NTS >= as.numeric(var_outlier_info[10,"min"]) & 
          PSP_NTS <= as.numeric(var_outlier_info[10,"max"]) &
          SXDS_SC_CD == '남성' ~ PSP_NTS,
        is.na(PSP_NTS) ~ NA ,
        .default = NA))  %>%
      dplyr::mutate(CLUP_NTS = case_when( 
        CLUP_NTS >= as.numeric(var_outlier_info[11,"min"]) & 
          CLUP_NTS <= as.numeric(var_outlier_info[11,"max"]) ~ CLUP_NTS,
        is.na(CLUP_NTS) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(GRIP_NMVL = case_when( 
        GRIP_NMVL >= as.numeric(var_outlier_info[12,"min"]) & 
          GRIP_NMVL <= as.numeric(var_outlier_info[12,"max"]) ~ GRIP_NMVL,
        is.na(GRIP_NMVL) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(R50M_REC_HR = case_when( 
        R50M_REC_HR >= as.numeric(var_outlier_info[13,"min"]) & 
          R50M_REC_HR <= as.numeric(var_outlier_info[13,"max"]) ~ R50M_REC_HR,
        is.na(R50M_REC_HR) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(SLJ_NMVL = case_when( 
        SLJ_NMVL >= as.numeric(var_outlier_info[14,"min"]) & 
          SLJ_NMVL <= as.numeric(var_outlier_info[14,"max"]) ~ SLJ_NMVL,
        is.na(SLJ_NMVL) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(BMI_IDEX = case_when( 
        BMI_IDEX >= as.numeric(var_outlier_info[15,"min"]) & 
          BMI_IDEX <= as.numeric(var_outlier_info[15,"max"]) ~ BMI_IDEX,
        is.na(BMI_IDEX) ~ NA ,
        .default = NA )) %>%
      dplyr::mutate(STU_HGHT = case_when(
        STU_HGHT >= as.numeric(var_outlier_info[16,"min"]) & 
          STU_HGHT <= as.numeric(var_outlier_info[16,"max"]) ~ STU_HGHT, 
        is.na(STU_HGHT) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(STU_BWGHT = case_when( 
        STU_BWGHT >= as.numeric(var_outlier_info[17,"min"]) & 
          STU_BWGHT <= as.numeric(var_outlier_info[17,"max"]) ~ STU_BWGHT,
        is.na(STU_BWGHT) ~ NA ,
        .default = NA)) 
    
  }
  
  return(df_new)
  
}

## 이상치 제거 함수 
## PAPS_col_outlier_treat.xlsx 가 필요함 

delete_outlier_retest <- function(df, outlier_info){
  
    df_new <- df %>% 
      dplyr::mutate(SHLR_NTS = case_when( 
        SHLR_NTS >= as.numeric(var_outlier_info[1,"min"]) & 
          SHLR_NTS <= as.numeric(var_outlier_info[1,"max"]) ~ SHLR_NTS,
        is.na(SHLR_NTS) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(BKRCH_NMVL = case_when( 
        BKRCH_NMVL >= as.numeric(var_outlier_info[7,"min"]) & 
          BKRCH_NMVL <= as.numeric(var_outlier_info[7,"max"]) ~ BKRCH_NMVL,
        is.na(BKRCH_NMVL) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(CLUP_NTS = case_when( 
        CLUP_NTS >= as.numeric(var_outlier_info[11,"min"]) & 
          CLUP_NTS <= as.numeric(var_outlier_info[11,"max"]) ~ CLUP_NTS,
        is.na(CLUP_NTS) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(GRIP_NMVL = case_when( 
        GRIP_NMVL >= as.numeric(var_outlier_info[12,"min"]) & 
          GRIP_NMVL <= as.numeric(var_outlier_info[12,"max"]) ~ GRIP_NMVL,
        is.na(GRIP_NMVL) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(R50M_REC_HR = case_when( 
        R50M_REC_HR >= as.numeric(var_outlier_info[13,"min"]) & 
          R50M_REC_HR <= as.numeric(var_outlier_info[13,"max"]) ~ R50M_REC_HR,
        is.na(R50M_REC_HR) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(SLJ_NMVL = case_when( 
        SLJ_NMVL >= as.numeric(var_outlier_info[14,"min"]) & 
          SLJ_NMVL <= as.numeric(var_outlier_info[14,"max"]) ~ SLJ_NMVL,
        is.na(SLJ_NMVL) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(BMI_IDEX = case_when( 
        BMI_IDEX >= as.numeric(var_outlier_info[15,"min"]) & 
          BMI_IDEX <= as.numeric(var_outlier_info[15,"max"]) ~ BMI_IDEX,
        is.na(BMI_IDEX) ~ NA ,
        .default = NA )) %>%
      dplyr::mutate(STU_HGHT = case_when(
        STU_HGHT >= as.numeric(var_outlier_info[16,"min"]) & 
          STU_HGHT <= as.numeric(var_outlier_info[16,"max"]) ~ STU_HGHT, 
        is.na(STU_HGHT) ~ NA ,
        .default = NA)) %>%
      dplyr::mutate(STU_BWGHT = case_when( 
        STU_BWGHT >= as.numeric(var_outlier_info[17,"min"]) & 
          STU_BWGHT <= as.numeric(var_outlier_info[17,"max"]) ~ STU_BWGHT,
        is.na(STU_BWGHT) ~ NA ,
        .default = NA)) 

  return(df_new)
  
}


## 생년 자료에서 종목을 선택하고 발달상황과 표 작성 
summ_by_birthyear <- function(df, df2, varname, sex){ 
  
  a03 <- df %>% dplyr::filter(선택종목 == varname) 
  a032 <- df2 %>% dplyr::filter(선택종목 == varname) 
  
  ft <- a032 |> 
    flextable(col_keys = names(birthyear_w)) |> 
    #separate_header() |> 
    autofit()  |>
    theme_booktabs(bold_header = TRUE) |>
    #align(align = "center", part = "header", j = 2:13) |> 
    colformat_num(big.mark = "", decimal.mark = ".") |>
    colformat_double(digits = 2) |>
    fix_border_issues()
  
  
  g <- a03 %>% ggplot(aes(x = `학교_학년`, y = 전국평균, group = `생년`,color=`생년`, shape=`생년`)) +
    geom_line() +
    geom_point(size=2) +
    scale_shape_manual(values = 15:24) + 
    labs(title = varname, x = "학교_학년", y = "전국평균") +
    theme(legend.position = "bottom",
          plot.title = element_text(size=40),
          axis.text=element_text(size=20),
          axis.title=element_text(size=25),
          legend.text=element_text(size=25),
          legend.title=element_text(size=25)) +
    theme(legend.position = "bottom")
  
  fname <- paste("세대분석_",varname,"_",sex,".png", sep="")
  ggsave(here("data","outputs",fname), plot = g, width = 7, height = 5, units = "in")
  
  
  return(list(g,ft))
}


