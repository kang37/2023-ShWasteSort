library(tidyverse)
library(readxl)

# 读取列名对应表
colname_mapping <- read_excel("data_raw/colname_mapping.xlsx")

# 创建列名重命名函数
create_rename_vector <- function(mapping_df, year) {
  col_year <- paste0("col_", year)
  mapping_year <- mapping_df %>%
    filter(!is.na(.data[[col_year]])) %>%
    select(old_name = all_of(col_year), new_name = unified_name_en)
  setNames(mapping_year$old_name, mapping_year$new_name)
}

# 批量读取数据
years <- 2019:2023
ws_list <- map2(
  paste0("data_raw/SHWS", years, ".xlsx"), 
  years, 
  function(file, year) {
    read_excel(file) %>%
      rename(any_of(create_rename_vector(colname_mapping, year))) %>% 
      mutate(year = as.factor(year))
  }
)

# 所有需要重编码的变量（合并为一个向量）
vars_2019_recode <- c(
  # 信息质量 (11个)
  "info_qual_tv", "info_qual_news", "info_qual_web", "info_qual_ad", 
  "info_qual_gov", "info_qual_school", "info_qual_street", 
  "info_qual_volunteer", "info_qual_neighbor", "info_qual_family", 
  "info_qual_property",
  # 联系频率 (10个)
  "freq_online_secondhand", "freq_gov", "freq_ngo", "freq_media", 
  "freq_school", "freq_waste_co", "freq_resident", "freq_street", 
  "freq_property", "freq_supervisor",
  # 部门作用 (9个)
  "role_gov", "role_ngo", "role_media", "role_school", "role_waste_co",
  "role_resident", "role_street", "role_property", "role_supervisor",
  # 部门支持 (9个)
  "support_gov", "support_ngo", "support_media", "support_school",
  "support_waste_co", "support_resident", "support_street", 
  "support_property", "support_supervisor"
)

# 通用重编码函数：a→1, b→2, c→3, d→4, e→5, -3→NA
recode_2019_letters <- function(x) {
  case_when(
    str_detect(x, "^a") ~ 1,  # 以a开头的任何字符串 → 1
    str_detect(x, "^b") ~ 2,  # 以b开头的任何字符串 → 2
    str_detect(x, "^c") ~ 3,  # 以c开头的任何字符串 → 3
    str_detect(x, "^d") ~ 4,  # 以d开头的任何字符串 → 4
    str_detect(x, "^e") ~ 5,  # 以e开头的任何字符串 → 5
    x == "-3" | x == -3 ~ NA_real_,  # -3 → NA
    TRUE ~ as.numeric(x)      # 其他情况转为数字（如果已经是数字）
  )
}

# 一次性处理所有变量。
ws_list[[1]] <- ws_list[[1]] %>%
  mutate(across(any_of(vars_2019_recode), recode_2019_letters))

# 处理其他有问题的变量。
# 老人数量。
# Bug：还要删除异常值。
ws_list[[2]] <- ws_list[[2]] %>% 
  mutate(across(any_of("elder_num"), as.numeric))
ws_list[[4]] <- ws_list[[4]] %>% 
  mutate(across(any_of("elder_num"), as.numeric))
ws_list[[5]] <- ws_list[[5]] %>% 
  mutate(across(any_of("elder_num"), as.numeric))
# 年龄。
ws_list[[4]] <- ws_list[[4]] %>% 
  mutate(across(any_of("age"), as.numeric))

# 生成完整数据框
ws_full <- bind_rows(ws_list)

# 生成共有变量数据框
share_colname <- reduce(map(ws_list, colnames), intersect)
ws_all <- bind_rows(map(ws_list, ~select(.x, all_of(share_colname))))
