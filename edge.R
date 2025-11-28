library(tidyverse)
library(readxl)

# 定义年份和文件路径。
years <- 2019:2023

# 批量读取和处理所有年份数据。
ws_list <- map2(
  paste0("data_raw/SHWS", years, ".xlsx"), 
  paste0("data_proc/namelist", substr(years, 3, 4), ".csv"), 
  function(file, namelist_file) {
    # 读取列名映射表。
    namelist <- read_csv(namelist_file, show_col_types = FALSE)
    
    # 读取数据并统一列名，替换缺失值为NA
    read_excel(file)
      # rename_with(~namelist$new_name) 
  }
)

# 添加年份变量
ws_list <- map2(ws_list, years, ~mutate(.x, year = as.factor(.y)))

# ============================================================================
# 生成两个数据框
# ============================================================================

# 1. 包含所有年份所有变量的完整数据框（全外连接）
ws_full <- bind_rows(ws_list) 
  select(-batch)  # 移除批次标识列

# 2. 找出所有年份的共有变量
share_colname <- reduce(map(ws_list, colnames), intersect)

# 只包含共有变量的数据框
ws_all <- map(ws_list, ~select(.x, all_of(share_colname))) %>%
  bind_rows()

# ============================================================================
# 输出信息
# ============================================================================

cat("✓ 数据处理完成\n")
cat("  - 共有变量数量:", length(share_colname), "\n")
cat("  - 完整数据框 (ws_full) 维度:", paste(dim(ws_full), collapse = " × "), "\n")
cat("  - 共有变量数据框 (ws_all) 维度:", paste(dim(ws_all), collapse = " × "), "\n")
cat("  - 总样本量:", nrow(ws_all), "\n\n")

# 查看共有变量列表（可选）
cat("共有变量列表:\n")
print(share_colname)

# ============================================================================
# 可选：查看23年特定变量分布（示例）
# ============================================================================

# 如果需要查看特定年份的变量分布
ws_23 <- ws_list[[5]]  # 2023年数据

check_vars <- c("transac_secondhand", "freq_online_secondhand", 
                "sens_of_oblg", "volun_expr", "sort_self", "put_self")

# 检查变量是否存在
check_vars_exist <- check_vars[check_vars %in% colnames(ws_23)]

if(length(check_vars_exist) > 0) {
  cat("\n2023年变量分布:\n")
  lapply(ws_23[, check_vars_exist], table, useNA = "ifany")
}