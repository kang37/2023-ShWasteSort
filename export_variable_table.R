# ============================================================================
# 输出各构念指标变量对应的问卷原文表格
# ============================================================================
pacman::p_load(dplyr, readxl, stringr)

colname_mapping <- read_excel("data_raw/colname_mapping.xlsx")

# ----------------------------------------------------------------------------
# 1. 构念-变量归属定义
# ----------------------------------------------------------------------------
construct_map <- tribble(
  ~construct,    ~role,              ~unified_name_en,
  # ATT
  "ATT",         "态度",             "ws_attitude",
  "ATT",         "态度",             "ws_interest",
  "ATT",         "态度",             "threat",
  # INJ_NORM
  "INJ_NORM",    "指令性规范",       "pr_atten",
  "INJ_NORM",    "指令性规范",       "regulate_law",
  "INJ_NORM",    "指令性规范",       "regulate_commu_rule",
  # DESC_NORM
  "DESC_NORM",   "描述性规范",       "if_neighbor_ws",
  "DESC_NORM",   "描述性规范",       "if_family_ws",
  # PBC
  "PBC",         "感知行为控制",     "category_trouble",
  "PBC",         "感知行为控制",     "time_cost_troub",
  # BI
  "BI",          "行为意图",         "wil_of_engage",
  # BEH
  "BEH",         "行为",             "seper_recyc",
  # GreenBehav（溢出）
  "GreenBehav",  "绿色行为（溢出）", "reuse_bag",
  "GreenBehav",  "绿色行为（溢出）", "energy_concern",
  "GreenBehav",  "绿色行为（溢出）", "save_energy"
)

# ----------------------------------------------------------------------------
# 2. 从 colname_mapping 提取原文，去除题号前缀
# ----------------------------------------------------------------------------
strip_qnum <- function(x) {
  # 去掉开头的 "数字." / "数字)" / "数字 " / "数字、" 等题号格式，保留正文
  str_trim(str_remove(x, "^\\d+[)）、.。\\s]+\\t?"))
}

mapping_sub <- colname_mapping %>%
  filter(unified_name_en %in% construct_map$unified_name_en) %>%
  select(
    unified_name_en,
    简短描述        = unified_name_cn,
    问卷原文_2021   = col_2021,
    问卷原文_2022   = col_2022,
    问卷原文_2023   = col_2023,
    var_scale5_rev
  ) %>%
  mutate(
    across(starts_with("问卷原文"), strip_qnum),
    反向计分 = ifelse(!is.na(var_scale5_rev) & var_scale5_rev == 1, "是", "否")
  ) %>%
  select(-var_scale5_rev)

# ----------------------------------------------------------------------------
# 3. 合并构念信息
# ----------------------------------------------------------------------------
var_table <- construct_map %>%
  left_join(mapping_sub, by = "unified_name_en") %>%
  rename(
    构念代码     = construct,
    构念含义     = role,
    变量名       = unified_name_en
  ) %>%
  # 以构念顺序排列
  mutate(构念代码 = factor(构念代码, levels = c(
    "ATT", "INJ_NORM", "DESC_NORM", "PBC", "BI", "BEH", "GreenBehav"
  ))) %>%
  arrange(构念代码) %>%
  mutate(构念代码 = as.character(构念代码))

# ----------------------------------------------------------------------------
# 4. 输出
# ----------------------------------------------------------------------------
out_path <- "data_proc/variable_question_table.csv"
write.csv(var_table, out_path, row.names = FALSE, fileEncoding = "UTF-8")
cat("表格已保存：", out_path, "\n\n")

# 控制台预览
print(var_table %>% select(构念代码, 构念含义, 变量名, 简短描述, 反向计分, 问卷原文_2021),
      n = Inf)
