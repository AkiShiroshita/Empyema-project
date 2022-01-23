
# Data import -------------------------------------------------------------

dpc <- read_excel("data/yokohama_dpc.xlsx")

# DPC ---------------------------------------------------------------------

dpc <- dpc %>% 
  mutate_all(.funs = ~ as.character(.)) 
dpc %>% colnames() 
total <- dpc %>% 
  filter(コード == "A006010") 
request <- total %>% 
  rename(name = "ペイロード2(コード等)") %>% 
  filter(name == "J869" | name == "J860") %>% 
  select(2,3) %>% 
  write.csv("request/yokohama_request.csv")
  

















## request 
request <- dpc %>% 
  rename(id = "匿名化ID",
         adm = "006 入院年月日2",
         tag = "195 主傷病名ICD10コード") %>% 
  filter(str_detect(tag, "J86")) %>% 
  select(id, adm)
request %>% write.csv("request/kumamoto_request.csv")
##
memo_2019 <- read_csv("memo/kuma_dpc_after.csv",
                      locale = locale(encoding = "SHIFT-JIS"))
num <- memo_2019 %>% 
  pull(1)
old <- memo_2019 %>% 
  pull(2)
new <- memo_2019 %>% 
  pull(5)
cases_2019 <- cases_2019 %>% 
  select(num)
cases_2019 <- cases_2019 %>% 
  rename_at(vars(old), ~ new)
cases_2019 <- cases_2019 %>% 
  unite(k, starts_with("k"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(route, starts_with("route"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(prognosis, starts_with("prognosis"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(height, starts_with("height"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(weight, starts_with("weight"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(main, starts_with("main"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(prep, starts_with("prep"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(reso, starts_with("reso"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(com1, starts_with("com1"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(com2, starts_with("com2"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(com3, starts_with("com3"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>% 
  unite(com4, starts_with("com4"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(subs1, starts_with("subs1"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(subs2, starts_with("subs2"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(subs3, starts_with("subs3"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(subs4, starts_with("subs4"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(ope_day1, starts_with("ope_day1"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(ope_day2, starts_with("ope_day2"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(ope_day3, starts_with("ope_day3"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(ope_day4, starts_with("ope_day4"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(ope_day5, starts_with("ope_day5"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(adl, starts_with("adl"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE) %>%
  unite(jcs, starts_with("jcs"),
        sep = "_",
        remove = FALSE,
        na.rm = TRUE)
cases_2019 <- cases_2019 %>% 
  select(-matches("\\d+$"))
cases_2019 %>% write.csv("cleaned_data/ichinomiya/dpc_2019.csv")

# oral --------------------------------------------------------------------

# Caution: check drugs after cleaning !!!
abx_list <- read_excel("memo/abx_list.xlsx")
abx_list <- abx_list %>% 
  rename(name = "商品名")
uk <- abx_list %>% 
  slice(2:4) %>% 
  pull(name)
abx <- abx_list %>% 
  slice(7:346) %>% 
  drop_na(name) %>% 
  pull(name)
oral %>% glimpse()
oral <- oral %>% 
  rename(id = "匿名化ID",
         adm = "入院年月日",
         day = "行為日時",
         drug = "薬剤名称") %>% 
  mutate(day = date(day),
         adm = date(adm))
memo <- str_c(abx, collapse = "|") 
oral <- oral %>% 
  filter(str_detect(drug, memo)) %>% 
  select(id, adm, day, drug) %>%
  arrange(id, adm, day) %>% 
  group_by(id, adm, drug) %>% 
  mutate(length = max(day) - min(day) + 1) %>% 
  ungroup() %>% 
  distinct(id, adm, drug, .keep_all=TRUE)
oral <- oral %>% 
  mutate_all(.funs = ~ as.character(.))

# IV ----------------------------------------------------------------------

# Caution: check drugs after cleaning !!!
iv %>% glimpse()
iv <- iv %>% 
  rename(id = "匿名化ID",
         adm = "入院年月日",
         day = "開始日",
         drug = "薬剤名称") %>% 
  mutate(day = date(day),
         adm = date(adm))
abx2 <- abx_list %>% 
  slice(423:816) %>% 
  drop_na(1) %>% 
  pull(1)
memo2 <- str_c(abx2, collapse = "|")
iv <- iv %>% 
  filter(str_detect(drug, memo2)) %>% 
  select(id, adm, day, drug) %>%
  arrange(id, adm, day) %>% 
  group_by(id, adm, drug) %>% 
  mutate(length = max(day) - min(day) + 1) %>% 
  ungroup() %>% 
  distinct(id, adm, drug, .keep_all=TRUE)
iv <- iv %>% 
  mutate_all(.funs = ~ as.character(.))

# Gram stain --------------------------------------------------------------

gram <- gram %>% 
  rename(id = "匿名化ID",
         adm = "入院年月日",
         day = "採取日時",
         organ = "検体",
         strain = "菌名",
         result = "結果") %>% 
  filter(str_detect(result, "\\+")) %>% 
  select(id, adm, day, organ, strain, result) %>% 
  mutate(adm = date(adm),
         day = date(day)) %>% 
  mutate_all(.funs = ~ as.character(.))

# Culture -----------------------------------------------------------------

culture <- culture %>% 
  rename(id = "匿名化ID",
         adm = "入院年月日",
         day = "採取日時",
         organ = "検体",
         strain = "菌名") %>% 
  filter(!str_detect(strain, "No Growth")) %>% 
  select(id, adm, day, organ, strain) %>% 
  mutate(adm = date(adm),
         day = date(day)) %>% 
  mutate_all(.funs = ~ as.character(.))


# Lab ---------------------------------------------------------------------

lab <- lab %>% 
  rename(id = "匿名化ID",
         adm = "006 入院年月日2",
         day = "検体受付日",
         organ = "検体名称",
         name = "検査項目名称",
         result = "検査結果値") %>% 
  mutate(adm = date(adm),
         day = date(day)) %>% 
  filter(adm == day | adm == day - 1)
lab$organ %>% unique()
lab$name %>% unique()
efussion <- lab %>% 
  filter(organ == "穿刺")
blood <- lab %>% 
  filter(organ == "採血")
