
# Data import -------------------------------------------------------------

dpc_2019 <- read_csv("data/ichinomiya/dpc_2019.csv",
                     col_names = FALSE,
                     locale = locale(encoding = "SHIFT-JIS"))
dpc_2020 <- read_csv("data/ichinomiya/dpc_2020.csv",
                     col_names = FALSE,
                     locale = locale(encoding = "SHIFT-JIS"))
dpc_2021 <- read_csv("data/ichinomiya/dpc_2021.csv",
                     col_names = FALSE,
                     locale = locale(encoding = "SHIFT-JIS"))
act_2019 <- read_tsv("data/ichinomiya/download_acts_2018.txt",
                     locale = locale(encoding = "SHIFT-JIS"))
act_2020 <- read_tsv("data/ichinomiya/download_acts_2019.txt",
                     locale = locale(encoding = "SHIFT-JIS"))
act_2021 <- read_tsv("data/ichinomiya/download_acts_2020.txt",
                     locale = locale(encoding = "SHIFT-JIS"))
cases_2019 <- read_tsv("data/ichinomiya/download_cases_2018.txt",
                     locale = locale(encoding = "SHIFT-JIS"))
cases_2020 <- read_tsv("data/ichinomiya/download_cases_2019.txt",
                       locale = locale(encoding = "SHIFT-JIS"))
cases_2021 <- read_tsv("data/ichinomiya/download_cases_2020.txt",
                       locale = locale(encoding = "SHIFT-JIS"))
culture2019 <- read_csv("data/ichinomiya/culture2019.csv", 
                        locale = locale(encoding = "SHIFT-JIS"), 
                        na = "－", skip = 1)
culture2020 <- read_csv("data/ichinomiya/culture2020.csv", 
                        locale = locale(encoding = "SHIFT-JIS"), 
                        na = "－", skip = 1)
culture2021 <- read_csv("data/ichinomiya/culture2021.csv", 
                        locale = locale(encoding = "SHIFT-JIS"), 
                        na = "－", skip = 1)
lab_2019 <- read_csv("data/ichinomiya/lab_data/ichinomiya_lab/2019.csv", 
                         locale = locale(encoding = "SHIFT-JIS"),
                         na = "－",
                         skip = 1)
lab_2020 <- read_csv("data/ichinomiya/lab_data/ichinomiya_lab/2020.csv", 
                     locale = locale(encoding = "SHIFT-JIS"),
                     na = "－",
                     skip = 1)
lab_2021 <- read_csv("data/ichinomiya/lab_data/ichinomiya_lab/2021.csv", 
                     locale = locale(encoding = "SHIFT-JIS"),
                     na = "－",
                     skip = 1)

# DPC ---------------------------------------------------------------------

dpc_2019 %>% dim() # not suitable
dpc_2020 %>% dim() # not suitable
dpc_2021 %>% dim() # not suitable 
cases_2019 %>% dim()
cases_2020 %>% dim()
cases_2021 %>% dim()

## DPC2019
cases_2019 <- cases_2019 %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  select_if(~sum(!is.na(.)) > 0)
cases_2019 %>% colnames() %>% 
  as.data.frame() %>% 
  write.csv("memo/dpc2019.csv",
            fileEncoding = "SHIFT-JIS") # edit in EXCEL
memo_2019 <- read_csv("memo/dpc2019_after.csv",
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
  unite(ope_day, starts_with("ope_day"),
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

# another way
# col_names_2019 <- cases_2019 %>%
#  names() %>% 
#  dput()

## DPC2020
cases_2020 <- cases_2020 %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  select_if(~sum(!is.na(.)) > 0)
cases_2020 %>% colnames() %>% 
  as.data.frame() %>% 
  write.csv("memo/dpc2020.csv",
            fileEncoding = "SHIFT-JIS") # edit in EXCEL
memo_2020 <- read_csv("memo/dpc2020_after.csv",
                      locale = locale(encoding = "SHIFT-JIS"))
num <- memo_2020 %>% 
  pull(1)
old <- memo_2020 %>% 
  pull(2)
new <- memo_2020 %>% 
  pull(5)
cases_2020 <- cases_2020 %>% 
  select(num)
cases_2020 <- cases_2020 %>% 
  rename_at(vars(old), ~ new)
cases_2020 <- cases_2020 %>% 
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
  unite(ope_day, starts_with("ope_day"),
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
cases_2020 <- cases_2020 %>% 
  select(-matches("\\d+$"))
cases_2020 %>% write.csv("cleaned_data/ichinomiya/dpc_2020.csv")

## DPC2021
cases_2021 <- cases_2021 %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  select_if(~sum(!is.na(.)) > 0)
cases_2021 %>% colnames() %>% 
  as.data.frame() %>% 
  write.csv("memo/dpc2021.csv",
            fileEncoding = "SHIFT-JIS") # edit in EXCEL
memo_2021 <- read_csv("memo/dpc2021_after.csv",
                      locale = locale(encoding = "SHIFT-JIS"))
num <- memo_2021 %>% 
  pull(1)
old <- memo_2021 %>% 
  pull(2)
new <- memo_2021 %>% 
  pull(5)
cases_2021 <- cases_2021 %>% 
  select(num)
cases_2021 <- cases_2021 %>% 
  rename_at(vars(old), ~ new)
cases_2021 <- cases_2021 %>% 
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
  unite(ope_day, starts_with("ope_day"),
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
cases_2021 <- cases_2021 %>% 
  select(-matches("\\d+$"))
cases_2021 %>% write.csv("cleaned_data/ichinomiya/dpc_2021.csv")

## Combine the datasets
cases_2019 %>% colnames() %>% length()
cases_2020 %>% colnames() %>% length()
cases_2021 %>% colnames() %>% length()
dpc <- bind_rows(cases_2019, cases_2020)
dpc <- bind_rows(dpc, cases_2021)
dpc %>% glimpse()
dpc %>% colnames()
dpc <- dpc %>% 
  distinct(id, adm, .keep_all=TRUE) 
dpc %>% write.csv("cleaned_data/ichinomiya/dpc.csv")
request <- dpc %>% 
  filter(str_detect(main, "J86")) %>% 
  select(id, adm)
request %>% write.csv("request/ichinishi_request.csv")

# Procedures --------------------------------------------------------------

## abx 2019
abx_2019 <- act_2019 %>% 
  filter(薬効分類2 == "抗生物質製剤") %>% 
  rename(id = "データ識別番号",
         adm = "入院日",
         day = "実施日付",
         name = "薬効分類7"
         ) %>% 
  arrange(id, adm, day) %>%  
  select(id, adm, day, name) %>%
  group_by(id, adm, name) %>% 
  mutate(day = ymd(day),
         length = max(day) - min(day) + 1) %>% 
  ungroup() %>% 
  distinct(id, adm, name, .keep_all=TRUE) 

## procedure 2020
abx_2020 <- act_2020 %>% 
  filter(薬効分類2 == "抗生物質製剤") %>% 
  rename(id = "データ識別番号",
         adm = "入院日",
         day = "実施日付",
         name = "薬効分類7"
  ) %>% 
  arrange(id, adm, day) %>%  
  select(id, adm, day, name) %>%
  group_by(id, adm, name) %>% 
  mutate(day = ymd(day),
         length = max(day) - min(day) + 1) %>% 
  ungroup() %>% 
  distinct(id, adm, name, .keep_all=TRUE) 

## procedure 2021
abx_2021 <- act_2021 %>% 
  filter(薬効分類2 == "抗生物質製剤") %>% 
  rename(id = "データ識別番号",
         adm = "入院日",
         day = "実施日付",
         name = "薬効分類7"
  ) %>% 
  arrange(id, adm, day) %>%  
  select(id, adm, day, name) %>%
  group_by(id, adm, name) %>% 
  mutate(day = ymd(day),
         length = max(day) - min(day) + 1) %>% 
  ungroup() %>% 
  distinct(id, adm, name, .keep_all=TRUE) 

## Combine the datasets
abx <- bind_rows(abx_2019, abx_2020)
abx <- bind_rows(abx, abx_2021)
abx <- abx %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  arrange(id, adm, day) %>% 
  distinct(id, adm, name, .keep_all=TRUE)
abx %>% write.csv("cleaned_data/ichinomiya/abx.csv")

# drainage 2019
drainage_2019 <- act_2019 %>% 
  rename(id = "データ識別番号",
         adm = "入院日",
         day = "実施日付",
         name = "マスタ解釈番号（基本）"
  ) %>% 
  filter(name == "J019" | name == "J0021" | name == "J008") %>% 
  arrange(id, adm, day) %>%  
  select(id, adm, day, name) %>%
  group_by(id, adm, name) %>% 
  mutate(day = ymd(day),
         length = max(day) - min(day) + 1) %>% 
  ungroup() %>% 
  distinct(id, adm, name, .keep_all=TRUE) 

# drainage 2020
drainage_2020 <- act_2020 %>% 
  rename(id = "データ識別番号",
         adm = "入院日",
         day = "実施日付",
         name = "マスタ解釈番号（基本）"
  ) %>% 
  filter(name == "J019" | name == "J0021" | name == "J008") %>% 
  arrange(id, adm, day) %>%  
  select(id, adm, day, name) %>%
  group_by(id, adm, name) %>% 
  mutate(day = ymd(day),
         length = max(day) - min(day) + 1) %>% 
  ungroup() %>% 
  distinct(id, adm, name, .keep_all=TRUE) 

# drainage 2021
drainage_2021 <- act_2021 %>% 
  rename(id = "データ識別番号",
         adm = "入院日",
         day = "実施日付",
         name = "マスタ解釈番号（基本）"
  ) %>% 
  filter(name == "J019" | name == "J0021" | name == "J008") %>% 
  arrange(id, adm, day) %>%  
  select(id, adm, day, name) %>%
  group_by(id, adm, name) %>% 
  mutate(day = ymd(day),
         length = max(day) - min(day) + 1) %>% 
  ungroup() %>% 
  distinct(id, adm, name, .keep_all=TRUE) 


# Culture -----------------------------------------------------------------

## culture 2019
culture2019 %>% colnames()
culture2019 <- culture2019 %>% 
  rename(id = "...2",
         day = "...3",
         organ = "材料名称...203") %>%
  filter(!is.na(organ)) %>% 
  select_if(~sum(!is.na(.)) > 0) 
culture2019 %>% dim()
culture2019 <- culture2019 %>% 
  select(1:385, -1, -(4:57))
culture2019 %>% write.csv("cleaned_data/ichinomiya/culture2019.csv")

## culture 2020
culture2020 %>% colnames()
culture2020 <- culture2020 %>% 
  rename(id = "...2",
         day = "...3",
         organ = "材料名称...203") %>%
  filter(!is.na(organ)) %>% 
  select_if(~sum(!is.na(.)) > 0) 
culture2020 %>% dim()
culture2020 <- culture2020 %>% 
  select(1:367, -1, -(4:54))
culture2020 %>% write.csv("cleaned_data/ichinomiya/culture2020.csv")

## culture 2021
culture2021 %>% colnames()
culture2021 <- culture2021 %>% 
  rename(id = "...2",
         day = "...3",
         organ = "材料名称...203") %>%
  filter(!is.na(organ)) %>% 
  select_if(~sum(!is.na(.)) > 0) 
culture2021 %>% dim()
culture2021 <- culture2021 %>% 
  select(1:378, -1, -(4:56))
culture2021 %>% write.csv("cleaned_data/ichinomiya/culture2021.csv")

# Lab ---------------------------------------------------------------------

## lab 2019
lab_2019 %>% colnames()
lab_2019 <- lab_2019 %>% 
  rename(id = "...2",
         day = "...3",
         name = "検査名称",
         result = "結果") %>%
  select(id, day, name, result)
lab_2019 <- lab_2019 %>% 
  mutate(day = date((ymd_hms(day))))

## lab 2020
lab_2020 %>% colnames()
lab_2020 <- lab_2020 %>% 
  rename(id = "...2",
         day = "...3",
         name = "検査名称",
         result = "結果") %>%
  select(id, day, name, result)
lab_2020 <- lab_2020 %>% 
  mutate(day = date((ymd_hms(day))))

## lab 2021
lab_2021 %>% colnames()
lab_2021 <- lab_2021 %>% 
  rename(id = "...2",
         day = "...3",
         name = "検査名称",
         result = "結果") %>%
  select(id, day, name, result)
lab_2021 <- lab_2021 %>% 
  mutate(day = date((ymd_hms(day))))

## Combine
lab <- bind_rows(lab_2019, lab_2020)
lab <- bind_rows(lab, lab_2021)
lab <- lab %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  arrange(id, desc(day)) 
lab$name %>% unique()
lab %>% write.csv("cleaned_data/ichinomiya/lab.csv")
