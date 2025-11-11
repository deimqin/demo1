rm(list = ls())

# Helper function
add_zero <- function(x) {
  tibble(x_text = as.character(x)) %>% 
    mutate(n_digits = str_count(x_text),
           n_max = max(n_digits, na.rm = TRUE), 
           delta = n_max - n_digits, 
           pre = strrep("0", times = delta), 
           full_code = str_c(pre, x_text)) %>% 
    pull(full_code) %>% 
    return()
}

library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(haven)

# Xác định hộ có dữ liệu cả 3 năm
read_dta("C:/Users/My PC/VHLSS 2016/Bang hoi VHLSS 2016/Data VHLSS 2016/Ho1.dta") -> ho1_2016

ho1_2016 <- ho1_2016 %>% 
  mutate(h_code16 = str_c(add_zero(tinh), add_zero(huyen), add_zero(xa), add_zero(diaban), add_zero(hoso)),
         h_code14 = str_c(add_zero(tinh14), add_zero(huyen14), add_zero(xa14), add_zero(diaban14), add_zero(hoso14)))

h_code_16_14 <- ho1_2016 %>% 
  filter(h_code16 == h_code14) %>% 
  pull(h_code16) %>% 
  unique()

read_dta("C:/Users/My PC/VHLSS 2104/VHLSS 2104/VHLSS2014_Households/Ho1.dta") -> ho1_2014

ho1_2014 <- ho1_2014 %>% 
  mutate(h_code14 = str_c(add_zero(tinh), add_zero(huyen), add_zero(xa), add_zero(diaban), add_zero(hoso)),
         h_code12 = str_c(add_zero(tinh12), add_zero(huyen12), add_zero(xa12), add_zero(diaban12), add_zero(hoso12)))

h_code_14_12 <- ho1_2014 %>%
  filter(h_code14 == h_code12) %>% 
  pull(h_code14) %>% 
  unique()

h_code_common <- base::intersect(h_code_16_14, h_code_14_12)

# Xử lý dữ liệu thực phẩm
process_food <- function(file_path, year) {
  read_dta(file_path) %>%
    filter(m5a2ma >= 101 & m5a2ma <= 154 & !m5a2ma %in% c(144, 145, 146, 147, 148, 149)) %>%
    mutate(h_code = str_c(add_zero(tinh), add_zero(huyen), add_zero(xa), add_zero(diaban), add_zero(hoso))) %>% 
    filter(h_code %in% h_code_common) %>% 
    select(tinh, h_code, food_con = m5a2c2b) %>%
    group_by(h_code, tinh) %>% 
    summarise(total_food_con = sum(food_con, na.rm = TRUE), .groups = 'drop') %>%
    mutate(tinh = as.numeric(tinh), year = year)
}

data_food_consumption <- bind_rows(
  process_food("C:\\Users\\My PC\\VHLSS 2016\\Bang hoi VHLSS 2016\\Data VHLSS 2016\\Muc5a2.dta", 2016),
  process_food("C:\\Users\\My PC\\VHLSS 2104\\VHLSS 2104\\VHLSS2014_Households\\Muc5a2.dta", 2014),
  process_food("C:\\Users\\My PC\\VHLSS 2012\\VHLSS 2012\\Muc5a2.dta", 2012)
)

# Chỉ giữ hộ có đủ 3 năm dữ liệu
full_16_14_12 <- data_food_consumption %>% 
  group_by(h_code) %>% 
  count() %>% 
  filter(n == 3) %>% 
  pull(h_code)

# 1: TARGET - Lấy total_food_con từ năm 2014
target_2014 <- data_food_consumption %>% 
  filter(h_code %in% full_16_14_12, year == 2014) %>%
  select(h_code, total_food_con_2014 = total_food_con)

# 2: FEATURES - Lấy tất cả biến từ năm 2014

# Đọc dữ liệu bổ sung
read_dta("C:/Users/My PC/VHLSS 2104/VHLSS 2104/VHLSS2014_Households/Muc1A.dta") -> muc1_2014

# Quy mô hộ gia đình năm 2014
household_size_2014 <- ho1_2014 %>% 
  select(h_code14, tsnguoi) %>% 
  rename(h_code = h_code14, household_size = tsnguoi) %>% 
  filter(h_code %in% full_16_14_12)

# Đặc điểm chủ hộ năm 2014
household_heads_2014 <- muc1_2014 %>% 
  filter(m1ac3 == 1) %>% 
  mutate(h_code = str_c(add_zero(tinh), add_zero(huyen), add_zero(xa), add_zero(diaban), add_zero(hoso))) %>% 
  select(h_code, head_gender = m1ac2, head_age = m1ac5, area_type1 = m1ac11, area_type2 = m1ac12) %>% 
  mutate(across(c(head_gender, head_age, area_type1, area_type2), as.numeric)) %>%
  filter(h_code %in% full_16_14_12)

# Function xử lý dữ liệu năm 2014
process_data_2014 <- function(file_path, columns) {
  read_dta(file_path) %>% 
    mutate(h_code = str_c(add_zero(tinh), add_zero(huyen), add_zero(xa), add_zero(diaban), add_zero(hoso))) %>% 
    filter(h_code %in% full_16_14_12) %>%
    select(h_code, all_of(columns)) %>%
    mutate(across(where(is.labelled), as.numeric)) %>%
    group_by(h_code) %>%
    summarise(across(everything(), ~first(na.omit(.))), .groups = 'drop')
}

# Dữ liệu y tế năm 2014
muc3a_2014 <- process_data_2014(
  "C:/Users/My PC/VHLSS 2104/VHLSS 2104/VHLSS2014_Households/Muc3B.dta", 
  c("m3c5a", "m3c5b", "m3c6a", "m3c6b")
)

muc3b_2014 <- process_data_2014(
  "C:/Users/My PC/VHLSS 2104/VHLSS 2104/VHLSS2014_Households/Muc3C.dta", 
  c("m3c11", "m3c12a", "m3c12b", "m3c13", "m3c14", "m3c15")
)

# Thu nhập năm 2014
income_2014 <- process_data_2014(
  "C:/Users/My PC/VHLSS 2104/VHLSS 2104/VHLSS2014_Households/Ho3.dta", 
  c("thubq", "thunhap")
)

# Tổng thu nhập năm 2014
sumincome_2014 <- process_data_2014(
  "C:/Users/My PC/VHLSS 2104/VHLSS 2104/VHLSS2014_Households/Ho2.dta", 
  c("m4atn", "m4dtn")
)

# Lấy mã tỉnh từ năm 2014
tinh_2014 <- data_food_consumption %>% 
  filter(h_code %in% full_16_14_12, year == 2014) %>%
  select(h_code, tinh)

# 3: MERGE 
food_consumption_14 <- target_2014 %>% 
  left_join(tinh_2014, by = "h_code") %>%
  left_join(household_size_2014, by = "h_code") %>%
  left_join(household_heads_2014, by = "h_code") %>%
  left_join(muc3a_2014, by = "h_code") %>%
  left_join(muc3b_2014, by = "h_code") %>%
  left_join(income_2014, by = "h_code") %>%
  left_join(sumincome_2014, by = "h_code")

# Đổi tên biến
food_consumption_14 <- food_consumption_14 %>%
  rename(
    # Target
    actual_food_2014 = total_food_con_2014,
    
    # Features từ 2014
    # Sử dụng dịch vụ
    outpatient_visits = m3c5a,          
    outpatient_cost = m3c5b,            
    inpatient_visits = m3c6a,           
    inpatient_cost = m3c6b,             
    
    # Bảo hiểm
    insurance_premium = m3c11,          
    insurance_outpatient = m3c12a,      
    insurance_inpatient = m3c12b,       
    
    # Chi phí khác
    medicine_cost = m3c13,              
    equipment_cost = m3c14,             
    health_subsidy = m3c15,              
    
    # Thu nhập
    salary = m4atn,
    other_income = m4dtn
  ) %>%
  # Sắp xếp lại cột: target cuối cùng
  relocate(actual_food_2014, .after = last_col())

# Xuất file
write.csv(food_consumption_14, "C:/Users/My PC/food_consumption_14.csv", row.names = FALSE)


