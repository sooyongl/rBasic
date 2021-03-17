library(tidyverse)
getwd()

fs::dir_ls()
data_list <- fs::dir_ls("KCYPS2010 m1[SPSS]")

args(foreign::read.spss)

# import spss data 
data_w1 <- foreign::read.spss(data_list[1],
                         use.value.labels = F,
                         to.data.frame = T)
data_w1 <- data_w1 %>% tbl_df()

# data select / rename
"ID"
"PSY2A"
"PSY2B"
"GENDERw1"

data_w1[, "PSY2A01w1"]
data_w1[, c("PSY2A01w1","PSY2B01w1")]

data_w1[, 103]

data_w1[["PSY2A01w1"]]
data_w1[[103]]

data_w1[, c(paste0("PSY2A0",1:9, "w1"), "PSY2A10w1")]
data_w1[, 103:112]

data_w1 %>% 
  select("PSY2A10w1")

data_w1 %>% 
  select(103)

new.data <- 
  data_w1 %>% 
  select(ID, 
         GENDERw1,
         starts_with("PSY2A"),
         starts_with("PSY2B"))

# starts_with(); ends_with(); contains(); matches(); num_range(); one_of(); everything(); group_cols()

# tibble::rownames_to_column()

data_w1 %>% 
  select(p2a_10_w1 = PSY2A10w1)

data_w1 %>% 
  rename(id_num = ID)

vars <- c(var1 = "cyl", var2 ="am")
new.data %>% rename_if(startsWith(names(.), "A"), ~paste0("df1_", .))

new.data %>% rename_at(vars(starts_with("PSY2A")), ~paste0("selfes_", .))

data_w1_1 <- 
  new.data %>% 
  rename_at(vars(contains('PSY2A')), ~(sub('PSY2A', 'selfes_', .))) %>% 
  rename_at(vars(contains('PSY2B')), ~(sub('PSY2B', 'good_', .)))

new.data %>% set_names(~sub('PSY2A', 'self_', .x))

new.data %>% select_all(~gsub("PSY2A", "self_", .))

new.data %>% select_all(., list(~ toupper(.)))

classification <- c("name", "genus", "vore", "order", "conservation")

msleep %>%
  select(!!classification)

msleep %>%
  select_if(is.numeric) %>%
  glimpse

msleep %>%
  select_if(is.numeric) %>%
  select_if(~mean(., na.rm=TRUE) > 10)

msleep %>%
  select_if(~is.numeric(.) & mean(., na.rm=TRUE) > 10)

msleep2 %>%
  select_all(~str_replace(., " ", "_"))

mtcars %>%
  tibble::rownames_to_column("car_model") %>%
  head

########################################################################
# Transformation

data_w1_1 %>%
  # select(matches("^selfes")) %>%
  select(3:5) %>%
  mutate(mean_selfes = (selfes_01w1 + selfes_02w1 + selfes_03w1)/3,
         mean_cent_w1 = selfes_01w1 - mean_selfes)

data_w1_1 %>%
  select(3:5) %>%
  rowwise() %>% 
  mutate(mean_selfes = mean(selfes_01w1 + selfes_02w1 + selfes_03w1),
         mean_cent_w1 = selfes_01w1 - mean_selfes) %>% 
  mutate(over = ifelse(selfes_01w1 > mean_selfes, 0, 1)) %>% # or case_when
  mutate_at(vars(contains("selfes")), ~(.*10))


msleep %>%
  mutate(conservation2 = recode(conservation,
                                "en" = "Endangered",
                                "lc" = "Least_Concern",
                                "domesticated" = "Least_Concern",
                                .default = "other")) %>%
  count(conservation2)

msleep %>%
  select(name, conservation) %>%
  mutate(conservation = toupper(conservation)) %>%
  left_join(conservation_table, by = c("conservation" = "abbreviation")) %>%
  mutate(description = ifelse(is.na(description), conservation, description))

msleep %>%
  select(name:order) %>%
  na_if("omni")

msleep %>% 
  select(name, sleep_total) %>% 
  filter(between(sleep_total, 16, 18))

msleep %>% 
  select(name, sleep_total) %>% 
  filter(near(sleep_total, 17, tol = sd(sleep_total)))

remove <- c("Rodentia", "Carnivora", "Primates")
msleep %>% 
  select(order, name, sleep_total) %>% 
  filter(!order %in% remove)

msleep %>% 
  select(name, sleep_total) %>% 
  filter(str_detect(tolower(name), pattern = "mouse"))

msleep %>% 
  select(name, conservation:sleep_cycle) %>% 
  filter(!is.na(conservation))

msleep %>%
  select(order, name, sleep_total) %>%
  group_by(order) %>%
  arrange(desc(sleep_total), .by_group = TRUE)

msleep %>%
  group_by(order) %>%
  summarise(average = mean(sleep_total)) %>%
  top_n(5)

msleep %>%
  group_by(order) %>%
  summarise(average_sleep = mean(sleep_total), max_sleep = max(sleep_total)) %>%
  top_n(5, average_sleep)

msleep %>%
  slice(50:55)

