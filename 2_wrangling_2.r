# Install packages ----------------------------------
# install.packages(c("tidyverse","readxl"))

# Load packages -------------------------------------
library(tidyverse);library(readxl) 

# Set the working directory -------------------------
dir <- getwd()
setwd(dir)

# Explore the files -------------------------------
fs::dir_ls()

# Import SPSS files ----------------------------------------
data_list <- fs::dir_ls("KCYPS2010 m1[SPSS]", regexp = ".sav$")
data_w1 <- foreign::read.spss(data_list[1],
                              use.value.labels = F,
                              to.data.frame = T)

# Import EXCEL files ----------------------------------------
data_list <- fs::dir_ls("KCYPS2010 m1[EXCEL]", regexp = ".xlsx$")
data_w1 <- readxl::read_xlsx(data_list[1])


# Wrangle data ----------------------------------------
data_w1 <- data_w1 %>% tbl_df()

data_w1_v1 <- 
  data_w1 %>% 
  select(ID, 
         GENDERw1,
         starts_with("PSY2A"),
         starts_with("PSY2B")) %>% 
  rename_at(vars(contains('PSY2A')), ~(sub('PSY2A', 'selfes_', .))) %>% 
  rename_at(vars(contains('PSY2B')), ~(sub('PSY2B', 'confidence_', .))) %>%
  rename("Gender" = GENDERw1 ) %>% 
  na_if(., -9) %>% 
  mutate(
    mean_selfes = rowMeans(select(., contains("selfes_")), na.rm = T),
    mean_conf = rowMeans(select(., contains("confidence_")), na.rm = T)
         ) %>% 
  select(ID, Gender, matches("^mean_")) %>% 
  mutate(Gender = case_when(Gender == 1 ~ "Girl", TRUE ~ "Boy")) %>% 
  filter(Gender == "Girl")

# Save the cleaned data ----------------------------------------
write_csv(data_w1_v1, "data_w1_v1.csv")

# Read the data ----------------------------------------
read_csv("data_w1_v1.csv")

# Additional stuff ---------------------------------------
# data_w1_v1[data_w1_v1 == -9] <- NA
# mutate_all(~ case_when(. == -9 ~ NA, TRUE ~ .))
# replace(., is.na(.), "")
# mutate_all(replace_na, "")
# mutate_all(~ case_when(is.na(.) ~ "", TRUE ~ as.character(.)))
# mutate_all(~ case_when(is.na(.)~ 0, TRUE ~ 1))  

# data_import <- map(data_list, readxl::read_excel)


# Import multiple data
data_list <- fs::dir_ls("KCYPS2010 m1[EXCEL]")

data_container <- list()
for(i in 1:length(data_list)) {
  # i <- 2
  data_w <- readxl::read_excel(data_list[i])
  # names(data_w)
  
  varName <- paste0("mean_selfes_w",i)
  
  data_w <- 
    data_w %>% 
    select(ID, 
           starts_with("GENDER"),
           starts_with("PSY2A")) %>% 
    rename_at(vars(contains('PSY2A')), ~(sub('PSY2A', 'selfes_', .))) %>% 
    rename_at(vars(contains('Gender')), ~(sub(., 'Gender', .))) %>% 
    na_if(., -9) %>% 
    mutate(
      !!varName := rowMeans(select(., contains("selfes_")), na.rm = T)
    ) %>% 
    select(ID, Gender, matches("^mean_")) %>% 
    mutate(Gender = case_when(Gender == 1 ~ "Girl", TRUE ~ "Boy"))
  
  data_container[[i]] <- data_w
}

# data_container %>% bind_rows(., .id = "timepoint")


# Merge data
merged <-
  data_container[[1]] %>% 
  left_join(., data_container[[2]], by = "ID") %>% 
  left_join(data_container[[3]], by = "ID") %>% 
  left_join(data_container[[4]], by = "ID") %>% 
  left_join(data_container[[5]], by = "ID") %>% 
  left_join(data_container[[6]], by = "ID") %>% 
  left_join(data_container[[7]], by = "ID") %>% 
  select(-contains("Gender")) %>% 
  left_join(., data_container[[1]][,c("ID","Gender")], by = "ID") %>% 
  select(ID, Gender, everything())

names(merged) <- c("ID", "gen",paste0("w",1:7))


# test SEM
library(lavaan)
girl.merged <- merged %>% filter(gen == "Girl")
model = '
  
  I =~ 1*w1 + 1*w3 + 1*w5 + 1*w6 + 1*w7
  S =~ 0*w1 + 1*w3 + 2*w5 + 3*w6 + 4*w7

'
fit <- lavaan::sem(model = model,
                   data = girl.merged)

summary(fit, fit.measures=TRUE)
