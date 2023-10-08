library(tidyverse)
data  <-  read_csv("/Users/shihaitao/Documents/DarknetProject/Nemesis/complete.csv")
colnames(data)


relation <- data %>% select(Vendor_Name,Drug_Type_L)
relation_table <- table(relation)  

relation_df <- as.data.frame.matrix(relation_table)
long_df <- relation_df %>% 
  rownames_to_column("Vendor_Name") %>% 
  pivot_longer(cols = -Vendor_Name, names_to = "Drug_Type_L", values_to = "Freq") %>% 
  filter(Freq > 0)  # 只保留 Freq 大于 0 的行

edge_list <- data.frame(Source = character(), Target = character(), Weight = numeric())


long_df %>%
  group_by(Vendor_Name) %>%
  filter(n() > 1) %>% # 只保留有超过一个药品的 Vendor
  group_by(Vendor_Name) %>%
  nest() %>%
  mutate(
    cooccur = map(data, ~combn(.x$Drug_Type_L, 2, simplify = TRUE) %>%
                    t() %>%
                    as.data.frame(stringsAsFactors = FALSE) %>%
                    setNames(c("Source", "Target")))
  ) %>%
  select(-data) %>%
  unnest(cols = c(cooccur)) %>%
  count(Source, Target, name = "Weight") -> edge_list

Edge_list <- edge_list %>% ungroup() %>% select(Source,Target,Weight) %>% group_by(Source,Target) %>% summarise(Weight=sum(Weight)) %>% ungroup()


write.csv(Edge_list, "edge_list.csv", row.names = FALSE)



long_df %>% select(Drug_Type_L,Freq) %>% group_by(Drug_Type_L) %>% summarise(Freq=sum(Freq))
