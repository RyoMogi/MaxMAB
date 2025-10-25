## Period
countryHFD <- getHFDcountries()
PDBx <- PDBx %>% 
  as.data.frame() %>% 
  rename(CNTRY = Code, Cohort = Year) %>% 
  left_join(countryHFD, by = "CNTRY") %>% 
  select(-CNTRY, -link)

PDBx_countryper <- func_cleandata(data = PDBx, outcome = "country&bc", minage = 12, maxage = 55)

PBT_all <- func_birthtable(PDBx_countryper, vars = c("Country", "Year"))

perex <- PBT_all %>% 
  filter(Age == 12) %>% 
  select(Country, Year, ex) %>%
  mutate(ex = ex + 12) %>% 
  group_by(Year) %>% 
  mutate(N_country = n()) %>% 
  slice(which.max(ex))

perex %>% 
  # here we can change the threshold
  filter(N_country >= 10) %>% 
  mutate(cntry_n = paste0(Country, " (", N_country, ")")) %>% 
  ggplot(aes(x = Year, y = ex, label = cntry_n)) +
  geom_line() +
  geom_point() +
  geom_text(size = 2, nudge_y = 0.15) +
  theme_bw() +
  theme(axis.title.y = element_blank())
ggsave("out/period_ex.png", width = 8, height = 6)

# selected countries
PBT_all %>% 
  filter(Age == 12,
         Country %in% c("Spain", "Italy", "Netherlands", "Finland", "Switzerland", "Republic.of.Korea", "Japan")) %>% 
  select(Country, Year, ex) %>%
  mutate(ex = ex + 12) %>% 
  group_by(Year) %>%
  mutate(max_ex = max(ex, na.rm = T),
         max_val = ifelse(ex == max_ex, "1", "0")) %>%
  ggplot(aes(x = Year, y = ex, group = Country, colour = Country, alpha = max_val)) +
  geom_line() +
  geom_point() +
  scale_alpha_discrete(range = c(0.3, 1), guide = "none") +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position = "top",
        legend.title = element_blank())
ggsave("out/period_ex_selcntry.png", width = 7.5, height = 6)

# all countries (grey)
PBT_all %>% 
  filter(Age == 12) %>% 
  select(Country, Year, ex) %>%
  mutate(ex = ex + 12) %>% 
  group_by(Year) %>%
  mutate(selcntry = ifelse(Country %out% c("Spain", "Italy", "Netherlands", "Finland", 
                                           "Republic of Korea", "Japan"), "none", Country),
         selcntry = factor(selcntry, levels = c("none", "Spain", "Italy", "Netherlands", "Finland", 
                                                "Republic of Korea", "Japan")),
         max_ex = max(ex, na.rm = T),
         max_val = ifelse(ex == max_ex, "1", "0")) %>% 
  ggplot(aes(x = Year, y = ex, group = Country, colour = selcntry, alpha = max_val)) +
  geom_line() +
  geom_point() +
  scale_colour_manual(values = c("grey", Mycol[1:5], "skyblue")) +
  scale_alpha_discrete(range = c(0.3, 1), guide = "none") +
  labs(x = "Year") +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position = "top",
        legend.title = element_blank())
ggsave("out/period_ex_selcntry_all.png", width = 7.5, height = 6)

## Cohort
CDBx <- CDBx %>% 
  as.data.frame() %>% 
  rename(CNTRY = Code) %>% 
  left_join(countryHFD, by = "CNTRY") %>% 
  select(-CNTRY, -link)

DBx_countrybc <- func_cleandata(data = CDBx, outcome = "country&bc", minage = 12, maxage = 55)

# for all parity
BT_all <- func_birthtable(DBx_countrybc, vars = c("Country", "Year"))

cohex <- BT_all %>% 
  filter(Age == 12) %>% 
  select(Country, Year, ex) %>%
  mutate(ex = ex + 12) %>% 
  group_by(Year) %>% 
  mutate(N_country = n()) %>% 
  slice(which.max(ex))

cohex %>% 
  # here we can change the threshold
  filter(N_country >= 10) %>% 
  mutate(cntry_n = paste0(Country, " (", N_country, ")")) %>% 
  ggplot(aes(x = Year, y = ex, label = cntry_n)) +
  geom_point() +
  geom_text(size = 2, nudge_y = 0.15) +
  theme_bw() +
  theme(axis.title.y = element_blank())
ggsave("out/cohort_ex.png", width = 7.5, height = 5)

# selected countries
BT_all %>% 
  filter(Age == 12,
         Country %in% c("Spain", "Italy", "Netherlands", "Finland", "Switzerland", "Republic.of.Korea", "Japan")) %>% 
  select(Country, Year, ex) %>%
  mutate(ex = ex + 12) %>% 
  group_by(Year) %>%
  mutate(max_ex = max(ex, na.rm = T),
         max_val = ifelse(ex == max_ex, "1", "0")) %>%
  ggplot(aes(x = Year, y = ex, group = Country, colour = Country, alpha = max_val)) +
  geom_line() +
  geom_point() +
  scale_alpha_discrete(range = c(0.3, 1), guide = "none") +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position = "top",
        legend.title = element_blank())
ggsave("out/cohort_ex_selcntry.png", width = 7.5, height = 6)

# all countries (grey)
BT_all %>% 
  filter(Age == 12) %>% 
  select(Country, Year, ex) %>%
  mutate(ex = ex + 12) %>% 
  group_by(Year) %>%
  mutate(selcntry = ifelse(Country %out% c("Spain", "Italy", "Netherlands", "Finland", 
                                           "Switzerland", "Republic.of.Korea", "Japan"), "none", Country),
         selcntry = factor(selcntry, levels = c("none", "Spain", "Italy", "Netherlands", "Finland", 
                                                "Switzerland", "Republic.of.Korea", "Japan")),
         max_ex = max(ex, na.rm = T),
         max_val = ifelse(ex == max_ex, "1", "0")) %>% 
  ggplot(aes(x = Year, y = ex, group = Country, colour = selcntry, alpha = max_val)) +
  geom_line() +
  geom_point() +
  scale_colour_manual(values = c("grey", Mycol[1:5], "red", "darkgreen")) +
  scale_alpha_discrete(range = c(0.3, 1), guide = "none") +
  labs(x = "Year") +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position = "top",
        legend.title = element_blank())
ggsave("out/cohort_ex_selcntry_all.png", width = 7.5, height = 6)

# limited till age of 40
DBx_countrybc_40 <- func_cleandata(data = CDBx, outcome = "country&bc", minage = 12, maxage = 40)
BT_all_40 <- func_birthtable(DBx_countrybc_40, vars = c("Country", "Year"))

cohex40 <- BT_all_40 %>% 
  filter(Age == 12) %>% 
  select(Country, Year, ex) %>%
  mutate(ex = ex + 12) %>% 
  group_by(Year) %>% 
  mutate(N_country = n()) %>% 
  slice(which.max(ex))

cohex40 %>% 
  # here we can change the threshold
  filter(N_country >= 10) %>% 
  mutate(cntry_n = paste0(Country, " (", N_country, ")")) %>% 
  ggplot(aes(x = Year, y = ex, label = cntry_n)) +
  geom_point() +
  geom_text(size = 2, nudge_y = 0.15) +
  theme_bw() +
  theme(axis.title.y = element_blank())
ggsave("out/cohort_ex_age40.png", width = 7.5, height = 5)
