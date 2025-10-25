## figure 1: maximum mab
PMAB_HFD <- MAB_HFD[-1, ] %>% 
  rename(year = COUNTRY) %>% 
  gather(key = Country, value = MAB, -year) %>% 
  filter(Country %out% c("Germany.East", "Germany.West", "United.Kingdom",
                         "UK.-.Northern.Ireland", "UK.-.Scotland", "Ireland")) %>% 
  mutate(MAB = as.numeric(as.character(MAB)),
         MAB = ifelse(MAB == 0, NA, MAB),
         year = as.numeric(as.character(year)))

PMAB <- PMAB_HFD %>% 
  drop_na() %>% 
  group_by(year) %>%
  mutate(N_country = n()) %>% 
  slice(which.max(MAB)) %>% 
  rename(max_country = Country, max_PMAB = MAB) %>% 
  filter(year >= 1979,
         N_country >= 10)

dpmab <- PMAB_HFD %>% 
  mutate(Country = ifelse(Country == "Republic.of.Korea", "South Korea", Country),
         selcntry = ifelse(Country %out% c("Spain", "Netherlands", "Italy", 
                                           "Switzerland", "South Korea"), "Others", Country),
         selcntry = factor(selcntry, levels = c("Spain", "Netherlands", "Italy", 
                                                "Switzerland", "South Korea", "Others"))) %>% 
  group_by(year) %>% 
  mutate(max_PMAB = max(MAB, na.rm = T),
         max_val = ifelse(MAB == max_PMAB, "1", "0")) %>% 
  ungroup() %>% 
  filter(year <= 2020)

# get the coefficients of fitted line
fitline <- dpmab %>% 
  filter(max_val == 1,
         year >= 1979) 

ols_mab <- lm(MAB ~ year, data = fitline)
summary(ols_mab)
fig1_r2 <- round(summary(ols_mab)$adj.r.squared, 3)

# plot
ggplot() +
  stat_poly_line(data = PMAB, aes(x = year, y = max_PMAB), method = 'lm', se = F, colour = "#FFEDA0", size = 5, alpha = 0.8) +
  stat_poly_eq(use_label("eq")) +
  #stat_regline_equation(label.y = 31, aes(label = after_stat(..eq.label..))) +
  geom_line(data = dpmab, aes(x = year, y = MAB, group = Country, colour = selcntry, 
                              alpha = max_val, size = max_val)) +
  geom_point(data = dpmab, aes(x = year, y = MAB, group = Country, colour = selcntry, 
                               alpha = max_val, size = max_val)) +
  #annotate("text", x = 1970, y = 32, label = paste0("R squared = ", fig1_r2)) +
  scale_colour_manual(values = c(col7[1:5], "grey")) +
  scale_alpha_discrete(range = c(0.3, 1), guide = "none") +
  scale_size_manual(values = c(0.8, 1.3), guide = "none") +
  labs(x = "Year", y = "Mean age at birth") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  guides(colour = guide_legend(override.aes = list(linewidth = 2, size = 3)))
ggsave("out/period_mab_fitline.png", width = 7.5, height = 6, bg = "white")

max_year <- dpmab %>% 
  select(Country, Year = year, max_val)

# figure 2: MA1B and TFR
d_mab1 <- mab1 %>% 
  rename(year = COUNTRY) %>% 
  gather(key = country, value = mab1, -year) %>% 
  mutate(mab1 = ifelse(mab1 == 0, NA_real_, mab1))

d_fig2 <- tfr %>% 
  rename(year = COUNTRY) %>% 
  gather(key = country, value = tfr, -year) %>% 
  mutate(tfr = ifelse(tfr == 0, NA_real_, tfr)) %>% 
  left_join(d_mab1, by = c("year", "country")) %>% 
  mutate(year = as.numeric(year),
         tfr = as.numeric(tfr),
         mab1 = as.numeric(mab1),
         country = ifelse(country == "Republic.of.Korea", "South Korea", country),
         selcntry = ifelse(country %out% c("Spain", "Netherlands", "Italy", 
                                           "Switzerland", "South Korea"), "Others", country),
         selcntry = factor(selcntry, levels = c("Spain", "Netherlands", "Italy", 
                                                "Switzerland", "South Korea", "Others"))) %>% 
  drop_na() %>% 
  left_join(max_year, by = c("country" = "Country", "year" = "Year"))

d_fig2 %>% 
  gather(key = index, value = value, c(tfr, mab1)) %>% 
  mutate(index = ifelse(index == "tfr", "Total fertility rate", "Mean age at first birth")) %>% 
  ggplot(aes(x = year, y = value, group = country, colour = selcntry, size = max_val, alpha = max_val)) +
  facet_wrap(~ index, nrow = 2, scales = "free_y") +
  geom_line() +
  xlim(1979, 2020) +
  scale_colour_manual(values = c(col7[1:5], "grey")) +
  scale_alpha_manual(values = c(0.3, 1), guide = "none") +
  scale_size_manual(values = c(0.8, 1.3), guide = "none") +
  labs(x = "Year") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face = "bold")) +
  guides(colour = guide_legend(override.aes = list(linewidth = 2)))
ggsave("out/period_tfr_mab1.png", width = 8, height = 6, bg = "white")

d_fig2 %>% 
  gather(key = index, value = value, c(tfr, mab1)) %>% 
  mutate(index = ifelse(index == "tfr", "Total fertility rate", "Mean age at first birth")) %>% 
  ggplot(aes(x = year, y = value, group = country, colour = selcntry, size = max_val, alpha = max_val)) +
  facet_wrap(~ index, nrow = 2, scales = "free_y") +
  geom_line() +
  xlim(1960, 2020) +
  scale_colour_manual(values = c(col7[1:5], "grey")) +
  scale_alpha_manual(values = c(0.3, 1), guide = "none") +
  scale_size_manual(values = c(0.8, 1.3), guide = "none") +
  labs(x = "Year") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "top",
        legend.title = element_blank())
ggsave("out/period_tfr_mab1_1960.png", width = 8, height = 6, bg = "white")

d_fig2 |> 
  filter(year == 1960) |> 
  count()

dpmab |> 
  filter(year == 1960) |> 
  drop_na() |> 
  count()

# reg line for the record hold country in Fig1
dmab1 <- mab1 %>% 
  rename(year = COUNTRY) %>% 
  gather(key = country, value = mab1, -year) %>% 
  mutate(mab1 = ifelse(mab1 == 0, NA_real_, mab1),
         country = ifelse(country == "Republic.of.Korea", "South Korea", country)) %>% 
  drop_na()

max_mab1_fig1 <- dmab1 %>% 
  mutate(year = as.numeric(year),
         mab1 = as.numeric(mab1)) %>% 
  left_join(max_year, by = c("country" = "Country", "year" = "Year")) %>% 
  filter(max_val == "1",
         year >= 1979)

ols_mab1 <- lm(mab1 ~ year, data = max_mab1_fig1)
summary(ols_mab1)

# reg line for the record hold country in Fig2
max_mab1_fig2 <- d_mab1 |> 
  mutate(country = ifelse(country == "Republic.of.Korea", "South Korea", country),
         year = as.numeric(year),
         mab1 = as.numeric(mab1)) |> 
  group_by(year) %>% 
  mutate(max_mab1 = max(mab1, na.rm = T),
         max_val = ifelse(mab1 == max_mab1, "1", "0")) |> 
  filter(max_val == "1",
         year >= 1979)

ols_mab1 <- lm(mab1 ~ year, data = max_mab1_fig2)
summary(ols_mab1)

# figure 3: % of age-specific fertility rate / TFR
countryHFD <- getHFDcountries()
dpasfr <- pasfr |> 
  as.data.frame() %>% 
  rename(CNTRY = Code) %>% 
  left_join(countryHFD, by = "CNTRY") %>% 
  select(-CNTRY, -link) |> 
  mutate(Age = case_when(Age == "12-" ~ "12",
                         Age == "55+" ~ "55",
                         T ~ Age),
         Age = as.numeric(as.character(Age)),
         age_cate = case_when(Age %in% 12:19 ~ "12-19",
                              Age %in% 20:24 ~ "20-24",
                              Age %in% 25:29 ~ "25-29",
                              Age %in% 30:34 ~ "30-34",
                              Age %in% 35:39 ~ "35-39",
                              Age %in% 40:44 ~ "40-44",
                              Age %in% 45:49 ~ "45-49",
                              Age %in% 50:55 ~ "50-55"))
  
tfr <- dpasfr |> 
  group_by(Country, Year) |> 
  summarise(TFR = sum(ASFR))

d_fig3 <- dpasfr |> 
  group_by(Country, Year, age_cate) %>% 
  summarise(ASFR = sum(ASFR)) |> 
  ungroup() |> 
  left_join(tfr, by = c("Country", "Year")) |> 
  mutate(prop_asfr = ASFR / TFR * 100,
         Country = ifelse(Country == "Republic of Korea", "South Korea", Country),
         selcntry = ifelse(Country %out% c("Spain", "Netherlands", "Italy", 
                                           "Switzerland", "South Korea"), "Others", Country),
         selcntry = factor(selcntry, levels = c("Spain", "Netherlands", "Italy", 
                                                "Switzerland", "South Korea", "Others"))) |> 
  left_join(max_year, by = c("Country", "Year"))

d_fig3 |> 
  filter(age_cate %out% c("12-19", "50-55")) %>% 
  ggplot(aes(x = Year, y = prop_asfr, group = Country, colour = selcntry, size = max_val, alpha = max_val)) +
  facet_wrap(~ age_cate) +
  geom_line() +
  xlim(1979, 2020) +
  scale_colour_manual(values = c(col7[1:5], "grey")) +
  scale_alpha_manual(values = c(0.3, 1), guide = "none") +
  scale_size_manual(values = c(0.8, 1.3), guide = "none") +
  labs(y = "Proportion of age-specific fertility rate out of total fertility rate") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 14, face = "bold"),
        panel.spacing = unit(0.7, "cm")) +
  guides(colour = guide_legend(override.aes = list(linewidth = 2)))
ggsave("out/period_asfr_tfr.png", width = 7.5, height = 6, bg = "white")

# reg line for the record hold country in Fig3
fitline <- d_fig3 |> 
  filter(max_val == 1,
         Year >= 1979)
  
ols_fig3_1219 <- lm(prop_asfr ~ Year, data = fitline |> filter(age_cate == "12-19"))
ols_fig3_2024 <- lm(prop_asfr ~ Year, data = fitline |> filter(age_cate == "20-24"))
ols_fig3_2529 <- lm(prop_asfr ~ Year, data = fitline |> filter(age_cate == "25-29"))
ols_fig3_3034 <- lm(prop_asfr ~ Year, data = fitline |> filter(age_cate == "30-34"))
ols_fig3_3539 <- lm(prop_asfr ~ Year, data = fitline |> filter(age_cate == "35-39"))
ols_fig3_4044 <- lm(prop_asfr ~ Year, data = fitline |> filter(age_cate == "40-44"))
ols_fig3_4549 <- lm(prop_asfr ~ Year, data = fitline |> filter(age_cate == "45-49"))
ols_fig3_5055 <- lm(prop_asfr ~ Year, data = fitline |> filter(age_cate == "50-55"))

fig3_r2_1219 <- round(summary(ols_fig3_1219)$adj.r.squared, 4)
fig3_coef_1219 <- round(summary(ols_fig3_1219)$coefficient[2, 1], 4)
fig3_r2_2024 <- round(summary(ols_fig3_2024)$adj.r.squared, 4)
fig3_coef_2024 <- round(summary(ols_fig3_2024)$coefficient[2, 1], 4)
fig3_r2_2529 <- round(summary(ols_fig3_2529)$adj.r.squared, 4)
fig3_coef_2529 <- round(summary(ols_fig3_2529)$coefficient[2, 1], 4)
fig3_r2_3034 <- round(summary(ols_fig3_3034)$adj.r.squared, 4)
fig3_coef_3034 <- round(summary(ols_fig3_3034)$coefficient[2, 1], 4)
fig3_r2_3539 <- round(summary(ols_fig3_3539)$adj.r.squared, 4)
fig3_coef_3539 <- round(summary(ols_fig3_3539)$coefficient[2, 1], 4)
fig3_r2_4044 <- round(summary(ols_fig3_4044)$adj.r.squared, 4)
fig3_coef_4044 <- round(summary(ols_fig3_4044)$coefficient[2, 1], 4)
fig3_r2_4549 <- round(summary(ols_fig3_4549)$adj.r.squared, 4)
fig3_coef_4549 <- round(summary(ols_fig3_4549)$coefficient[2, 1], 4)
fig3_r2_5055 <- round(summary(ols_fig3_5055)$adj.r.squared, 4)
fig3_coef_5055 <- round(summary(ols_fig3_5055)$coefficient[2, 1], 4)

fig3_fit <- data.frame(
  agecate = c("12-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-55"),
  r2 = c(fig3_r2_1219, fig3_r2_2024, fig3_r2_2529, fig3_r2_3034, fig3_r2_3539, 
         fig3_r2_4044, fig3_r2_4549, fig3_r2_5055),
  coef = c(fig3_coef_1219, fig3_coef_2024, fig3_coef_2529, fig3_coef_3034, fig3_coef_3539, 
           fig3_coef_4044, fig3_coef_4549, fig3_coef_5055)
)
write.csv(fig3_fit, "out/fig3_fit_mab.csv")

# figure 4: % of parity-specific fertility rate by TFR
d_fig4 <- pasfrbo |> 
  as.data.frame() %>% 
  rename(CNTRY = Code) %>% 
  left_join(countryHFD, by = "CNTRY") %>% 
  select(-CNTRY, -link) |> 
  group_by(Country, Year) |> 
  summarise(TFR = sum(ASFR),
            TFR1 = sum(ASFR1),
            TFR2 = sum(ASFR2),
            TFR3 = sum(ASFR3),
            TFR4 = sum(ASFR4),
            TFR5 = sum(ASFR5p)) |> 
  ungroup() |> 
  gather(key = parity, value = tfri, -c(Country, Year, TFR)) |> 
  mutate(parity = case_when(parity == "TFR1" ~ "Parity 1",
                            parity == "TFR2" ~ "Parity 2",
                            parity == "TFR3" ~ "Parity 3",
                            parity == "TFR4" ~ "Parity 4",
                            parity == "TFR5" ~ "Parity 5"),
         prop_tfri = tfri / TFR * 100,
         Country = ifelse(Country == "Republic of Korea", "South Korea", Country),
         selcntry = ifelse(Country %out% c("Spain", "Netherlands", "Italy", 
                                           "Switzerland", "South Korea"), "Others", Country),
         selcntry = factor(selcntry, levels = c("Spain", "Netherlands", "Italy", 
                                                "Switzerland", "South Korea", "Others"))) |> 
  left_join(max_year, by = c("Country", "Year"))

d_fig4 %>% 
  ggplot(aes(x = Year, y = prop_tfri, group = Country, colour = selcntry, size = max_val, alpha = max_val)) +
  facet_wrap(~ parity) +
  geom_line() +
  xlim(1979, 2020) +
  scale_colour_manual(values = c(col7[1:5], "grey")) +
  scale_alpha_manual(values = c(0.3, 1), guide = "none") +
  scale_size_manual(values = c(0.8, 1.3), guide = "none") +
  labs(y = "Proportion of total fertility rate by birth order out of total fertility rate") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 14, face = "bold"),
        panel.spacing = unit(0.7, "cm")) +
  guides(colour = guide_legend(override.aes = list(linewidth = 2)))
ggsave("out/period_tfri_tfr.png", width = 8, height = 6, bg = "white")

# reg line for the record hold country in Fig4
fitline <- d_fig4 |> 
  filter(max_val == 1,
         Year >= 1979)

ols_fig4_p1 <- lm(prop_tfri ~ Year, data = fitline |> filter(parity == "Parity 1"))
ols_fig4_p2 <- lm(prop_tfri ~ Year, data = fitline |> filter(parity == "Parity 2"))
ols_fig4_p3 <- lm(prop_tfri ~ Year, data = fitline |> filter(parity == "Parity 3"))
ols_fig4_p4 <- lm(prop_tfri ~ Year, data = fitline |> filter(parity == "Parity 4"))
ols_fig4_p5 <- lm(prop_tfri ~ Year, data = fitline |> filter(parity == "Parity 5"))

fig4_r2_p1 <- round(summary(ols_fig4_p1)$adj.r.squared, 4)
fig4_coef_p1 <- round(summary(ols_fig4_p1)$coefficient[2, 1], 4)
fig4_r2_p2 <- round(summary(ols_fig4_p2)$adj.r.squared, 4)
fig4_coef_p2 <- round(summary(ols_fig4_p2)$coefficient[2, 1], 4)
fig4_r2_p3 <- round(summary(ols_fig4_p3)$adj.r.squared, 4)
fig4_coef_p3 <- round(summary(ols_fig4_p3)$coefficient[2, 1], 4)
fig4_r2_p4 <- round(summary(ols_fig4_p4)$adj.r.squared, 4)
fig4_coef_p4 <- round(summary(ols_fig4_p4)$coefficient[2, 1], 4)
fig4_r2_p5 <- round(summary(ols_fig4_p5)$adj.r.squared, 4)
fig4_coef_p5 <- round(summary(ols_fig4_p5)$coefficient[2, 1], 4)

fig4_fit <- data.frame(
  parity = c("p1", "p2", "p3", "p4", "p5"),
  r2 = c(fig4_r2_p1, fig4_r2_p2, fig4_r2_p3, fig4_r2_p4, fig4_r2_p5),
  coef = c(fig4_coef_p1, fig4_coef_p2, fig4_coef_p3, fig4_coef_p4, fig4_coef_p5)
)
write.csv(fig4_fit, "out/fig4_fit_mab.csv")

# fig 5
dfig5_mab <- pasfrbo %>% 
  select(Code, Year, Age, ASFR) %>% 
  left_join(countryHFD, by = c("Code" = "CNTRY")) %>%
  select(-Code, -link) %>% 
  mutate(Age = case_when(Age == "12-" ~ "12",
                         Age == "55+" ~ "55",
                         T ~ Age),
         Age = as.numeric(as.character(Age)),
         asfr_condi = ifelse(Age < 30, 0, ASFR)) %>% 
  group_by(Country, Year) %>% 
  summarise(mab = sum((Age + 0.5) * ASFR) / sum(ASFR),
            mab_condi = sum((Age + 0.5) * asfr_condi) / sum(asfr_condi)) %>% 
  mutate(Country = ifelse(Country == "Republic of Korea", "South Korea", Country),
         selcntry = ifelse(Country %out% c("Spain", "Netherlands", "Italy", 
                                           "Switzerland", "South Korea"), "Others", Country),
         selcntry = factor(selcntry, levels = c("Spain", "Netherlands", "Italy", 
                                                "Switzerland", "South Korea", "Others"))) %>% 
  left_join(max_year, by = c("Country", "Year"))

dfig5_mab %>% 
  ggplot(aes(x = Year, y = mab_condi, group = Country, 
             colour = selcntry, size = max_val, alpha = max_val)) +
  geom_line() +
  geom_point() +
  xlim(1960, 2020) +
  scale_colour_manual(values = c(col7[1:5], "grey")) +
  scale_alpha_manual(values = c(0.3, 1), guide = "none") +
  scale_size_manual(values = c(0.8, 1.3), guide = "none") +
  labs(x = "Year", y = "Mean age at birth conditional on age 30") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 14)) +
  guides(colour = guide_legend(override.aes = list(linewidth = 2, size = 3)))
ggsave("out/period_mab_selcntry_all_age30.png", width = 7.5, height = 6)

dfig5_mab |> 
  group_by(Year) |> 
  summarise(max = max(mab)) |> View()

#-------
# figure : % of age-specific birth counts among all birth counts
countryHFD <- getHFDcountries()
dPDBx <- PDBx %>% 
  as.data.frame() %>% 
  rename(CNTRY = Code) %>% 
  left_join(countryHFD, by = "CNTRY") %>% 
  select(-CNTRY, -link) %>% 
  mutate(Age = case_when(Age == "12-" ~ "12",
                         Age == "55+" ~ "55",
                         T ~ Age),
         Age = as.numeric(as.character(Age)),
         Total = as.numeric(as.character(Total)),
         age_cate = case_when(Age %in% 12:19 ~ "12-19",
                              Age %in% 20:24 ~ "20-24",
                              Age %in% 25:29 ~ "25-29",
                              Age %in% 30:34 ~ "30-34",
                              Age %in% 35:39 ~ "35-39",
                              Age %in% 40:44 ~ "40-44",
                              Age %in% 45:49 ~ "45-49",
                              Age %in% 50:55 ~ "50-55")) %>% 
  rename(bx = Total) %>% 
  group_by(Country, Year, age_cate) %>% 
  summarise(bx = sum(bx)) %>% 
  group_by(Country, Year) %>% 
  mutate(B = sum(bx),
         prop_bx = bx / B * 100,
         Country = ifelse(Country == "Republic of Korea", "South Korea", Country),
         selcntry = ifelse(Country %out% c("Spain", "Italy", "Netherlands", "Switzerland",
                                           "South Korea", "Portugal"), "Others", Country),
         selcntry = factor(selcntry, levels = c("Spain", "Italy", "Portugal", "Netherlands", 
                                                "Switzerland", "South Korea", "Others"))) %>% 
  ungroup() %>% 
  left_join(max_year, by = c("Country", "Year"))

dPDBx %>% 
  filter(age_cate %out% c("12-19", "50-55")) %>% 
  ggplot(aes(x = Year, y = prop_bx, group = Country, colour = selcntry, size = max_val, alpha = max_val)) +
  facet_wrap(~ age_cate) +
  geom_line() +
  xlim(1979, 2020) +
  scale_colour_manual(values = col7) +
  scale_alpha_manual(values = c(0.3, 1), guide = "none") +
  scale_size_manual(values = c(0.8, 1.3), guide = "none") +
  labs(y = "Proportion of number of births among total births") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank())
ggsave("out/period_prop_bx_mab.png", width = 7.5, height = 6, bg = "white")

# get the coefficients of fitted line
fitline <- dPDBx %>% 
  filter(max_val == 1,
         Year >= 1979,
         age_cate == "25-29") 

ols_bx25 <- lm(prop_bx ~ Year, data = fitline)
summary(ols_bx25)

fitline <- dPDBx %>% 
  filter(max_val == 1,
         Year >= 1979,
         age_cate == "35-39") 

ols_bx35 <- lm(prop_bx ~ Year, data = fitline)
summary(ols_bx35)

# figure 3: % of parity among total births
dpBxbo <- pBxbo %>% 
  rename(CNTRY = Code) %>% 
  left_join(countryHFD, by = "CNTRY") %>% 
  select(-CNTRY) %>% 
  mutate(B4 = B4 + B5p) %>% 
  select(-B5p) %>% 
  group_by(Country, Year) %>% 
  summarise(Total = sum(Total),
            B1 = sum(B1),
            B2 = sum(B2),
            B3 = sum(B3),
            B4 = sum(B4)) %>% 
  mutate(Parity1 = round((B1 / Total) * 100, 1),
         Parity2 = round((B2 / Total) * 100, 1),
         Parity3 = round((B3 / Total) * 100, 1),
         Parity4 = round((B4 / Total) * 100, 1)) %>%
  select(Country, Year, Parity1, Parity2, Parity3, Parity4) %>% 
  gather(key = parity, value = prop, c(Parity1, Parity2, Parity3, Parity4)) %>% 
  mutate(Country = ifelse(Country == "Republic of Korea", "South Korea", Country),
         selcntry = ifelse(Country %out% c("Spain", "Italy", "Netherlands", "Switzerland",
                                           "South Korea", "Portugal"), "Others", Country),
         selcntry = factor(selcntry, levels = c("Spain", "Italy", "Portugal", "Netherlands", 
                                                "Switzerland", "South Korea", "Others"))) %>% 
  ungroup() %>%
  left_join(max_year, by = c("Country", "Year"))

dpBxbo %>% 
  ggplot(aes(x = Year, y = prop, group = Country, colour = selcntry, size = max_val, alpha = max_val)) +
  facet_wrap(~ parity) +
  geom_line() +
  xlim(1979, 2020) +
  scale_colour_manual(values = col7) +
  scale_alpha_manual(values = c(0.3, 1), guide = "none") +
  scale_size_manual(values = c(0.8, 1.3), guide = "none") +
  labs(y = "Proportion of each birth order among total number of births") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank())
ggsave("out/period_propbx_parity_mab.png", width = 8, height = 6, bg = "white")