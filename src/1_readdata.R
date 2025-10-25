## Please download the following data file from the Human Fertility Database

# period: downloaded on 13 Mar 2025
MAB_HFD <- read.xlsx("data/MAB.xlsx", sheet = "Mean age at birth", startRow = 2)

# period age-specific fertility rate: downloaded on 13 Mar 2025
pasfr <- read.table("data/asfrRR.txt", skip = 2, header = T)

# period age- and parity-specific fertility rate: downloaded on 16 13 Mar 2025
pasfrbo <- read.table("data/asfrRRbo.txt", skip = 2, header = T)

# period total fertility rate: : downloaded on 16 Sep 2024
tfr <- read.xlsx("data/TFR.xlsx", sheet = "Total fertility rates", startRow = 2)[-1, ]

# period mean age at first birth: : downloaded on 16 Sep 2024
mab1 <- read.xlsx("data/MAB1.xlsx", sheet = "Mean age at first birth", startRow = 2)[-1, ]