# period: downloaded on 13 Mar 2025
MAB_HFD <- read.xlsx("data/MAB.xlsx", sheet = "Mean age at birth", startRow = 2)

# cohort: downloaded on 16 Sep 2024
CMAB_HFD <- read.table("data/mabVH.txt", skip = 2, header = T)

# period birth counts: downloaded on 16 Sep 2024
PDBx <- read.table("data/trifon_birthsRR.txt", skip = 2, header = T)

# period birth counts by birth order: downloaded on 16 Sep 2024
pBxbo <- read.table("../../../../Library/CloudStorage/GoogleDrive-ryohei.mogi@upf.edu/My Drive/BigData/HFD/birthsRRbo.txt", skip = 2, header = T)

# cohort birth counts: downloaded on 16 Sep 2024
CDBx <- read.table("../../fertility_linear/Analysis/data/birthsVH.txt", skip = 2, header = T)

# period age-specific fertility rate: downloaded on 13 Mar 2025
pasfr <- read.table("../../../../Library/CloudStorage/GoogleDrive-ryohei.mogi@upf.edu/My Drive/BigData/HFD/asfrRR.txt", skip = 2, header = T)

# period age- and parity-specific fertility rate: downloaded on 16 13 Mar 2025
pasfrbo <- read.table("../../../../Library/CloudStorage/GoogleDrive-ryohei.mogi@upf.edu/My Drive/BigData/HFD/asfrRRbo.txt", skip = 2, header = T)

# cohort death rates
#cmx <- read.table("../../../../Library/CloudStorage/GoogleDrive-ryohei.mogi@upf.edu/My Drive/BigData/HMD/cMx_1x1/ESP.cMx_1x1.txt", skip = 2, header = T)

# period total fertility rate: : downloaded on 16 Sep 2024
tfr <- read.xlsx("data/TFR.xlsx", sheet = "Total fertility rates", startRow = 2)[-1, ]

# period mean age at first birth: : downloaded on 16 Sep 2024
mab1 <- read.xlsx("data/MAB1.xlsx", sheet = "Mean age at first birth", startRow = 2)[-1, ]

# exposures from the HFD: downloaded on 13 Mar 2025
exp <- read.table("../../../../Library/CloudStorage/GoogleDrive-ryohei.mogi@upf.edu/My Drive/BigData/HFD/exposRR.txt", skip = 2, header = T)

# life table functions from Human Mortality Database: downloaded on 21 Oct 2024

file_list <- list.files(path = "../../../../Library/CloudStorage/GoogleDrive-ryohei.mogi@upf.edu/My Drive/BigData/HMD/lt_female/fltper_1x1", 
                        full.names = T)

read_and_add_country <- function(file_path) {
  country_name <- tools::file_path_sans_ext(basename(file_path))
  
  data <- readHMD(file_path)
  
  data <- data %>% mutate(Country = country_name)
  
  return(data)
}

d_lt <- file_list %>%
  lapply(read_and_add_country) %>%
  bind_rows()

## cohort birth counts by parity
#DBxparity <- read.table("../../fertility_linear/Analysis/data/birthsVHbo.txt", skip = 2, header = T)
#
## CTFR
#CTFR <- read.xlsx("../../../HFDdata/summary/CCF.xlsx", sheet = "Completed cohort fertility", startRow = 3)
#
## cohort mean age at first birth
#MA1B <- read.table("../../../HFDdata/summary/mabVHbo.txt", skip = 2, header = T)
#
## cohort parity distribution
#CPD0 <- read.xlsx("../../../HFDdata/summary/PARITY.xlsx", sheet = "Cohort childlessness", startRow = 3)
#CPD1 <- read.xlsx("../../../HFDdata/summary/PARITY.xlsx", sheet = "Parity 1", startRow = 3)
#CPD2 <- read.xlsx("../../../HFDdata/summary/PARITY.xlsx", sheet = "Parity 2", startRow = 3)
#CPD3 <- read.xlsx("../../../HFDdata/summary/PARITY.xlsx", sheet = "Parity 3+", startRow = 3)
#CPD_15p <- read.table("../../../HFDdata/summary/birthsVHbo.txt", header = TRUE, fill = TRUE, skip = 2)
