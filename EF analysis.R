install.packages("readxl")
library(readxl)
df <- read_excel(file.choose(), skip = 1)
dim(df)
names(df)
head(df, 3)
df_raw <- read_excel(file.choose(), col_names = FALSE)
head(df_raw, 3)
df <- df_raw[-1, ]
names(df) <- as.character(df_raw[2, ])
df <- df[-1, ]
dim(df)
names(df)
head(df, 3)
element_cols <- setdiff(names(df), "Sample ID")

df[element_cols] <- lapply(df[element_cols], as.numeric)
element_cols <- setdiff(names(df), "Sample ID")

df[element_cols] <- lapply(df[element_cols], as.numeric)
df[setdiff(names(df), "Sample ID")] <- lapply(
  df[setdiff(names(df), "Sample ID")],
  as.numeric
)
str(df)
colSums(is.na(df))
df <- df_raw[-1, ]
names(df) <- as.character(df_raw[2, ])
df <- df[-1, ]
df[-1] <- lapply(df[-1], as.numeric)
dim(df)
str(df)
colSums(is.na(df))
df[df$Cr %>% is.na(), c("Sample ID", "Cr")]
df[is.na(df$Cr), c("Sample ID", "Cr")]
df[is.na(df$As), c("Sample ID", "As")]
df[is.na(df$Hg), c("Sample ID", "Hg")]
df[is.na(df$Hg), c("Sample ID", "Hg")]
df[is.na(df$Sc), c("Sample ID", "Sc")]
df_raw[df_raw[[1]] %in% c("TH. 1", "TH. 2", "TH. 3", "TH. 4", "TH. 5"),
       c(1, 16, 19)]
data.frame(
  Column_Number = seq_along(df_raw[2, ]),
  Column_Name = as.character(df_raw[2, ])
)
df_raw[c(3:42), c(1, 16, 19)]
df_EF <- df
df_EF$As[is.na(df_EF$As)] <- 2.19 / 2
df_EF$Hg[is.na(df_EF$Hg)] <- 0.21 / 2
df_EF[c("Sample ID", "As", "Hg")]
df_EF <- df

df_EF$As[is.na(df_EF$As)] <- 2.19 / 2
df_EF$Hg[is.na(df_EF$Hg)] <- 0.21 / 2
df_EF[c("Sample ID", "As", "Hg")]
df_EF <- df

df_EF$As[is.na(df_EF$As)] <- 2.19 / 2
df_EF$Hg[is.na(df_EF$Hg)] <- 0.21 / 2

df_EF[c("Sample ID", "As", "Hg")]
exists("df")
df_EF <- df
exists("df_EF")
df_EF <- df
exists("df_EF")
df_EF$As[is.na(df_EF$As)] <- 2.19 / 2
df_EF$Hg[is.na(df_EF$Hg)] <- 0.21 / 2
df_EF[c("Sample ID", "As", "Hg")]
df_EF$As[is.na(df_EF$As)] <- 1.095
sum(is.na(df_EF$As))
sum(is.na(df_EF$Hg))
df_EF[c("Sample ID", "As", "Hg")]
