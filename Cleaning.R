library(readr)
library(tidyverse)
data <- read_csv("data.csv")
View(data)
str(data)

colnames(data)
sum(is.na(data))

#Cleaning positions
data$LS <- gsub('\\+[0-9]','',data$LS)
data$ST <- gsub('\\+[0-9]','',data$ST)
data$RS <- gsub('\\+[0-9]','',data$RS)
data$LW <- gsub('\\+[0-9]','',data$LW)
data$LF <- gsub('\\+[0-9]','',data$LF)
data$CF <- gsub('\\+[0-9]','',data$CF)
data$RF <- gsub('\\+[0-9]','',data$RF)
data$RW <- gsub('\\+[0-9]','',data$RW)
data$LAM <- gsub('\\+[0-9]','',data$LAM)
data$CAM <- gsub('\\+[0-9]','',data$CAM)
data$RAM <- gsub('\\+[0-9]','',data$RAM)
data$LM <- gsub('\\+[0-9]','',data$LM)
data$LCM <- gsub('\\+[0-9]','',data$LCM)
data$CM <- gsub('\\+[0-9]','',data$CM)
data$RCM <- gsub('\\+[0-9]','',data$RCM)
data$RM <- gsub('\\+[0-9]','',data$RM)
data$LWB <- gsub('\\+[0-9]','',data$LWB)
data$LDM <- gsub('\\+[0-9]','',data$LDM)
data$CDM <- gsub('\\+[0-9]','',data$CDM)
data$RDM <- gsub('\\+[0-9]','',data$RDM)
data$RWB <- gsub('\\+[0-9]','',data$RWB)
data$LB <- gsub('\\+[0-9]','',data$LB)
data$LCB <- gsub('\\+[0-9]','',data$LCB)
data$CB <- gsub('\\+[0-9]','',data$CB)
data$RCB <- gsub('\\+[0-9]','',data$RCB)
data$RB <- gsub('\\+[0-9]','',data$RB)

position.Cols <- c('LS','ST','RS','LW','LF','CF','RF','RW','LAM','CAM','RAM','LM','LCM','CM','RCM','RM',
          'LWB','LDM','CDM','RDM','RWB','LB','LCB','CB','RCB','RB')

data[position.Cols] <- lapply(data[position.Cols], factor)

#Removing units from weight col
data$Weight <- gsub('\\lbs', '', data$Weight)
 
data <- data %>%
  mutate(multiplier = ifelse(str_detect(Value, "K"), 1000, ifelse(str_detect(Value, "M"), 1000000, 1))) %>%
  mutate(Value = as.numeric(str_extract(Value, "[[:digit:]]+\\.*[[:digit:]]*")) * multiplier)

data <- data %>%
  mutate(multiplier = ifelse(str_detect(Wage, "K"), 1000, ifelse(str_detect(Wage, "M"), 1000000, 1))) %>%
  mutate(Wage = as.numeric(str_extract(Wage, "[[:digit:]]+\\.*[[:digit:]]*")) * multiplier)

data <- data %>%
  mutate(multiplier = ifelse(str_detect(`Release Clause`, "K"), 1000, ifelse(str_detect(`Release Clause`, "M"), 1000000, 1))) %>%
  mutate(`Release Clause` = as.numeric(str_extract(`Release Clause`, "[[:digit:]]+\\.*[[:digit:]]*")) * multiplier)

data <- data %>%
  separate(Height,c('feet', 'inches'), sep = '\'', convert = TRUE, remove = FALSE) %>%
  mutate(Height = 12*feet + inches)

df <- data
positions <- unique(df$Position) #NA are available in positions

gk <- 'GK'
def <- positions[str_detect(positions, 'B$')]
mid <- positions[str_detect(positions,'M$')]
f1 <- positions[str_detect(positions,'S$')]
f2 <- positions[str_detect(positions,'F$')]
f3 <- positions[str_detect(positions,'W$')]
f4 <- positions[str_detect(positions,'T$')]
fwd <- c(f1,f2,f3,f4)


df <- df %>% 
  mutate(PositionGroup = ifelse(Position %in% gk, "GK", ifelse(Position %in% def, "DEF", ifelse(Position %in% mid, "MID", ifelse(Position %in% fwd, "FWD", "Unknown")))))

View(df)
df <- df[,-c(1,2,5,7,11,28,29,92)]

write.csv(df, "CleanedDataN.csv", row.names = F)
