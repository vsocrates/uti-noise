library(dplyr)
library(tidyr)
library(arrow)
library(stringr)
library(data.table)

#Set directory and load clean data for reference
setwd("/Users/msi7/Library/CloudStorage/Box-Box/UTI Noise Project/Data/interim")

df_clean <- read.csv("full_data.csv")

#Load full new dataset components
df_full <- read.csv("ua_results.csv")
df_ucx <- read.csv("/Users/msi7/Library/CloudStorage/Box-Box/UTI Noise Project/New_Data/urinecx_updated.csv")
df_enc <- read_feather("encounter_info.ftr")
df_diag <- read_feather("diagnoses.ftr")
df_cc <- read_feather("chief_complaint.ftr")
df_edabx <- read.csv("/Users/msi7/Library/CloudStorage/Box-Box/UTI Noise Project/Raw Data/abx_revised.csv")
df_fulldemo <- read.csv("/Users/msi7/Library/CloudStorage/Box-Box/UTI Noise Project/Raw Data/demo.csv")

#Count number of distinct CSN IDs in each datafield
n_distinct(df_full$PAT_ENC_CSN_ID)
n_distinct(df_ucx$PAT_ENC_CSN_ID)
n_distinct(df_enc$PAT_ENC_CSN_ID)
n_distinct(df_diag$PAT_ENC_CSN_ID)
n_distinct(df_cc$PAT_ENC_CSN_ID)
n_distinct(df_edabx$PAT_ENC_CSN_ID)
n_distinct(df_fulldemo$PAT_ENC_CSN_ID)

#Create data cleaning functions written by Andrew Taylor
count_levels<-function(df, column){
  df %>% 
    group_by(.,df[,column]) %>%
    summarise(count=n()) %>%
    arrange(desc(count))
}

repl_text<-function(df, col, sterm, rterm){
  df[grepl(sterm, df[,col]), col]<-rterm
  return(df)
}

clean_col<-function(lst, df, col){
  dftemp<-df
  for (key in names(lst)){
    dftemp<-repl_text(dftemp,col,lst[key], key)
  }
  dftemp[,col]<-ifelse(dftemp[,col]=="", NA, dftemp[,col])
  return(dftemp)
}

#Run cleaning functions on count columns with single digits rather than ranges

df_full <- df_full %>%
  mutate(WBC.HPF..UA = case_when(
    WBC.HPF..UA=="['1' '/HPF' 'Normal: 0-5 ']"~ "low",
    WBC.HPF..UA=="['2' '/HPF' 'Normal: 0-5 ']" ~ "low",
    WBC.HPF..UA=="['3' '/HPF' 'Normal: 0-5 ']" ~ "low",
    WBC.HPF..UA=="['4' '/HPF' 'Normal: 0-5 ']" ~ "low",
    WBC.HPF..UA=="['5' '/HPF' 'Normal: 0-5 ']" ~ "low",
    WBC.HPF..UA=="['<1' '/HPF' 'Normal: 0-5 ']" ~ "low",
    WBC.HPF..UA=="['0' '/HPF' 'Normal: 0-5 ']" ~ "none",
    T ~ WBC.HPF..UA
  ))

column_name = "WBC.HPF..UA"
chg_list<-list(large = "\\d{3,4}",
               moderate = "\\d{2}",
               small = "\\d{1}")
df_full<-clean_col(chg_list, df_full, column_name)

df_full <- df_full %>%
  mutate(Urine.Squamous.Epithelial.Cells..UA = case_when(
    Urine.Squamous.Epithelial.Cells..UA=="['1' '/HPF' 'Normal: 0-5 ']"~ "low",
    Urine.Squamous.Epithelial.Cells..UA=="['2' '/HPF' 'Normal: 0-5 ']" ~ "low",
    Urine.Squamous.Epithelial.Cells..UA=="['3' '/HPF' 'Normal: 0-5 ']" ~ "low",
    Urine.Squamous.Epithelial.Cells..UA=="['4' '/HPF' 'Normal: 0-5 ']" ~ "low",
    Urine.Squamous.Epithelial.Cells..UA=="['5' '/HPF' 'Normal: 0-5 ']" ~ "low",
    Urine.Squamous.Epithelial.Cells..UA=="['<1' '/HPF' 'Normal: 0-5 ']" ~ "low",
    Urine.Squamous.Epithelial.Cells..UA=="['0' '/HPF' 'Normal: 0-5 ']" ~ "none",
    T ~ Urine.Squamous.Epithelial.Cells..UA
  ))

column_name = "Urine.Squamous.Epithelial.Cells..UA"
chg_list<-list(large = "\\d{3,4}",
               moderate = "\\d{2}",
               small = "\\d{1}")
df_full<-clean_col(chg_list, df_full, column_name)


#Collapse differently-named columns measuring the same thing
dfs <- df_full
dfs$ua_leuk <- paste(dfs$Leukocyte.Esterase..UA,dfs$Leukocytes..UA)
dfs$ua_epi <- paste(dfs$Squam.Epithel..UA,dfs$Urine.Squamous.Epithelial.Cells..UA)
dfs$ua_wbc <- paste(dfs$WBC..UA,dfs$WBC.HPF..UA)

#Name new datafield, with relevant columns selected
dfs <- dfs %>% select(c(2,3,5,32,31,13,33))

# Give those columns the same name as the clean dataset
names(dfs)[1] <- "PAT_ENC_CSN_ID"
names(dfs)[2] <- "ua_bacteria"
names(dfs)[3] <- "ua_blood"
names(dfs)[4] <- "ua_epi"
names(dfs)[5] <- "ua_leuk"
names(dfs)[6] <- "ua_nitrite"
names(dfs)[7] <- "ua_wbc"

#Convert variables to character vectors and change all data to lower case
i <- sapply(dfs, is.character)
dfs[i] <- lapply(dfs[i], tolower)

#Replace all blanks with "not_reported"
dfs[dfs==""] <- "not_reported" 

#Clean UA data column by column

#Bacteria
dfs <- dfs %>%
  mutate(ua_bacteria = case_when(
    str_detect(ua_bacteria,"cancelled") ~ "other",
    str_detect(ua_bacteria,"moderate") ~ "moderate",
    str_detect(ua_bacteria,"occasional|occassional|rare|slight|1+") ~ "few",
    str_detect(ua_bacteria,"2+") ~ "moderate",
    str_detect(ua_bacteria,"3+") ~ "many",
    str_detect(ua_bacteria,"4+") ~ "marked",
    str_detect(ua_bacteria,"many") ~ "many",
    ua_bacteria=="['none seen' '/hpf' 'normal: absent ']" ~ "none",
    ua_bacteria=="['none seen' '/hpf' 'normal: none seen ']" ~ "none",
    ua_bacteria=="['none' '/hpf' 'normal: none-few ']" ~ "none",
    ua_bacteria=="[none none none]" ~ "none",
    ua_bacteria=="[none '/hpf' 'normal: none ']" ~ "none",
    ua_bacteria=="[none '/hpf' 'normal: none-few ']" ~ "none",
    ua_bacteria=="[none '/hpf' 'normal: none seen ']" ~ "none",
    ua_bacteria=="['few' '/hpf' 'normal: none-few ']" ~ "few",
    ua_bacteria=="['few' '/hpf' 'normal: none ']" ~ "few",
    ua_bacteria=="['few' '/hpf' 'normal: none seen ']" ~ "few",
    ua_bacteria=="['mod' '/hpf' 'normal: none-few ']" ~ "moderate",
    ua_bacteria=="['moderate' '/hpf' 'normal: none-few ']" ~ "moderate",
    ua_bacteria=="['marked' none none]" ~ "marked",
    ua_bacteria=="['78' '/hpf' 'normal: <65 high: <65']" ~ "many",
    ua_bacteria=="['....' none none]" ~ "not_reported",
    ua_bacteria=="['----' none none]" ~ "not_reported",
    T ~ ua_bacteria
  ))

#Blood
dfs <- dfs %>%
  mutate(ua_blood = case_when(
    str_detect(ua_blood,"cancelled|pending|comment|performed|error|invalid") ~ "other",
    str_detect(ua_blood,"small|trace|positive|1+") ~ "small",
    str_detect(ua_blood,"moderate|2+") ~ "moderate",
    str_detect(ua_blood,"large|many|3+|4+") ~ "large",
    str_detect(ua_blood,"neg") ~ "negative",
    ua_blood=="['negative' none 'normal: negative ']" ~ "negative",
    ua_blood=="[none '/hpf' 'normal: none seen ']" ~ "negative",
    ua_blood=="[none none none]" ~ "negative",
    T ~ ua_blood
  ))


# Apply Andrew Taylor's data cleaning steps, using functions created above

#ua_epi
column_name = "ua_epi"
chg_list<-list(negative = "negative|neg|none",
               large = "large|many|lrg|innum|packed|marked|\\d{2,4}\\-{1}\\d{2,4}",
               moderate = "mod|\\d{1}\\-{1}\\d{2}",
               small = "small|trace|positive|rare|\\d{1}\\-{1}\\d{1}|occas|slight|few",
               other = "\\-|\\.|cancelled|unable|test|#|invalid|see|error|comment|pending")
dfs<-clean_col(chg_list, dfs, column_name)

chg_list<-list(large = "\\d{3,4}",
               moderate = "\\d{2}",
               small = "\\d{1}")
dfs <-clean_col(chg_list, dfs, column_name)


#ua_leuk


column_name = "ua_leuk"
chg_list<-list(small = "1+|small|trace|positive|\\d{2}",
               moderate = "2+|moderate|\\d{3}",
               large = "3+|many|large|\\d{4}",
               other = "\\-|\\.|cancelled|unable|test|#|invalid|see|error|comment|pending",
               negative = "negative|neg|none")
dfs<-clean_col(chg_list, dfs, column_name)

table(dfs$ua_leuk)


#ua_nitrite
dfs <- dfs %>%
  mutate(ua_nitrite = case_when(
    ua_nitrite=="[none none none]" ~ "negative",
    T ~ ua_nitrite
  ))

column_name = "ua_nitrite"
chg_list<-list(positive = "(1+)|trace|positive|\\d{2}|(2+)|mod|\\d{3}|(3+)|many|lrg|\\d{4}|few|rare|pos|occ",
               other = "\\-|\\.|cancelled|unable|test|#|invalid|see|error|comment|pending|applicable",
               negative = "negative|neg")
dfs<-clean_col(chg_list, dfs, column_name)




#ua_wbc - HERE I'M USING A COMBINATION OF SPECIFIC MUTATIONS TO HANDLE THE INEQUALITIES AND RULE-BASED ONES FOR EVERYTHING ELSE
dfs <- dfs %>%
  mutate(ua_wbc = case_when(
    ua_wbc=="['0-5' '/HPF' 'Low: 0 High: 5']" ~ "low",
    ua_wbc=="['0-1' '/HPF' 'Low: 0 High: 1']" ~ "low",
    ua_wbc=="['0-5' '/HPF' 'Normal: < OR = 5 ']" ~ "low",
    ua_wbc=="['<5' none 'normal: <5 ']" ~ "low",
    ua_wbc=="['>30' '/hpf' 'low: 0 high: 1']" ~ "moderate",
    ua_wbc=="['>100' none 'normal: <5 ']" ~ "large",
    T ~ ua_wbc
  ))

column_name = "ua_wbc"
chg_list<-list(large = "large|many|lrg|large|innumerable|packed|\\d{3,4}\\-{1}\\d{3,4}",
               moderate = "mod|\\d{1,2}\\-{1}\\d{2}",
               small = "small|trace|positive|rare|\\d{1}\\-{1}\\d{1}",
               other = "\\.|\\-{4}|cancelled|qns|insufficient|unable|test|#|invalid|see|error|comment|pending|tnp",
               low = "low",
               negative = "negative|neg|none")
dfs<-clean_col(chg_list, dfs, column_name)

chg_list<-list(large = "\\d{3,4}",
               moderate = "\\d{2}",
               small = "\\d{1}")
dfs<-clean_col(chg_list, dfs, column_name)


#Testing
table(dfs$ua_bacteria)
table(dfs$ua_blood)
table(dfs$ua_epi)
table(dfs$ua_nitrite)
table(dfs$ua_wbc)




######Process UCx data#####

#Select columns 
df_ucx <- df_ucx %>% select(c(1,9))

#Make lower case
i <- sapply(df_ucx, is.character)
df_ucx[i] <- lapply(df_ucx[i], tolower)

#Collapse UCX results
df_ucx <- df_ucx %>%
  group_by(PAT_ENC_CSN_ID) %>%
  summarize(labtext_extra = str_c(labtext_extra, collapse = "| "))

#Rename result column
names(df_ucx)[2] <- "ucx_result"

#Summarize results
df_ucx %>%
  group_by(labtext_extra) %>%
  summarise(Count=  n()) %>%
  arrange(-Count) %>%
  write.csv("ucx_result.csv")

#Test value FOR QI ONLY
testvalue <- df_ucx[df_ucx$PAT_ENC_CSN_ID %like% "110709665", ]

#Select any CSNs that have "mixed flora" results
mixed_flora <- df_ucx[df_ucx$ucx_result %like% "mixed", ]

#Select any CSNs that grew some e coli
ecoli <- df_ucx[df_ucx$ucx_result %like% "coli", ]

#Select any CSNs that grew <10K CFUs
lessthanten <- df_ucx[df_ucx$ucx_result %like% "<10,000" |df_ucx$ucx_result %like% "less than 10,000" | df_ucx$ucx_result %like% "500 cfu" | df_ucx$ucx_result %like% "600 cfu"|df_ucx$ucx_result %like% "700 cfu" | df_ucx$ucx_result %like% "800 cfu"| df_ucx$ucx_result %like% "900 cfu" |df_ucx$ucx_result %like% "1,000 cfu" | df_ucx$ucx_result %like% "2,000" | df_ucx$ucx_result %like% "3,000"| df_ucx$ucx_result %like% "\\s4,000"| df_ucx$ucx_result %like% "\\s5,000"| df_ucx$ucx_result %like% "\\s6,000"| df_ucx$ucx_result %like% "\\s7,000"| df_ucx$ucx_result %like% "\\s8,000"| df_ucx$ucx_result %like% "\\s9,000", ]

#Select any CSNs that grew >10K of something
greaterthanten <- df_ucx[df_ucx$ucx_result %like% ">10,000" |df_ucx$ucx_result %like% "10,000-" |df_ucx$ucx_result %like% "15,000" | df_ucx$ucx_result %like% "20,000" | df_ucx$ucx_result %like% "24,000" | df_ucx$ucx_result %like% "25,000" | df_ucx$ucx_result %like% "30,000" | df_ucx$ucx_result %like% "35,000" | df_ucx$ucx_result %like% "40,000" | df_ucx$ucx_result %like% "49,000" | df_ucx$ucx_result %like% "50,000" | df_ucx$ucx_result %like% "60,000" | df_ucx$ucx_result %like% "70,000" | df_ucx$ucx_result %like% "74,000" | df_ucx$ucx_result %like% "75,000" | df_ucx$ucx_result %like% "80,000" |df_ucx$ucx_result %like% "99,000" |df_ucx$ucx_result %like% "100,000", ]

#Select any CSNs with any pathogenic organisms
pathogen <- df_ucx[df_ucx$ucx_result %like% "proteus|klebsiella|coli|strep|staph|morganella|pseudomonas|diphtheroid|enterococcus|acinetobacter|enterobacter|serratia|citrobacter|providencia|aeromonas|aerococcus|corynebacterium|salmonella|shigella|gardnerella|achromobacter|pantoea|raoultella|actinobaculum", ]

#Add test columns for CSNs with high CFUs or pathogenic species that don't overlap
greaterthanten$test <- ifelse(greaterthanten$PAT_ENC_CSN_ID %in% pathogen$PAT_ENC_CSN_ID,1,0)
pathogen$test <- ifelse(pathogen$PAT_ENC_CSN_ID %in% greaterthanten$PAT_ENC_CSN_ID,1,0)

#Add column for "positive" result: Logic: Must have pathogen AND (Must have greater than tenK OR not be less than tenK)

#First, create %notin% function
`%notin%` <- Negate(`%in%`)

df_ucx$positive <- ifelse((df_ucx$PAT_ENC_CSN_ID %in% pathogen$PAT_ENC_CSN_ID) & ((df_ucx$PAT_ENC_CSN_ID %in% greaterthanten$PAT_ENC_CSN_ID) | (df_ucx$PAT_ENC_CSN_ID %notin% lessthanten$PAT_ENC_CSN_ID)),1,0) 

#Add column for mixed flora
#df_ucx$mixed_flora <- as.integer(df_ucx$PAT_ENC_CSN_ID %in% mixed_flora$PAT_ENC_CSN_ID)

#Add column for e coli
#df_ucx$ecoli <- as.integer(df_ucx$PAT_ENC_CSN_ID %in% ecoli$PAT_ENC_CSN_ID)

#Add column for "abnormal" results NOTE: ONLY WORKS IF ABNORMAL FLAG IS PRESENT
#df_ucx$is_abn <- as.integer(df_ucx$PAT_ENC_CSN_ID %in% isabn$PAT_ENC_CSN_ID)


#PROCESS ENCOUNTER DATA
#Keep only columns needed (CNS ID, age, sex, attending name, attending id )
df_enc <- df_enc %>% select(c(2,5,10,8,6,7))
df_enc <- df_enc %>% select(c(1,2,5,6))

# Give those columns the same name as the clean dataset
names(df_enc)[1] <- "PAT_ENC_CSN_ID"
names(df_enc)[3] <- "attending_ID"
names(df_enc)[4] <- "attending_name"


#PROCESS CC DATA
#Keep only columns needed
df_cc <- df_cc %>% select(c(2,3))
#collapse, make lower case
df_cc <- df_cc %>%
  group_by(PAT_ENC_CSN_ID) %>%
  summarize(chief_complaints = str_c(chief_complaint, collapse = "| "))

i <- sapply(df_cc, is.character)
df_cc[i] <- lapply(df_cc[i], tolower)


#PROCESS DIAGNOSIS DATA
#Keep only columns needed
df_diag <- df_diag %>% select(c(2,3))
#collapse, make lower case
df_diag <- df_diag %>%
    group_by(PAT_ENC_CSN_ID) %>%
    summarize(diagnoses = str_c(ed_diagnosis, collapse = "| "))

i <- sapply(df_diag, is.character)
df_diag[i] <- lapply(df_diag[i], tolower)

#add column for UTI dx

df_diag$utidiagnosis <- ifelse(str_detect(df_diag$diagnoses, "cystitis"),1,0)
df_diag$utidiagnosis <- ifelse(str_detect(df_diag$diagnoses, "interstitial cystitis") | str_detect(df_diag$diagnoses, "cholecystitis"),0,df_diag$utidiagnosis)
df_diag$utidiagnosis <- ifelse(str_detect(df_diag$diagnoses, "pyelonephritis")|str_detect(df_diag$diagnoses, "bladder infection")| str_detect(df_diag$diagnoses, "urinary tract infection")|str_detect(df_diag$diagnoses, "uti\\ "), 1,df_diag$utidiagnosis)

#add column for alternative infectious diagnosis 
df_diag$altdiagnosis <- ifelse(str_detect(df_diag$diagnoses, "pneumonia")|
                                 str_detect(df_diag$diagnoses, "pna")|
                                 str_detect(df_diag$diagnoses, "\\ dental\\ ")|
                                 str_detect(df_diag$diagnoses, "cellulitis")|
                                 str_detect(df_diag$diagnoses, "erysipelas")|
                                 str_detect(df_diag$diagnoses, "osteomyelitis")|
                                 str_detect(df_diag$diagnoses, "diverticulitis")|
                                 str_detect(df_diag$diagnoses, "cholecystitis")|
                                 str_detect(df_diag$diagnoses, "appendicitis")|
                                 str_detect(df_diag$diagnoses, "abscess")|
                                 str_detect(df_diag$diagnoses, "pharyngitis")|
                                 str_detect(df_diag$diagnoses, "tonsillitis")|
                                 str_detect(df_diag$diagnoses, "strep")|
                                 str_detect(df_diag$diagnoses, "pharyngitis")|
                                 str_detect(df_diag$diagnoses, "vaginosis")|
                                 str_detect(df_diag$diagnoses, "chlamydia")|
                                 str_detect(df_diag$diagnoses, "gonorrhea")|
                                 str_detect(df_diag$diagnoses, "std")|
                                 str_detect(df_diag$diagnoses, "sexually transmitted")|
                                 str_detect(df_diag$diagnoses, "pelvic inflammatory")|
                                 str_detect(df_diag$diagnoses, "endometritis")|
                                 str_detect(df_diag$diagnoses, "prostatitis")|
                                 str_detect(df_diag$diagnoses, "orchitis")|
                                 str_detect(df_diag$diagnoses, "periodontitis")|
                                 str_detect(df_diag$diagnoses, "gingivitis")|
                                 str_detect(df_diag$diagnoses, "colitis")|
                                 str_detect(df_diag$diagnoses, "enteritis")|
                                 str_detect(df_diag$diagnoses, "diarrhea")|
                                 str_detect(df_diag$diagnoses, "pneumonia")|
                                 str_detect(df_diag$diagnoses, "pylori")|
                                 str_detect(df_diag$diagnoses, "gastritis")|
                                 str_detect(df_diag$diagnoses, "phlebitis")|
                                 str_detect(df_diag$diagnoses, "otitis")|
                                 str_detect(df_diag$diagnoses, "balanoposthitis")|
                                 str_detect(df_diag$diagnoses, "balanitis")|
                                 str_detect(df_diag$diagnoses, "epididymitis")|
                                 str_detect(df_diag$diagnoses, "lymphadenitis")|
                                 str_detect(df_diag$diagnoses, "lymphangitis")|
                                 str_detect(df_diag$diagnoses, "bite")|
                                 str_detect(df_diag$diagnoses, "scratch")|
                                 str_detect(df_diag$diagnoses, "infected")|
                                 str_detect(df_diag$diagnoses, "wound infection")|
                                 str_detect(df_diag$diagnoses, "mycobacterium")|
                                 str_detect(df_diag$diagnoses, "pyomyositis")|
                                 str_detect(df_diag$diagnoses, "bacterial")|
                                 str_detect(df_diag$diagnoses, "cholangitis")|
                                 str_detect(df_diag$diagnoses, "osteitis")|
                                 str_detect(df_diag$diagnoses, "pneumonitis")|
                                 str_detect(df_diag$diagnoses, "bursitis")|
                                 str_detect(df_diag$diagnoses, "neutropenic fever")|
                                 str_detect(df_diag$diagnoses, "meningitis")|
                                 str_detect(df_diag$diagnoses, "endocarditis")|
                                 str_detect(df_diag$diagnoses, "gangrene")|
                                 str_detect(df_diag$diagnoses, "mastitis")|
                                 str_detect(df_diag$diagnoses, "typhlitis")|
                                 str_detect(df_diag$diagnoses, "parotitis")|
                                 str_detect(df_diag$diagnoses, "sialoadenitis")|
                                 str_detect(df_diag$diagnoses, "pericoronitis")|
                                 str_detect(df_diag$diagnoses, "osteitis")|
                                 str_detect(df_diag$diagnoses, "prophylaxis")|
                                 str_detect(df_diag$diagnoses, "lyme")|
                                 str_detect(df_diag$diagnoses, "folliculitis")|
                                 str_detect(df_diag$diagnoses, "sinusitis")|
                                 str_detect(df_diag$diagnoses, "pustular")|
                                 str_detect(df_diag$diagnoses, "paronychia"),1,0)

#Add column for UTI symptoms - based on chief complaints and symptoms used in 2018 paper, and some synonyms. Excluded vague symptoms (e.g., fever), which could easily apply to other diagnoses.
df_diag$utisymptomdx <- ifelse(str_detect(df_diag$diagnoses, "flank pain")|
                                 str_detect(df_diag$diagnoses, "suprapubic pain")|
                                 str_detect(df_diag$diagnoses, "suprapubic pressure")|
                                 str_detect(df_diag$diagnoses, "suprapubic abdominal pain")|
                                 str_detect(df_diag$diagnoses, "pelvic pain")|
                                 str_detect(df_diag$diagnoses, "pelvic pressure")|
                                 str_detect(df_diag$diagnoses, "bladder pain")|
                                 str_detect(df_diag$diagnoses, "hematuria")|
                                 str_detect(df_diag$diagnoses, "pyuria")|
                                 str_detect(df_diag$diagnoses, "dysuria")|
                                 str_detect(df_diag$diagnoses, "bacteriuria")|
                                 str_detect(df_diag$diagnoses, "painful urination")|
                                 str_detect(df_diag$diagnoses, "pain with urination")|
                                 str_detect(df_diag$diagnoses, "burning with urination")|
                                 str_detect(df_diag$diagnoses, "urinary retention")|
                                 str_detect(df_diag$diagnoses, "difficulty urinating")|
                                 str_detect(df_diag$diagnoses, "urinary frequency")|
                                 str_detect(df_diag$diagnoses, "frequent urination")|
                                 str_detect(df_diag$diagnoses, "urinary urgency"),1,0)


table(df_diag$utidiagnosis,df_diag$altdiagnosis)       
#Proccess med data

#Ed abx - add column for relevant abx

df_edabx %>%
  group_by(Name) %>%
  summarise(Count=  n()) %>%
  arrange(-Count) %>%
  write.csv("Names.csv")

#Define vector for possible uti abx
utiabx <- c("CEPHALEXIN 500 MG CAPSULE",
"CIPROFLOXACIN 500 MG TABLET",
"SULFAMETHOXAZOLE 800 MG-TRIMETHOPRIM 160 MG TABLET",
"NITROFURANTOIN MONOHYDRATE/MACROCRYSTALS 100 MG CAPSULE",
"AMOXICILLIN 875 MG-POTASSIUM CLAVULANATE 125 MG TABLET",
"CEPHALEXIN 250 MG/5 ML ORAL SUSPENSION",
"CIPROFLOXACIN 250 MG TABLET",
"CEFDINIR 125 MG/5 ML ORAL SUSPENSION",
"CEPHALEXIN 250 MG CAPSULE",
"CEFUROXIME AXETIL 250 MG TABLET",
"NITROFURANTOIN MACROCRYSTAL 100 MG CAPSULE",
"CEFUROXIME AXETIL 500 MG TABLET",
"SULFAMETHOXAZOLE 200 MG-TRIMETHOPRIM 40 MG/5 ML ORAL SUSPENSION",
"LEVOFLOXACIN 500 MG TABLET",
"CEFDINIR 300 MG CAPSULE",
"NITROFURANTOIN MACROCRYSTAL 50 MG CAPSULE",
"CEFDINIR 250 MG/5 ML ORAL SUSPENSION",
"FOSFOMYCIN TROMETHAMINE 3 GRAM ORAL PACKET",
"AMOXICILLIN 600 MG-POTASSIUM CLAVULANATE 42.9 MG/5 ML ORAL SUSPENSION",
"AMOXICILLIN 500 MG-POTASSIUM CLAVULANATE 125 MG TABLET",
"AMOXICILLIN 400 MG-POTASSIUM CLAVULANATE 57 MG/5 ML ORAL SUSPENSION",
"SULFAMETHOXAZOLE 400 MG-TRIMETHOPRIM 80 MG TABLET",
"AMOXICILLIN 250 MG-POTASSIUM CLAVULANATE 62.5 MG/5 ML ORAL SUSPENSION",
"CEFPODOXIME 200 MG TABLET",
"LEVOFLOXACIN 750 MG TABLET",
"CIPROFLOXACIN 750 MG TABLET",
"LEVOFLOXACIN 250 MG TABLET",
"CEFPODOXIME 100 MG TABLET",
"CEPHALEXIN 125 MG/5 ML ORAL SUSPENSION",
"NITROFURANTOIN 25 MG/5 ML ORAL SUSPENSION",
"CIPROFLOXACIN 250 MG/5 ML ORAL SUSPENSION",
"AMOXICILLIN 125 MG-POTASSIUM CLAVULANATE 31.25 MG/5 ML ORAL SUSP",
"CEFADROXIL 500 MG CAPSULE",
"CEFIXIME 400 MG CAPSULE",
"CEFIXIME 200 MG/5 ML ORAL SUSPENSION",
"CEFIXIME 400 MG TABLET",
"CEFIXIME 100 MG/5 ML ORAL SUSPENSION",
"AMOXICILLIN 250 MG-POTASSIUM CLAVULANATE 125 MG TABLET",
"CEFUROXIME AXETIL 250 MG/5 ML ORAL SUSPENSION",
"AMOXICILLIN-POTASSIUM CLAVULANATE 1,000 MG-62.5 MG TABLET,EXT.REL 12HR",
"AUGMENTIN 500 MG-125 MG TABLET",
"KEFLEX 500 MG CAPSULE",
"AUGMENTIN 875 MG-125 MG TABLET",
"MACROBID 100 MG CAPSULE",
"BACTRIM 400 MG-80 MG TABLET",
"BACTRIM ORAL",
"CIPRO 500 MG TABLET",
"LEVAQUIN ORAL",
"AUGMENTIN ES-600  600 MG-42.9 MG/5 ML ORAL SUSPENSION",
"CEFADROXIL 1 GRAM TABLET",
"CEFPODOXIME 100 MG/5 ML ORAL SUSPENSION",
"CEFTIN 250 MG TABLET",
"CEPHALEXIN 500 MG TABLET",
"CEPHALEXIN 750 MG CAPSULE",
"LEVAQUIN 750 MG TABLET",
"METHENAMINE HIPPURATE 1 GRAM TABLET",
"NITROFURANTOIN MACROCRYSTAL ORAL",
"OFLOXACIN 200 MG TABLET",
"OFLOXACIN 300 MG TABLET",
"OFLOXACIN 400 MG TABLET",
"AMOXICILLIN 250 MG/5 ML ORAL SUSPENSION",
"AMOXICILLIN 500 MG CAPSULE",
"AMOXICILLIN 400 MG/5 ML ORAL SUSPENSION",
"AMOXICILLIN 250 MG CAPSULE",
"AMOXICILLIN 875 MG TABLET",
"AMOXICILLIN ORAL",
"AMOXICILLIN 200 MG/5 ML ORAL SUSPENSION",
"AMOXICILLIN 250 MG CHEWABLE TABLET",
"AMOXICILLIN 400 MG CHEWABLE TABLET")

#Add column for Rx
df_edabx$utiabxrx <- ifelse (df_edabx$Name %in% utiabx & df_edabx$Mode=="Outpatient", 1, 0)
utiabxrx <- df_edabx[df_edabx$utiabxrx==1, ]
#Remove duplicate CSNs
utiabxrx <- utiabxrx[order(utiabxrx$PAT_ENC_CSN_ID),]
utiabxrx <- utiabxrx[!duplicated(utiabxrx$PAT_ENC_CSN_ID),]

#Keep only columns needed (CSN, Rx)
utiabxrx <- utiabxrx %>% select(c(1,14))
names(utiabxrx)[2] <- "UTI_Antibiotic_Rx"

#JOIN DATAFRAMES 

uniqueCSN <- unique(df_full$PAT_ENC_CSN_ID)
length(uniqueCSN)


#Left join files
joined <- right_join(df_fulldemo, dfs, by = c("PAT_ENC_CSN_ID"="PAT_ENC_CSN_ID"))
joined <- left_join(joined, df_ucx, by =c("PAT_ENC_CSN_ID"="PAT_ENC_CSN_ID"))
joined <- left_join(joined, df_enc, by =c("PAT_ENC_CSN_ID"="PAT_ENC_CSN_ID"))
joined <- left_join(joined, df_cc, by =c("PAT_ENC_CSN_ID"="PAT_ENC_CSN_ID"))
joined <- left_join(joined, df_diag, by =c("PAT_ENC_CSN_ID"="PAT_ENC_CSN_ID"))
joined <- left_join(joined, utiabxrx, by =c("PAT_ENC_CSN_ID"="PAT_ENC_CSN_ID"))

#Replace NAs with "not performed" and blanks with 0
joined$ucx_result <- ifelse(is.na(joined$ucx_result),"not_performed",joined$ucx_result)
joined$positive <- ifelse(is.na(joined$positive),"not_performed",joined$positive)
joined$UTI_Antibiotic_Rx <- ifelse(is.na(joined$UTI_Antibiotic_Rx),0,joined$UTI_Antibiotic_Rx)

#Add column for our final provider decision outcome -- official diagnosis or presumed treatment of UTI
#Goal: UTI diagnosis OR (antibiotics and UTI symptom diagnosis) OR (antibiotics  AND NOT alternative diagnosis)

names(joined)[24] <- "UTI_Diagnosis"
names(joined)[25] <- "Alternative_Diagnosis"
names(joined)[26] <- "UTI_Symptom_Diagnosis"

joined$UTI_Diagnosis_Or_Treatment <- ifelse(joined$UTI_Diagnosis=="1"|(joined$UTI_Antibiotic_Rx=="1" & joined$UTI_Symptom_Diagnosis=="1" & joined$Alternative_Diagnosis=="0"),1,0)

#Exclude those under 18 and pediatric/urgent care sites
joined_adults <- subset(joined, age>17)
joined_adults_EDs <- subset(joined_adults, DepartmentName!="BH FAIRFIELD URGENT CARE" & DepartmentName!="YHC ACUTE AND INPATIENT CARE" & DepartmentName!="YNH CRITICAL CARE TRANSPORT" & DepartmentName!="YNH EMERGENCY PEDIATRIC")


#Save new table
write.csv(joined_adults_EDs,"processedresults7.11.2022.csv", row.names = FALSE)

#Include ONLY patients seen by physicians who saw 80+ patients (i.e, those with physician-level models)
df_processed <- read.csv("/Users/msi7/Library/CloudStorage/Box-Box/UTI Noise Project/Data/interim/processedresults7.11.2022.csv")

attendings <- table(df_processed$attending_ID)

df_processed80 <- subset(df_processed, attending_ID %in% names(attendings[attendings >= 80])) 

write.csv(df_processed80,"processedresults7.11.2022_80plus.csv", row.names = FALSE)

###Join "Master" and processed files

#Drop redundant columns from master 
df_master <- subset(df_master, select = -c(63,3,55,50,56,57,49,52,59,167,170,169,53))

#Left join
df_master_processed <- left_join(df_processed, df_master, by =c("PAT_ENC_CSN_ID"="PAT_ENC_CSN_ID"))
df_master_processed80 <- left_join(df_processed80, df_master, by =c("PAT_ENC_CSN_ID"="PAT_ENC_CSN_ID"))

#write csvs
write.csv(df_master_processed,"master_processed_results7.11.2022.csv", row.names = FALSE)
write.csv(df_master_processed80,"master_processed_results7.11.2022_80plus.csv", row.names = FALSE)

dfmaster <- read.csv("")
#Investigating whether there may be true positives in the subset of patients without a UCx sent
df_final <- read.csv("processedresults3.1.2022.csv")
df_final$positive <- ifelse(is.na(df_final$positive),"not_performed",df_final$positive)
df_final$UTI_Antibiotic_Rx <- ifelse(is.na(df_final$UTI_Antibiotic_Rx),0,df_final$UTI_Antibiotic_Rx)
write.csv(df_final,"processedresults3.1.2022.csv", row.names = FALSE)





#Testing how patients meet inclusion in final physician decision variable 
#table(df_final$ua_nitrite,df_final$positive)
#table(df_final$ua_bacteria,df_final$positive)
#table(df_final$ua_wbc,df_final$positive)
#table(df_final$ua_leuk,df_final$positive)

table(joined$Alternative_Diagnosis,joined$UTI_Symptom_Diagnosis)
table(joined$Alternative_Diagnosis,joined$UTI_Antibiotic_Rx) 
table(joined$UTI_Diagnosis_Or_Treatment)
table(joined$UTI_Diagnosis_Or_Treatment,joined$UTI_Antibiotic_Rx)


table(joined$UTI_Diagnosis)
table(joined$UTI_Diagnosis,joined$UTI_Antibiotic_Rx)
table(joined$UTI_Diagnosis_Or_Treatment)
table(joined$utidiagnosis,joined$positive)
table(joined$utidiagnosis,joined$UTI_Antibiotic_Rx)
nodiag <- subset(joined, UTI_Diagnosis==0)
secondcat <- subset(nodiag, UTI_Antibiotic_Rx==1 & UTI_Symptom_Diagnosis==1)
thirdeligible <- subset(nodiag, nodiag$PAT_ENC_CSN_ID %notin% secondcat$PAT_ENC_CSN_ID)
nodiag <- subset(nodiag, UTI_Symptom)

table(nodiag$UTI_Antibiotic_Rx,nodiag$UTI_Symptom_Diagnosis)
table(thirdeligible$UTI_Antibiotic_Rx,thirdeligible$Alternative_Diagnosis)

noua <- is.na(joined$ArrivalInstant)

table(noua)
table(df_enc$attn_name)

checkingsep <- separate_rows(checking, chief_complaints, sep='\\| ')

ccs <- checkingsep %>% count(chief_complaints, sort = TRUE)
checking %>% count(chief_complaints, sort = TRUE)