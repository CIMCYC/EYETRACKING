# install packages if not installed previously, delete the hashtags and run the code, needs to be done only once

# install.packages("tidyverse")
# install.packages("MASS")
# install.packages("lme4")
# install.packages("jtools")
# install.packages("harrypotter")
# install.packages("afex")
# install.packages("ggeffects")
# install.packages("see")
# install.packages("performance")
# install.packages("emmeans")
# install.packages("lattice")
# install.packages("eyetrackingR")
# install.packages("tictoc")

# load packages, needs to be done every time you start a new session in R
library(rio)
library(readr)
library(tidyverse)
library(MASS)
library(lme4)
library(jtools)
library(harrypotter)
library(afex)
library(ggeffects)
library(see)
library(performance)
library(emmeans)
library(lattice)
library(eyetrackingR)
library(Matrix)
library(tictoc)

# prevent scientific notation
options(scipen = 999)

# increase max print in the console
options(max.print = 10000)

# get your working directory
getwd()

# set your working directory here, it should contain the data:
setwd("/  /")
# setwd("/Users/Filip/Desktop/PhD 2020/___arXives/_20211125 - mentor (Paula) TFM/3. data analysis/data/")

# verify it is set correctly
getwd()

# create a list that contains names of each data file
temp = list.files(pattern="*.csv")
View(temp)

# load the data (might take a while)
tic()
myfiles = lapply(temp, read_csv)
toc()
# 16.446 sec elapsed

# convert the data into a single data frame (will take a while)
tic()
df_eye <- Reduce(rbind, lapply(myfiles, data.frame))
toc()
# 286.909 sec elapsed

# clean the environment
rm(myfiles,
   temp)

# I do some preprocessing, data transformation and cleaning:

# arrange the data frame by participant name
df_eye <- df_eye %>% arrange("RECORDING_SESSION_LABEL")

# eliminate filler trials
df_eye <- df_eye %>% filter(condition != "fill1_con" &
                            condition != "fill1_inc" &
                            condition != "fill2_con" &
                            condition != "fill2_inc"
                            )

# check dimensions of the data frame
dim(df_eye)
# rows           collumns
# 14414378       12

# check the first six rows of the data frame
head(df_eye)

# check column names
colnames(df_eye)

# explanation of the column names:

# RECORDING_SESSION_LABEL   ~ block (each participant completed two, for example, blocks 103 and 104 belong to participant 1)
# TRIAL_INDEX  
# condition                 ~ 2 (animacy: animate vs. inanimate) x 2 (gender-congruency: congruent vs. incongruent) orthogonal manipulation
# TIMESTAMP                 ~ time in [ms], needs to be recoded
# SAMPLE_MESSAGE            ~ will be used to recode timestamp
# RIGH_INTEREST_AREA_ID     ~ where is the participant looking: 1 = taget, 2 = distractor, "." = other area
# RIGHT_IN_BLINK            ~ 1 = there was a blink, 0 = no blink (blinks will be eliminated later)
# RIGHT_IN_SACCADE          ~ 1 = there was a saccadic movement, 0 = there was none (saccades will be eliminated later)
# RESPONSE                  ~ correct/incorrect
# RT                        ~ response times in [ms]
# target                    ~ the target stimuli
# distractor                ~ the distractor

# transform and contrast-code dependent variables for later analysis
df_eye <- df_eye %>% 
  mutate(ACC = if_else(RESPONSE == "correct", 1,0),
         subject = NA,
         # item = NA,
         # block.cod = NA,
         # age = NA,
         # sex.cod =  NA,
         # voice_judgment.cod = NA,
         # vision.cod = NA,
         animacy            = if_else(condition == "bio_con" | condition == "bio_inc", "animado", "inanimado"),
         animacy.cod        = if_else(condition == "bio_con" | condition == "bio_inc", -0.5, 0.5), # effects contrast-coding
         gen_congruency     = if_else(condition == "art_con" | condition == "bio_con", "congruente", "incongruente"),
         gen_congruency.cod = if_else(condition == "art_con" | condition == "bio_con", -0.5, 0.5)  # effects contrast-coding
         #, target_gender.cod = NA
  )

# sanity check
xtabs(~ animacy + gen_congruency, data = df_eye) # more or less the same amount of data per each cell: ok

# code subjects
df_eye$RECORDING_SESSION_LABEL <- as.factor(df_eye$RECORDING_SESSION_LABEL)
df_eye$subject <- df_eye$RECORDING_SESSION_LABEL

# # code subjects with for loop: takes a lot of time
# df_block <- read_csv2("block.csv")
# 
# i = 1
# j = 1
# 
# for(i in 1:nrow(df_eye)){
#   for(j in 1:nrow(df_block)) {
#     if(df_eye$RECORDING_SESSION_LABEL[i] == df_block$RECORDING_SESSION_LABEL[j]){
#     df_eye$subject[i] <- df_block$subject[j]
#     }
#   }
# }

# somewhat a crude way to code subject column (but it is fast and it works)
levels(df_eye$subject) <- c("S001",
                            "S001",
                            "S002",
                            "S002",
                            "S003",
                            "S003",
                            "S004",
                            "S004",
                            "S005",
                            "S005",
                            "S006",
                            "S006",
                            "S007",
                            "S007",
                            "S008",
                            "S008",
                            "S009",
                            "S009",
                            "S010",
                            "S010",
                            "S011",
                            "S011",
                            "S012",
                            "S012",
                            "S013",
                            "S013",
                            "S014",
                            "S014",
                            "S015",
                            "S015",
                            "S016",
                            "S016",
                            "S017",
                            "S017",
                            "S018",
                            "S018",
                            "S019",
                            "S019",
                            "S020",
                            "S020",
                            "S021",
                            "S021",
                            "S022",
                            "S022",
                            "S023",
                            "S023",
                            "S024",
                            "S024",
                            "S025",
                            "S025",
                            "S026",
                            "S026",
                            "S027",
                            "S027",
                            "S028",
                            "S028",
                            "S029",
                            "S029",
                            "S030",
                            "S030",
                            "S031",
                            "S031",
                            "S032",
                            "S032",
                            "S033",
                            "S033",
                            "S034",
                            "S034",
                            "S035",
                            "S035",
                            "S036",
                            "S036",
                            "S037",
                            "S037",
                            "S038",
                            "S038",
                            "S039",
                            "S039",
                            "S040",
                            "S040",
                            "S041",
                            "S041",
                            "S042",
                            "S042",
                            "S043",
                            "S043",
                            "S044",
                            "S044",
                            "S045",
                            "S045",
                            "S046",
                            "S046",
                            "S047",
                            "S047",
                            "S048",
                            "S048",
                            "S049",
                            "S049")

# sanity check
xtabs(~subject + RECORDING_SESSION_LABEL, data = df_eye)
# two consecutive "blocks" belong to the same participant: ok


# code age
# AGE
df_eye$age <- case_when(df_eye$subject == "S001" ~ 21,
                        df_eye$subject == "S002" ~ 20,
                        df_eye$subject == "S003" ~ 22,
                        df_eye$subject == "S004" ~ 21,
                        df_eye$subject == "S005" ~ 20,
                        df_eye$subject == "S006" ~ 19,
                        df_eye$subject == "S007" ~ 27,
                        df_eye$subject == "S008" ~ 20,
                        df_eye$subject == "S009" ~ 19,
                        df_eye$subject == "S010" ~ 26,
                        df_eye$subject == "S011" ~ 22,
                        df_eye$subject == "S012" ~ 19,
                        df_eye$subject == "S013" ~ 25,
                        df_eye$subject == "S014" ~ 26,
                        df_eye$subject == "S015" ~ 22,
                        df_eye$subject == "S016" ~ 20,
                        df_eye$subject == "S017" ~ 22,
                        df_eye$subject == "S018" ~ 19,
                        df_eye$subject == "S019" ~ 19,
                        df_eye$subject == "S020" ~ 22,
                        df_eye$subject == "S021" ~ 25,
                        df_eye$subject == "S022" ~ 29,
                        df_eye$subject == "S023" ~ 29,
                        df_eye$subject == "S024" ~ 20,
                        df_eye$subject == "S025" ~ 22,
                        df_eye$subject == "S026" ~ 20,
                        df_eye$subject == "S027" ~ 33,
                        df_eye$subject == "S028" ~ 19,
                        df_eye$subject == "S029" ~ 19,
                        df_eye$subject == "S030" ~ 22,
                        df_eye$subject == "S031" ~ 23,
                        df_eye$subject == "S032" ~ 22,
                        df_eye$subject == "S033" ~ 21,
                        df_eye$subject == "S034" ~ 18,
                        df_eye$subject == "S035" ~ 23,
                        df_eye$subject == "S036" ~ 27,
                        df_eye$subject == "S037" ~ 23,
                        df_eye$subject == "S038" ~ 20,
                        df_eye$subject == "S039" ~ 31,
                        df_eye$subject == "S040" ~ 20,
                        df_eye$subject == "S041" ~ 24,
                        df_eye$subject == "S042" ~ 19,
                        df_eye$subject == "S043" ~ 27,
                        df_eye$subject == "S044" ~ 18,
                        df_eye$subject == "S045" ~ 30,
                        df_eye$subject == "S046" ~ 22,
                        df_eye$subject == "S047" ~ 29,
                        df_eye$subject == "S048" ~ 29,
                        df_eye$subject == "S049" ~ 26
                        )

df_eye$age.cod <- scale(df_eye$age, center = TRUE, scale = TRUE)

# code handedness
df_eye$handedness <- case_when(df_eye$subject == "S001" ~ "D",
                               df_eye$subject == "S002" ~ "D",
                               df_eye$subject == "S003" ~ "D",
                               df_eye$subject == "S004" ~ "D",
                               df_eye$subject == "S005" ~ "D",
                               df_eye$subject == "S006" ~ "D",
                               df_eye$subject == "S007" ~ "D",
                               df_eye$subject == "S008" ~ "D",
                               df_eye$subject == "S009" ~ "D",
                               df_eye$subject == "S010" ~ "D",
                               df_eye$subject == "S011" ~ "D",
                               df_eye$subject == "S012" ~ "D",
                               df_eye$subject == "S013" ~ "D",
                               df_eye$subject == "S014" ~ "D",
                               df_eye$subject == "S015" ~ "D",
                               df_eye$subject == "S016" ~ "Z",
                               df_eye$subject == "S017" ~ "D",
                               df_eye$subject == "S018" ~ "D",
                               df_eye$subject == "S019" ~ "D",
                               df_eye$subject == "S020" ~ "D",
                               df_eye$subject == "S021" ~ "D",
                               df_eye$subject == "S022" ~ "D",
                               df_eye$subject == "S023" ~ "D",
                               df_eye$subject == "S024" ~ "D",
                               df_eye$subject == "S025" ~ "D",
                               df_eye$subject == "S026" ~ "D",
                               df_eye$subject == "S027" ~ "D",
                               df_eye$subject == "S028" ~ "D",
                               df_eye$subject == "S029" ~ "Z",
                               df_eye$subject == "S030" ~ "D",
                               df_eye$subject == "S031" ~ "D",
                               df_eye$subject == "S032" ~ "D",
                               df_eye$subject == "S033" ~ "D",
                               df_eye$subject == "S034" ~ "D",
                               df_eye$subject == "S035" ~ "D",
                               df_eye$subject == "S036" ~ "D",
                               df_eye$subject == "S037" ~ "D",
                               df_eye$subject == "S038" ~ "D",
                               df_eye$subject == "S039" ~ "D",
                               df_eye$subject == "S040" ~ "D",
                               df_eye$subject == "S041" ~ "D",
                               df_eye$subject == "S042" ~ "D",
                               df_eye$subject == "S043" ~ "D",
                               df_eye$subject == "S044" ~ "D",
                               df_eye$subject == "S045" ~ "D",
                               df_eye$subject == "S046" ~ "D",
                               df_eye$subject == "S047" ~ "D",
                               df_eye$subject == "S048" ~ "Z",
                               df_eye$subject == "S049" ~ "D"
                               )

xtabs(~ handedness + subject, data = df_eye)

# code voice judgment
df_eye$voice_judgment <- case_when(df_eye$subject == "S001" ~ "M",
                                   df_eye$subject == "S002" ~ "M",
                                   df_eye$subject == "S003" ~ "F",
                                   df_eye$subject == "S004" ~ "M",
                                   df_eye$subject == "S005" ~ "F",
                                   df_eye$subject == "S006" ~ "M",
                                   df_eye$subject == "S007" ~ "M",
                                   df_eye$subject == "S008" ~ "M",
                                   df_eye$subject == "S009" ~ "F",
                                   df_eye$subject == "S010" ~ "M",
                                   df_eye$subject == "S011" ~ "M",
                                   df_eye$subject == "S012" ~ "M",
                                   df_eye$subject == "S013" ~ "M",
                                   df_eye$subject == "S014" ~ "M",
                                   df_eye$subject == "S015" ~ "M",
                                   df_eye$subject == "S016" ~ "M",
                                   df_eye$subject == "S017" ~ "M",
                                   df_eye$subject == "S018" ~ "F",
                                   df_eye$subject == "S019" ~ "F",
                                   df_eye$subject == "S020" ~ "M",
                                   df_eye$subject == "S021" ~ "F",
                                   df_eye$subject == "S022" ~ "M",
                                   df_eye$subject == "S023" ~ "M",
                                   df_eye$subject == "S024" ~ "M",
                                   df_eye$subject == "S025" ~ "F",
                                   df_eye$subject == "S026" ~ "M",
                                   df_eye$subject == "S027" ~ "F",
                                   df_eye$subject == "S028" ~ "M",
                                   df_eye$subject == "S029" ~ "M",
                                   df_eye$subject == "S030" ~ "F",
                                   df_eye$subject == "S031" ~ "F",
                                   df_eye$subject == "S032" ~ "F",
                                   df_eye$subject == "S033" ~ "M",
                                   df_eye$subject == "S034" ~ "M",
                                   df_eye$subject == "S035" ~ "F",
                                   df_eye$subject == "S036" ~ "M",
                                   df_eye$subject == "S037" ~ "M",
                                   df_eye$subject == "S038" ~ "M",
                                   df_eye$subject == "S039" ~ "M",
                                   df_eye$subject == "S040" ~ "M",
                                   df_eye$subject == "S041" ~ "M",
                                   df_eye$subject == "S042" ~ "M",
                                   df_eye$subject == "S043" ~ "F",
                                   df_eye$subject == "S044" ~ "M",
                                   df_eye$subject == "S045" ~ "M",
                                   df_eye$subject == "S046" ~ "M",
                                   df_eye$subject == "S047" ~ "M",
                                   df_eye$subject == "S048" ~ "F",
                                   df_eye$subject == "S049" ~ "M"
                                   )

df_eye$voice_judgment.cod <- if_else(df_eye$voice_judgment == "F", -0.5, 0.5)
xtabs(~ voice_judgment.cod, data = df_eye)

# code block
df_eye$block <- case_when(df_eye$RECORDING_SESSION_LABEL == "103" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "104" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "105" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "106" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "107" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "108" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "109" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "110" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "111" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "112" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "113" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "114" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "115" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "116" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "117" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "118" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "119" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "120" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "121" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "122" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "123" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "124" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "125" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "126" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "127" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "128" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "129" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "130" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "131" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "132" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "133" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "134" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "135" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "136" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "137" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "138" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "139" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "140" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "141" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "142" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "143" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "144" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "145" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "146" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "147" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "148" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "149" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "150" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "151" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "152" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "153" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "154" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "201" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "202" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "203" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "204" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "205" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "206" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "207" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "208" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "209" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "210" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "211" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "212" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "213" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "214" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "215" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "216" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "217" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "218" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "219" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "220" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "221" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "222" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "223" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "224" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "225" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "226" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "227" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "228" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "229" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "230" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "231" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "232" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "233" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "234" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "237" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "238" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "239" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "240" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "241" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "242" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "243" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "244" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "245" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "246" ~ 2,
                          df_eye$RECORDING_SESSION_LABEL == "247" ~ 1,
                          df_eye$RECORDING_SESSION_LABEL == "248" ~ 2
)

df_eye$block.cod <- if_else(df_eye$block == 1, -0.5, 0.5)
xtabs(~ block.cod, df_eye)

# code gender of the target
df_eye$target_gender.cod <- case_when(df_eye$target == "CHICA" ~ -0.5,
                                      df_eye$target == "MONJA" ~ -0.5,
                                      df_eye$target == "CAMARERA" ~ -0.5,
                                      df_eye$target == "PROFESORA" ~ -0.5,
                                      df_eye$target == "REINA" ~ -0.5,
                                      df_eye$target == "ENFERMERA" ~ -0.5,
                                      df_eye$target == "RATA" ~ -0.5,
                                      df_eye$target == "CEBRA" ~ -0.5,
                                      df_eye$target == "BALLENA" ~ -0.5,
                                      df_eye$target == "ARDILLA" ~ -0.5,
                                      df_eye$target == "JIRAFA" ~ -0.5,
                                      df_eye$target == "FOCA" ~ -0.5,
                                      df_eye$target == "CIGUENA" ~ -0.5,
                                      df_eye$target == "PALOMA" ~ -0.5,
                                      df_eye$target == "HORMIGA" ~ -0.5,
                                      df_eye$target == "ABEJA" ~ -0.5,
                                      df_eye$target == "TORTUGA" ~ -0.5,
                                      df_eye$target == "COBRA" ~ -0.5,
                                      df_eye$target == "GRANJA" ~ -0.5,
                                      df_eye$target == "MANSION" ~ -0.5,
                                      df_eye$target == "FARMACIA" ~ -0.5,
                                      df_eye$target == "TORRE" ~ -0.5,
                                      df_eye$target == "PIRAMIDE" ~ -0.5,
                                      df_eye$target == "MURALLA" ~ -0.5,
                                      df_eye$target == "RUEDA" ~ -0.5,
                                      df_eye$target == "PANDERETA" ~ -0.5,
                                      df_eye$target == "LAVADORA" ~ -0.5,
                                      df_eye$target == "COMETA" ~ -0.5,
                                      df_eye$target == "CAMARA" ~ -0.5,
                                      df_eye$target == "CESTA" ~ -0.5,
                                      df_eye$target == "GUITARRA" ~ -0.5,
                                      df_eye$target == "PLANCHA" ~ -0.5,
                                      df_eye$target == "ESTUFA" ~ -0.5,
                                      df_eye$target == "JERINGUILLA" ~ -0.5,
                                      df_eye$target == "MIRILLA" ~ -0.5,
                                      df_eye$target == "PIPA" ~ -0.5,
                                      TRUE ~  0.5)

xtabs(~target_gender.cod, data = df_eye)

# code "trial.cod" (combinations of targets and distractors)
df_eye$trial.cod <- case_when(df_eye$target == "CHICA"       & df_eye$distractor == "ADIVINA"      ~ 1    ,
                              df_eye$target == "MONJA"       & df_eye$distractor == "PRINCESA"     ~ 2    ,
                              df_eye$target == "CAMARERA"    & df_eye$distractor == "BAILARINA"    ~ 3    ,
                              df_eye$target == "PROFESORA"   & df_eye$distractor == "BAILARIN"     ~ 4    ,
                              df_eye$target == "REINA"       & df_eye$distractor == "MIMO"         ~ 5    ,
                              df_eye$target == "ENFERMERA"   & df_eye$distractor == "PORTERO"      ~ 6    ,
                              df_eye$target == "CARTERO"     & df_eye$distractor == "NADADOR"      ~ 7    ,
                              df_eye$target == "BOMBERO"     & df_eye$distractor == "HOMBRE"       ~ 8    ,
                              df_eye$target == "MEDICO"      & df_eye$distractor == "PROFESOR"     ~ 9    ,
                              df_eye$target == "TORERO"      & df_eye$distractor == "EMBARAZADA"   ~ 10   ,
                              df_eye$target == "PINTOR"      & df_eye$distractor == "NINA"         ~ 11   ,
                              df_eye$target == "NINO"        & df_eye$distractor == "MUJER"        ~ 12   ,
                              df_eye$target == "RATA"        & df_eye$distractor == "MORSA"        ~ 13   ,
                              df_eye$target == "CEBRA"       & df_eye$distractor == "LIEBRE"       ~ 14   ,
                              df_eye$target == "BALLENA"     & df_eye$distractor == "MOFETA"       ~ 15   ,
                              df_eye$target == "ARDILLA"     & df_eye$distractor == "CANGURO"      ~ 16   ,
                              df_eye$target == "JIRAFA"      & df_eye$distractor == "CAMELLO"      ~ 17   ,
                              df_eye$target == "FOCA"        & df_eye$distractor == "ERIZO"        ~ 18   ,
                              df_eye$target == "LINCE"       & df_eye$distractor == "CIERVO"       ~ 19   ,
                              df_eye$target == "DELFIN"      & df_eye$distractor == "ELEFANTE"     ~ 20   ,
                              df_eye$target == "BUFALO"      & df_eye$distractor == "HIPOPOTAMO"   ~ 21   ,
                              df_eye$target == "MURCIELAGO"  & df_eye$distractor == "PANTERA"     ~  22   ,
                              df_eye$target == "JABALI"      & df_eye$distractor == "OVEJA"        ~ 23   ,
                              df_eye$target == "RINOCERONTE" & df_eye$distractor == "LLAMA"        ~ 24   ,
                              df_eye$target == "CIGUENA"     & df_eye$distractor == "GAVIOTA"      ~ 25   ,
                              df_eye$target == "PALOMA"      & df_eye$distractor == "CISNE"        ~ 26   ,
                              df_eye$target == "HORMIGA"     & df_eye$distractor == "ARANA"        ~ 27   ,
                              df_eye$target == "ABEJA"       & df_eye$distractor == "MOSQUITO"     ~ 28   ,
                              df_eye$target == "TORTUGA"     & df_eye$distractor == "LAGARTIJA"    ~ 29   ,
                              df_eye$target == "COBRA"       & df_eye$distractor == "DINOSAURIO"   ~ 30   ,
                              df_eye$target == "PINGUINO"    & df_eye$distractor == "BUHO"         ~ 31   ,
                              df_eye$target == "TUCAN"       & df_eye$distractor == "AVESTRUZ"     ~ 32   ,
                              df_eye$target == "ESCARABAJO"  & df_eye$distractor == "SALTAMONTES"  ~ 33   ,
                              df_eye$target == "ESCORPION"   & df_eye$distractor == "ORUGA"        ~ 34   ,
                              df_eye$target == "SAPO"        & df_eye$distractor == "CAMALEON"     ~ 35   ,
                              df_eye$target == "COCODRILO"   & df_eye$distractor == "RANA"         ~ 36   ,
                              df_eye$target == "GRANJA"      & df_eye$distractor == "FUENTE"       ~ 37   ,
                              df_eye$target == "MANSION"     & df_eye$distractor == "FABRICA"      ~ 38   ,
                              df_eye$target == "FARMACIA"    & df_eye$distractor == "CATEDRAL"     ~ 39   ,
                              df_eye$target == "TORRE"       & df_eye$distractor == "MUSEO"        ~ 40   ,
                              df_eye$target == "PIRAMIDE"    & df_eye$distractor == "INVERNADERO"  ~ 41   ,
                              df_eye$target == "MURALLA"     & df_eye$distractor == "GARAJE"       ~ 42   ,
                              df_eye$target == "CASTILLO"    & df_eye$distractor == "ESTADIO"      ~ 43   ,
                              df_eye$target == "LABORATORIO" & df_eye$distractor == "CEMENTERIO"   ~ 44   ,
                              df_eye$target == "GIMNASIO"    & df_eye$distractor == "MOLINO"       ~ 45   ,
                              df_eye$target == "PUENTE"      & df_eye$distractor == "CASA"         ~ 46   ,
                              df_eye$target == "AEROPUERTO"  & df_eye$distractor == "CARCEL"       ~ 47   ,
                              df_eye$target == "MERCADO"     & df_eye$distractor == "CABANA"       ~ 48   ,
                              df_eye$target == "RUEDA"       & df_eye$distractor == "MANTA"        ~ 49   ,
                              df_eye$target == "PANDERETA"   & df_eye$distractor == "CARRETILLA"   ~ 50   ,
                              df_eye$target == "LAVADORA"    & df_eye$distractor == "IMPRESORA"    ~ 51   ,
                              df_eye$target == "COMETA"      & df_eye$distractor == "ESTUCHE"      ~ 52   ,
                              df_eye$target == "CAMARA"      & df_eye$distractor == "PLATO"        ~ 53   ,
                              df_eye$target == "CESTA"       & df_eye$distractor == "BARRIL"       ~ 54   ,
                              df_eye$target == "PIANO"       & df_eye$distractor == "GRIFO"        ~ 55   ,
                              df_eye$target == "PARAGUAS"    & df_eye$distractor == "EXTINTOR"     ~ 56   ,
                              df_eye$target == "MICROFONO"   & df_eye$distractor == "CORTACESPED"  ~ 57   ,
                              df_eye$target == "SAXOFON"     & df_eye$distractor == "BOMBILLA"     ~ 58   ,
                              df_eye$target == "VIOLIN"      & df_eye$distractor == "BATERIA"      ~ 59   ,
                              df_eye$target == "SALERO"      & df_eye$distractor == "ESCOBA"       ~ 60   ,
                              df_eye$target == "GUITARRA"    & df_eye$distractor == "TROMPETA"     ~ 61   ,
                              df_eye$target == "PLANCHA"     & df_eye$distractor == "TUERCA"       ~ 62   ,
                              df_eye$target == "ESTUFA"      & df_eye$distractor == "BRUJULA"      ~ 63   ,
                              df_eye$target == "JERINGUILLA" & df_eye$distractor == "MICROSCOPIO"  ~ 64   ,
                              df_eye$target == "MIRILLA"     & df_eye$distractor == "TENDERETE"    ~ 65   ,
                              df_eye$target == "PIPA"        & df_eye$distractor == "CORCHO"       ~ 66   ,
                              df_eye$target == "ACORDEON"    & df_eye$distractor == "CLARINETE"    ~ 67   ,
                              df_eye$target == "CASCO"       & df_eye$distractor == "HORNO"        ~ 68   ,
                              df_eye$target == "HILO"        & df_eye$distractor == "TELEFONO"     ~ 69   ,
                              df_eye$target == "TAMBOR"      & df_eye$distractor == "CAFETERA"     ~ 70   ,
                              df_eye$target == "VENTILADOR"  & df_eye$distractor == "ANTORCHA"     ~ 71   ,
                              df_eye$target == "TELESCOPIO"  & df_eye$distractor == "OLLA"         ~ 72   ,
                              df_eye$target == "CHICA"       & df_eye$distractor == "NADADOR"      ~ 73   ,
                              df_eye$target == "MONJA"       & df_eye$distractor == "HOMBRE"       ~ 74   ,
                              df_eye$target == "CAMARERA"    & df_eye$distractor == "PROFESOR"     ~ 75   ,
                              df_eye$target == "PROFESORA"   & df_eye$distractor == "EMBARAZADA"   ~ 76   ,
                              df_eye$target == "REINA"       & df_eye$distractor == "NINA"         ~ 77   ,
                              df_eye$target == "ENFERMERA"   & df_eye$distractor == "MUJER"        ~ 78   ,
                              df_eye$target == "CARTERO"     & df_eye$distractor == "ADIVINA"      ~ 79   ,
                              df_eye$target == "BOMBERO"     & df_eye$distractor == "PRINCESA"     ~ 80   ,
                              df_eye$target == "MEDICO"      & df_eye$distractor == "BAILARINA"    ~ 81   ,
                              df_eye$target == "TORERO"      & df_eye$distractor == "BAILARIN"     ~ 82   ,
                              df_eye$target == "PINTOR"      & df_eye$distractor == "MIMO"         ~ 83   ,
                              df_eye$target == "NINO"        & df_eye$distractor == "PORTERO"      ~ 84   ,
                              df_eye$target == "RATA"        & df_eye$distractor == "CIERVO"       ~ 85   ,
                              df_eye$target == "CEBRA"       & df_eye$distractor == "ELEFANTE"     ~ 86   ,
                              df_eye$target == "BALLENA"     & df_eye$distractor == "HIPOPOTAMO"   ~ 87   ,
                              df_eye$target == "ARDILLA"     & df_eye$distractor == "PANTERA"      ~ 88   ,
                              df_eye$target == "JIRAFA"      & df_eye$distractor == "OVEJA"        ~ 89   ,
                              df_eye$target == "FOCA"        & df_eye$distractor == "LLAMA"        ~ 90   ,
                              df_eye$target == "LINCE"       & df_eye$distractor == "MORSA"        ~ 91   ,
                              df_eye$target == "DELFIN"      & df_eye$distractor == "LIEBRE"       ~ 92   ,
                              df_eye$target == "BUFALO"      & df_eye$distractor == "MOFETA"       ~ 93   ,
                              df_eye$target == "MURCIELAGO"  & df_eye$distractor == "CANGURO"      ~ 94   ,
                              df_eye$target == "JABALI"      & df_eye$distractor == "CAMELLO"      ~ 95   ,
                              df_eye$target == "RINOCERONTE" & df_eye$distractor == "ERIZO"        ~ 96   ,
                              df_eye$target == "CIGUENA"     & df_eye$distractor == "BUHO"         ~ 97   ,
                              df_eye$target == "PALOMA"      & df_eye$distractor == "AVESTRUZ"     ~ 98   ,
                              df_eye$target == "HORMIGA"     & df_eye$distractor == "SALTAMONTES"  ~ 99   ,
                              df_eye$target == "ABEJA"       & df_eye$distractor == "ORUGA"        ~ 100  ,
                              df_eye$target == "TORTUGA"     & df_eye$distractor == "CAMALEON"     ~ 101  ,
                              df_eye$target == "COBRA"       & df_eye$distractor == "RANA"         ~ 102  ,
                              df_eye$target == "PINGUINO"    & df_eye$distractor == "GAVIOTA"      ~ 103  ,
                              df_eye$target == "TUCAN"       & df_eye$distractor == "CISNE"        ~ 104  ,
                              df_eye$target == "ESCARABAJO"  & df_eye$distractor == "ARANA"        ~ 105  ,
                              df_eye$target == "ESCORPION"   & df_eye$distractor == "MOSQUITO"     ~ 106  ,
                              df_eye$target == "SAPO"        & df_eye$distractor == "LAGARTIJA"    ~ 107  ,
                              df_eye$target == "COCODRILO"   & df_eye$distractor == "DINOSAURIO"   ~ 108  ,
                              df_eye$target == "GRANJA"      & df_eye$distractor == "ESTADIO"      ~ 109  ,
                              df_eye$target == "MANSION"     & df_eye$distractor == "CEMENTERIO"   ~ 110  ,
                              df_eye$target == "FARMACIA"    & df_eye$distractor == "MOLINO"       ~ 111  ,
                              df_eye$target == "TORRE"       & df_eye$distractor == "CASA"         ~ 112  ,
                              df_eye$target == "PIRAMIDE"    & df_eye$distractor == "CARCEL"       ~ 113  ,
                              df_eye$target == "MURALLA"     & df_eye$distractor == "CABANA"       ~ 114  ,
                              df_eye$target == "CASTILLO"    & df_eye$distractor == "FUENTE"       ~ 115  ,
                              df_eye$target == "LABORATORIO" & df_eye$distractor == "FABRICA"      ~ 116  ,
                              df_eye$target == "GIMNASIO"    & df_eye$distractor == "CATEDRAL"     ~ 117  ,
                              df_eye$target == "PUENTE"      & df_eye$distractor == "MUSEO"        ~ 118  ,
                              df_eye$target == "AEROPUERTO"  & df_eye$distractor == "INVERNADERO"  ~ 119  ,
                              df_eye$target == "MERCADO"     & df_eye$distractor == "GARAJE"       ~ 120  ,
                              df_eye$target == "RUEDA"       & df_eye$distractor == "GRIFO"        ~ 121  ,
                              df_eye$target == "PANDERETA"   & df_eye$distractor == "EXTINTOR"     ~ 122  ,
                              df_eye$target == "LAVADORA"    & df_eye$distractor == "CORTACESPED"  ~ 123  ,
                              df_eye$target == "COMETA"      & df_eye$distractor == "BOMBILLA"     ~ 124  ,
                              df_eye$target == "CAMARA"      & df_eye$distractor == "BATERIA"      ~ 125  ,
                              df_eye$target == "CESTA"       & df_eye$distractor == "ESCOBA"       ~ 126  ,
                              df_eye$target == "PIANO"       & df_eye$distractor == "MANTA"        ~ 127  ,
                              df_eye$target == "PARAGUAS"    & df_eye$distractor == "CARRETILLA"   ~ 128  ,
                              df_eye$target == "MICROFONO"   & df_eye$distractor == "IMPRESORA"    ~ 129  ,
                              df_eye$target == "SAXOFON"     & df_eye$distractor == "ESTUCHE"      ~ 130  ,
                              df_eye$target == "VIOLIN"      & df_eye$distractor == "PLATO"        ~ 131  ,
                              df_eye$target == "SALERO"      & df_eye$distractor == "BARRIL"       ~ 132  ,
                              df_eye$target == "GUITARRA"    & df_eye$distractor == "CLARINETE"    ~ 133  ,
                              df_eye$target == "PLANCHA"     & df_eye$distractor == "HORNO"        ~ 134  ,
                              df_eye$target == "ESTUFA"      & df_eye$distractor == "TELEFONO"     ~ 135  ,
                              df_eye$target == "JERINGUILLA" & df_eye$distractor == "CAFETERA"     ~ 136  ,
                              df_eye$target == "MIRILLA"     & df_eye$distractor == "ANTORCHA"     ~ 137  ,
                              df_eye$target == "PIPA"        & df_eye$distractor == "OLLA"         ~ 138  ,
                              df_eye$target == "ACORDEON"    & df_eye$distractor == "TROMPETA"     ~ 139  ,
                              df_eye$target == "CASCO"       & df_eye$distractor == "TUERCA"       ~ 140  ,
                              df_eye$target == "HILO"        & df_eye$distractor == "BRUJULA"      ~ 141  ,
                              df_eye$target == "TAMBOR"      & df_eye$distractor == "MICROSCOPIO"  ~ 142  ,
                              df_eye$target == "VENTILADOR"  & df_eye$distractor == "TENDERETE"    ~ 143  ,
                              df_eye$target == "TELESCOPIO"  & df_eye$distractor == "CORCHO"       ~ 144
)

# code semantic relatedness between targets and distractors
df_eye$sem_rel <- case_when(df_eye$target == "CHICA"       & df_eye$distractor == "ADIVINA"     ~ 3.581   ,
                            df_eye$target == "CHICA"       & df_eye$distractor == "NADADOR"     ~ 2.836   ,
                            df_eye$target == "MONJA"       & df_eye$distractor == "PRINCESA"    ~ 2.909   ,
                            df_eye$target == "MONJA"       & df_eye$distractor == "HOMBRE"      ~ 2.436   ,
                            df_eye$target == "CAMARERA"    & df_eye$distractor == "BAILARINA"   ~ 3.8     ,
                            df_eye$target == "CAMARERA"    & df_eye$distractor == "PROFESOR"    ~ 2.836   ,
                            df_eye$target == "PROFESORA"   & df_eye$distractor == "BAILARIN"    ~ 3.509   ,
                            df_eye$target == "PROFESORA"   & df_eye$distractor == "EMBARAZADA"  ~ 3.181   ,
                            df_eye$target == "REINA"       & df_eye$distractor == "MIMO"        ~ 2.509   ,
                            df_eye$target == "REINA"       & df_eye$distractor == "NINA"        ~ 4.036   ,
                            df_eye$target == "ENFERMERA"   & df_eye$distractor == "PORTERO"     ~ 2.618   ,
                            df_eye$target == "ENFERMERA"   & df_eye$distractor == "MUJER"       ~ 4.763   ,
                            df_eye$target == "CARTERO"     & df_eye$distractor == "NADADOR"     ~ 2.672   ,
                            df_eye$target == "CARTERO"     & df_eye$distractor == "ADIVINA"     ~ 2.436   ,
                            df_eye$target == "BOMBERO"     & df_eye$distractor == "HOMBRE"      ~ 4.981   ,
                            df_eye$target == "BOMBERO"     & df_eye$distractor =="PRINCESA"     ~ 2.036   ,
                            df_eye$target == "MEDICO"      & df_eye$distractor == "PROFESOR"    ~ 4.745   ,
                            df_eye$target == "MEDICO"      & df_eye$distractor == "BAILARINA"   ~ 2.818   ,
                            df_eye$target == "TORERO"      & df_eye$distractor == "EMBARAZADA"  ~ 1.636   ,
                            df_eye$target == "TORERO"      & df_eye$distractor == "BAILARIN"    ~ 3.309   ,
                            df_eye$target == "PINTOR"      & df_eye$distractor == "NINA"        ~ 2.654   ,
                            df_eye$target == "PINTOR"      & df_eye$distractor == "MIMO"        ~ 3.690   ,
                            df_eye$target == "NINO"        & df_eye$distractor == "MUJER"       ~ 3.818   ,
                            df_eye$target == "NINO"        & df_eye$distractor == "PORTERO"     ~ 2.454   ,
                            df_eye$target == "RATA"        & df_eye$distractor == "MORSA"       ~ 3.036   ,
                            df_eye$target == "RATA"        & df_eye$distractor == "CIERVO"      ~ 3.018   ,
                            df_eye$target == "CEBRA"       & df_eye$distractor == "LIEBRE"      ~ 3.654   ,
                            df_eye$target == "CEBRA"       & df_eye$distractor == "ELEFANTE"    ~ 4       ,
                            df_eye$target == "BALLENA"     & df_eye$distractor == "MOFETA"      ~ 3       ,
                            df_eye$target == "BALLENA"     & df_eye$distractor == "HIPOPOTAMO"  ~ 3.763   ,
                            df_eye$target == "ARDILLA"     & df_eye$distractor == "CANGURO"     ~ 3.581   ,
                            df_eye$target == "ARDILLA"     & df_eye$distractor == "PANTERA"     ~ 3.127   ,
                            df_eye$target == "JIRAFA"      & df_eye$distractor == "CAMELLO"     ~ 4.436   ,
                            df_eye$target == "JIRAFA"      & df_eye$distractor == "OVEJA"       ~ 3.690   ,
                            df_eye$target == "FOCA"        & df_eye$distractor == "ERIZO"       ~ 3.254   ,
                            df_eye$target == "FOCA"        & df_eye$distractor == "LLAMA"       ~ 3.181   ,
                            df_eye$target == "LINCE"       & df_eye$distractor == "CIERVO"      ~ 4.418   ,
                            df_eye$target == "LINCE"       & df_eye$distractor == "MORSA"       ~ 3.327   ,
                            df_eye$target == "DELFIN"      & df_eye$distractor == "ELEFANTE"    ~ 3.290   ,
                            df_eye$target == "DELFIN"      & df_eye$distractor == "LIEBRE"      ~ 3.090   ,
                            df_eye$target == "KOALA"       & df_eye$distractor == "HIPOPOTAMO"  ~ 3.218   ,
                            df_eye$target == "KOALA"       & df_eye$distractor == "MOFETA"      ~ 3.290   ,
                            df_eye$target == "MURCIELAGO"  & df_eye$distractor == "PANTERA"     ~ 3.127   ,
                            df_eye$target == "MURCIELAGO"  & df_eye$distractor == "CANGURO"     ~ 3.363   ,
                            df_eye$target == "JABALI"      & df_eye$distractor == "OVEJA"       ~ 3.890   ,
                            df_eye$target == "JABALI"      & df_eye$distractor == "CAMELLO"     ~ 3.690   ,
                            df_eye$target == "RINOCERONTE" & df_eye$distractor == "LLAMA"       ~ 3.472   ,
                            df_eye$target == "RINOCERONTE" & df_eye$distractor == "ERIZO"       ~ 3.109   ,
                            df_eye$target == "CIGUENA"     & df_eye$distractor == "GAVIOTA"     ~ 5.290   ,
                            df_eye$target == "CIGUENA"     & df_eye$distractor == "BUHO"        ~ 4.109   ,
                            df_eye$target == "PALOMA"      & df_eye$distractor == "CISNE"       ~ 4.690   ,
                            df_eye$target == "PALOMA"      & df_eye$distractor == "AVESTRUZ"    ~ 4.581   ,
                            df_eye$target == "HORMIGA"     & df_eye$distractor == "ARANA"       ~ 4.472   ,
                            df_eye$target == "HORMIGA"     & df_eye$distractor == "SALTAMONTES" ~ 4.509   ,
                            df_eye$target == "ABEJA"       & df_eye$distractor == "MOSQUITO"    ~ 4.836   ,
                            df_eye$target == "ABEJA"       & df_eye$distractor == "ORUGA"       ~ 3.781   ,
                            df_eye$target == "TORTUGA"     & df_eye$distractor == "LAGARTIJA"   ~ 4.254   ,
                            df_eye$target == "TORTUGA"     & df_eye$distractor == "CAMALEON"    ~ 3.945   ,
                            df_eye$target == "COBRA"       & df_eye$distractor == "DINOSAURIO"  ~ 3.090   ,
                            df_eye$target == "COBRA"       & df_eye$distractor == "RANA"        ~ 3.4     ,
                            df_eye$target == "PINGUINO"    & df_eye$distractor == "BUHO"        ~ 3.781   ,
                            df_eye$target == "PINGUINO"    & df_eye$distractor == "GAVIOTA"     ~ 3.963   ,
                            df_eye$target == "TUCAN"       & df_eye$distractor == "AVESTRUZ"    ~ 4.472   ,
                            df_eye$target == "TUCAN"       & df_eye$distractor == "CISNE"       ~ 4.6     ,
                            df_eye$target == "ESCARABAJO"  & df_eye$distractor == "SALTAMONTES" ~ 4.727   ,
                            df_eye$target == "ESCARABAJO"  & df_eye$distractor == "ARANA"       ~ 4.290   ,
                            df_eye$target == "ESCORPION"   & df_eye$distractor == "ORUGA"       ~ 3.345   ,
                            df_eye$target == "ESCORPION"   & df_eye$distractor == "MOSQUITO"    ~ 3.272   ,
                            df_eye$target == "SAPO"        & df_eye$distractor == "CAMALEON"    ~ 4.036   ,
                            df_eye$target == "SAPO"        & df_eye$distractor == "LAGARTIJA"   ~ 4.563   ,
                            df_eye$target == "COCODRILO"   & df_eye$distractor == "RANA"        ~ 3.781   ,
                            df_eye$target == "COCODRILO"   & df_eye$distractor == "DINOSAURIO"  ~ 4.581   ,
                            df_eye$target == "GRANJA"      & df_eye$distractor == "FUENTE"      ~ 2.690   ,
                            df_eye$target == "GRANJA"      & df_eye$distractor == "ESTADIO"     ~ 2.418   ,
                            df_eye$target == "MANSION"     & df_eye$distractor == "FABRICA"     ~ 2.490   ,
                            df_eye$target == "MANSION"     & df_eye$distractor == "CEMENTERIO"  ~ 2.727   ,
                            df_eye$target == "FARMACIA"    & df_eye$distractor == "CATEDRAL"    ~ 2.218   ,
                            df_eye$target == "FARMACIA"    & df_eye$distractor == "MOLINO"      ~ 1.654   ,
                            df_eye$target == "TORRE"       & df_eye$distractor == "MUSEO"       ~ 3.4     ,
                            df_eye$target == "TORRE"       & df_eye$distractor == "CASA"        ~ 3.818   ,
                            df_eye$target == "PIRAMIDE"    & df_eye$distractor == "INVERNADERO" ~ 2.145   ,
                            df_eye$target == "PIRAMIDE"    & df_eye$distractor == "CARCEL"      ~ 2.545   ,
                            df_eye$target == "MURALLA"     & df_eye$distractor == "GARAJE"      ~ 2.090   ,
                            df_eye$target == "MURALLA"     & df_eye$distractor == "CABANA"      ~ 2.072   ,
                            df_eye$target == "CASTILLO"    & df_eye$distractor == "ESTADIO"     ~ 2.636   ,
                            df_eye$target == "CASTILLO"    & df_eye$distractor == "FUENTE"      ~ 3.145   ,
                            df_eye$target == "LABORATORIO" & df_eye$distractor == "CEMENTERIO"  ~ 2.290   ,
                            df_eye$target == "LABORATORIO" & df_eye$distractor == "FABRICA"     ~ 3.872   ,
                            df_eye$target == "GIMNASIO"    & df_eye$distractor == "MOLINO"      ~ 1.745   ,
                            df_eye$target == "GIMNASIO"    & df_eye$distractor == "CATEDRAL"    ~ 2.290   ,
                            df_eye$target == "PUENTE"      & df_eye$distractor == "CASA"        ~ 2.709   ,
                            df_eye$target == "PUENTE"      & df_eye$distractor == "MUSEO"       ~ 2.6     ,
                            df_eye$target == "AEROPUERTO"  & df_eye$distractor == "CARCEL"      ~ 2.509   ,
                            df_eye$target == "AEROPUERTO"  & df_eye$distractor == "INVERNADERO" ~ 2.054   ,
                            df_eye$target == "MERCADO"     & df_eye$distractor == "CABANA"      ~ 2.345   ,
                            df_eye$target == "MERCADO"     & df_eye$distractor == "GARAJE"      ~ 3.090   ,
                            df_eye$target == "RUEDA"       & df_eye$distractor == "MANTA"       ~ 1.418   ,
                            df_eye$target == "RUEDA"       & df_eye$distractor == "GRIFO"       ~ 2.145   ,
                            df_eye$target == "PANDERETA"   & df_eye$distractor == "CARRETILLA"  ~ 1.836   ,
                            df_eye$target == "PANDERETA"   & df_eye$distractor == "EXTINTOR"    ~ 1.490   ,
                            df_eye$target == "LAVADORA"    & df_eye$distractor == "IMPRESORA"   ~ 2.909   ,
                            df_eye$target == "LAVADORA"    & df_eye$distractor == "CORTACESPED" ~ 3.163   ,
                            df_eye$target == "COMETA"      & df_eye$distractor == "ESTUCHE"     ~ 1.8     ,
                            df_eye$target == "COMETA"      & df_eye$distractor == "BOMBILLA"    ~ 2.145   ,
                            df_eye$target == "CAMARA"      & df_eye$distractor == "PLATO"       ~ 1.672   ,
                            df_eye$target == "CAMARA"      & df_eye$distractor == "BATERIA"     ~ 3.290   ,
                            df_eye$target == "CESTA"       & df_eye$distractor == "BARRIL"      ~ 4.145   ,
                            df_eye$target == "CESTA"       & df_eye$distractor == "ESCOBA"      ~ 3.127   ,
                            df_eye$target == "PIANO"       & df_eye$distractor == "GRIFO"       ~ 1.527   ,
                            df_eye$target == "PIANO"       & df_eye$distractor == "MANTA"       ~ 1.654   ,
                            df_eye$target == "PARAGUAS"    & df_eye$distractor == "EXTINTOR"    ~ 2.181   ,
                            df_eye$target == "PARAGUAS"    & df_eye$distractor == "CARRETILLA"  ~ 1.818   ,
                            df_eye$target == "MICROFONO"   & df_eye$distractor == "CORTACESPED" ~ 1.672   ,
                            df_eye$target == "MICROFONO"   & df_eye$distractor == "IMPRESORA"   ~ 2.418   ,
                            df_eye$target == "SAXOFON"     & df_eye$distractor == "BOMBILLA"    ~ 1.563   ,
                            df_eye$target == "SAXOFON"     & df_eye$distractor == "ESTUCHE"     ~ 4.436   ,
                            df_eye$target == "VIOLIN"      & df_eye$distractor == "BATERIA"     ~ 4.309   ,
                            df_eye$target == "VIOLIN"      & df_eye$distractor == "PLATO"       ~ 1.963   ,
                            df_eye$target == "SALERO"      & df_eye$distractor == "ESCOBA"      ~ 1.981   ,
                            df_eye$target == "SALERO"      & df_eye$distractor == "BARRIL"      ~ 2.618   ,
                            df_eye$target == "GUITARRA"    & df_eye$distractor == "TROMPETA"    ~ 5.236   ,
                            df_eye$target == "GUITARRA"    & df_eye$distractor == "CLARINETE"   ~ 5.163   ,
                            df_eye$target == "PLANCHA"     & df_eye$distractor == "TUERCA"      ~ 1.945   ,
                            df_eye$target == "PLANCHA"     & df_eye$distractor == "HORNO"       ~ 4.345   ,
                            df_eye$target == "ESTUFA"      & df_eye$distractor == "BRUJULA"     ~ 1.709   ,
                            df_eye$target == "ESTUFA"      & df_eye$distractor == "TELEFONO"    ~ 2.563   ,
                            df_eye$target == "JERINGUILLA" & df_eye$distractor == "MICROSCOPIO" ~ 3.6     ,
                            df_eye$target == "JERINGUILLA" & df_eye$distractor == "CAFETERA"    ~ 1.763   ,
                            df_eye$target == "MIRILLA"     & df_eye$distractor == "TENDERETE"   ~ 2       ,
                            df_eye$target == "MIRILLA"     & df_eye$distractor == "ANTORCHA"    ~ 1.927   ,
                            df_eye$target == "ARPA"        & df_eye$distractor == "CORCHO"      ~ 1.490   ,
                            df_eye$target == "ARPA"        & df_eye$distractor == "OLLA"        ~ 1.763   ,
                            df_eye$target == "ACORDEON"    & df_eye$distractor == "CLARINETE"   ~ 4.981   ,
                            df_eye$target == "ACORDEON"    & df_eye$distractor == "TROMPETA"    ~ 4.727   ,
                            df_eye$target == "CASCO"       & df_eye$distractor == "HORNO"       ~ 1.709   ,
                            df_eye$target == "CASCO"       & df_eye$distractor == "TUERCA"      ~ 2.254   ,
                            df_eye$target == "HILO"        & df_eye$distractor == "TELEFONO"    ~ 2.872   ,
                            df_eye$target == "HILO"        & df_eye$distractor == "BRUJULA"     ~ 2.545   ,
                            df_eye$target == "TAMBOR"      & df_eye$distractor == "CAFETERA"    ~ 1.745   ,
                            df_eye$target == "TAMBOR"      & df_eye$distractor == "MICROSCOPIO" ~ 1.836   ,
                            df_eye$target == "VENTILADOR"  & df_eye$distractor == "ANTORCHA"    ~ 2.236   ,
                            df_eye$target == "VENTILADOR"  & df_eye$distractor == "TENDERETE"   ~ 2.218   ,
                            df_eye$target == "TELESCOPIO"  & df_eye$distractor == "OLLA"        ~ 1.672   ,
                            df_eye$target == "TELESCOPIO"  & df_eye$distractor == "CORCHO"      ~ 1.654   ,
                            df_eye$target == "BUFALO"      & df_eye$distractor == "HIPOPOTAMO"  ~ 3.773475,         # these are NAs: imputing them for the mean value
                            df_eye$target == "BUFALO"      & df_eye$distractor == "MOFETA"      ~ 3.444570,         # these are NAs: imputing them for the mean value
                            df_eye$target == "PIPA"        & df_eye$distractor == "OLLA"        ~ 2.484616,         # these are NAs: imputing them for the mean value
                            df_eye$target == "PIPA"        & df_eye$distractor == "CORCHO"      ~ 2.634969)         # these are NAs: imputing them for the mean value

df_eye$sem_rel.cod <- scale(df_eye$sem_rel, center = TRUE, scale = TRUE)

# code visual complexity of the images
df_eye$comp_vis  <- case_when(df_eye$target == "CHICA"        ~ 2.71   , 
                              df_eye$target == "MONJA"        ~ 2.727  ,
                              df_eye$target == "CAMARERA"     ~ 2.909  ,
                              df_eye$target == "PROFESORA"    ~ 2.89   ,
                              df_eye$target == "REINA"        ~ 3.05   ,
                              df_eye$target == "ENFERMERA"    ~ 2.939  ,
                              df_eye$target == "RATA"         ~ 2.77   ,
                              df_eye$target == "CEBRA"        ~ 2.646  ,
                              df_eye$target == "BALLENA"      ~ 2.77   ,
                              df_eye$target == "ARDILLA"      ~ 2.73   ,
                              df_eye$target == "JIRAFA"       ~ 3.02   ,
                              df_eye$target == "FOCA"         ~ 2.84   ,
                              df_eye$target == "CIGUENA"      ~ 2.87   ,
                              df_eye$target == "PALOMA"       ~ 2.74   ,
                              df_eye$target == "HORMIGA"      ~ 2.59   ,
                              df_eye$target == "ABEJA"        ~ 2.85   ,
                              df_eye$target == "TORTUGA"      ~ 3.     ,
                              df_eye$target == "COBRA"        ~ 3.02   ,
                              df_eye$target == "GRANJA"       ~ 3.56   ,
                              df_eye$target == "MANSION"      ~ 3.11   ,
                              df_eye$target == "FARMACIA"     ~ 3.12   ,
                              df_eye$target == "TORRE"        ~ 2.25   ,
                              df_eye$target == "PIRAMIDE"     ~ 2.43   ,
                              df_eye$target == "MURALLA"      ~ 3.51   ,
                              df_eye$target == "RUEDA"        ~ 2.43   ,
                              df_eye$target == "PANDERETA"    ~ 2.33   ,
                              df_eye$target == "LAVADORA"     ~ 2.55   ,
                              df_eye$target == "COMETA"       ~ 2.65   ,
                              df_eye$target == "CAMARA"       ~ 2.37   ,
                              df_eye$target == "CESTA"        ~ 2.53   ,
                              df_eye$target == "GUITARRA"     ~ 2.29   ,
                              df_eye$target == "PLANCHA"      ~ 2.24   ,
                              df_eye$target == "ESTUFA"       ~ 2.34   ,
                              df_eye$target == "JERINGUILLA"  ~ 2.44   ,
                              df_eye$target == "MIRILLA"      ~ 2.45   ,
                              df_eye$target == "PIPA"         ~ 2.85   ,
                              df_eye$target == "CARTERO"      ~ 2.92   ,
                              df_eye$target == "BOMBERO"      ~ 2.73   ,
                              df_eye$target == "MEDICO"       ~ 2.81   ,
                              df_eye$target == "TORERO"       ~ 2.74   ,
                              df_eye$target == "PINTOR"       ~ 2.93   ,
                              df_eye$target == "NINO"         ~ 2.47   ,
                              df_eye$target == "LINCE"        ~ 3.34   ,
                              df_eye$target == "DELFIN"       ~ 2.45   ,
                              df_eye$target == "BUFALO"       ~ 3.33   ,
                              df_eye$target == "MURCIELAGO"   ~ 3.09   ,
                              df_eye$target == "JABALI"       ~ 3.08   ,
                              df_eye$target == "RINOCERONTE"  ~ 2.74   ,
                              df_eye$target == "PINGUINO"     ~ 2.38   ,
                              df_eye$target == "TUCAN"        ~ 2.64   ,
                              df_eye$target == "ESCARABAJO"   ~ 2.88   ,
                              df_eye$target == "ESCORPION"    ~ 2.58   ,
                              df_eye$target == "SAPO"         ~ 3.08   ,
                              df_eye$target == "COCODRILO"    ~ 3.08   ,
                              df_eye$target == "CASTILLO"     ~ 2.765  ,
                              df_eye$target == "LABORATORIO"  ~ 3.47   ,
                              df_eye$target == "GIMNASIO"     ~ 3.1    ,
                              df_eye$target == "PUENTE"       ~ 2.585  ,
                              df_eye$target == "AEROPUERTO"   ~ 2.97   ,
                              df_eye$target == "MERCADO"      ~ 3.41   ,
                              df_eye$target == "PIANO"        ~ 2.85   ,
                              df_eye$target == "PANRAGUAS"    ~ 2.08   ,
                              df_eye$target == "MICROFONO"    ~ 2.272  ,
                              df_eye$target == "SAXOFON"      ~ 2.9    ,
                              df_eye$target == "VIOLIN"       ~ 2.58   ,
                              df_eye$target == "SALERO"       ~ 2.06   ,
                              df_eye$target == "ACORDEON"     ~ 2.8    ,
                              df_eye$target == "CASCO"        ~ 2.33   ,
                              df_eye$target == "HILO"         ~ 2.22   ,
                              df_eye$target == "TAMBOR"       ~ 2.4    ,
                              df_eye$target == "VENTILADOR"   ~ 2.48   ,
                              df_eye$target == "TELESCOPIO"   ~ 2.47   )

df_eye$comp_vis.cod <- scale(df_eye$comp_vis, center = TRUE, scale = TRUE)

# code audio length
df_eye$audio_length <- case_when(df_eye$target == "PROFESORA"     ~ 0.85796,
                                 df_eye$target == "MONJA"         ~ 0.603326,
                                 df_eye$target == "CAMARERA"      ~ 0.719350,
                                 df_eye$target == "ENFERMERA"     ~ 0.928194,
                                 df_eye$target == "REINA"         ~ 0.667139,
                                 df_eye$target == "CHICA"         ~ 0.554016,
                                 df_eye$target == "PINTOR"        ~ 0.525010,
                                 df_eye$target == "NINO"          ~ 0.609127,
                                 df_eye$target == "TORERO"        ~ 0.519724,
                                 df_eye$target == "CARTERO"       ~ 0.716450,
                                 df_eye$target == "BOMBERO"       ~ 0.771561,
                                 df_eye$target == "MEDICO"        ~ 0.748356,
                                 df_eye$target == "FOCA"          ~ 0.620730,
                                 df_eye$target == "CEBRA"         ~ 0.562718,
                                 df_eye$target == "ARDILLA"       ~ 0.771561,
                                 df_eye$target == "BALLENA"       ~ 0.762859,
                                 df_eye$target == "JIRAFA"        ~ 0.771561,
                                 df_eye$target == "RATA"          ~ 0.635233,
                                 df_eye$target == "JABALI"        ~ 0.661338,
                                 df_eye$target == "MURCIELAGO"    ~ 1.015212,
                                 df_eye$target == "DELFIN"        ~ 0.812170,
                                 df_eye$target == "RINOCERONTE"   ~ 1.157342,
                                 df_eye$target == "LINCE"         ~ 0.696145,
                                 df_eye$target == "BUFALO"        ~ 0.775432,
                                 df_eye$target == "CIGUENA"       ~ 0.733853,
                                 df_eye$target == "PALOMA"        ~ 0.655537,
                                 df_eye$target == "HORMIGA"       ~ 0.730953,
                                 df_eye$target == "ABEJA"         ~ 0.699046,
                                 df_eye$target == "TORTUGA"       ~ 0.715450,
                                 df_eye$target == "COBRA"         ~ 0.487302,
                                 df_eye$target == "PINGUINO"      ~ 0.742555,
                                 df_eye$target == "TUCAN"         ~ 0.571419,
                                 df_eye$target == "ESCARABAJO"    ~ 1.058721,
                                 df_eye$target == "ESCORPION"     ~ 0.957200,
                                 df_eye$target == "COCODRILO"     ~ 0.829573,
                                 df_eye$target == "SAPO"          ~ 0.658438,
                                 df_eye$target == "GRANJA"        ~ 0.725151,
                                 df_eye$target == "MANSION"       ~ 0.855678,
                                 df_eye$target == "FARMACIA"      ~ 0.858579,
                                 df_eye$target == "TORRE"         ~ 0.472799,
                                 df_eye$target == "PIRAMIDE"      ~ 0.736754,
                                 df_eye$target == "MURALLA"       ~ 0.780263,
                                 df_eye$target == "MERCADO"       ~ 0.794766,
                                 df_eye$target == "CASTILLO"      ~ 0.774462,
                                 df_eye$target == "AEROPUERTO"    ~ 0.997504,
                                 df_eye$target == "GIMNASIO"      ~ 0.875983,
                                 df_eye$target == "LABORATORIO"   ~ 1.061622,
                                 df_eye$target == "PUENTE"        ~ 0.542413,
                                 df_eye$target == "RUEDA"         ~ 0.612028,
                                 df_eye$target == "PANDERETA"     ~ 0.844976,
                                 df_eye$target == "LAVADORA"      ~ 0.841176,
                                 df_eye$target == "COMETA"        ~ 0.655537,
                                 df_eye$target == "CAMARA"        ~ 0.585922,
                                 df_eye$target == "CESTA"         ~ 0.614928,
                                 df_eye$target == "PIANO"         ~ 0.516308,
                                 df_eye$target == "PARAGUAS"      ~ 0.783164,
                                 df_eye$target == "MICROFONO"     ~ 0.960101,
                                 df_eye$target == "SAXOFON"       ~ 1.012311,
                                 df_eye$target == "VIOLIN"        ~ 0.704847,
                                 df_eye$target == "SALERO"        ~ 0.78263,
                                 df_eye$target == "GUITARRA"      ~ 0.803468,
                                 df_eye$target == "PLANCHA"       ~ 0.623630,
                                 df_eye$target == "ESTUFA"        ~ 0.762859,
                                 df_eye$target == "JERINGUILLA"   ~ 0.954299,
                                 df_eye$target == "MIRILLA"       ~ 0.783164,
                                 df_eye$target == "PIPA"          ~ 0.455789,
                                 df_eye$target == "TAMBOR"        ~ 0.513407,
                                 df_eye$target == "HILO"          ~ 0.429290,
                                 df_eye$target == "ACORDEON"      ~ 0.916591,
                                 df_eye$target == "CASCO"         ~ 0.548215,
                                 df_eye$target == "TELESCOPIO"    ~ 0.933995,
                                 df_eye$target == "VENTILADOR"    ~ 0.878884
)

df_eye$audio_length.cod <- scale(df_eye$audio_length)

# code trackloss that combines RIGHT_IN_BLINK & RIGHT_IN_SACCADE
df_eye$trackloss <- if_else(df_eye$RIGHT_IN_BLINK == 1 | df_eye$RIGHT_IN_SACCADE == 1, 1, 0)
df_eye$trackloss <- as.numeric(df_eye$trackloss)

# code fixation for targets
df_eye$target_fix <- if_else(df_eye$RIGHT_INTEREST_AREA_ID == 1, 1, 0)
df_eye$target_fix <- as.numeric(df_eye$target_fix)

# code fixation for distractors
df_eye$distractor_fix <- if_else(df_eye$RIGHT_INTEREST_AREA_ID == 2, 1, 0)
df_eye$distractor_fix <- as.numeric(df_eye$distractor_fix)

# sanity checks
mean(df_eye$target_fix)
mean(df_eye$distractor_fix)
mean(df_eye$trackloss, na.rm = TRUE)

# code target
df_eye$target <- as.factor(df_eye$target)
levels(df_eye$target)
unique(df_eye$target[is.na(df_eye$sem_rel) == TRUE])

d_eliminate2 <- df_eye |> filter(target == "R" | target == "incorrect" | target == "correct") # eliminate the problematic trials, they are the first trial from each subject
df_eye <- anti_join(df_eye, d_eliminate)

# df containing errors:
df_eye_errors <- df_eye |> filter(ACC != 1)

# df without errors:
df_eye <- df_eye %>%  filter(ACC == 1)

# code outliers (defined as RTs greater or smaller than 2.5 SD*average mean)
df_eye <- df_eye |> mutate(outlier = if_else(RT > mean(RT) + 2.5 * sd(RT) | RT < mean(RT) - 2.5 * sd(RT), 1, 0))

# df containing outliers:
df_eye_outlier <- df_eye %>% filter(outlier == 1)

# outliers per subject:
xtabs(~ subject, df_eye_outlier)

# df without outliers:
df_eye <- df_eye %>% filter(outlier != 1)

# these subjects had trackloss > 40%, so we collected more data
df_eye <- df_eye %>% 
  mutate(subject_elim = if_else(subject == "S004" |
                                subject == "S005" |
                                subject == "S008" |
                                subject == "S019" |
                                subject == "S027", 1, 0))

df_eye$subject_elim <- as.factor(df_eye$subject_elim)
xtabs(~ subject + subject_elim, data = df_eye)

# these subjects are the new ones
df_eye <- df_eye %>% 
  mutate(subject_new = if_else(subject == "S023" |
                               subject == "S024" |
                               subject == "S025" |
                               subject == "S026" |
                               subject == "S049", 1, 0))

df_eye$subject_new <- as.factor(df_eye$subject_new)
xtabs(~ subject + subject_new, data = df_eye)

# save the image
save.image("df_eye_preprocessed.RData")
# load(file = "df_eye_preprocessed.RData")

sessionInfo()
# R version 4.3.1 (2023-06-16)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Big Sur 11.6.5
# 
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
# LAPACK: /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# time zone: Europe/Madrid
# tzcode source: internal
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] tictoc_1.2         eyetrackingR_0.2.0 lattice_0.21-8     emmeans_1.8.7     
# [5] performance_0.10.4 see_0.8.0          ggeffects_1.2.3    afex_1.3-0        
# [9] harrypotter_2.1.1  jtools_2.2.1       lme4_1.1-34        Matrix_1.5-4.1    
# [13] MASS_7.3-60        lubridate_1.9.2    forcats_1.0.0      stringr_1.5.0     
# [17] dplyr_1.1.2        purrr_1.0.1        tidyr_1.3.0        tibble_3.2.1      
# [21] ggplot2_3.4.2      tidyverse_2.0.0    readr_2.1.4        rio_0.5.29        
# 
# loaded via a namespace (and not attached):
#   [1] tidyselect_1.2.0    fastmap_1.1.1       lazyeval_0.2.2      digest_0.6.33      
# [5] timechange_0.2.0    estimability_1.4.1  lifecycle_1.0.3     magrittr_2.0.3     
# [9] compiler_4.3.1      rlang_1.1.1         tools_4.3.1         utf8_1.2.3         
# [13] yaml_2.3.7          data.table_1.14.8   knitr_1.43          bit_4.0.5          
# [17] curl_5.0.1          plyr_1.8.8          abind_1.4-5         withr_2.5.0        
# [21] foreign_0.8-84      numDeriv_2016.8-1.1 grid_4.3.1          fansi_1.0.4        
# [25] xtable_1.8-4        colorspace_2.1-0    scales_1.2.1        insight_0.19.3     
# [29] cli_3.6.1           mvtnorm_1.2-2       rmarkdown_2.23      crayon_1.5.2       
# [33] generics_0.1.3      rstudioapi_0.14     reshape2_1.4.4      tzdb_0.4.0         
# [37] readxl_1.4.2        minqa_1.2.5         pander_0.6.5        splines_4.3.1      
# [41] parallel_4.3.1      cellranger_1.1.0    vctrs_0.6.3         boot_1.3-28.1      
# [45] carData_3.0-5       car_3.1-2           hms_1.1.3           bit64_4.0.5        
# [49] glue_1.6.2          nloptr_2.0.3        stringi_1.7.12      gtable_0.3.3       
# [53] lmerTest_3.1-3      munsell_0.5.0       pillar_1.9.0        htmltools_0.5.5    
# [57] R6_2.5.1            vroom_1.6.3         evaluate_0.21       haven_2.5.3        
# [61] openxlsx_4.2.5.2    Rcpp_1.0.11         zip_2.3.0           coda_0.19-4        
# [65] gridExtra_2.3       nlme_3.1-162        xfun_0.39           pkgconfig_2.0.3    