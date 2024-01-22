# install packages if not installed before (remove thw hashtag and run the code, needs to be done only once)
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

# load packages (needs to be done every time you start a new session in R)
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

# set seed for reproducibility
set.seed(123)

# get your working directory
getwd()

# set your working directory here, it should contain the Rdata generated in the previous script
setwd("/  /")
# setwd("/Users/Filip/Desktop/PhD 2020/___arXives/_20211125 - mentor (Paula) TFM/3. data analysis/data/")

# verify it is set correctly
getwd()

# load preprocessed data frame
load(file = "df_eye_preprocessed.RData")

# clean the environment
rm(temp,
   d_eliminate,
   df_eye_errors,
   df_eye_outlier)

head(df_eye)

# we'll use the eyetrackingR package here that modifies some columns
tic()
df_eye <- eyetrackingR::make_eyetrackingr_data(data = df_eye,
                                               participant_column = "subject",     
                                               trackloss_column =   "trackloss",
                                               time_column =        "TIMESTAMP",
                                               trial_column =       "trial.cod",
                                               aoi_columns =        "target_fix",
                                               treat_non_aoi_looks_as_missing = FALSE
                                               # item_columns = "trial.cod"
)
toc()
# 66.208 sec elapsed

# recode SAMPLE MESSAGE which is out "trigger", it is the point where the audio started in our experiment
# the column TIMESTAMP will be re-zeroed based on this trigger by the following function "subset_by_window()"
df_eye <- df_eye |>
  mutate(SAMPLE_MESSAGE.2 = if_else(SAMPLE_MESSAGE == "AUDIO_OFFSET;PLAY_SOUND" | # correcting some errors, the message should have been "PLAY_SOUND"
                                    SAMPLE_MESSAGE == '"AUDIO_OFFSET;PLAY_SOUND"', 
                                    "PLAY_SOUND", df_eye$SAMPLE_MESSAGE),
         SAMPLE_MESSAGE.3 = if_else(SAMPLE_MESSAGE.2 == "PLAY_SOUND", 1, 0))      # code PLAY_SOUND as "1"

# xtabs(~ SAMPLE_MESSAGE + SAMPLE_MESSAGE.2, data = df_eye)
xtabs(~ SAMPLE_MESSAGE.2 + SAMPLE_MESSAGE.3, data = df_eye)

# recode TIMESTAMP
tic()
df_eye.1 <- eyetrackingR::subset_by_window(df_eye, 
                                           window_start_msg = 1,                 # using "1" in the column "SAMPLE_MESSAGE.3" as the starting point                 
                                           msg_col = "SAMPLE_MESSAGE.3",
                                           rezero = TRUE,
                                           remove = FALSE
)
toc()
# 8.544 sec elapsed


# convert TIMESTAMP to ms
df_eye.2 <- df_eye.1 |>
  mutate(TIMESTAMP = case_when(RECORDING_SESSION_LABEL == 133 ~ TIMESTAMP,       # some issues with timestamp in subject 16, 23 & 24
                               RECORDING_SESSION_LABEL == 148 ~ TIMESTAMP,
                               subject == "S024" ~ TIMESTAMP,
                                TRUE ~ TIMESTAMP/100))

# reduce data to 200 to 1200 ms
# to "launch" a saccade takes between 150-200 ms, so I make this cutoff at 200 ms
# in another (control) analysis I may verify what happens in those first 200 ms (conditions should not differ there)
df_eye.3 <- df_eye.2 |> filter(TIMESTAMP >= 200 & TIMESTAMP < 1200)              

xtabs(~subject, data = df_eye.3)

dim(df_eye.3)
# [1] 6845248      41

# show mean trackloss by subject
subject.trackloss <- df_eye.3 |> group_by(subject) %>% summarize(mean_trackloss = round(mean(trackloss)*100, digits = 2))
View(subject.trackloss) 
# nothing really excessive, but may inspect subject S006

# show mean trackloss by item
item.trackloss <- df_eye.3 |> group_by(trial.cod) %>% summarize(mean_trackloss = round(mean(trackloss)*100, digits = 2))
View(item.trackloss) 
# seems ok

# check  participants that have high proportion of fixations outside the target/distractor
subject.outside <- df_eye.3 |>
  group_by(subject) |>
  summarise(mean.target_fix = round(mean(target_fix, na.rm = TRUE)*100, 2),
            mean.distractor_fix = round(mean(distractor_fix, na.rm = TRUE)*100, 2),
            mean.trackloss = round(mean(trackloss)*100, 2),
            outside = round(100-(mean.target_fix + mean.distractor_fix + mean.trackloss), 2))
View(subject.outside)
# exclude subjects 4, 5, 8, 19, 27, 
# they are probably fixating the central point most of the time compared to other participants (might be a valid strategy but a lot of data are "lost")

df_eye.3 <- df_eye.3 |> filter(subject_elim == 0)
df_eye.3$subject <- droplevels(df_eye.3$subject)

# analysis: Bootstrapped cluster-based permutation analysis (following the vignete of eyetrackingR() package)
# to go to http://www.eyetracking-r.com/vignettes/divergence and scroll down to get the "Bootstrapped cluster-based permutation analysis"
# to familiarize with the steps I take below

# pick threshold t based on alpha = .05 two tailed
threshold_t = qt(p = 1 - .05/2, df = 44 -1) 

# make clustered data
# this reduced my data (which are in milliseconds) to time-bins of my choice (I choose 10 ms)
tic()
df.clust <- make_time_sequence_data(df_eye.3,
                                    time_bin_size = 10,    # time windows
                                    predictor_columns = c(
                                      "animacy.cod", 
                                      "gen_congruency.cod",
                                      "target", 
                                      "comp_vis.cod",
                                      "sem_rel.cod",
                                      "block.cod",
                                      "audio_length.cod"),
                                    aois = "target_fix")
toc()
# 3.764 sec elapsed

# check dimensions of the data frame
dim(df.clust)
# [1] 614240     26
# you instantly see that the data are reduced 10 times in size, if I chose time-bin of 100 ms, it would be reduced 100 times

# sanity check
xtabs(~ TimeBin + subject, data = df.clust)
# nothing weird

# gender congruency analysis
# I do a mixed-effect model including some covariates (that were significant in response times analysis)
df_clust.gen <- make_time_cluster_data(df.clust,
                                       test= "lmer", 
                                       formula = Prop ~ gen_congruency.cod +
                                         comp_vis.cod + sem_rel.cod + audio_length.cod + block.cod +
                                         (1|subject) + 
                                         (1|target), 
                                       predictor_column = "gen_congruency.cod",
                                       threshold = threshold_t)

# plot it
plot(df_clust.gen) + ylab("T-Statistic") + theme_light()
# very small effect

# summarize it
summary(df_clust.gen)
# Cluster Direction SumStatistic StartTime EndTime
# 1       1  Negative    -2.085914       400     410


# bootstrapping part:
tic()
clust_analysis.gen <- analyze_time_clusters(df_clust.gen, 
                                            within_subj = FALSE,
                                            # paired=TRUE,
                                            # shuffle_by = "subject",
                                            samples = 2000,                      # 2000 is an ok number of iterations                 
                                            formula = Prop ~ gen_congruency.cod +
                                              comp_vis.cod + sem_rel.cod + audio_length.cod + block.cod +
                                              (1|subject) + (1|target)
)         
toc()
# 15266.306 sec elapsed

# plot and summarize the bootstrapping part:
plot(clust_analysis.gen) + theme_light()
summary(clust_analysis.gen)

# Test Type:	 lmer 
# Predictor:	 gen_congruency.cod 
# Formula:	 Prop ~ gen_congruency.cod + comp_vis.cod + sem_rel.cod + audio_length.cod +      block.cod + (1 | subject) + (1 | target) 
# Null Distribution   ====== 
#   Mean:		 -11.3774 
# 2.5%:		 -115.5544 
# 97.5%:		 20.7554 
# Summary of Clusters ======
#   Cluster Direction SumStatistic StartTime EndTime Probability
# 1       1  Negative    -2.085914       400     410        0.32

# plot it neatly, even though nothing significant
# tiff('gen2.tiff', units="in", width=6, height=4, res=300, compression = 'lzw')
plot(df.clust, predictor_column = "gen_congruency.cod") + 
  coord_cartesian(ylim = c(0,1)) + 
  scale_x_continuous(breaks = c(-500, - 200, 0, 200, 400, 600, 800, 1000, 1200),
                     labels = c(-500, -200, 0, 200, 400, 600, 800, 1000, 1200)) +
  labs(
    # title = "La proporción de fijaciones en el tiempo para la congruencia del género",
    x = "Tiempo desde la presentación del estímulo auditivo [ms]",
    y = "La proporción de fijaciones") +
  theme_apa() +
  annotate("rect", xmin = 400, xmax = 410, ymin = 0.325, ymax = 0.775, alpha = 1/5)
# dev.off()

# animacy analysis
df_clust.ani <- make_time_cluster_data(df.clust,
                                       test = "lmer",
                                       aoi = "target_fix",
                                       formula = Prop ~ animacy.cod +
                                         comp_vis.cod + sem_rel.cod + audio_length.cod + block.cod +
                                         (1|subject) + (1|target), 
                                       predictor_column = "animacy.cod",
                                       threshold = threshold_t)
# plot it
plot(df_clust.ani) + ylab("T-Statistic") +  theme_light()

# summarize it
summary(df_clust.ani)
# Test Type:	 lmer 
# Predictor:	 animacy.cod 
# Formula:	 Prop ~ animacy.cod + comp_vis.cod + sem_rel.cod + audio_length.cod +      block.cod + (1 | subject) + (1 | target) 
# Summary of Clusters ======
#   Cluster Direction SumStatistic StartTime EndTime
# 1       1  Positive    40.410497      1050    1200
# 2       2  Negative    -2.049731       650     660
# 3       3  Negative    -9.422204       670     710

# bootstrapping part
tic()
clust_analysis.ani <- analyze_time_clusters(df_clust.ani, 
                                            within_subj = FALSE,  
                                            # paired=TRUE,
                                            # shuffle_by = "subject",
                                            samples = 10,            # the iterations should be again in the range of 2000 - 5000            
                                            formula = Prop ~ animacy.cod +
                                              comp_vis.cod + sem_rel.cod + audio_length.cod + block.cod +
                                              (1|subject) + (1|target)
)
toc()

# Throws an error "The term 'animacy.cod' was not found in your model." which I was unable to solve it, it seems to ignore one level of animacy

# let's analyze gender congruency separately for animate and inanimate targets
d.clust.gen_ani <- df.clust |> filter(animacy.cod == -0.5)
d.clust.gen_ina <- df.clust |> filter(animacy.cod == 0.5)

# gender congruency in animate targets
df_clust.gen_ani <- make_time_cluster_data(d.clust.gen_ani,
                                           test = "lmer",
                                           formula = Prop ~ gen_congruency.cod + comp_vis.cod + sem_rel.cod + (1|subject) + (1|target),
                                           predictor_column = "gen_congruency.cod",
                                           threshold = threshold_t)

# plot it
plot(df_clust.gen_ani) + ylab("T-Statistic") + theme_light()
# nothing to analyze


# gender congruency in inanimate targets
df_clust.gen_ina <- make_time_cluster_data(d.clust.gen_ina,
                                           test = "lmer",
                                           formula = Prop ~ gen_congruency.cod + comp_vis.cod + sem_rel.cod + (1|subject) + (1|target),
                                           predictor_column = "gen_congruency.cod",
                                           threshold = threshold_t)

# plot it
plot(df_clust.gen_ina) + ylab("T-Statistic") + theme_light()
# nothing to analyze

# save the image
save.image("df_eye_analysis.RData")

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
#   [1] tictoc_1.2         eyetrackingR_0.2.0 lattice_0.21-8     emmeans_1.8.7      performance_0.10.4
# [6] see_0.8.0          ggeffects_1.2.3    afex_1.3-0         harrypotter_2.1.1  jtools_2.2.1      
# [11] lme4_1.1-34        Matrix_1.5-4.1     MASS_7.3-60        lubridate_1.9.2    forcats_1.0.0     
# [16] stringr_1.5.0      dplyr_1.1.2        purrr_1.0.1        tidyr_1.3.0        tibble_3.2.1      
# [21] ggplot2_3.4.2      tidyverse_2.0.0    readr_2.1.4        rio_0.5.29        
# 
# loaded via a namespace (and not attached):
#   [1] tidyselect_1.2.0    farver_2.1.1        lazyeval_0.2.2      digest_0.6.33       timechange_0.2.0   
# [6] estimability_1.4.1  lifecycle_1.0.3     magrittr_2.0.3      compiler_4.3.1      rlang_1.1.1        
# [11] tools_4.3.1         utf8_1.2.3          data.table_1.14.8   labeling_0.4.2      curl_5.0.1         
# [16] plyr_1.8.8          abind_1.4-5         withr_2.5.0         foreign_0.8-84      numDeriv_2016.8-1.1
# [21] grid_4.3.1          fansi_1.0.4         xtable_1.8-4        colorspace_2.1-0    future_1.33.0      
# [26] globals_0.16.2      scales_1.2.1        insight_0.19.3      cli_3.6.1           mvtnorm_1.2-2      
# [31] crayon_1.5.2        generics_0.1.3      rstudioapi_0.14     reshape2_1.4.4      tzdb_0.4.0         
# [36] readxl_1.4.2        minqa_1.2.5         pander_0.6.5        splines_4.3.1       parallel_4.3.1     
# [41] cellranger_1.1.0    vctrs_0.6.3         boot_1.3-28.1       carData_3.0-5       car_3.1-2          
# [46] hms_1.1.3           listenv_0.9.0       parallelly_1.36.0   glue_1.6.2          nloptr_2.0.3       
# [51] codetools_0.2-19    stringi_1.7.12      gtable_0.3.3        broom.mixed_0.2.9.4 lmerTest_3.1-3     
# [56] munsell_0.5.0       pillar_1.9.0        furrr_0.3.1         R6_2.5.1            haven_2.5.3        
# [61] backports_1.4.1     openxlsx_4.2.5.2    broom_1.0.5         Rcpp_1.0.11         zip_2.3.0          
# [66] coda_0.19-4         gridExtra_2.3       nlme_3.1-162        pkgconfig_2.0.3    
