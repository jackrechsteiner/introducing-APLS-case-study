Intro to APLS Case Study
================
Jack Rechsteiner
2025-06-15

- [Simple vowel case study](#simple-vowel-case-study)
- [Not used in article: Speaker
  trajectories](#not-used-in-article-speaker-trajectories)
- [Session Info](#session-info)

Note: This script uses the `joeyr` package for finding outliers, measure
euclidian distance, and ANAE normalization. However, `joeyr` is not
available on CRAN. To install `joeyr`:

    #install.packages("devtools") # <- if not already installed
    devtools::install_github("JoeyStanley/joeyr")

You can find more about `joeyr` at
<https://github.com/JoeyStanley/joeyr>.

## Simple vowel case study

We’ll start off with reading in the vowel data and taking a look to make
sure it loaded correctly.

While we’re at it, we’ll also create columns for the full Neighborhood
names and a column that reminds us what vowel we’re working with.

``` r
#reading in vowel data
vowel_data <- read.csv("csvs/results_processwithpraat.csv") |>
  mutate(
    #basing the values for the Neighborhood column on the neighborhood code in the speaker column
    Neighborhood = case_when(startsWith(Speaker, "CB") ~ "Cranberry Township",
                                  startsWith(Speaker, "FH") ~ "Forest Hills",
                                  startsWith(Speaker, "HD") ~ "Hill District",
                                  startsWith(Speaker, "LV") ~ "Lawrenceville",
                                  TRUE ~ "Interviewer"),
    #our results only included the "AW" vowel, so we can have "AW" be the value for every row of this column
    vowel = "AW")

vowel_data |> 
  head()
```

               Title                                          SearchName Number
    1 Search results phonemes=.*6[pbtdkgfvTDszSZhJ_mnNlrwjFHP] segment=6      1
    2 Search results phonemes=.*6[pbtdkgfvTDszSZhJ_mnNlrwjFHP] segment=6      2
    3 Search results phonemes=.*6[pbtdkgfvTDszSZhJ_mnNlrwjFHP] segment=6      3
    4 Search results phonemes=.*6[pbtdkgfvTDszSZhJ_mnNlrwjFHP] segment=6      4
    5 Search results phonemes=.*6[pbtdkgfvTDszSZhJ_mnNlrwjFHP] segment=6      5
    6 Search results phonemes=.*6[pbtdkgfvTDszSZhJ_mnNlrwjFHP] segment=6      6
              Transcript           Speaker    Line LineEnd
    1 CB01interview1.eaf Barbara Johnstone  20.860  29.431
    2 CB01interview1.eaf Barbara Johnstone 238.681 239.999
    3 CB01interview1.eaf Barbara Johnstone 292.018 294.228
    4 CB01interview2.eaf Barbara Johnstone 119.593 122.137
    5 CB01interview2.eaf Barbara Johnstone 205.334 205.848
    6 CB01interview2.eaf Barbara Johnstone 240.621 241.834
                                                                                                       MatchId
    1 g_352;em_12_107932;n_3092553-n_4584645;p_146;#=es_1_1767198;prefix=0001-;[0]=ew_0_685107;[1]=ew_0_685107
    2 g_352;em_12_108054;n_3092671-n_4584700;p_146;#=es_1_1767905;prefix=0002-;[0]=ew_0_685768;[1]=ew_0_685768
    3 g_352;em_12_108083;n_3092697-n_4584714;p_146;#=es_1_1768027;prefix=0003-;[0]=ew_0_685933;[1]=ew_0_685933
    4 g_353;em_12_108579;n_3097998-n_4584878;p_146;#=es_1_1769656;prefix=0004-;[0]=ew_0_688639;[1]=ew_0_688639
    5 g_353;em_12_108629;n_3098049-n_4584902;p_146;#=es_1_1769974;prefix=0005-;[0]=ew_0_688900;[1]=ew_0_688900
    6 g_353;em_12_108651;n_3098069-n_4584908;p_146;#=es_1_1770007;prefix=0006-;[0]=ew_0_689001;[1]=ew_0_689001
                                                                                     URL
    1 https://apls.pitt.edu/labbcat/transcript?transcript=CB01interview1.eaf#ew_0_685107
    2 https://apls.pitt.edu/labbcat/transcript?transcript=CB01interview1.eaf#ew_0_685768
    3 https://apls.pitt.edu/labbcat/transcript?transcript=CB01interview1.eaf#ew_0_685933
    4 https://apls.pitt.edu/labbcat/transcript?transcript=CB01interview2.eaf#ew_0_688639
    5 https://apls.pitt.edu/labbcat/transcript?transcript=CB01interview2.eaf#ew_0_688900
    6 https://apls.pitt.edu/labbcat/transcript?transcript=CB01interview2.eaf#ew_0_689001
      Before.Match     Text After.Match Target.word Target.word.start
    1          for      our         own         our            27.730
    2         what    about        your       about           239.211
    3           my hometown                hometown           293.598
    4      talking  about .         and     about .           120.503
    5         that  about ?                 about ?           205.514
    6        comes      out       later         out           240.821
      Target.word.end Target.phonemes Target.orthography Target.segment
    1          27.830              6r                our              6
    2         239.331            @b6t              about              6
    3         294.078          h5mt6n           hometown              6
    4         120.723            @b6t              about              6
    5         205.744            @b6t              about              6
    6         241.021              6t                out              6
      Target.segment.start Target.segment.end time_0.2 F1.time_0.2 F2.time_0.2
    1               27.730             27.760   27.736         641        1331
    2              239.271            239.301  239.277         600        1513
    3              293.898            294.018  293.922         472        1571
    4              120.563            120.693  120.589         506        1520
    5              205.574            205.714  205.602         647        1518
    6              240.821            240.981  240.853         639        1523
      time_0.5 F1.time_0.5 F2.time_0.5 time_0.8 F1.time_0.8 F2.time_0.8 Error
    1   27.745         625        1268   27.754         603        1261    NA
    2  239.286         624        1572  239.295         618        1590    NA
    3  293.958         341        1472  293.994         397        1416    NA
    4  120.628         590        1454  120.667         496        1448    NA
    5  205.644         658        1385  205.686         641        1292    NA
    6  240.901         649        1402  240.949         619        1284    NA
      Neighborhood vowel
    1  Interviewer    AW
    2  Interviewer    AW
    3  Interviewer    AW
    4  Interviewer    AW
    5  Interviewer    AW
    6  Interviewer    AW

The data looks good, but there’s more columns here than we need. Let’s
just select the columns we’ll actually be working with.

``` r
vowel_data_smaller <- vowel_data %>%
  #selecting only necessary columns
  select(Speaker, Neighborhood, Text, Target.phonemes, starts_with("F"), MatchId, vowel)

#another quick look
vowel_data_smaller |> 
  head()
```

                Speaker Neighborhood     Text Target.phonemes F1.time_0.2
    1 Barbara Johnstone  Interviewer      our              6r         641
    2 Barbara Johnstone  Interviewer    about            @b6t         600
    3 Barbara Johnstone  Interviewer hometown          h5mt6n         472
    4 Barbara Johnstone  Interviewer  about .            @b6t         506
    5 Barbara Johnstone  Interviewer  about ?            @b6t         647
    6 Barbara Johnstone  Interviewer      out              6t         639
      F2.time_0.2 F1.time_0.5 F2.time_0.5 F1.time_0.8 F2.time_0.8
    1        1331         625        1268         603        1261
    2        1513         624        1572         618        1590
    3        1571         341        1472         397        1416
    4        1520         590        1454         496        1448
    5        1518         658        1385         641        1292
    6        1523         649        1402         619        1284
                                                                                                       MatchId
    1 g_352;em_12_107932;n_3092553-n_4584645;p_146;#=es_1_1767198;prefix=0001-;[0]=ew_0_685107;[1]=ew_0_685107
    2 g_352;em_12_108054;n_3092671-n_4584700;p_146;#=es_1_1767905;prefix=0002-;[0]=ew_0_685768;[1]=ew_0_685768
    3 g_352;em_12_108083;n_3092697-n_4584714;p_146;#=es_1_1768027;prefix=0003-;[0]=ew_0_685933;[1]=ew_0_685933
    4 g_353;em_12_108579;n_3097998-n_4584878;p_146;#=es_1_1769656;prefix=0004-;[0]=ew_0_688639;[1]=ew_0_688639
    5 g_353;em_12_108629;n_3098049-n_4584902;p_146;#=es_1_1769974;prefix=0005-;[0]=ew_0_688900;[1]=ew_0_688900
    6 g_353;em_12_108651;n_3098069-n_4584908;p_146;#=es_1_1770007;prefix=0006-;[0]=ew_0_689001;[1]=ew_0_689001
      vowel
    1    AW
    2    AW
    3    AW
    4    AW
    5    AW
    6    AW

Now we have a data frame that will be easier to manage in our workflow.

Next, let’s remove outliers, normalize measurements, and do some
subsetting on the data.

``` r
vowel_data_outliers_removed <-
  vowel_data_smaller %>%
  group_by(Speaker) %>%
  ##Drop speakers w/ fewer than 75 tokens for reliable outlier-checking
  filter(n() >= 75,
         #finding outliers based on all F1 and F2 measurements
         !find_outliers(F1.time_0.2, F2.time_0.2, F1.time_0.5, F2.time_0.5, F1.time_0.8, F2.time_0.8)) %>%
  ungroup()

#ANAE normalization
vowel_data_normed <-
  vowel_data_outliers_removed %>% 
  group_by(Speaker) %>% 
  joeyr_norm_anae(hz_cols = c(F1.time_0.2, F2.time_0.2, F1.time_0.5, F2.time_0.5, F1.time_0.8, F2.time_0.8), token_id = row.names(.), speaker_id = Speaker) %>%
  ungroup() 

#subsetting to remove incomplete words
vowel_data_subset <- 
  vowel_data_normed |>  
  filter(!str_detect(Text, "~$"))
```

The final steps we’ll do before making plots are calculating the
trajectory length of vowel tokens and pivoting the data to be longer.

We’ll also take another look at the data to see what shape it’s in
before plotting it.

``` r
vowel_data_longer <-
  vowel_data_subset |>
  #calculating trajectory length
  mutate(traj_length = eucl_dist(F2.time_0.2_anae, F2.time_0.5_anae, F1.time_0.2_anae, F1.time_0.5_anae) +
           eucl_dist(F2.time_0.5_anae, F2.time_0.8_anae, F1.time_0.5_anae, F1.time_0.8_anae)) |> 
  ##Longer for geom_paths
  pivot_longer(ends_with("_anae"), names_to=c(".value","timestamp"), names_pattern="(F[12]).time_(0\\.\\d)_anae") |>
  ##Remove unnormalized meas
  select(-contains("time_"))

vowel_data_longer |> 
  head()
```

    # A tibble: 6 × 10
      Speaker Neighborhood Text  Target.phonemes MatchId vowel traj_length timestamp
      <chr>   <chr>        <chr> <chr>           <chr>   <chr>       <dbl> <chr>    
    1 Barbar… Interviewer  our   6r              g_352;… AW           91.4 0.2      
    2 Barbar… Interviewer  our   6r              g_352;… AW           91.4 0.5      
    3 Barbar… Interviewer  our   6r              g_352;… AW           91.4 0.8      
    4 Barbar… Interviewer  about @b6t            g_352;… AW           85.8 0.2      
    5 Barbar… Interviewer  about @b6t            g_352;… AW           85.8 0.5      
    6 Barbar… Interviewer  about @b6t            g_352;… AW           85.8 0.8      
    # ℹ 2 more variables: F1 <dbl>, F2 <dbl>

The first plot is all the midpoints for all the vowels in the subset.

``` r
#plotting the vowel data
ggplot(vowel_data_subset, aes(x = F2.time_0.5_anae, y = F1.time_0.5_anae, color = vowel, label = vowel)) +
  #making slightly transparent points for each individual vowel
  geom_point(alpha = 0.2) +
  #making an ellipse for the vowel measurements overall
  stat_ellipse(level = .67, geom = "polygon", alpha = 0.6, aes(fill = vowel)) +
  #using the means data to add a label to the center
  geom_label(data = vowel_data_subset %>% 
               summarise(across(ends_with("0.5_anae"), mean),
                         across(vowel, unique))) +
  #flipping the scales, like a good vowel plot should
  scale_x_reverse("F1 (ANAE-normalized Hz)") + 
  scale_y_reverse("F2 (ANAE-normalized Hz)") +
  theme_classic() + 
  theme(legend.position="none")
```

![](Images/all_midpoints-1.png)<!-- -->

The second plot is the trajectories of all the subset vowels by
participant neighborhood, excluding tokens from interviewers.

``` r
vowel_data_longer |>
  filter(Neighborhood != "Interviewer") |>
  arrange(desc(traj_length)) |>
  ggplot(aes(x = F2, y = F1, color=traj_length)) +
  #making slightly transparent points for each individual vowel
  geom_path(aes(group=MatchId), alpha=0.2, arrow=arrow(length=unit(0.05, "inches"))) +
  facet_wrap(~ Neighborhood) +
  #flipping the scales, like a good vowel plot should
  scale_x_reverse("F1 (ANAE-normalized Hz)") + 
  scale_y_reverse("F2 (ANAE-normalized Hz)") +
  labs(color="Trajectory\nlength (Hz)") +
  theme_classic()
```

![](Images/all_trajectories-1.png)<!-- -->

## Not used in article: Speaker trajectories

It seems like there are some neighborhood-level differences in vowel
productions, and we can investigate this further by looking at the
average trajectories by speaker.

Note: These plots are not included in *Introducing the Archive of
Pittsburgh Language and Speech (APLS), a publicly accessible, richly
annotated corpus of sociolinguistic interviews* but are included here as
they may be of interest to other researchers.

``` r
vowel_data_means <- 
  vowel_data_longer |>
  summarise(across(Neighborhood, unique),
            across(c(F1, F2, traj_length), mean),
            .by=c(Speaker, timestamp)) |>
  ##Break Cranberry Township across two lines
  mutate(across(Neighborhood, \(x) if_else(x=="Cranberry Township", "Cranberry\nTownship", x)))

vowel_data_means |>
  filter(Neighborhood != "Interviewer") |>
  ggplot(aes(x = F2, y = F1, color=Neighborhood)) +
  geom_path(aes(group=Speaker), alpha=0.5, arrow=arrow(length=unit(0.125, "inches"))) +
  geom_text(data=vowel_data_means |> 
              filter(Neighborhood != "Interviewer",
                     timestamp==0.2),
            aes(label = Speaker), show.legend=FALSE) +
  #flipping the scales, like a good vowel plot should
  scale_x_reverse("F1 (ANAE-normalized Hz)") + 
  scale_y_reverse("F2 (ANAE-normalized Hz)") +
  theme_classic()
```

![](Images/speaker_trajectories-1.png)<!-- -->

This is nice, but it’s still a little messy and it isn’t capturing any
information about participant’s average trajectory lengths.

Instead, let’s use color to indicate trajectory length and facet the
plots by neighborhood to make the differences easier to see.

``` r
vowel_data_means |>
  filter(Neighborhood != "Interviewer") |>
  #changing Cranberry Township back to a space instead of a newline break
  mutate(across(Neighborhood, \(x) if_else(x=="Cranberry\nTownship", "Cranberry Township", x))) |> 
  ggplot(aes(x = F2, y = F1, color=traj_length)) +
  geom_path(aes(group=Speaker), alpha=0.5,arrow=arrow(length=unit(0.125, "inches"))) +
  geom_text(data=vowel_data_means |>
              #changing Cranberry Township back to a space instead of a newline break
              mutate(across(Neighborhood, \(x) if_else(x=="Cranberry\nTownship", "Cranberry Township", x))) |> 
              filter(Neighborhood != "Interviewer",
                     timestamp==0.2),
            aes(label = Speaker), show.legend=FALSE) +
  facet_wrap(~ Neighborhood) +
  #flipping the scales, like a good vowel plot should
  scale_x_reverse("F1 (ANAE-normalized Hz)") + 
  scale_y_reverse("F2 (ANAE-normalized Hz)") +
  theme_classic()
```

![](Images/trajectories_by_neighborhood-1.png)<!-- -->

# Session Info

``` r
sessionInfo()
```

    R version 4.3.2 (2023-10-31)
    Platform: x86_64-apple-darwin20 (64-bit)
    Running under: macOS 15.3.1

    Matrix products: default
    BLAS:   /Library/Frameworks/R.framework/Versions/4.3-x86_64/Resources/lib/libRblas.0.dylib 
    LAPACK: /Library/Frameworks/R.framework/Versions/4.3-x86_64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0

    locale:
    [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

    time zone: America/Detroit
    tzcode source: internal

    attached base packages:
    [1] stats     graphics  grDevices utils     datasets  methods   base     

    other attached packages:
     [1] joeyr_0.11      tidynorm_0.3.0  lubridate_1.9.4 forcats_1.0.0  
     [5] stringr_1.5.1   dplyr_1.1.4     purrr_1.1.0     readr_2.1.5    
     [9] tidyr_1.3.1     tibble_3.3.0    ggplot2_3.5.2   tidyverse_2.0.0

    loaded via a namespace (and not attached):
     [1] gtable_0.3.6       compiler_4.3.2     Rcpp_1.1.0         tidyselect_1.2.1  
     [5] scales_1.4.0       yaml_2.3.10        fastmap_1.2.0      R6_2.6.1          
     [9] labeling_0.4.3     generics_0.1.4     knitr_1.50         MASS_7.3-60.0.1   
    [13] pillar_1.11.0      RColorBrewer_1.1-3 tzdb_0.5.0         rlang_1.1.6       
    [17] utf8_1.2.6         stringi_1.8.7      xfun_0.52          timechange_0.3.0  
    [21] cli_3.6.5          withr_3.0.2        magrittr_2.0.3     digest_0.6.37     
    [25] grid_4.3.2         rstudioapi_0.17.1  hms_1.1.3          lifecycle_1.0.4   
    [29] vctrs_0.6.5        evaluate_1.0.4     glue_1.8.0         farver_2.1.2      
    [33] rmarkdown_2.29     tools_4.3.2        pkgconfig_2.0.3    htmltools_0.5.8.1 
