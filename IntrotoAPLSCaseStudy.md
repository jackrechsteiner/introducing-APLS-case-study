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

                                                    Title DataVersion
    1 APLS: the Archive of Pittsburgh Language and Speech       0.4.3
    2 APLS: the Archive of Pittsburgh Language and Speech       0.4.3
    3 APLS: the Archive of Pittsburgh Language and Speech       0.4.3
    4 APLS: the Archive of Pittsburgh Language and Speech       0.4.3
    5 APLS: the Archive of Pittsburgh Language and Speech       0.4.3
    6 APLS: the Archive of Pittsburgh Language and Speech       0.4.3
                                                SearchName Number
    1 syllables=.*6[pbtdkgfvTDszSZhJ_mnNlrwjFHP] segment=6      1
    2 syllables=.*6[pbtdkgfvTDszSZhJ_mnNlrwjFHP] segment=6      2
    3 syllables=.*6[pbtdkgfvTDszSZhJ_mnNlrwjFHP] segment=6      3
    4 syllables=.*6[pbtdkgfvTDszSZhJ_mnNlrwjFHP] segment=6      4
    5 syllables=.*6[pbtdkgfvTDszSZhJ_mnNlrwjFHP] segment=6      5
    6 syllables=.*6[pbtdkgfvTDszSZhJ_mnNlrwjFHP] segment=6      6
              Transcript           Speaker    Line LineEnd
    1 CB01interview1.eaf Barbara Johnstone  20.860  29.431
    2 CB01interview1.eaf Barbara Johnstone  33.051  36.396
    3 CB01interview1.eaf Barbara Johnstone 238.681 239.999
    4 CB01interview1.eaf Barbara Johnstone 262.537 265.795
    5 CB01interview1.eaf Barbara Johnstone 292.018 294.228
    6 CB01interview1.eaf Barbara Johnstone 891.901 897.676
                                                                                                       MatchId
    1 g_352;em_12_107932;n_3092553-n_4584645;p_146;#=es_1_1767198;prefix=0001-;[0]=ew_0_685107;[1]=ew_0_685107
    2 g_352;em_12_107935;n_3092558-n_4584647;p_146;#=es_1_1767250;prefix=0002-;[0]=ew_0_685124;[1]=ew_0_685124
    3 g_352;em_12_108054;n_3092671-n_4584700;p_146;#=es_1_1767905;prefix=0003-;[0]=ew_0_685768;[1]=ew_0_685768
    4 g_352;em_12_108073;n_3092689-n_4584708;p_146;#=es_1_1768003;prefix=0004-;[0]=ew_0_685849;[1]=ew_0_685849
    5 g_352;em_12_108083;n_3092697-n_4584714;p_146;#=es_1_1768027;prefix=0005-;[0]=ew_0_685933;[1]=ew_0_685933
    6 g_352;em_12_108469;n_3093028-n_3093030;p_146;#=es_1_1769186;prefix=0006-;[0]=ew_0_687825;[1]=ew_0_687825
                                                                                     URL
    1 https://apls.pitt.edu/labbcat/transcript?transcript=CB01interview1.eaf#ew_0_685107
    2 https://apls.pitt.edu/labbcat/transcript?transcript=CB01interview1.eaf#ew_0_685124
    3 https://apls.pitt.edu/labbcat/transcript?transcript=CB01interview1.eaf#ew_0_685768
    4 https://apls.pitt.edu/labbcat/transcript?transcript=CB01interview1.eaf#ew_0_685849
    5 https://apls.pitt.edu/labbcat/transcript?transcript=CB01interview1.eaf#ew_0_685933
    6 https://apls.pitt.edu/labbcat/transcript?transcript=CB01interview1.eaf#ew_0_687825
      Before.Match       Text After.Match       word word.start word.end syllables
    1          for        our         own        our     27.730   27.830       '6r
    2    Cranberry   Township         and   Township     34.331   34.841      't6n
    3         what      about        your      about    239.211  239.331      'b6t
    4    Cranberry Township ?             Township ?    265.347  265.767      't6n
    5           my   hometown               hometown    293.598  294.078      "t6n
    6          sit    outside          on    outside    896.511  896.661       "6t
      syllables.start syllables.end orthography segment segment.start segment.end
    1          27.730        27.830         our       6        27.730      27.760
    2          34.331        34.561    township       6        34.411      34.481
    3         239.241       239.331       about       6       239.271     239.301
    4         265.347       265.557    township       6       265.417     265.517
    5         293.808       294.078    hometown       6       293.898     294.018
    6         896.511       896.571     outside       6       896.511     896.541
      time_0.2 F1.time_0.2 F2.time_0.2 time_0.5 F1.time_0.5 F2.time_0.5 time_0.8
    1   27.736         641        1331   27.745         625        1268   27.754
    2   34.425         378        1575   34.446         343        1567   34.467
    3  239.277         600        1513  239.286         624        1572  239.295
    4  265.437         386        1729  265.467         442        1621  265.497
    5  293.922         472        1571  293.958         341        1472  293.994
    6  896.517         632        1556  896.526         642        1588  896.535
      F1.time_0.8 F2.time_0.8 Error Neighborhood vowel
    1         603        1261    NA  Interviewer    AW
    2         357        1489    NA  Interviewer    AW
    3         618        1590    NA  Interviewer    AW
    4         420        1497    NA  Interviewer    AW
    5         397        1416    NA  Interviewer    AW
    6         643        1551    NA  Interviewer    AW

The data looks good, but there’s more columns here than we need. Let’s
just select the columns we’ll actually be working with.

``` r
vowel_data_smaller <- vowel_data %>%
  #selecting only necessary columns
  select(Speaker, Neighborhood, Text, syllables, starts_with("F"), MatchId, vowel)

#another quick look
vowel_data_smaller |> 
  head()
```

                Speaker Neighborhood       Text syllables F1.time_0.2 F2.time_0.2
    1 Barbara Johnstone  Interviewer        our       '6r         641        1331
    2 Barbara Johnstone  Interviewer   Township      't6n         378        1575
    3 Barbara Johnstone  Interviewer      about      'b6t         600        1513
    4 Barbara Johnstone  Interviewer Township ?      't6n         386        1729
    5 Barbara Johnstone  Interviewer   hometown      "t6n         472        1571
    6 Barbara Johnstone  Interviewer    outside       "6t         632        1556
      F1.time_0.5 F2.time_0.5 F1.time_0.8 F2.time_0.8
    1         625        1268         603        1261
    2         343        1567         357        1489
    3         624        1572         618        1590
    4         442        1621         420        1497
    5         341        1472         397        1416
    6         642        1588         643        1551
                                                                                                       MatchId
    1 g_352;em_12_107932;n_3092553-n_4584645;p_146;#=es_1_1767198;prefix=0001-;[0]=ew_0_685107;[1]=ew_0_685107
    2 g_352;em_12_107935;n_3092558-n_4584647;p_146;#=es_1_1767250;prefix=0002-;[0]=ew_0_685124;[1]=ew_0_685124
    3 g_352;em_12_108054;n_3092671-n_4584700;p_146;#=es_1_1767905;prefix=0003-;[0]=ew_0_685768;[1]=ew_0_685768
    4 g_352;em_12_108073;n_3092689-n_4584708;p_146;#=es_1_1768003;prefix=0004-;[0]=ew_0_685849;[1]=ew_0_685849
    5 g_352;em_12_108083;n_3092697-n_4584714;p_146;#=es_1_1768027;prefix=0005-;[0]=ew_0_685933;[1]=ew_0_685933
    6 g_352;em_12_108469;n_3093028-n_3093030;p_146;#=es_1_1769186;prefix=0006-;[0]=ew_0_687825;[1]=ew_0_687825
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
      Speaker Neighborhood Text  syllables MatchId vowel traj_length timestamp    F1
      <chr>   <chr>        <chr> <chr>     <chr>   <chr>       <dbl> <chr>     <dbl>
    1 Barbar… Interviewer  our   '6r       g_352;… AW           91.4 0.2        665.
    2 Barbar… Interviewer  our   '6r       g_352;… AW           91.4 0.5        649.
    3 Barbar… Interviewer  our   '6r       g_352;… AW           91.4 0.8        626.
    4 Barbar… Interviewer  Town… 't6n      g_352;… AW          120.  0.2        392.
    5 Barbar… Interviewer  Town… 't6n      g_352;… AW          120.  0.5        356.
    6 Barbar… Interviewer  Town… 't6n      g_352;… AW          120.  0.8        371.
    # ℹ 1 more variable: F2 <dbl>

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

    R version 4.5.2 (2025-10-31 ucrt)
    Platform: x86_64-w64-mingw32/x64
    Running under: Windows 11 x64 (build 26100)

    Matrix products: default
      LAPACK version 3.12.1

    locale:
    [1] LC_COLLATE=English_United States.utf8 
    [2] LC_CTYPE=English_United States.utf8   
    [3] LC_MONETARY=English_United States.utf8
    [4] LC_NUMERIC=C                          
    [5] LC_TIME=English_United States.utf8    

    time zone: America/New_York
    tzcode source: internal

    attached base packages:
    [1] stats     graphics  grDevices utils     datasets  methods   base     

    other attached packages:
     [1] joeyr_0.11      tidynorm_0.3.0  lubridate_1.9.4 forcats_1.0.1  
     [5] stringr_1.6.0   dplyr_1.1.4     purrr_1.2.0     readr_2.1.5    
     [9] tidyr_1.3.2     tibble_3.3.0    ggplot2_4.0.1   tidyverse_2.0.0

    loaded via a namespace (and not attached):
     [1] gtable_0.3.6       compiler_4.5.2     Rcpp_1.1.0         tidyselect_1.2.1  
     [5] scales_1.4.0       yaml_2.3.10        fastmap_1.2.0      R6_2.6.1          
     [9] labeling_0.4.3     generics_0.1.4     knitr_1.51         MASS_7.3-65       
    [13] pillar_1.11.0      RColorBrewer_1.1-3 tzdb_0.5.0         rlang_1.1.6       
    [17] utf8_1.2.6         stringi_1.8.7      xfun_0.52          S7_0.2.1          
    [21] otel_0.2.0         timechange_0.3.0   cli_3.6.5          withr_3.0.2       
    [25] magrittr_2.0.4     digest_0.6.37      grid_4.5.2         hms_1.1.3         
    [29] lifecycle_1.0.4    vctrs_0.6.5        evaluate_1.0.4     glue_1.8.0        
    [33] farver_2.1.2       rmarkdown_2.30     tools_4.5.2        pkgconfig_2.0.3   
    [37] htmltools_0.5.8.1 
