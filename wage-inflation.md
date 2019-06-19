---
title: "Labor Costs and Inflation"
author: Michael Kozin and Itamar Caspi
date: "2019-06-19"
abstract: |
  In this note we estimate the dynamic relationship between CPI inflation and labor costs. 
output: 
  html_document:
    theme: journal
    highlight: haddock 
    toc: yes
    toc_depth: 4
    toc_float: yes
    keep_md: true
    code_folding: hide
---




## Introduction

tba

## Data

tna 


## The method

I will implement the first basic evaluation methods for the relation between labor costs inflation and price inflation following the first section displayed in the ECB's working paper no.2235. The evaluation will include *cross-correlations and Granger causality* tests.

I will use ULC (calculated as the total compensation per sector divided by the real GDP per sector) and use the sector inflation.


The data starts from Q1 2006 because of previously missing data or a change of measurement method for the compensation and sector GDP (from sna93 to cls11).



```r
library(readxl)

df <- read_xlsx("data/wage-inflation-data.xlsx",
                sheet = "R_data",
                skip = 1)
```



```r
library(tidyverse)

sector_code2name <- 
  tibble(sector_code = c("ag", "ind", "el", "const",
                         "tran", "ser_pub",
                          "ser_bis", "all"),
         sector_name = c("Agriculture", "Industry",
                         "Electricity and Water",
                         "Construction", "Transportation",
                         "Public Services",
                         "Business Services", "Total"))
```


Next, we tidy the data (each variable is a column, each observation is a row):


```r
library(lubridate)

df_tidy <- df %>% 
  mutate(DATE = ymd(DATE)) %>% 
  gather("type_sector_code", "value", -DATE) %>% 
  separate(type_sector_code,
           into = c("type", "sector_code"),
           sep = "_",
           extra = "merge") %>%
  filter(sector_code != "bs") %>% 
  left_join(sector_code2name) %>% 
  mutate(sector_name = as_factor(sector_name))
```



```r
df_change <- df_tidy %>% 
  group_by(type, sector_code) %>% 
  mutate(dlog_value_q = 400*(log(value) - log(lag(value)))) %>% 
  mutate(dlog_value_y = 100*(log(value) - log(lag(value, 4)))) %>% 
  mutate(dlog_value_q_scaled = scale(dlog_value_q)[,]) %>% 
  drop_na() %>% 
  ungroup()
```

Seasonally adjust the data


```r
seas_adj <- function(df) {
  df_ts <- ts(df, frequency = 4)
  df_ts_sa <- stats::stl(df_ts,
                         s.window = "periodic",
                         robust = TRUE)
  df_ts_tidy <- broom::augment(df_ts_sa)
  df_ts_tidy$.seasadj
}

df_change_sa <- df_change %>%
  group_by(type, sector_code) %>% 
  mutate(dlog_value_q_sa = seas_adj(dlog_value_q)) %>% 
  ungroup()
```


Next, we plot the data


```r
theme_set(theme_minimal())

df_change %>% 
  ggplot(aes(DATE, dlog_value_y, color = type)) +
  geom_line(size = 1) +
  facet_wrap( ~ sector_name, scales = "free") +
  labs(x = "Year",
       y = "Percent change",
       color = "Index",
       title = "Figure 1. Year over year percent change") +
  theme(legend.position = "top")
```

Prepare (nest) the data for the `ccf` and `grangertet` functions


```r
df_nest <- df_change %>%
  select(DATE, type, sector_name, dlog_value_q) %>% 
  nest(-sector_name) %>% 
  mutate(data_spread = map(data, ~ spread(.x, type, dlog_value_q))) %>% 
  mutate(data_spread = map(data_spread, ~ drop_na(.x)))
```


## Cross correlations

When working with a time series, one important thing we wish to determine is whether one series precede changes in another. In other words, is there a strong correlation between a time series and another given a number of lags? The way we can detect this is through measuring cross-correlation.

For instance, one time series could serve as a lagging indicator. This is where the effect of a change in one time series transfers to the other time series several periods later. This is quite common in economic data; e.g. an economic shock having an effect on GDP two quarters later.

But how do we measure the lag where this is significant? One very h


Run the `ccf` test on the data by sectors

```r
conf.level = 0.9

df_ccf <- df_nest %>%
  mutate(n.used = map_int(data_spread, ~ length(.x$p))) %>% 
  mutate(conf.int = qnorm((1 + conf.level) / 2) / sqrt(n.used)) %>% 
  mutate(ccf = map(data_spread, ~ ccf(.x$p, .x$lc,
                                      type = "correlation",
                                      lag.max = 8,
                                      plot = FALSE))) %>% 
  mutate(ccf_tidy = map(ccf, broom::tidy)) %>% 
  unnest(ccf_tidy) %>% 
  arrange(sector_name, lag)
```


Plot results


```r
df_ccf %>% 
  ggplot(aes(lag, acf)) +
  geom_segment(aes(xend = lag, yend = 0)) +
  geom_point(size = 1, colour = "black") +
  geom_line(aes(lag, conf.int),color='blue', linetype='dashed') +
  geom_line(aes(lag, -conf.int),color='blue', linetype='dashed') +
  scale_x_continuous(breaks=seq(-8,8,2)) +
  facet_wrap( ~ sector_name) +
  labs(x = "Lag",
       y = "Cross-correlation",
       title = "Figure 2. Cross-correlations: inflation and ULC")
```


## Granger Causality Tests


Let $y$ and $x$ be stationary time series. To test the null hypothesis that $x$ does not Granger-cause $y$, we run the following linear regression model 

$$y_{t}=a_{0}+a_{1} y_{t-1}+\cdots+a_{p} y_{t-p}+\beta_1{x_{t-1}} +\cdots+\beta_{q} x_{t-q}+\varepsilon_{t}$$

Under the null hypothesis, the coefficients of lagged $x$ are different from zero, i.e.

$$H_0: \beta_1=\beta_2=\dots=\beta_q=0\qquad (x\not\to y),$$
where $(\not\to$) reads $x$ does not Granger cause $y$. 

The results of the Granger causality tests are given in Figure 3. Three results stand out: unit labor cost Granger causes inflation $(p$-value $<10\%)$, and not vice versa. Similar finding holds for the transportation sector. By contrast, in the agriculture sector, we find that inflation Granger causes unit labor costs. For the rest of the sectors...


```r
lag.order <- 4

df_granger <- df_nest %>% 
  # test inflation -> ulc ?
  mutate(granger_p2lc = map(data_spread,
                       ~ lmtest::grangertest(lc ~ p,
                                             order = lag.order,
                                             data = .x))) %>%
  mutate(granger_p2lc_tidy = map(granger_p2lc, broom::tidy)) %>% 
  # test ulc -> inflation ?
  mutate(granger_lc2p = map(data_spread,
                       ~ lmtest::grangertest(p ~ lc,
                                             order = lag.order,
                                             data = .x))) %>%
  mutate(granger_lc2p_tidy = map(granger_lc2p, broom::tidy)) %>% 
  unnest(granger_p2lc_tidy, granger_lc2p_tidy) %>% 
  rename(p.value_p2lc = p.value, p.value_lc2p = p.value1) %>% 
  drop_na()
```



```r
df_granger %>%
  arrange(sector_name) %>% 
  ggplot(aes(x = sector_name)) +
  geom_linerange(aes(ymin = p.value_p2lc, ymax = p.value_lc2p)) +
  geom_point(aes(y = p.value_p2lc,
                 colour = "inflation to labor costs"),
             size = 3, shape = 15) +
  geom_point(aes(y = p.value_lc2p,
                 colour = "labor costs to inflation"),
             size = 3, shape = 16) +
  geom_hline(yintercept = 0.1, color = "gray50") +
  labs(x = "",
       y = "p-value",
       color = "Granger causality",
       title = "Figure 3. Granger causality tests") +
  scale_y_continuous(expand = c(0.01,0.01), breaks = seq(0, 1, 0.1)) +
  coord_flip() +
  theme(legend.position="top")
```


## Bootstrap


```r
granger_fun <- function(tsb, lag.order) {
  
  result_21 <- lmtest::grangertest(tsb[,1] ~ tsb[,2],
                                order = lag.order)
  
  result_12 <- lmtest::grangertest(tsb[,2] ~ tsb[,1],
                                order = lag.order)
  bind_rows(broom::tidy(result_21), broom::tidy(result_12)) %>% 
    drop_na() %>% 
    select(statistic) %>% 
    pull()
}
```



```r
library(boot) # for the bootstrap
library(furrr) # for parallel computation
plan(multiprocess)

boot_rep <- 199
block_length <- round(48^(1/3))
lag_order <- 4
sim <- "geom"

df_granger_boot <- df_nest %>% 
  mutate(ts_data = map(data_spread, ~ ts(cbind(.x$p, .x$lc)))) %>% 
  mutate(granger_ouput = map(ts_data,
                            ~ tsboot(.x, granger_fun, R = boot_rep,
                                     l = block_length, sim = sim,
                                     lag.order = lag_order))) %>% 
  mutate(p.value_p2lc = map_dbl(granger_ouput, ~
                                 mean(.x$t[,2] > .x$t0[2]))) %>% 
  mutate(p.value_lc2p = map_dbl(granger_ouput, ~
                                 mean(.x$t[,1] > .x$t0[1])))
```




```r
df_granger_boot %>%
  arrange(sector_name) %>% 
  ggplot(aes(x = sector_name)) +
  geom_linerange(aes(ymin = p.value_p2lc, ymax = p.value_lc2p)) +
  geom_point(aes(y = p.value_p2lc,
                 colour = "inflation to labor costs"),
             size = 3, shape = 15) +
  geom_point(aes(y = p.value_lc2p,
                 colour = "labor costs to inflation"),
             size = 3, shape = 16) +
  geom_hline(yintercept = 0.1, color = "gray50") +
  labs(x = "",
       y = "p-value",
       color = "Granger causality",
       title = "Figure 3. Granger causality tests") +
  scale_y_continuous(expand = c(0.01,0.01), breaks = seq(0, 1, 0.1)) +
  coord_flip() +
  theme(legend.position="top")
```


## References

