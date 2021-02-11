#Tom Working script to play with ozab - changing dataset for Clarno
#See sagebrush.R script
sagebrush<-sagebrush_clarno

#alluvial plot

#sagebrush %>%
#  alluvial_plot(`Cover Class`, Fire) #need to bring in a factor

### Continuous Variable
sagebrush %>%
  mutate(scosa = cut(scosa, breaks = c(-0.25, -0.15, 0, 0.15, 0.25))) %>%
  plot_cover_class_by_covariate('BROTEC', scosa)

sagebrush %>%
  plot_cover_class_by_covariate('BROTEC', Year)

sagebrush %>%
  plot_cover_class_by_covariate('PSESPI', Year)
## Big Sagebrush Analysis

### Filter Dataset, Add Presence Indicator, and Transform Covariates
sagebrush2 <-
  sagebrush %>%
  filter(Species == 'BROTEC') %>%
  add_presence(cover_class_col = `Cover Class`)# %>%
 # mutate(
 #   `Dist. to Bound` = `Dist. to Bound` / 1000,
 #   Topography2 = Topography^2
 # )

### Run Model
btec_result <-
  ozab(
    sagebrush2,
    `Presence` ~ scosa + I(scosa^2) + deficit,
    `Cover Class` ~ scosa + I(scosa^2) + deficit,
    cutpoint_scheme = daubenmire(),
    chains = 1,iter=2000
  )

Inference for Stan model: OZAB_model_logit.
1 chains, each with iter=2000; warmup=1000; thin=1;
post-warmup draws per chain=1000, total post-warmup draws=1000.

mean se_mean   sd     2.5%      25%      50%      75%    97.5% n_eff Rhat
presence_intercept       1.03    0.00 0.05     0.93     1.00     1.03     1.06     1.12   811 1.00
presence_scosa           0.89    0.01 0.22     0.47     0.75     0.89     1.04     1.33  1190 1.00
presence_I(scosa^2)     -0.25    0.04 1.03    -2.14    -0.95    -0.25     0.46     1.81   800 1.00
presence_deficit        -0.05    0.00 0.03    -0.12    -0.07    -0.05    -0.02     0.02  1842 1.00
abundance_intercept     -2.92    0.01 0.16    -3.25    -3.03    -2.91    -2.80    -2.61   792 1.00
abundance_scosa          3.29    0.03 1.03     1.36     2.56     3.27     4.02     5.42  1089 1.00
abundance_I(scosa^2)   -13.11    0.18 4.76   -23.02   -16.29   -12.90    -9.86    -4.18   703 1.00
abundance_deficit       -0.14    0.00 0.14    -0.40    -0.24    -0.14    -0.05     0.12   955 1.00
phi                      1.67    0.00 0.07     1.54     1.62     1.67     1.72     1.81  1137 1.00
lp__                 -2294.72    0.10 2.08 -2299.73 -2296.01 -2294.40 -2293.16 -2291.64   444 1.01

Samples were drawn using NUTS(diag_e) at Tue Feb 09 17:00:31 2021.
For each parameter, n_eff is a crude measure of effective sample size,
and Rhat is the potential scale reduction factor on split chains (at
                                                                  convergence, Rhat=1).
#######################
#PSESPI

sagebrush3 <-
  sagebrush %>%
  filter(Species == 'PSESPI') %>%
  add_presence(cover_class_col = `Cover Class`)

### Run Model
psespi_result <-
  ozab(
    sagebrush3,
    `Presence` ~ scosa + I(scosa^2) + deficit,
    `Cover Class` ~ scosa + I(scosa^2) + deficit,
    cutpoint_scheme = daubenmire(),
    chains = 1,iter=2000
  )
