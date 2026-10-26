#' GFS Love by God and Feeling Punished by God
#' Primary analyses excluding the participants responding "Not relevant"
#'
#' (1) for tables in the supplemental materials that show
#'   (a) the ordered proportions for loved by God across countries,
#'   (b) the ordered proportions for punished by God across countries,
#'   (c) the random-effects meta-analysis of Loved by God, and
#'   (d) the random-effects meta-analysis of Punished by God ONLY the participants who report belief in "One God," "Multiple Gods," or a "Spiritual Force."

# Supplemental analysis on the subpopulation of participants responding ``"One God," "Multiple Gods," or a "Spiritual Force.''

library(Rglobalflourishing)
load_packages()
# global options
options(
  survey.lonely.psu = "certainty",
  future.plan = "multisession"
)
handlers(global = TRUE)
num_cores <- availableCores(constraints = "connections") - 1

data.dir <- "data"
dataset.name <- "gfs_all_countries_Y1_Y2.sav"

# Meta-analyic means by religious group
gc(full=TRUE)

RECODE.DEFAULTS <- list(
  FOCAL_VARIABLE = NULL,
  DEMOGRAPHICS.CHILDHOOD.PRED.VEC = c(
    get_variable_codes("GENDER.RACE", appnd=""),
    get_variable_codes("RETROSPECTIVE.VARS", appnd="_Y1"),
    get_variable_codes("DEMOGRAPHIC.VARS", appnd="_Y1"),
    get_variable_codes("DEMOGRAPHIC.VARS", appnd="_Y2")
  ),
  VARIABLES.VEC = c(get_variable_codes("VARS.Y1"), get_variable_codes("VARS.Y2")),
  VALUES_DEFINING_UPPER_CATEGORY = NULL,
  VALUES_DEFINING_LOWER_CATEGORY = NULL,
  FORCE_BINARY = FALSE,
  FORCE_CONTINUOUS = FALSE,
  USE_DEFAULT = TRUE
)
recode.fx <- function(data){
  data |>
    mutate(
      ORIG_BELIEVE_GOD_Y1 = BELIEVE_GOD_Y1,
      ORIG_LOVED_BY_GOD_Y1 = LOVED_BY_GOD_Y1,
      ORIG_GOD_PUNISH_Y1 = GOD_PUNISH_Y1,
      ALL_REL2_Y1 = recode_labels(REL2_Y1, var = "REL2_Y1", include.value = FALSE),
      # From Kathy: Religion groups for analysis:
      # Christianity, Islam, Hinduism, Buddhism, Judaism, Folk Religions (Animist, Spiritism, African Religions), Other (Sikhism, Shinto, Tao, Chinese, Confucianism, Other), and None
      REL2_GRPS = case_when(
        REL2_Y1 == 1 ~ "Christianity",
        REL2_Y1 == 2 ~ "Islam",
        REL2_Y1 == 3 ~ "Hinduism",
        REL2_Y1 == 4 ~ "Buddhism",
        REL2_Y1 == 5 ~ "Judaism",
        REL2_Y1 %in% c(12:15) ~ "Folk Religions",
        REL2_Y1 %in% c(6:11, 96) ~ "Other",
        REL2_Y1 == 97 ~ "None",
        .default = "Other"
      ),
    )
}
recode_imp_by_country(
  data.dir =  data.dir,
  wave = 2 ,
  nimp = 20,
  list.default = RECODE.DEFAULTS,
  list.composites = get_variable_codes('LIST.COMPOSITES'),
  parallel = TRUE, num_cores = num_cores,
  wgt = "ANNUAL_WEIGHT_C1",
  inject.recode.fx = recode.fx
)

# (a) ordered proportions of loved by God AMONG those who believe in God
fit.love <- gfs_svyglm_mi(
  fx = as.formula("LOVED_BY_GOD_Y1 ~ 1"),
  data.dir = paste0(data.dir, "/recoded"),
  domain.subset = quote(ORIG_LOVED_BY_GOD_Y1 != 3 & ORIG_BELIEVE_GOD_Y1 %in% c(1,2,3)),
  wgt = ANNUAL_WEIGHT_C1,
  psu = PSU,
  strata = STRATA,
  force.linear = TRUE,
  res.dir = "punish",
  appnd.txt.to.filename = "_belief_subset"
)


# (b) ordered proportions of punished by god AMONG those to believe in God
fit.punish <- gfs_svyglm_mi(
  fx = as.formula("GOD_PUNISH_Y1 ~ 1"),
  data.dir = paste0(data.dir, "/recoded"),
  domain.subset = quote(ORIG_GOD_PUNISH_Y1 != 3 & ORIG_BELIEVE_GOD_Y1 %in% c(1,2,3)),
  wgt = ANNUAL_WEIGHT_C1,
  psu = PSU,
  strata = STRATA,
  force.linear = TRUE,
  res.dir = "punish",
  appnd.txt.to.filename = "_belief_subset"
)

# (c) meta-analytic mean of love

fit.love.meta <- fit.love |>
  mutate(
    df = df.approx
  ) |>
  filter(COUNTRY != "China") |>
  group_by(term) |>
  nest() |>
  gfs_meta_analysis(yi = estimate.pooled, sei = se.pooled, dfi = df, grpi = COUNTRY)


# (d) meta-analytic mean of punish

fit.punish.meta <- fit.punish |>
  mutate(
    df = df.approx
  ) |>
  filter(COUNTRY != "China") |>
  group_by(term) |>
  nest() |>
  gfs_meta_analysis(yi = estimate.pooled, sei = se.pooled, dfi = df, grpi = COUNTRY)

# ---- Tables ----

# Love + Punish meta table
ft.meta <- rbind(
  fit.love.meta |> ungroup() |>
    select(theta.rma,theta.rma.se,theta.lb,theta.ub,theta.pred.int.lb,theta.pred.int.ub,tau,I2)|>
    mutate(Outcome = "Loved by God", .before=1)  ,
  fit.punish.meta |>ungroup() |>
    select(theta.rma,theta.rma.se,theta.lb,theta.ub,theta.pred.int.lb,theta.pred.int.ub,tau,I2)|>
    mutate(Outcome = "Punished by God", .before=1)
) |>
  mutate(across(where(is.numeric), ~.round(., allow.sci = FALSE, digits=3))) |>
  rename(
    Proportion = "theta.rma",
    SE = "theta.rma.se",
    `95% CI LB` = "theta.lb",
    `95% CI UB` = "theta.ub",
    `PredInt LB` = "theta.pred.int.lb",
    `PredInt UB` = "theta.pred.int.ub",
    `Heterogeneity (tau)` = "tau"
  ) |>
  flextable() |>
  add_header_lines(
    "Overall random-effects meta-analytic proportions of 'Love' and 'feel punished by God' among those responded beleiving in one god, many gods, or the spiritual (excludes those who responded 'Not relevant)."
  )

ft.love.ord.prop <- fit.love.meta |>
  ungroup() |>
  select(data) |>
  unnest(c(data)) |>
  select(COUNTRY, estimate.pooled, se.pooled, df.approx, ci.low, ci.up) |>
  arrange(-estimate.pooled) |>
  mutate(across(where(is.numeric), ~.round(., allow.sci = FALSE, digits=3))) |>
  rename(
    Country = "COUNTRY",
    Proportion = "estimate.pooled",
    SE = "se.pooled",
    df = "df.approx",
    `95% CI LB` = ci.low,
    `95% CI UB` = ci.up
  ) |>
  flextable() |>
  add_header_lines(
    "Estimated proportion of population within each country that 'feel loved by God' among those responded beleiving in one god, many gods, or the spiritual (excludes those who responded 'Not relevant)."
  )
ft.punish.ord.prop <- fit.punish.meta |>
  ungroup() |>
  select(data) |>
  unnest(c(data)) |>
  select(COUNTRY, estimate.pooled, se.pooled, df.approx, ci.low, ci.up) |>
  arrange(-estimate.pooled) |>
  mutate(across(where(is.numeric), ~.round(., allow.sci = FALSE, digits=3))) |>
  rename(
    Country = "COUNTRY",
    Proportion = "estimate.pooled",
    SE = "se.pooled",
    df = "df.approx",
    `95% CI LB` = ci.low,
    `95% CI UB` = ci.up
  ) |>
  flextable() |>
  add_header_lines(
    "Estimated proportion of population within each country that 'feel punished by God' among those responded beleiving in one god, many gods, or the spiritual (excludes those who responded 'Not relevant)."
  )

read_docx() |>
  body_add_flextable(ft.meta) |>
  body_add_break() |>
  body_add_flextable(ft.love.ord.prop ) |>
  body_add_break() |>
  body_add_flextable(ft.punish.ord.prop ) |>
  print(target = here::here("punish", "supp_tbs.docx"))

