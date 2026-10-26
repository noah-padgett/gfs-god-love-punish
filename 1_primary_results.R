#' GFS Love by God and Feeling Punished by God
#' Primary analyses excluding the participants responding "Not relevant"
#'
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
dataset.name <- "gfs_all_countries_Y1_Y2_Y3.sav"

# Meta-analyic means by religious group
gc(full=TRUE)

RECODE.DEFAULTS <- list(
  FOCAL_VARIABLE = NULL,
  DEMOGRAPHICS.CHILDHOOD.PRED.VEC = c(
    get_variable_codes("GENDER.RACE", appnd=""),
    get_variable_codes("RETROSPECTIVE.VARS", appnd="_Y1"),
    get_variable_codes("DEMOGRAPHIC.VARS", appnd="_Y1"),
    get_variable_codes("DEMOGRAPHIC.VARS", appnd="_Y2"),
    get_variable_codes("DEMOGRAPHIC.VARS", appnd="_Y3")
  ),
  VARIABLES.VEC = c(get_variable_codes("VARS.Y1"), get_variable_codes("VARS.Y2"), get_variable_codes("VARS.Y3")),
  VALUES_DEFINING_UPPER_CATEGORY = NULL,
  VALUES_DEFINING_LOWER_CATEGORY = NULL,
  FORCE_BINARY = FALSE,
  FORCE_CONTINUOUS = FALSE,
  USE_DEFAULT = TRUE
)

recode.fx <- function(data){
  data |>
    mutate(
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
  wave = 3 ,
  nimp = 20,
  list.default = RECODE.DEFAULTS,
  list.composites = get_variable_codes('LIST.COMPOSITES'),
  parallel = TRUE, num_cores = num_cores,
  wgt = "ANNUAL_WEIGHT_C1",
  inject.recode.fx = recode.fx
)

# Get estimated proportion of those feeling loved by GOD for those for did not respond
fit.love <- gfs_svyglm_mi(
  fx = as.formula("LOVED_BY_GOD_Y1 ~ 1"),
  data.dir = paste0(data.dir, "/recoded"),
  domain.subset = quote(ORIG_LOVED_BY_GOD_Y1 != 3),
  wgt = ANNUAL_WEIGHT_C1,
  psu = PSU,
  strata = STRATA,
  force.linear = TRUE,
  res.dir = "punish"
)

fit.punish <- gfs_svyglm_mi(
  fx = as.formula("GOD_PUNISH_Y1 ~ 1"),
  data.dir = paste0(data.dir, "/recoded"),
  domain.subset = quote(ORIG_GOD_PUNISH_Y1 != 3),
  wgt = ANNUAL_WEIGHT_C1,
  psu = PSU,
  strata = STRATA,
  force.linear = TRUE,
  res.dir = "punish"
)


# random effect meta-analysis of results
fit.love.meta <- fit.love |>
  mutate(
   df = df.approx
  ) |>
  filter(COUNTRY != "China") |>
  group_by(term) |>
  nest() |>
  gfs_meta_analysis(yi = estimate.pooled, sei = se.pooled, dfi = df, grpi = COUNTRY)

fit.punish.meta <- fit.punish |>
  mutate(
    df = df.approx
  ) |>
  filter(COUNTRY != "China") |>
  group_by(term) |>
  nest() |>
  gfs_meta_analysis(yi = estimate.pooled, sei = se.pooled, dfi = df, grpi = COUNTRY)


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
    "Overall random-effects meta-analytic averages among those who responded one of ['Agree', 'Disagree' or 'Unsure'] excluding those who responded 'Not relevant'."
  )

ft.love <- fit.love.meta |>
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
    "Estimated proportion of population within each country that 'feel loved by God' among those who responded one of ['Agree', 'Disagree' or 'Unsure'] excluding those who responded 'Not relevant'."
  )

ft.punish <- fit.punish.meta |>
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
    "Estimated proportion of population within each country that 'feel punished by God' among those who responded one of ['Agree', 'Disagree' or 'Unsure'] excluding those who responded 'Not relevant'."
  )

read_docx() |>
  body_add_flextable(ft.meta) |> body_add_break() |>
  body_add_flextable(ft.love) |> body_add_break() |>
  body_add_flextable(ft.punish) |>
  print(target = here::here("punish", "global_results.docx"))


## Estimates of group-specific proportions categories

demo.vars <- c(paste0("COV_",c("AGE_GRP_Y1", "GENDER", "MARITAL_STATUS_Y1",
                             "EMPLOYMENT_Y1", "ATTEND_SVCS_Y1", "EDUCATION_3_Y1",
                             "BORN_COUNTRY_Y1")), "REL2_GRPS")

fit.love.grps <- map(seq_len(length(demo.vars)),\(i){
  d = demo.vars[i]
  gfs_svyglm_mi(
    fx = as.formula(paste0("LOVED_BY_GOD_Y1 ~ 0 + ", d)),
    data.dir = paste0(data.dir, "/recoded"),
    domain.subset = quote(ORIG_LOVED_BY_GOD_Y1 != 3),
    wgt = ANNUAL_WEIGHT_C1,
    psu = PSU,
    strata = STRATA,
    force.linear = TRUE,
    res.dir = "punish",
    appnd.txt.to.filename = paste0("_", d)
  )
})
fit.punish.grps <- map(seq_len(length(demo.vars)),\(i){
  d = demo.vars[i]
  gfs_svyglm_mi(
    fx = as.formula(paste0("GOD_PUNISH_Y1 ~ 0 + ", d)),
    data.dir = paste0(data.dir, "/recoded"),
    domain.subset = quote(ORIG_GOD_PUNISH_Y1 != 3),
    wgt = ANNUAL_WEIGHT_C1,
    psu = PSU,
    strata = STRATA,
    force.linear = TRUE,
    res.dir = "punish",
    appnd.txt.to.filename = paste0("_", d)
  )
})


# need to also calculate group sizes:
df.raw <- gfs_get_labelled_raw_data(
  file = here::here(data.dir, dataset.name), wave = 3,
  list.composites = get_variable_codes('LIST.COMPOSITES'),
  add.whitespace = TRUE, reverse.code.cont = TRUE
)
df.svy <- df.raw |>
  mutate(
    REL2_GRPS = case_when(
      REL2_Y1 ==  '1. Christianity' ~ "Christianity",
      REL2_Y1 == '2. Islam' ~ "Islam",
      REL2_Y1 == '3. Hinduism' ~ "Hinduism",
      REL2_Y1 == '4. Buddhism' ~ "Buddhism",
      REL2_Y1 == '5. Judaism' ~ "Judaism",
      REL2_Y1 %in% c('12. Primal, Animist, or Folk religion', "13. Spiritism", "14. Umbanda, Candomblé, and other African-derived religions",  "15. Chinese folk/traditional religion") ~ "Folk Religions",
      REL2_Y1 %in% c('6. Sikhism', '7. Baha’i','8. Jainism','9. Shinto','10. Taoism', '11. Confucianism', '96. Some other religion') ~ "Other",
      REL2_Y1 == "97. No religion/Atheist/Agnostic" ~ "None",
      .default = "Missing"
    ),
    REL2_GRPS = as.factor(REL2_GRPS)
  ) |>
  as_survey_design(
    weights = ANNUAL_WEIGHT_C1,
    strata = STRATA,
    ids = PSU
  )

# observed sample sizes
fit.grp.sizes.rel2 <- df.svy |>
  group_by(COUNTRY, REL2_GRPS) |>
  summarise(
    Ng = survey_total()
  )
tb.rel.grp.sizes <- fit.grp.sizes.rel2 |> ungroup() |>
  select(COUNTRY, REL2_GRPS, Ng) |>
  pivot_wider(
    names_from=REL2_GRPS,
    values_from = Ng,
    values_fill = 0.0
  )

# observed sample sizes
fit.grp.sizes.love.subset <- df.svy |>
  filter(REL2_GRPS != "Missing", LOVED_BY_GOD_Y1 %in% c('4. Unsure', '1. Agree', '2. Disagree')) |>
  group_by(COUNTRY, REL2_GRPS) |>
  summarise(
    Ng_love = survey_total()
  )
fit.grp.sizes.love.subset <- map(demo.vars,\(x){
  y <- as.name(str_remove(x,"COV_"))
  df.svy |>
    mutate(grp = {{y}}) |>
    filter(!({{x}} %in% c("Missing", "(Missing)")),
           LOVED_BY_GOD_Y1 %in% c('4. Unsure', '1. Agree', '2. Disagree')) |>
    group_by(COUNTRY, grp) |>
    summarise(
      Ng_love = survey_total()
    ) |>
    mutate(
      Variable = as.character(x),
      type = "love"
    )
})
fit.grp.sizes.punish.subset <- map(demo.vars,\(x){
  y <- as.name(str_remove(x,"COV_"))
  df.svy |>
    mutate(grp = {{y}}) |>
  filter(!({{x}} %in% c("Missing", "(Missing)")),
         GOD_PUNISH_Y1 %in% c('4. Unsure', '1. Agree', '2. Disagree')) |>
  group_by(COUNTRY, grp) |>
  summarise(
    Ng_punish = survey_total()
  ) |>
    mutate(
      Variable = as.character(x),
      type = "punish"
    )
})

fit.grp.sizes.love.subset <- fit.grp.sizes.love.subset |>
  bind_rows() |>
  rowwise() |>
  mutate(
    cat = case_when(
      str_detect(grp, "\\.") ~ str_trim(str_split(grp, "\\.",simplify = TRUE)[2]),
      .default = str_trim(grp)
    ),
    grp.cat = paste0(Variable,cat)
  )

fit.grp.sizes.punish.subset <- fit.grp.sizes.punish.subset |>
  bind_rows() |>
  rowwise() |>
  mutate(
    cat = case_when(
     str_detect(grp, "\\.") ~ str_trim(str_split(grp, "\\.",simplify = TRUE)[2]),
     .default = str_trim(grp)
    ),
    grp.cat = paste0(Variable,cat)
  )


fit.grp.sizes.love.subset |> ungroup() |>
  filter(Variable == "REL2_GRPS") |>
  filter(Ng_love > 9.99, cat != "None") |>
  summarise(
    n=sum(Ng_love)
  )

fit.grp.sizes.punish.subset|> ungroup() |>
  filter(Variable == "REL2_GRPS") |>
  filter(Ng_punish > 9.99, cat != "None") |>
  summarise(
    n=sum(Ng_punish)
  )
# love n = 140,503; punish n=136,186


## boxplots

plot.dat <- full_join(
  fit.love.grps |> bind_rows() |> mutate(type = "love", grp.cat = term) ,
  fit.punish.grps |> bind_rows() |> mutate(type = "punish", grp.cat = term)
  ) |>
  left_join(fit.grp.sizes.love.subset |> select(COUNTRY, type, Variable, grp.cat, cat, Ng_love)
            , by = c("COUNTRY", "type", "grp.cat")) |> #, by = c("term" = "grp.cat", "COUNTRY" = "COUNTRY")
  left_join(fit.grp.sizes.punish.subset |> select(COUNTRY, type, Variable, grp.cat, cat, Ng_punish)
            , by = c("COUNTRY", "type", "grp.cat")) |>
  mutate(
    Ng = ifelse(is.na(Ng_love), Ng_punish, Ng_love),
    Variable = ifelse(is.na(Variable.x), Variable.y, Variable.x),
    cat = ifelse(is.na(cat.x), cat.y, cat.x),
    grp.cat = ifelse(str_detect(grp.cat, "Folk"), "Folk", grp.cat),
    grp.size = case_when(
      Ng < 5 ~ "< 5",
      Ng < 10 & Ng >= 5 ~ "< 10",
      Ng < 100 & Ng >= 10 ~ "< 100",
      Ng >= 100 ~ "> 100"
    ),
    grp.size = factor(grp.size, levels=c("> 100","< 100", "< 10", "< 5"), ordered=TRUE),
    type = case_when(
      type == "love" ~ "Feeling Loved by God",
      type == "punish" ~ "Feeling Punished by God"
    )
  )

p1 <- plot.dat |>
  filter(Variable == "REL2_GRPS") |>
  group_by(cat) |>
  mutate(
    grp.avg = mean(estimate.pooled)
  ) |>
  ggplot(aes(y=reorder(cat, estimate.pooled), x= estimate.pooled))+
    geom_boxplot(outliers = FALSE) +
    geom_jitter(aes(color=grp.size, shape = grp.size),position = position_jitter(height=0.175, width=0, seed=42)) +
    facet_wrap(.~type) +
    scale_color_tableau(name="Group N")+
    scale_shape_manual(name="Group N", values = c(5,17,15,4))+
    labs(y = NULL, x = "Estimate Proportion") +
    theme_Publication()
p1
ggsave(here("punish", "love_punish_god_relgrp.pdf"), p1, units="in", width=7, height=4)


# Define band height and range
y_min <- 0.5
y_max <- 23.5
band_height <- 1  # height of each band

# Create a data frame for the bands
bands <- data.frame(
  ymin = seq(y_min, y_max, by = band_height * 2),
  ymax = seq(y_min + band_height, y_max + band_height, by = band_height * 2)
)

p1 <- plot.dat |>
  filter(Variable == "REL2_GRPS") |>
  filter(Ng > 9.99, cat != "None") |>
  group_by(COUNTRY) |>
  mutate(
    yi = ifelse(str_detect(type, "Loved"), estimate.pooled, NA),
    grp.avg = mean(yi, na.rm=TRUE)
  ) |>
  ggplot(aes(y=reorder(COUNTRY, grp.avg), x= estimate.pooled, group = interaction(type,COUNTRY ), color=type))+
  geom_rect(
    data = bands,
    aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "grey90",
    alpha = 0.90
  ) +
  geom_boxplot(outlier.shape = NA,position = position_dodge(), linewidth=0.33, coef = 500) +
  geom_point(aes(shape = cat),position = position_dodgev(height = 0.5)) +
  #facet_wrap(.~type) +
  scale_color_tableau()+
  scale_shape_manual(values = c(15,16,17,3,8,4,6))+
  labs(y= NULL, x = "Estimate Proportion",
       caption = str_wrap("Excludes country estimates with less than 10 participants self-identifying as being part of that religious tradition or group of traditions.")) +
  theme_Publication() +
  theme(legend.title = element_blank(),
        axis.text.y = element_text(vjust=0.5),
        panel.grid.major = element_blank())
p1
ggsave(here("punish", "love_god_prop_country.pdf"), p1, units="in", width=9, height=9)


plot.dat |>
  filter(Variable == "REL2_GRPS") |>
  filter(Ng > 9.99, cat != "None") |>
  group_by(type) |>
  summarize(
    n = sum(Ng)
  )
