library(tidyverse)

#4.1

teams <- Lahman::Teams %>%
  select(yearID, lgID, teamID, W, L, R, RA) %>%
  mutate(RD = R - RA, 
         winPct = W / (W + L))

run_diff_preds <- function(start_year, end_year) {
  dat <- filter(teams, yearID >= start_year & yearID <= end_year)
  
  lm(winPct ~ RD, data = dat)
}

run_diff_preds(1961, 1970)
run_diff_preds(1971, 1980)
run_diff_preds(1981, 1990)
run_diff_preds(1991, 2000)
run_diff_preds(2001, 2010)

.000714 * 50
.0006216 * 50
# In more recent years an increase in RD has slightly less effect on
# winning percentage. A fifty run RD used to increase winning percentage
# from .500 to .536 in the eighties, but it only increased it to .531
# in the two-thousands. That .005 difference is the difference between
# an 86 and an 87 win season.



#4.2

nineteenth <- teams %>%
  filter(yearID < 1900) %>%
  mutate(pytWPct = R^2 / (R^2 + RA^2),
         residuals = winPct - pytWPct)

mod <- lm(pytWPct ~ RD, data = nineteenth)

ggplot(nineteenth,  aes(RD, residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 3)

# Generally the residuals distribution seems to be relatively normal
# here, not highlighting any trends among really good and poor teams
# in the nineteenth century. The only notable points are the two furthest
# on the right and two furthest on the left. The best teams outperformed
# and worst teams under-performed their expected Pythagorean win percentage
# by about 0.025 to 0.05 percentage points, which corresponds to 5-10 games


# 4.3
library(retrosheet)
library(tidyverse)

game_dat <- rbind(getRetrosheet("game", 2010),
                  getRetrosheet("game", 2011),
                  getRetrosheet("game", 2012),
                  getRetrosheet("game", 2013),
                  getRetrosheet("game", 2014),
                  getRetrosheet("game", 2015),
                  getRetrosheet("game", 2016),
                  getRetrosheet("game", 2017),
                  getRetrosheet("game", 2018),
                  getRetrosheet("game", 2019))

head(game_dat)
# game_dat %>%
#   select(VisRuns, HmRuns, VisMgrNm, HmMgrNm) %>%
#   mutate(RD = VisRuns - HmRuns,
#          winner = ifelse(RD > 0, VisMgrNm, HmMgrNm),
#          loser = ifelse(RD > 0, HmMgrNm, VisMgrNm))
#   group_by(winner, loser) %>%
#   summarize(MgrWins = n(),
#             MgrLosses = n()))

mgrs_by_season <- game_dat %>%
  distinct(Manager = HmMgrNm, HmTm, yearID = substr(Date, 1, 4)) %>%
  mutate(teamID = ifelse(HmTm == "ANA", "LAA", as.character(HmTm)),
         yearID = as.integer(yearID))

mgrs_by_season
season_dat

season_dat <- Lahman::Teams %>%
  select(yearID, teamID, W, L, R, RA) %>%
  filter(yearID > 2009 & yearID < 2020) %>%
  mutate(RD = R - RA,
         wPct = W / (W + L),
         pytWPct = R^2 / (R^2 + RA^2),
         residuals = wPct - pytWPct)

mgrs_dat <- left_join(mgrs_by_season, season_dat) %>%
  select(yearID, teamID, Manager, RD, wPct, pytWPct, residuals) %>%
  group_by(Manager) %>%
  summarize(resids = sum(residuals), RD = sum(RD))
# high residuals means outperformed the Pythagorean Winning Percentage

#install.packages("ggrepel")
highlight_mgrs <- mgrs_dat %>%
  arrange(desc(abs(resids))) %>%
  head(12)
highlight_mgrs

crcblue <- "#2905a1"

ggplot(mgrs_dat, aes(RD, resids)) +
  geom_point(alpha = 0.3) +
  geom_point(data = highlight_mgrs, color = crcblue) +
  geom_hline(yintercept = 0, linetype = 3) +
  ggrepel::geom_text_repel(highlight_mgrs, color = crcblue,
                           mapping = aes(label = Manager))

left_join(mgrs_by_season, season_dat) %>%
  select(yearID, teamID, Manager, RD, wPct, pytWPct, residuals) %>%
  filter(Manager %in% highlight_mgrs$Manager) %>%
  select(teamID, Manager) %>%
  distinct(teamID, Manager) %>%
  arrange(Manager)





# 5.1