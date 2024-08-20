library(tidyverse)
library(gtsummary)

colnames(nlsy)


nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))


# Customization of `tbl_summary()`

tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir))


tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		region_cat ~ "Region",
		eyesight_cat ~ "Eyesight",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	missing_text = "Missing")


tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, glasses, age_bir),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		eyesight_cat ~ "Eyesight",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	missing_text = "Missing") |>
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
	add_overall(col_label = "**Total**") |>
	bold_labels() |>
	modify_footnote(update = everything() ~ NA) |>
	modify_header(label = "**Variable**", p.value = "**P**")

#Include categorical region, race/ethnicity, income, and the sleep variables
tbl_summary(
	nlsy,
	include = c(race_eth_cat, region_cat,
							income, sleep_wkdy, sleep_wknd),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		region_cat ~ "Region",
		income ~ "Income",
		sleep_wkdy ~ "Sleep per Weekday",
		sleep_wknd ~ "Sleep per Weekend"
	),
	missing_text = "Missing")

#Stratify the table by sex. Add a p-value comparing the sexes and an overall column combining both sexes.
tbl_summary(
	nlsy,
	include = c(race_eth_cat, region_cat,
							income, sleep_wkdy, sleep_wknd),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		region_cat ~ "Region",
		income ~ "Income",
		sleep_wkdy ~ "Sleep per Weekday",
		sleep_wknd ~ "Sleep per Weekend"
	),
	missing_text = "Missing") |>
	#adds p-value
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
	#adds total column
	add_overall(col_label = "**Total**") |>
	#bold labels
	bold_labels() |>
	modify_footnote(update = everything() ~ NA) |>
	#changes desired headers, asteriks make it bold
	modify_header(label = "**Variable**", p.value = "**P**")

#For the income variable, show the 10th and 90th percentiles of
#income with 3 digits, and for the sleep variables,
#show the min and the max with 1 digit.

tbl_summary(
	nlsy,
	include = c(race_eth_cat, region_cat,
							income, sleep_wkdy, sleep_wknd),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		region_cat ~ "Region",
		income ~ "Income",
		sleep_wkdy ~ "Sleep per Weekday",
		sleep_wknd ~ "Sleep per Weekend"
	),
	missing_text = "Missing") |>

	statistic = list(
		income ~ "{p10}; {p90}",
		starts_with("sleep") ~ "{min}; {max}"	) |>

	digits = list()


