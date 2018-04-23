library(tidymeta)
m <- iud_cxca %>% 
 group_by(group) %>% 
 meta_analysis(yi = lnes, 
               sei = selnes, 
               slab = study_name, 
               exponentiate = TRUE) %>% 
  #  add pub_year to results
  left_join(select(iud_cxca, study_name, pub_year), by = c("study" = "study_name"))

library(metafor)
library(broom)
#  tidy a metafor object with broom::tidy()
rma(data = iud_cxca, yi = lnes, sei = selnes, slab = study_name) %>% 
  tidy()

fp <- forest_plot(m, group = group)

library(ggplot2)

fp <- fp + 
  #  compress the x axis
  scale_x_log() +
  #  bold the first line of each axis group
  theme(axis.text.y = element_text(face = c("bold", rep("plain", 21)))) +
  #  use a colorblind friendly color palette
  colorblindr::scale_color_OkabeIto()

library(mbmisc)
txt_tbl <- m %>%
  #  round weights without dropping zeroes
  mutate(weight = round_with_zeros(weight),
  #  paste together estimate and CIs in format OR (Lower-Upper)
         est_ci95 = est_ci(estimate, conf.low, conf.high, descriptor = "")) %>% 
  #  use text_table without y-axis information since that's in the forest plot
  text_table(group = group, "Weights" = weight,
                             "OR (95% CI)" = est_ci95,
             show_y_axis = FALSE,
             show_y_facets = FALSE) 

library(patchwork)
#  combine ggplots with `+`
fp + txt_tbl

m %>% 
  funnel_plot(log_summary = TRUE, col = group, shape = group, size = 3) +
  #  change axis breaks for x axis
  scale_x_log(breaks = c(.25, .64, 1, 2))

inf_plot <- m %>% 
  sensitivity(exponentiate = TRUE) %>% 
  influence_plot()

#  difference between leave-one-out estimate and overall sOR
diff_tbl <- m %>% 
  sensitivity(exponentiate = TRUE) %>% 
  #  calculate the difference and round it
  mutate(diff = estimate - l1o_estimate,
         diff = round_with_zeros(diff)) %>%
  #  filter out the subgroup sORs 
  filter(type == "study" | study == "Overall") %>% 
  text_table("Difference in sOR" = diff, show_y_facets = FALSE, show_y_axis = FALSE)

#  use patchwork to combine
inf_plot + diff_tbl

m %>% 
  #  sort by weight
  arrange(desc(weight)) %>% 
  cumulative(exponentiate = TRUE) %>% 
  cumulative_plot()

m %>% 
  #  sort by publication year
  arrange(pub_year) %>% 
  cumulative(exponentiate = TRUE) %>% 
  cumulative_plot(sum_lines = FALSE)

m %>% 
  arrange(pub_year) %>% 
  cumulative(exponentiate = TRUE) %>% 
  cumulative_plot(sum_lines = FALSE) + 
  #  add vertical line at 1
  geom_vline(xintercept = 1, size = 1.1, color = "#474747") + 
  #  label the x axis
  xlab("Summary Odds Ratio")