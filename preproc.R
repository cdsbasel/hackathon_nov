require(tidyverse)

a = readRDS('~/Dropbox (2.0)/Work/Projects/DfE/-- MetaAna/1 Data/_master/des.RDS')

d = a %>% 
  filter(probA1 + probA2 == 1,
         probB1 + probB2 == 1,
         !is.na(risky)) %>%
  mutate(risky_choice = as.numeric(choice == risky),
         study = paste(short, sub, sep = '_'),
         first_author = str_sub(authors, 1, 3),
         ev_risky = ifelse(risky == 0, ev0, ev1),
         ev_safe = ifelse(risky == 0, ev1, ev0),
         ev_diff = ev_risky - ev_safe,
         var_0 = (probA1 * outA1 ** 2 + probA2 * outA2 ** 2) - (probA1 * outA1 + probA2 * outA2)**2,
         var_1 = (probB1 * outB1 ** 2 + probB2 * outB2 ** 2) - (probB1 * outB1 + probB2 * outB2)**2,
         cv_risky = ifelse(risky == 0, var_0, var_1) / abs(ev_risky),
         cv_safe = ifelse(risky == 0, var_1, var_0) / abs(ev_safe),
         cv_diff = cv_risky - cv_safe,
         out_risky_1 = ifelse(risky == 0, outA1, outB1),
         out_risky_2 = ifelse(risky == 0, outA2, outB2),
         prob_risky_1 = ifelse(risky == 0, probA1, probB1),
         prob_risky_2 = ifelse(risky == 0, probA2, probB2),
         out_safe_1 = ifelse(risky == 1, outA1, outB1),
         out_safe_2 = ifelse(risky == 1, outA2, outB2),
         prob_safe_1 = ifelse(risky == 1, probA1, probB1),
         prob_safe_2 = ifelse(risky == 1, probA2, probB2)
         ) %>%
  select(study, subject, pid, risky_choice, 
         dom, cert, 
         ev_risky, ev_safe, ev_diff, cv_risky, cv_safe, cv_diff,  
         first_author,
         out_risky_1, out_risky_2, prob_risky_1, prob_risky_2,
         out_safe_1, out_safe_2, prob_safe_1, prob_safe_2) %>%
  rename(problem = pid) %>%
  mutate(problem = str_replace_all(problem, '[:punct:]', '')) %>%
  filter(!is.na(cv_diff)) 

d_split = split(d, list(d$study)) 
d_split_sel = lapply(d_split, function(x) {
  subs = unique(x$subject)
  subs_sel = sample(subs, min(length(subs),20))
  probs = unique(x$problem)
  probs_sel = sample(probs, min(length(probs),20))
  x = x[x$subject %in% subs_sel & x$problem %in% probs_sel, ]; x[order(x$subject), ]
  })
d_reduced = do.call(rbind, d_split_sel)

d_reduced %>% group_by(study) %>% summarize(length(unique(subject)))
d_reduced %>% group_by(study) %>% summarize(length(unique(problem))) %>% print(n = 100)

st = d_reduced %>% group_by(study) %>% summarize(enough = n() >= 100) %>% filter(enough) %>% pull(study)
d_reduced = d_reduced %>% filter(study %in% st)

write_csv(as_tibble(d_reduced),'hackathon_nov/meta.csv')



require(lme4)

m = glmer(risky_choice ~ ev_diff + (1 + ev_diff|study:subject) + (1|problem), family = 'binomial', data = d_reduced %>% mutate(ev_diff = scale(ev_diff)), control = glmerControl(optimizer = 'bobyqa'))

summary(m)


plot(d$ev_diff, d$cv_diff)

mean(d$hertwig)

cor.test(as.numeric(d$hertwig), d$risky_choice)
