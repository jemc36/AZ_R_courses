#############################################################################
### Project: Swimmer Plot                                                 ###
### Author : Jem Chang                                                    ###
### Date   : 20OCT2020                                                    ###
### Purpose: Create a swimmer plot for response assessment and ACP dosing ###
#############################################################################

# 1. Install libraries and read datasets
setwd('C:/Users/krht381/OneDrive - AZCollaboration/Documents/training/R Trainee/dev/swimmer')
library(haven)
#library(plyr) # mask some functions from tidyverse, not a good idea to use both of them at the same time.
library(tidyverse)

rs <- read_sas('rs.sas7bdat')
ec <- read_sas('ec.sas7bdat')
ds <- read_sas('ds.sas7bdat')

# 2. Data Pre-processing 
# Get acala dosing
ec_per <- ec %>% 
  filter(ECTRT == 'ACP-196' & ECMOOD == 'PERFORMED') %>%
  mutate(ECENDY = if_else(is.na(ECENDY) & !is.na(ECSTDY), ECSTDY, ECENDY ),
         SUBJID = substr(USUBJID, 12, 21),
         ECDOSE = if_else(is.na(ECDOSE), 0 , ECDOSE),
         ECTPT  = if_else(ECDOSFRQ == 'QD' & ECTPT == '', 'AM', ECTPT)) %>%
  filter(!is.na(ECSTDY) & !is.na(ECENDY)) %>%
  select(SUBJID, ECDOSE, ECSTDY, ECENDY, ECTPT) %>%
  pivot_wider(
    names_from = ECTPT,
    values_from = ECDOSE
  ) %>% mutate( AM = if_else(is.na(AM), 0, AM),
                PM = if_else(is.na(PM), 0, PM),
                SUMDOSE = AM + PM,
                DOSE = paste(SUMDOSE, 'mg')) 

# Create sorting order based on length of ECENDY
sort_id <- ec_per %>%
  group_by(SUBJID) %>%
  summarise(max_dy = max(ECENDY)) %>%
  arrange(desc(max_dy)) %>%
  mutate(sortid = row_number()) 

ec_per <- ec_per %>%
  left_join(sort_id, by='SUBJID') %>%
  mutate(SUBJID = fct_reorder(SUBJID, desc(sortid))) 

# Get longest day from RS and EC
maxdy <- sort_id %>%
  filter(sortid == 1) %>%
  select(max_dy) %>%
  mutate(max_dy = max(max_dy, max(rs$RSDY))) %>%
  as.numeric() 

# Get response assessment: in order to overlap plots, rename variables to match ec_per
rs2 <- rs %>%
  mutate(SUBJID = substr(USUBJID, 12, 21)) %>%
  select(SUBJID, RSLNKGRP, RSSTRESC, RSDY) %>%
  mutate(ECSTDY = RSDY,
         ECENDY = RSDY,
         DOSE = factor(RSSTRESC, levels = c('CR', 'PR', 'SD', 'PD'))) %>%
  left_join(sort_id, by='SUBJID') %>%
  mutate(SUBJID = fct_reorder(SUBJID, desc(sortid))) 

  
#ec_rs <- rbind.fill(ec_per, rs2) %>%
#  left_join(sort_id, by = 'SUBJID') %>%
#  arrange(sortid)

# 3. Plotting
#p <- ggplot(ec_per, aes(x = ECSTDY, xend = ECENDY, y = SUBJID, yend=SUBJID, col = dose)) +
#  geom_segment(size = 4) +
#  scale_color_manual(values = c('pink', 'lightgreen', 'seagreen3')) +
#  xlab('Study Day') + 
#  ylab('Subject ID') +
#  labs(color = 'acala dose (mg)') +
#  scale_x_continuous(breaks = seq(1, maxdy, 28))
#p


p2 <- ggplot(ec_per, aes(x = ECSTDY, xend = ECENDY, y = SUBJID, yend=SUBJID, col = DOSE)) + 
  geom_segment(size = 4) +
  geom_point(data = rs2, aes(shape = DOSE), size = 4) +
  scale_color_manual(values = c('pink', 'lightgreen', 'seagreen3', 'blue', 'red', 'purple', 'yellow3')) +
  scale_shape_manual(values = c(19, 18, 15, 17)) +
  xlab('Study Day') + 
  ylab('Subject ID') +
  ggtitle('Swimmter Plot for Response Assessment in Acala Dosing Period') +
  labs(color = '', shape = 'response') +
  scale_x_continuous(breaks = seq(1, maxdy, 28)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

p2 

ggsave('f_ec_rs.png', width = 10, height = 7)
