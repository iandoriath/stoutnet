library(tidyverse)

# load big data bowl 2023 raw data
players = read_csv('players.csv')
games = read_csv('games.csv')
pff = read_csv('pffScoutingData.csv')
plays = read_csv('plays.csv')

# create dataframe for play data
weeks = tibble()
for(i in 1:8){
  weeks = weeks %>%
    bind_rows(read_csv(paste0('week', i, '.csv')))
}

# function to rotate plays to the same orientation
angle_swap = function(angle){
  angle = angle + 180
  if(angle >= 360) angle = angle - 360
  return(angle)
}

# exclude players who are not blockers, rushers or passers
weeks = weeks %>% 
  left_join(pff) %>% 
  filter(pff_role %in% c('Pass Block', 'Pass Rush', 'Pass'))

# find snap time and call it f0
weeks = weeks %>% 
  filter(event == 'ball_snap') %>% 
  distinct(gameId, playId, frameId) %>% 
  group_by(gameId, playId) %>% 
  rename(f0 = frameId) %>% 
  right_join(weeks) %>% 
  filter(frameId >= f0)

# find end of play
weeks = weeks %>% 
  filter(event %in% c('pass_forward', 'qb_strip_sack', 'qb_sack', 'fumble', 'out_of_bounds', 'fumble_offense_recovered', 'tackle')) %>% 
  distinct(gameId, playId, frameId, event) %>% 
  group_by(gameId, playId) %>% 
  slice_min(frameId) %>% 
  rename(fEnd = frameId,
         eventEnd = event) %>% 
  right_join(weeks) %>% 
  filter(frameId <= fEnd)

# make all times relative to f0
weeks = weeks %>% 
  mutate(t = frameId-f0)

# make all plays same direction
weeks = weeks %>% filter(playDirection == 'left') %>% 
  mutate(x = 120-x,
         y = 53.3-y,
         o = map_dbl(o, ~angle_swap(.x)),
         dir = map_dbl(dir, ~angle_swap(.x))) %>% 
  bind_rows(weeks %>% filter(playDirection == 'right'))

# to reduce size of CNN, we'll round to hundredths place
weeks = weeks %>%
  mutate(x = (10*round(x, 1)),
         y = (10*round(y, 1)))

# we want to encode as an image. pixels will be colored based on pff player role.
# for simplicity's sake, we never want two players to be in the same pixel.
# we will nudge overlapping players 1 pixel away
duplicate_coords = weeks %>% 
  group_by(gameId, playId, t, x, y) %>% 
  summarize(n=n()) %>% 
  filter(n>1)

weeks = duplicate_coords %>% 
  select(-n) %>% 
  mutate(nudge = TRUE) %>% 
  right_join(weeks) %>%
  replace_na(list(nudge=FALSE)) %>% 
  group_by(gameId, playId, t, x, y) %>% 
  mutate(posn = (1:n())-1) %>% 
  mutate(x = (if_else(nudge & posn==1, if_else(pff_role=='Pass Rush', x+1, x-1), x)))
# that takes care of duplicates

# let's make all x coordinates relative to QB at f0
# TODO: should we change this to relative to Line of Scrimmage?
# Does it matter?
weeks = weeks %>%
  ungroup() %>% 
  filter(pff_role == 'Pass') %>% 
  slice_min(t) %>% 
  rename(xQb = x) %>% 
  select(gameId, playId, xQb) %>% 
  right_join(weeks) %>% 
  mutate(x=x-xQb)
  
# # the result is
# diff(range(weeks$x)) # 326 size-x
# diff(range(weeks$y)) # 546 size-y
# max(weeks$t) # 98 size-t

# align to new origin
weeks = weeks %>% 
  mutate(x = x - min(x), y=y-min(y))

# final output of play data
play_tensor = weeks %>% 
  distinct(gameId, playId, x, y, t, pff_role, nflId, eventEnd) %>% 
  mutate(offense = if_else(pff_role == 'Pass Rush', 0, 1)) %>% 
  mutate(pass_or_def = if_else(pff_role %in% c('Pass', 'Pass Rush'), 1, 0))

play_tensor = play_tensor %>% 
  group_by(gameId, playId) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(uid = 1:n() - 1) %>% 
  unnest(data)

play_tensor %>% 
  select(t, x, y, offense, pass_or_def, uid, nflId) %>% 
  write_csv('playtensor.csv')

# assign unique uid for each play and save labels dataframe
play_maxtime = play_tensor %>% 
  group_by(uid) %>% 
  summarize(maxtime = max(t))

uid = play_tensor %>% 
  distinct(uid, gameId, playId) %>% 
  left_join(play_maxtime)

clean = pff %>% 
  select(gameId, playId, pff_hit, pff_hurry, pff_sack, pff_hitAllowed, pff_hurryAllowed, pff_sackAllowed) %>% 
  pivot_longer(-c(gameId, playId), names_to='event', values_to='count') %>% 
  replace_na(list(count=0)) %>%
  group_by(gameId, playId) %>% 
  summarize(clean=as.integer(sum(count)==0))

uid = uid %>% left_join(clean)

uid %>% write_csv('uid_labels.csv')