pyrs = wk %>%
  distinct(nflId, displayName, position)

vnt = wk %>%
  filter(event!= 'None') %>%
  distinct(gameId, playId, frameId, event)

cmp = play %>%
  select(gameId, playId, passResult, epa, offensePlayResult)
