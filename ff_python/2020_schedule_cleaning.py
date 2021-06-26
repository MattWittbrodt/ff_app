#%%
import pandas as pd

#%%
d = pd.read_csv("C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/nfl_schedule.csv").drop('Date', axis = 1)
d = d.rename(columns={"Unnamed: 3":"location","Winner/tie":"winner", "Loser/tie":"loser","Week":"week"})

# Getting home and away teams using the @
d['home_team'] = d.apply(lambda row: row.winner if row.location != "@" else row.loser, axis = 1)
d['away_team'] = d.apply(lambda row: row.winner if row.location == "@" else row.loser, axis = 1)
d = d.drop(['location', 'winner', 'loser'], axis = 1)

#%% Loading teams sheet to use 3 letter abbreviations for teams vs whole name for game_id
teams = pd.read_csv("C:/Users/mattw/Documents/ff_shiny_app/ff_app/sql/team_mysql.csv")
teams = teams[['full_name', 'pfr_abbreviation']].set_index('full_name')

# Gets output like:
# teams['Arizona Cardinals'] returns {'pfr_abbreviation': 'ARI'}
teams = teams.to_dict('index')

#%% Switch out the names for abbreviations
d['home_team'] = d.apply(lambda row: teams[row.home_team]['pfr_abbreviation'], axis = 1)
d['away_team'] = d.apply(lambda row: teams[row.away_team]['pfr_abbreviation'], axis = 1)
d['season'] = 2020
d['reformat_week'] = d.apply(lambda row: f'0{row.week}' if row.week < 10 else f'{row.week}', axis = 1)

# Greating game id similar to pro football ref: 202009130jax {year}{date}{win}...{year}{week}{home}{away}
d['game_id'] = d.apply(lambda row: f'{row.season}{row.reformat_week}{row.home_team.lower()}{row.away_team.lower()}', axis = 1)

# %%
