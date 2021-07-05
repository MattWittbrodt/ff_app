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

# %% Clean up - set index as game_id and remove reformatted week
d.set_index('game_id', inplace=True)
d.drop('reformat_week', axis = 1, inplace=True)

# %% Now, add vegas stuff
v = pd.read_excel('C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to17_combined.xlsx')
v = v[['proj_week', 'proj_tm','proj_opp','line','total']]
v.drop_duplicates(inplace = True)

# %% Merging the two
# In this case, if the line is negative, the home team is favored.
c = pd.merge(d,v, how = "left", 
            left_on = ['home_team','week'], 
            right_on = ['proj_tm','proj_week']).set_index(d.index)
c.drop(['proj_week','proj_tm','proj_opp'], axis = 1, inplace = True)

# %% Calculating the home and away implied total
# Implied total = (Total / 2) +/- (spread/2)
def implied_calc(total,line,away):
    # for away teams
    if away == 1 and line < 0:
        corrected_line = line
    elif away == 1 and line > 0:
        corrected_line = line
    # for home team
    elif away == 0 and line < 0:
        corrected_line = line * -1
    else:
        corrected_line = line*-1
           
    return (total/2) + (corrected_line/2) 

c['home_implied'] = c.apply(lambda row: implied_calc(row.total, row.line, 0), axis = 1)
c['away_implied'] = c.apply(lambda row: implied_calc(row.total, row.line, 1), axis = 1)

# %% Write out
c.to_csv('C:/Users/mattw/Desktop/2020_games.csv', index=True, header=True)

# %% Needing to make some small changes to week 1 and odd game days
#xx = pd.read_csv('C:/Users/mattw/Desktop/2020_games.csv')
#xx['home_implied'] = xx.apply(lambda row: implied_calc(row.total, row.line, 0), axis = 1)
#xx['away_implied'] = xx.apply(lambda row: implied_calc(row.total, row.line, 1), axis = 1)
xx.to_csv('C:/Users/mattw/Desktop/2020_games.csv', index=True, header=True)

# %%
