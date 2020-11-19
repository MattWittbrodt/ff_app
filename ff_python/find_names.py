#@param x = column of data with random team names
#@param col = what column is the data coming in as

def find_names(x,col):

#%% Importing Packages
import pandas as pd
import numpy as np

#%% Loading names of teams into a Dataframe

teams = pd.DataFrame({
'full_name' : ["Arizona Cardinals", "Atlanta Falcons","Baltimore Ravens", "Buffalo Bills",
              "Carolina Panthers", "Chicago Bears", "Cincinnati Bengals", "Cleveland Browns", 
              "Dallas Cowboys", "Denver Broncos", "Detroit Lions", "Green Bay Packers", 
              "Houston Texans", "Indianapolis Colts", "Jacksonville Jaguars", "Kansas City Chiefs",
              "Los Angeles Chargers", "Los Angeles Rams", "Miami Dolphins", "Minnesota Vikings", 
              "New England Patriots", "New Orleans Saints", "New York Giants", "New York Jets", "Oakland Raiders",
              "Philadelphia Eagles", "Pittsburgh Steelers", "San Francisco 49ers", "Seattle Seahawks",
              "Tampa Bay Buccaneers", "Tennessee Titans", "Washington Football Team", "Las Vegas Raiders"],
'pfr_abbreviation' : ["ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN",
                    "DET", "GNB", "HOU", "IND", "JAX", "KAN", "LAC", "LAR", "MIA","MIN", "NWE", "NOR", "NYG", "NYJ",
                    "LVR", "PHI", "PIT", "SFO","SEA", "TAM", "TEN", "WAS", "LVR"],
'fff_abbreviation' : ["ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN",
                    "DET", "GB", "HOU", "IND", "JAX", "KC", "LAC", "LAR", "MIA","MIN",
                    "NE", "NO", "NYG", "NYJ", "xxx", "PHI", "PIT", "SF", "SEA","TB", "TEN", "WAS", "LV"],
'vegas' : ["Arizona", "Atlanta", "Baltimore","Buffalo", "Carolina", "Chicago", "Cincinnati", "Cleveland",
         "Dallas", "Denver", "Detroit", "Green_Bay", "Houston", "Indianapolis", "Jacksonville", 
         "Kansas", "LA_Chargers", "LA_Rams", "Miami", "Minnesota", "New_England", "New_Orleans", 
         "NY_Giants", "NY_Jets", "Oakland", "Philadelphia", "Pittsburgh", "San_Francisco", "Seattle",
         "Tampa_Bay", "Tennessee", "Washington", "Las_Vegas"]
         })

# %%
x = pd.Series(['ARI', 'ATL', 'BAL', 'BUF', 'CAR', 'CHI', 'CIN', 'CLE', 'DAL',
       'DEN', 'DET', 'GB', 'HOU', 'IND', 'JAX', 'KC', 'LAC', 'LAR', 'LV',
       'MIA', 'MIN', 'NE', 'NO', 'NYG', 'NYJ', 'PHI', 'PIT', 'SEA', 'SF',
       'TB', 'TEN', 'WAS'])

# %%
for t in range(len(x)):
    # Gets team name
    team = x[t]
    # Converts to pfr abbreviation
    team2 = teams[teams[col] == team]
    team_n = team2['pfr_abbreviation'].to_string(index = False).strip()
    x[t] = team_n

return(x)
