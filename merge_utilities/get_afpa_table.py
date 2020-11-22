#%% Importing custom functions
import sys
sys.path.insert(0,'C:/Users/mattw/Documents/ff_shiny_app/ff_app')
from ff_python import find_names

#%%
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
import pandas as pd
from bs4 import BeautifulSoup
import re
from sqlalchemy import create_engine

# %% Getting 4for4 Login
chrome_options = Options()
chrome_options.add_argument("--headless")
driver = webdriver.Chrome(options=chrome_options)
driver.get("https://www.4for4.com/user/login")

# %% Logging in to 4for4
login_form = driver.find_element_by_xpath('//form[@id="user-login"]')
username = login_form.find_element_by_id('edit-name')
password = login_form.find_element_by_id('edit-pass')
username.send_keys('witt0608')
password.send_keys('Witt3161')
driver.find_element_by_id('edit-submit').click()

# %% Going to aFPA page
driver.get("https://www.4for4.com/reports/sos/adjusted")

# %% Reading in Table using Beautiful Soup
afpa_pg = BeautifulSoup(driver.page_source)
afpa_table = afpa_pg.find('table')
table_body = afpa_table.find('tbody')
rows = table_body.find_all('tr')

#%% Adding data
data = []
for row in rows:
    cols = row.find_all('td')
    cols = [ele.get_text() for ele in cols]
    data.append([ele for ele in cols if ele])

#%% Getting column names
col_names = afpa_table.find('thead')
names = col_names.find('tr')
n = [ele.get_text() for ele in names]

# Converting all names to lowercase
def clean_names(x):
    d = x.lower()
    d = re.sub("-","_",d)
    d = re.sub("#","rank",d)
    return(d)

n = [clean_names(x) for x in n]

# Smartly naming the columns with rank to be more specific
for ii in range(len(n)):
    if n[ii] == "rank":
        new_name = n[ii+1] + "_" + n[ii]
        n[ii] = new_name

# %% Make into a dataframe
data = pd.DataFrame(data, columns = n)
data = data.drop(['k_rank', 'k','def_rank','def'],axis = 1)

# %% Runing the custom function
data['team'] = find_names.find_names(data['team'], 'fff_abbreviation')
data = data.rename(columns={'team':'team_id'})

#%% Adding 2020 as column
year = 2020
data.insert(loc = 1, 
          column = 'year', 
          value = year) 

# %% Connecting to Database and appending table
engine = create_engine('postgresql://postgres:matt@localhost:5432/fantasy_football')
# First we need to truncate the table
engine.execute('TRUNCATE TABLE afpa')

# Adding data
data.to_sql('afpa', engine, if_exists = 'append', index = False)

#%% CLosing window
driver.quit()

# %%
#  sql = '''CREATE TABLE afpa (
#           team_id CHAR (3) NOT NULL,
#           year NUMERIC (4) NOT NULL,
#           qb_rank INT NOT NULL,
#           qb NUMERIC NOT NULL,
#           rb_rank INT NOT NULL,
#           rb NUMERIC NOT NULL,
#           rb_half_rank INT NOT NULL,
#           rb_half NUMERIC NOT NULL,
#           rb_ppr_rank INT NOT NULL,
#           rb_ppr NUMERIC NOT NULL,
#           wr_rank INT NOT NULL,
#           wr NUMERIC NOT NULL,
#           wr_half_rank INT NOT NULL,
#           wr_half NUMERIC NOT NULL,
#           wr_ppr_rank INT NOT NULL,
#           wr_ppr NUMERIC NOT NULL,
#           te_rank INT NOT NULL,
#           te NUMERIC NOT NULL,
#           te_half_rank INT NOT NULL,
#           te_half NUMERIC NOT NULL,
#           te_ppr_rank INT NOT NULL,
#           te_ppr NUMERIC NOT NULL,
#           off_rank INT NOT NULL,
#           off NUMERIC NOT NULL,
#           off_half_rank INT NOT NULL,
#           off_half NUMERIC NOT NULL,
#           off_ppr_rank INT NOT NULL,
#           off_ppr NUMERIC NOT NULL,
#           PRIMARY KEY (team_id, year)
#           ); '''

# Open the file in write mode and store the content in file_object