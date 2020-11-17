
#%%
from selenium import webdriver
import pandas as pd
from bs4 import BeautifulSoup

# %% Getting 4for4 Login
driver = webdriver.Chrome()
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
names = col_names.find_all('th')
n = [ele.get_text() for ele in names]

# %% Make into a dataframe
data = pd.DataFrame(data)

# %%
