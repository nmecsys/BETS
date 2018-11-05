import requests
import sys
from bs4 import BeautifulSoup
import pandas as pd

data = requests.get('https://www3.bcb.gov.br/expectativas/publico/ajuda/glossario')
soup = BeautifulSoup(data.text, 'html.parser')

expectativas = soup.find_all('table')[1]
tbody = expectativas.find('tbody')
df_final = pd.DataFrame(columns = ['termo','definicao'])

for tr in tbody.find_all('tr'):
    termo = tr.find_all('td')[0].text.strip()
    definicao = tr.find_all('td')[1].text.strip()
    aux = [termo, definicao]
    df_final = df_final.append({'termo': termo, 'definicao': definicao}, ignore_index=True)

df_final.to_csv('expectativas_info.csv',sep = ';', index = False)
print(df_final.head())
