import pandas as pd
import requests
import json
import io

import pandas as pd

count = 0
for year in range(1950, 2021):
    url = f"http://ergast.com/api/f1/{year}/circuits.json"

    payload={}
    headers = {}

    response = requests.request("GET", url, headers=headers, data=payload)
    data = response.json()["MRData"]["CircuitTable"]["Circuits"]

    data_dict = {}
    for d in data:
        data_dict[d['Location']['country']] = [year, d['Location']['locality'], d['Location']['long'], d['Location']['lat']]
    new_df = pd.DataFrame.from_dict(data_dict, orient = 'index', columns= ['year', 'locality', 'long', 'lat'])
    if count == 0:
        df = new_df
    else:
        df = pd.concat([df, new_df])
    count += 1
print(df)
df.to_excel('circuits.xlsx')