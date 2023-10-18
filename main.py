# Import packages
import pandas as pd
# from pandas_profiling import ProfileReport
from shapely.geometry import Point
from shapely.geometry import shape
from shapely.geometry import MultiPolygon

# Import data
data_mediciones = pd.read_csv("acumar_mediciones.csv")

limites_subcuencas = pd.read_csv("LIMITES_SUBCUENCAS_.csv")

# data_mediciones["Latitud"] = data_mediciones["Latitud"].str.replace(",",".")
# data_mediciones["Longitud"] = data_mediciones["Latitud"].replace(",",".")
# data_mediciones["Latitud"] = data_mediciones["Latitud"].astype(float)
# data_mediciones["Longitud"] = data_mediciones["Latitud"].astype(float)
# data_mediciones.to_csv("acumar_mediciones.csv",index=False)
for medicion in data_mediciones:
    print(medicion[3])




for medicion in data_mediciones:
    point = Point(medicion["Longitud"],medicion["Latitud"])
    for subcuenca in limites_subcuencas:
        polygon = shape(MultiPolygon.from_wkt(subcuenca[1]))
        if point.within(polygon):
            medicion["subcuenca"] = subcuenca[2]
