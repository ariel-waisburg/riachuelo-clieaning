# Import packages
import pandas as pd
from shapely.geometry import Point
from shapely.wkt import loads
import geopandas as gpd

# Functions
def parse_ia(ia_value):
    ia_value = ia_value.lstrip("<>").replace(',', '.')
    if '-' in ia_value:
        ia_value_range = map(float, ia_value.split('-'))
        return tuple(ia_value_range)
    return float(ia_value)
    
def check_dummie_condition(row, tipo_uso):
    medida = row['Medida']
    valor = row['Valor (=)']
    for _, ia in data_limites_uso.iterrows():
        if medida == ia['Parámetro'] and 'Aguas' in row['Análisis']:
            ia_value = parse_ia(ia[tipo_uso])
            if isinstance(ia_value, tuple):
                ia_min, ia_max = ia_value
                return ia_min <= valor <= ia_max
            return (valor > ia_value) if tipo_uso.startswith('>') else (valor < ia_value)
    return None

def standardize_measurement(medida):
    standardized_measurements = {
        "Nitrógeno de Amoníaco (N-NH3)": "Nitrógeno Amoniacal",
        "Clorofila A": "Clorofilia a",
        "Demanda Biológica de Oxígeno (DBO5)": "DBO5",
        "Detergentes (SAAM)": "Detergentes (S.A.A.M.)",
        "Índice de Estado Trófico- TSI- (Fósforo Total) (TSI Fósforo Total)": "Fósforo Total",
        "Nitrato (NO3-)": "Nitrógeno de Nitratos", # Revisar
        "Oxigeno disuelto": "OD",
        "Temperatura (T)": "Temperatura",
        "Arsenico (As)": "Arsénico total", # Revisar
        "Cadmio": "Cadmio total", # Revisar
        "Cadmio (Cd)": "Cadmio total", # Revisar
        "Zinc (Zn)": "Cinc total", # Zinc tiene distinta unidad es ml/kg y zinc (zn) es mg/l
        "Zinc": "Cinc total", # Zinc tiene distinta unidad es ml/kg y zinc (zn) es mg/l
        "Cianuros (CN)": "Cianuro total",
        "Cobre (Cu)": "Cobre total", # Diferentes unidades
        "Cobre": "Cobre total", # Diferentes unidades
        "Cromo (Cr)": "Cromo total", # Diferentes unidades
        "Cromo": "Cromo total", # Diferentes unidades
        "Mercurio": "Mercurio total",
        "Mercurio (Hg)": "Mercurio total",
        "Niquel (Ni)": "Níquel total",
        "Niquel": "Níquel total",
        "Plomo (Pb)": "Plomo total",
        "Plomo": "Plomo total",
        "Clordano Técnico": "Clordano",
        "Dieldrin": "Dieldrín",
        "Endosulfán total": "Endosulfán",
        "Endrin": "Endrín",
        "Hexaclorobenceno": "Hexacloro benceno",
    }
    return standardized_measurements.get(medida, medida)
    
def geomapping(point, gdf, column_geom, col_name):
    for idx, valor in gdf.iterrows():
        if point.within(valor[column_geom]):
            return valor[col_name]
    return None

# Sin mediciones:
    # Cromo hexavalente
    # Aldrín  
    # DDT (Total Isómeros)
    # Heptacloro
    # Heptacloro epóxido
    # Metoxicloro
    # Paration
    # Malation
    # 2,4 D

# Import data
data_mediciones = pd.read_csv("acumar_mediciones.csv")
data_mediciones['Valor (=)'] = data_mediciones['Valor (=)'].str.replace('"', '',regex=True).replace(',', '.',regex=True).astype(float)
data_mediciones['Fecha'] = pd.to_datetime(data_mediciones['Fecha'], format = "%d/%m/%Y")
data_mediciones['Longitud'] = data_mediciones['Longitud'].str.replace(',', '.').astype(float)
data_mediciones['Latitud'] = data_mediciones['Latitud'].str.replace(',', '.').astype(float)
data_limites_uso = pd.read_csv("limites_admisibles_uso.csv")

# Load data
data_limites_subcuencas = gpd.read_file('LIMITES_SUBCUENCAS_.csv')
data_limites_cuencas = gpd.read_file('TRAMOS_DE_CUENCA.csv')

# Apply mapping
data_mediciones['Medida'] = data_mediciones['Medida'].apply(standardize_measurement)
data_mediciones['Cumple Limites Ia'] = data_mediciones.apply(lambda row: check_dummie_condition(row, "I a"), axis=1)
data_mediciones['Cumple Limites Ib'] = data_mediciones.apply(lambda row: check_dummie_condition(row, "I b"), axis=1)
data_mediciones['Cumple Limites II'] = data_mediciones.apply(lambda row: check_dummie_condition(row, "II"), axis=1)
data_mediciones['Cumple Limites III'] = data_mediciones.apply(lambda row: check_dummie_condition(row, "III"), axis=1)
data_mediciones['Cumple Limites IV'] = data_mediciones.apply(lambda row: check_dummie_condition(row, "IV"), axis=1)

# Convert geometries
data_limites_subcuencas['geometry_subcuencas'] = data_limites_subcuencas['wkb_geometry'].apply(lambda x: loads(x))
data_limites_cuencas['geometry_cuencas'] = data_limites_cuencas['wkb_geometry'].apply(lambda x: loads(x))

# Create GeoDataFrames
data_mediciones['geometry'] = data_mediciones.apply(lambda row: Point(row['Longitud'], row['Latitud']), axis=1)
data_mediciones = gpd.GeoDataFrame(data_mediciones, geometry='geometry')
geo_df_subcuencas = gpd.GeoDataFrame(data_limites_subcuencas, geometry='geometry_subcuencas')
geo_df_cuencas = gpd.GeoDataFrame(data_limites_cuencas, geometry='geometry_cuencas')

data_mediciones['Subcuenca'] = data_mediciones['geometry'].apply(lambda x: geomapping(x, geo_df_subcuencas, 'geometry_subcuencas', 'ID'))
data_mediciones['Cuenca'] = data_mediciones['geometry'].apply(lambda x: geomapping(x, geo_df_cuencas, 'geometry_cuencas', 'TRAMO'))

# Save to CSV
data_mediciones.to_csv("data_mediciones_geolocaliazadas_dummies.csv", index = False)