# Import packages
import pandas as pd
from shapely.geometry import Point
from shapely.geometry import shape
from shapely.geometry import MultiPolygon

# Functions
def parse_ia(ia_value):
    ia_value = ia_value.lstrip("<>").replace(',', '.')
    if '-' in ia_value:
        ia_value_range = map(float, ia_value.split('-'))
        return tuple(ia_value_range)
    return float(ia_value)
    
def check_condition(row, tipo_uso):
    medida = row['Medida']
    valor = row['Valor (=)']
    for _, ia in data_limites_uso.iterrows():
        if medida == ia['Parámetro'] and 'Aguas' in row['Análisis']:
            ia_value = parse_ia(ia[tipo_uso]) # Porque aca directamente no usamos valor? te ahorras que use toda la funcion total la columna valor ya la limpiamos
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
data_limites_uso = pd.read_csv("limites_admisibles_uso.csv")
limites_subcuencas = pd.read_csv("LIMITES_SUBCUENCAS_.csv")

# Apply mapping
data_mediciones['Medida'] = data_mediciones['Medida'].apply(standardize_measurement)
data_mediciones['Cumple Limites Ia'] = data_mediciones.apply(lambda row: check_condition(row, "I a"), axis=1)
data_mediciones['Cumple Limites Ib'] = data_mediciones.apply(lambda row: check_condition(row, "I b"), axis=1)
data_mediciones['Cumple Limites II'] = data_mediciones.apply(lambda row: check_condition(row, "II"), axis=1)
data_mediciones['Cumple Limites III'] = data_mediciones.apply(lambda row: check_condition(row, "III"), axis=1)
data_mediciones['Cumple Limites IV'] = data_mediciones.apply(lambda row: check_condition(row, "IV"), axis=1)

# Save to CSV
data_mediciones.to_csv("data_mediciones_con_dummies.csv", index = False)
