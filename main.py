# Import packages
import pandas as pd
# from pandas_profiling import ProfileReport

# Import data
data_mediciones = pd.read_csv("acumar_mediciones.csv")
data_mediciones['Valor (=)'] = data_mediciones['Valor (=)'].str.replace('"', '',regex=True)
data_mediciones['Valor (=)'] = data_mediciones['Valor (=)'].str.replace(',', '.',regex=True)
data_mediciones['Valor (=)'] = data_mediciones['Valor (=)'].astype(float)
data_mediciones['Fecha'] = pd.to_datetime(data_mediciones['Fecha'], format = "%d/%m/%Y")
#data_mediciones['Valor (=)'] = data_mediciones['Valor (=)'].str.replace('["\,]', '', regex = True).astype(float)

#print(data_mediciones)

data_limites_uso = pd.read_csv("limites_admisibles_uso.csv")
#print(data_limites_uso)

# EDA report
# profile = ProfileReport(data_mediciones)
# profile.to_file("eda_mediciones_reporte.html")

# Hay que analizar si una estacion cumple con los limites de uso o no.

def parse_ia(Ia_value):
    if Ia_value.startswith("<") or Ia_value.startswith(">"):
        Ia_value = Ia_value[1:]
        Ia_value = Ia_value.replace(',', '.')
        return float(Ia_value)
    elif "-" in Ia_value:
        Ia_value_range = Ia_value.split('-')
        Ia_value_min = float(Ia_value_range[0].replace(',', '.'))
        Ia_value_max = float(Ia_value_range[1].replace(',', '.'))
        return (Ia_value_min, Ia_value_max)
    else:
        return float(Ia_value.replace(',', '.'))
    
def check_condition(row, tipo_uso):
    medida = row['Medida']
    valor = row['Valor (=)']
    for _, ia in data_limites_uso.iterrows():
        if medida == ia['Parámetro']:
            # Haria un if en esta fila ya esta True algun tipo anterior, que ponga automaticamente True, y te ahorras que entre en 
            # los ifs de abajo
            ia_value = parse_ia(ia[tipo_uso]) # Porque aca directamente no usamos valor? te ahorras que use toda la funcion total la columna valor ya la limpiamos
            if isinstance(ia_value, float) and ia[tipo_uso].startswith("<"):
                return True if valor < ia_value else False
            elif isinstance(ia_value, float) and ia[tipo_uso].startswith(">"):
                return True if valor > ia_value else False
            elif isinstance(ia_value, tuple):
                ia_min, ia_max = ia_value
                return True if ia_min <= valor <= ia_max else False
    return None

#def cumple_limites(row):
    if (row['Medida'] in data_limites_uso['Parámetro'].values and row['Valor (=)'] >= data_limites_uso.loc[data_limites_uso['Parámetro'] == row['Medida'], 'I a'].values[0]):
        return True
    elif (row['Medida'] in data_limites_uso['Parámetro'].values):
        return False
    else:
        return None

data_mediciones['Cumple Limites Ia'] = data_mediciones.apply(lambda row: check_condition(row, "I a"), axis=1)
data_mediciones['Cumple Limites Ib'] = data_mediciones.apply(lambda row: check_condition(row, "I b"), axis=1)
data_mediciones['Cumple Limites II'] = data_mediciones.apply(lambda row: check_condition(row, "II"), axis=1)
data_mediciones['Cumple Limites III'] = data_mediciones.apply(lambda row: check_condition(row, "III"), axis=1)
data_mediciones['Cumple Limites IV'] = data_mediciones.apply(lambda row: check_condition(row, "IV"), axis=1)
# print(data_mediciones[data_mediciones['Cumple Limites Ia'] == False])

data_mediciones.to_csv("data_mediciones_con_dummies", index = False)

# Aplicar esto al resto de los limites de usos y armar matriz de DUMMIES.
    # En el primer TRUE, al resto se le aplica TRUE.
    
# Agregar subcuenca
# Agregar cuenca

# PREGUNTAS
# 1. Que hacer con la base de limites_admisibles_vertido
# 2. Si hay parametros especificos relacionadas a la contaminacion.