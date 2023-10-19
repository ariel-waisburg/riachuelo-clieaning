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
        if medida == ia['Parámetro'] and 'Aguas' in row['Análisis']:
            ia_value = parse_ia(ia[tipo_uso]) # Porque aca directamente no usamos valor? te ahorras que use toda la funcion total la columna valor ya la limpiamos
            if isinstance(ia_value, float) and ia[tipo_uso].startswith("<"):
                return True if valor < ia_value else False
            elif isinstance(ia_value, float) and ia[tipo_uso].startswith(">"):
                return True if valor > ia_value else False
            elif isinstance(ia_value, tuple):
                ia_min, ia_max = ia_value
                return True if ia_min <= valor <= ia_max else False
    return None


# Los datos figuran distinto en los dos csv hay que cambiar alguno de los dos por ejemplo 
# En limites tenes Nitrógeno Amoniacal que en las mediciones figura como Nitrógeno de Amoníaco (N-NH3) hay que ver todos y cambiarlo manual

def igualar_medidas(row):
    medida = row['Medida']
    if medida == "Nitrógeno de Amoníaco (N-NH3)":  
        return "Nitrógeno Amoniacal"
    elif medida == "Clorofila A":
        return "Clorofilia a"
    elif medida == "Demanda Biológica de Oxígeno (DBO5)":
        return "DBO5"
    elif medida == "Detergentes (SAAM)":
        return "Detergentes (S.A.A.M.)"
    elif medida == "Índice de Estado Trófico- TSI- (Fósforo Total) (TSI Fósforo Total)":
        return "Fósforo Total"
    elif medida == "Nitrato (NO3-)": # Revisar
        return "Nitrógeno de Nitratos"
    elif medida == "Oxigeno disuelto":
        return "OD"        
    elif medida == "Temperatura (T)":
        return "Temperatura"
    elif medida == "Arsenico (As)": # Revisar
        return "Arsénico total"
    elif medida == "Cadmio" or medida == "Cadmio (Cd)": # Revisar
        return "Cadmio total"
    elif medida == "Zinc (Zn)" or medida == "Zinc": #Zinc tiene distinta unidad es ml/kg y zinc (zn) es mg/l
        return "Cinc total"     
    elif medida == "Cianuros (CN)":
        return "Cianuro total"  
    elif medida == "Cobre (Cu)" or medida == "Cobre": # Diferentes unidades
        return "Cobre total"  
    elif medida == "Cromo (Cr)" or medida == "Cromo": # Diferentes unidades
        return "Cromo total"  
    elif medida == "Cromo (Cr)" or medida == "Cromo": # Diferentes unidades
        return "Cromo total"  
    # Cromo hexavalente
    elif medida == "Mercurio" or medida == 'Mercurio (Hg)':
        return 'Mercurio total'
    elif medida == "Niquel (Ni)" or medida == 'Niquel':
        return 'Níquel total'
    elif medida == "Plomo (Pb)" or medida == 'Plomo':
        return 'Plomo total'    
    elif medida == "Plomo (Pb)" or medida == 'Plomo':
        return 'Plomo total'    
    # Aldrín     
    elif medida == "Clordano Técnico":
        return "Clordano"   
    # DDT (Total Isómeros)
    elif medida == "Dieldrin":
        return "Dieldrín"
    elif medida == "Endosulfán total":
        return "Endosulfán"       
    elif medida == "Endrin":
        return "Endrín"  
    # Heptacloro 
    # Heptacloro epóxido
    elif medida == "Hexaclorobenceno":
        return "Hexacloro benceno"
    else:
        return medida
    # Metoxicloro
    # Paration
    # Malation
    # 2,4 D


# def buscar_medida_con_cadena(cadena):
#     medidas = set()
#     for _, row in data_mediciones.iterrows():
#         if cadena in row['Medida']:
#             medidas.add(row['Medida'])
#     for valor in medidas:
#         print(valor)

# buscar_medida_con_cadena("amon")





# data_mediciones['Medida'] = data_mediciones.apply(lambda row: igualar_medidas(row),axis=1)
# data_mediciones['Cumple Limites Ia'] = data_mediciones.apply(lambda row: check_condition(row, "I a"), axis=1)
# data_mediciones['Cumple Limites Ib'] = data_mediciones.apply(lambda row: check_condition(row, "I b"), axis=1)
# data_mediciones['Cumple Limites II'] = data_mediciones.apply(lambda row: check_condition(row, "II"), axis=1)
# data_mediciones['Cumple Limites III'] = data_mediciones.apply(lambda row: check_condition(row, "III"), axis=1)
# data_mediciones['Cumple Limites IV'] = data_mediciones.apply(lambda row: check_condition(row, "IV"), axis=1)
# data_mediciones.to_csv("data_mediciones_con_dummies.csv", index = False)

# Aplicar esto al resto de los limites de usos y armar matriz de DUMMIES.
    # En el primer TRUE, al resto se le aplica TRUE.
    
# Agregar subcuenca
# Agregar cuenca

# PREGUNTAS
# 1. Que hacer con la base de limites_admisibles_vertido
# 2. Si hay parametros especificos relacionadas a la contaminacion.