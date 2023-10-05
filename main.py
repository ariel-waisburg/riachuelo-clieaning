# Import packages
import pandas as pd
from pandas_profiling import ProfileReport

# Import data
data_mediciones = pd.read_csv("acumar_mediciones.csv")

# EDA report
# profile = ProfileReport(data_mediciones)
# profile.to_file("eda_mediciones_reporte.html")