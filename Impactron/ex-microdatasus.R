library(microdatasus)

dados <- fetch_datasus(year_start = 2022, month_start = 6, month_end = 6, year_end = 2022, information_system = "SIA-PA", uf="PR")
dados <- process_sia(dados)

