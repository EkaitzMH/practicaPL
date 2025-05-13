import unicodedata

def quitar_tildes(texto):
    # Normaliza el texto eliminando las tildes
    return ''.join(
        c for c in unicodedata.normalize('NFD', texto)
        if unicodedata.category(c) != 'Mn'
    )

def procesar_archivo(entrada, salida):
    with open(entrada, 'r', encoding='utf-8') as archivo_entrada:
        lineas = archivo_entrada.readlines()
    
    with open(salida, 'w', encoding='utf-8') as archivo_salida:
        for linea in lineas:
            archivo_salida.write(quitar_tildes(linea))

# Cambia 'archivo_entrada.txt' y 'archivo_salida.txt' por los nombres de tus archivos
procesar_archivo('palabras_castellano.txt', 'palabras_castellano_sin_tildes.txt')