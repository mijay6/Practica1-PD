 # Práctica 1: Programación Lógica Pura

[![Prolog](https://img.shields.io/badge/language-Prolog-blue.svg)](https://ciao-lang.org/)  
[![Status: Academic](https://img.shields.io/badge/Status-Academic%20Project-success.svg)]()  
[![Version](https://img.shields.io/badge/Version-1.0.0-brightgreen.svg)]()  

## Descripción
Esta práctica tiene como objetivo aplicar los conceptos de listas, estructuras y recursividad en Prolog puro. Se implementarán tipos y operaciones para bits, nibbles y bytes, tanto en notación binaria como hexadecimal, utilizando únicamente cláusulas, términos y unificación, y siguiendo aritmética de Peano.

## Estructura del Proyecto
- `code.pl`  
  Archivo principal con la implementación de los predicados, las aserciones para documentación (lpdoc) y los casos de prueba.

## Predicados principales implementados
1. **`byte_list(L)`** 
   Verifica o genera una lista de bytes (binarios o hexadecimales).

2. **`byte_convert(HexByte, BinByte)`**
   Convierte un byte hexadecimal a su representación binaria.

3. **`byte_list_convert(HL, BL)`**
   Convierte una lista de bytes hexadecimales a su lista equivalente de bytes binarios.

4. **`get_nth_bit_from_byte(N, B, BN)`**  
   Extrae el bit índice N (Peano) de un byte, binario o hexadecimal.

5. **`byte_list_clsh(L, CLShL)`**
   Desplazamiento circular a la izquierda de una lista de bytes.

6. **`byte_list_crsh(L, CRShL)`**  
   Desplazamiento circular a la derecha de una lista de bytes.

7. **`byte_xor(B1, B2, B3)`**  
   XOR polimórfico entre dos bytes (binarios o hexadecimales).

## Documentación
La documentación se genera automáticamente con **lpdoc** a partir de las aserciones y comentarios en `code.pl`. Para ello:
```bash
lpdoc code.pl
```
## Casos de Prueba
Se han incluido aserciones test dentro de `code.pl` para verificar el correcto funcionamiento de cada predicado. Para ejecutar los tests en Ciao Prolog:
```bash
?- make.
?- run_tests.
```
## Instalación y Uso

### Requisitos Previos
- **Ciao Prolog** (versión 1.20 o superior): Necesario para compilar y ejecutar el módulo, procesar aserciones y ejecutar tests.

### Pasos para Ejecutar

1. **Clonar el repositorio:**
   ```bash
   git clone https://github.com/<tu-usuario>/<nombre-del-repo>.git
   cd Practica1-PD-main
   ```

2. **Cargar el módulo en el top-level de Ciao (`ciaosh`):**
   ```bash
   ciaosh
   ```
   Dentro de la shell interactiva de Ciao:
   ```prolog
   ?- use_module('code.pl').
   ```

3. **Ejemplos de ejecución de consultas:**
   ```prolog
   % 1. Conversión de byte hexadecimal a binario:
   ?- byte_convert([h(5), h(a)], BinByte).
   % BinByte = [b(0),b(1),b(0),b(1),b(1),b(0),b(1),b(0)]

   % 2. Extraer el bit N (en notación de Peano, s(s(s(0))) = 3) de un byte:
   ?- get_nth_bit_from_byte(s(s(s(0))), [h(5), h(a)], Bit).
   % Bit = b(0)

   % 3. Operación XOR entre bytes:
   ?- byte_xor([h(a), h(5)], [h(5), h(a)], Result).
   % Result = [h(f), h(f)]
   ```

## Author
Dobra Mihai

Technical University of Madrid  
Escuela Técnica Superior de Ingenieros Informáticos
Programación Declarativa
Academic Year 2024-2025

## Contributing
Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct and the process for submitting pull requests.
