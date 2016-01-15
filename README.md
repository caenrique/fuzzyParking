# Controlador de aparcamiento en batería usando lógica difusa
Consiste en un sistema de reglas basadas en lógica difusa que describen el comportamiento del controlador. El sistema recibe dos parámetros: el ángulo del coche con respecto a la paralela del aparcamiento, y la distancia a la bisectriz del apacamiento. Con esos dos sistemas se calcula el valor de la posición del volante.
Los valores para el ángulo van desde -180 hasta 180, los de la distancia de -50 hasta 50 y los del volante de -30 hasta 30.

## Instalación
En un terminal ejecuta `cabal install` y tendrás el ejecutable en `dist/build/fuzzyParking/`

## Licencia
Este código está distribuído bajo licencia GPL-2