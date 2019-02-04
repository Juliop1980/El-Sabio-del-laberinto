Integrantes: Leonardo López Almazán 14-10576, Julio Pérez 14-10820

			Proyecto 1 de Lenguajes de Programación

En este proyecto se tenía la tarea de realizar un juego en el que un viajero recorre 
un laberinto y tiene distintas opciones durante todo el trayecto. El sabio de los laberintos
va aprendiendo a medida que los viajeros recorren los laberintos averiguando la ubicación
de tesoros, derrumbes y más. 

Para este propósito se creó un tipo de dato laberinto con 2 constructores (Trifurcacion y Tesoro)
para los cuales se definió un formato de string
La string de una Trifurcacion es de la forma (a|b|c) donde a, b y c son los laberintos a los
que lleva por la Izquierda, Recto y Derecha respectivamente
y la de Tesoro de la forma descripción/laberinto. 
Se definió el tipo de dato Direccion que puede ser Izquierda, Derecha o Recto, de modo que una
ruta sería una lista de direcciones del tipo
[Izquierda,Derecha,Recto,Derecha,Derecha,Izquierda].

Mediante funciones que manejan este tipo de datos junto a las funciones que manejan 
las opciones del usuario le dan vida al juego pudiendo descubrir los más recónditos
secretos de los laberintos explorados.
