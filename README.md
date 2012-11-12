
Simple Racket Game Library
==========================

This is a small library written in racket that aims to make developing
games with the native racket/gui library more convenient.

## Kenmerken ##

 - Volledig event-driven muis en toetsenbord
   >> detecteer muiswiel, indrukken/loslaten van toets, ...
 - Gebufferde frames
   >> voorkom flikkeringen tijdens het tekenen
 - Snelheidsoptimisaties 
   >> gebruik van hashtabellen, class-generics, ...
 - Geavanceerde tekenfuncties
   >> teken bitmaps, gebruik rotaties, transparantie, ...
 - Uitgebreid kleurenpallet
   >> kleuren als indigo, goudgeel, turqoise, ...

## Gebruik ##

 Alleen de bestanden in de hoofdmap zijn nodig om het programma te
 runnen. Deze moeten in dezelfde map blijven staan. Het wordt
 aangeraden om de library in een aparte directory te houden zodat
 ze gemakkelijk kan worden geupdated.
 
 Zie de bestanden in de map "examples" voor meer informatie over
 het gebruik van deze library. De hoofdlibrary includen gaat via
 (#%require "graphics.rkt"). Gebruik (#%require "canvas-v2.rkt")
 om alleen functies te gebruiken die conform zijn met diegene die
 gegeven werden bij de opdracht van het jaarproject.
 
 - [Grafische functies](url http://docs.racket-lang.org/draw/Drawing_Functions.html)
 - [Tekenoperaties](url http://docs.racket-lang.org/draw/dc___.html)
 - [Kleurennamen](url http://docs.racket-lang.org/draw/color-database___.html)
 - [Bitmaps](url http://docs.racket-lang.org/draw/bitmap_.html)

 De configuratie-instellingen kunnen aangepast worden in het
 bestand "constants.rkt". Kijk na of je nieuwe configuratie
 werkt door een van de testbestanden runnen alvorens je de nieuwe
 configuratie in je project gebruikt. 
 
## To Do List ##

 - Vinden en fixen van (eventuele) bugs
 - Gebruik van BST-algoritme voor speciale toetsen
 - Performantietests op generics uitvoeren
 - OpenGL: gebruik de grafische kaart van de computer
 - Uitgebreide events: pre/post-acties, events chainen, ...
 - Meer high-level tekenfuncties

## Credits ##

> Ere wie ere toekomt:

 Library gemaakt en onderhouden door Sam Vervaeck.

 Met dank aan Adriaan Leynse voor het vertrouwd maken met de racket
 libraries zoals racket/gui en racket/class. Ook voor het maken van
 een eerste implementatie gebaseerd op racket/gui.

 Gebaseerd op de code van het Software Languages Lab. Een volledige
 backport naar het originele "Canvas.rkt"-bestand zit bijgevoegd in
 een aparte file. Gebruik (#%require "canvas.rkt")
 
 Alle referentiecode is terug te vinden in de map "references".

## Disclaimer ##

           Code mag vrij gebruikt en/of aangepast worden. 

                   ** Alles op eigen risico! **

         https://www.facebook.com/groups/496604190350242/
