# id_item

<!-- Numerisch: z.B. 100 (muss eineindeutig sein) -->

# learning_area

<!---
String (eines von):
    - Deskriptivstatistik
    - Wahrscheinlichkeit
    - Grundlagen der Inferenzstatistik
    - Gruppenvergleiche
    - Poweranalyse
    - Zusammenhangsmaße
    - Regression
 --->

# type_item

<!---
String (eines von):
    - content
    - coding
 --->

# bloom_taxonomy

<!---
String (eines von):
    - knowledge
    - comprehension
    - application
--->

# theo_diff

<!---
String (eines von):
    - easy
    - medium
    - hard
--->

# answer_mode

num

<!---
Bitte so lassen: "num" = numerisches Item (Studierende tippen eine Zahl ein).
--->

# stimulus_text

<!-- 
String. Enhält Stimulustext (kann ebenfalls Markdown-Tabellen enthalten).
Tipp: Angeben, wie gerundet werden soll (z.B. "auf zwei Nachkommastellen").
--->

# stimulus_image

<!-- 
String. Falls zusätztlich zu `stimulus_text` ein Bild als Stimulus verwendet werden soll, kann
hier ein Pfad für das Bild eingefügt werden. (ansonsten auslassen)
--->

# type_stimulus

<!---
String (eines von):
    - text
    - image
Muss `text` sein, wenn als Stimulus ein Textformat genutzt wurde, und `image`, wenn als Stimulus ein Bild verwendet wurde.
--->

# answeroption_01

<!---
REGELN FÜR NUMERISCHE ANTWORTOPTIONEN (gelten für alle Optionen 01-06):

Jede Antwortoption besteht aus vier Feldern:
    - answeroption_XX:        der Zahlenwert (Punkt oder Komma als Dezimaltrennzeichen), z.B. 6.67
    - lower_answeroption_XX:  untere Grenze des akzeptierten Bereichs (inklusive), z.B. 6.66
    - upper_answeroption_XX:  obere Grenze des akzeptierten Bereichs (inklusive), z.B. 6.67
    - if_answeroption_XX:     Feedback, wenn die eingegebene Zahl zu dieser Option passt

1. Beide Grenzen leer  -> nur der exakte Wert zählt (z.B. für ganze Zahlen, Freiheitsgrade).
2. Beide Grenzen gesetzt -> jede Zahl von lower bis upper passt. Der Wert selbst muss im Bereich liegen.
   Asymmetrische Bereiche sind erlaubt (z.B. 2.58 bis 2.59 für 2.582, deckt Runden und Abschneiden ab).
3. Nur eine Grenze gesetzt -> ungültig.
4. Die Bereiche verschiedener Optionen dürfen sich nicht überschneiden.
5. Es gibt keine "Überspringen"-Option (dafür gibt es in der App einen eigenen Button).
   Nicht benötigte Optionen einfach leer lassen.

Distraktoren (falsche Optionen) sind typische Fehler, z.B. "Quadratsumme statt Varianz";
ihr Feedback erklärt den Fehler. Eine eingegebene Zahl, die zu keiner Option passt, wird als
"Antwort nicht erkannt" gewertet.
--->

# lower_answeroption_01

# upper_answeroption_01

# if_answeroption_01

# answeroption_02

# lower_answeroption_02

# upper_answeroption_02

# if_answeroption_02

# answeroption_03

# lower_answeroption_03

# upper_answeroption_03

# if_answeroption_03

# answeroption_04

# lower_answeroption_04

# upper_answeroption_04

# if_answeroption_04

# answeroption_05

# lower_answeroption_05

# upper_answeroption_05

# if_answeroption_05

# answeroption_06

# lower_answeroption_06

# upper_answeroption_06

# if_answeroption_06

# answer_correct

<!---
Nummer(n) der richtigen Antwortoption(en), mehrere mit Semikolon getrennt.
    - eine richtige Option: 1
    - mehrere richtige Optionen: 1;2   (z.B. Varianz mit n - 1 UND mit n)
Jede richtige Option hat ihr eigenes Feedback.
--->
