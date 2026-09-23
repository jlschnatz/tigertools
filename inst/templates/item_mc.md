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

mc

<!---
Bitte so lassen: "mc" = Multiple-Choice-Item.
Für numerische Items stattdessen create(answer_mode = "num") verwenden.
--->

# stimulus_text

<!-- 
String. Enhält Stimulustext (kann ebenfalls Markdown-Tabellen enthalten)
--->

# stimulus_image

<!-- 
String. Falls zusätztlich zu `stimulus_text` ein Bild als Stimulus verwendet werden soll, kann
hier ein Pfad für das Bild eingefügt werden. (ansonsten auslassen)
--->

# answeroption_01

# answeroption_02

# answeroption_03

# answeroption_04

# answeroption_05

# answeroption_06

<!---
String (ohne Anführungszeichen):
    - "Frage überspringen."
    - "www/skip.png"
  
Wenn type_answer: `text`, dann "Frage überspringen"
Wenn type_answer: `image`, dann "www/skip.png"
--->

# answer_correct

<!-- Numerisch (Integer): Nummer der einen richtigen Antwortoption, z.B. 3 -->


# type_stimulus

<!---
String (eines von):
    - text
    - image
Muss `text` sein, wenn als Stimulus ein Textformat genutzt wurde, und `image`, wenn als Stimulus ein Bild verwendet wurde.
--->

# type_answer

<!---
String (eines von):
    - text
    - image
Muss `text` sein, wenn als Antwortoptionen ein Textformat genutzt wurde, und `image`, wenn als Antwortoptionen Bilder verwendet wurden.
--->

# if_answeroption_01

<!---
Die `if_answeroption_XX` Felder müssen das Feedback für die ausgewählte Antwortoption XX enthalten.
--->

# if_answeroption_02

<!---
Die `if_answeroption_XX` Felder müssen das Feedback für die ausgewählte Antwortoption XX enthalten.
--->

# if_answeroption_03

<!---
Die `if_answeroption_XX` Felder müssen das Feedback für die ausgewählte Antwortoption XX enthalten.
--->

# if_answeroption_04

<!---
Die `if_answeroption_XX` Felder müssen das Feedback für die ausgewählte Antwortoption XX enthalten.
--->

# if_answeroption_05

<!---
Die `if_answeroption_XX` Felder müssen das Feedback für die ausgewählte Antwortoption XX enthalten.
--->

# if_answeroption_06

<!--
Bitte so lassen.
-->

Alles klar! Du hast die Aufgabe übersprungen.
