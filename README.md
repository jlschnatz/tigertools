## Das `tigertools`-Paket

Das tigertools-R-Paket wird verwendet, um neue Items für [shinytigeR](https://github.com/jlschnatz/shinytigeR) auf Basis eines standardisierten Markdown-Templates zu erstellen, zu validieren und in die Item-Datenbank (`data_item_tiger.csv` + `db_item.sqlite`) einzupflegen.

**Installation und Laden des Pakets**

``` r
# install.packages("remotes") # falls nicht installiert
remotes::install_github("jlschnatz/tigertools")
library(tigertools)
```

Alle Funktionen setzen voraus, dass das Arbeitsverzeichnis der Ordner des Item-Datenbank-Projekts ist (dort, wo die `.Rproj`-Datei und der Ordner `items/` liegen) — am einfachsten das RStudio-Projekt über die `.Rproj`-Datei öffnen.

## Workflow

1.  **Item erstellen** mit `create()` — erzeugt `items/tiger_item_XXX.md` mit der nächsten freien ID.
2.  **Datei ausfüllen** gemäß den Kommentaren im Template (siehe unten).
3.  **Validieren und einpflegen** mit `push()` (ein Item), `push_recent()` (alle seit dem letzten Einpflegen geänderten Items) oder `push_all()` (alle Items).
    -   Jedes Item wird vor dem Schreiben automatisch geprüft. Jede Prüfung meldet ✔ oder ✖; schlägt eine fehl, bricht die Funktion mit einem Fehler ab und nichts wird geschrieben. Das Item überarbeiten und erneut pushen.

### Schritt 1: Item erstellen

``` r
create(open = TRUE, r_file = NULL, answer_mode = "mc")
```

-   `answer_mode`: `"mc"` (Standard) für ein Multiple-Choice-Item, `"num"` für ein numerisches Item (Studierende tippen eine Zahl ein). Die beiden Item-Arten haben unterschiedliche Templates.
-   `open`: Ob die erstellte Datei direkt geöffnet werden soll.
-   `r_file`: Optional der Name einer zusätzlichen R-Datei in `data-raw/` (z. B. für die Simulation eines Datensatzes bei R-Programmieraufgaben).

``` r
create(r_file = "therapie.R")          # MC-Item mit R-Datei für einen Datensatz
create(answer_mode = "num")            # numerisches Item
```

### Schritt 2: Inhalt

#### Felder für alle Items

| Feld | Inhalt |
|---|---|
| `id_item` | Eindeutige ID, wird von `create()` vorausgefüllt — nicht ändern |
| `learning_area` | Eines von: `Deskriptivstatistik`, `Wahrscheinlichkeit`, `Grundlagen der Inferenzstatistik`, `Gruppenvergleiche`, `Poweranalyse`, `Zusammenhangsmaße`, `Regression` |
| `type_item` | `content` (inhaltlich) oder `coding` (R-Code) |
| `bloom_taxonomy` | `knowledge`, `comprehension` oder `application` |
| `theo_diff` | Subjektive Schwierigkeit: `easy`, `medium` oder `hard` |
| `answer_mode` | `mc` oder `num` — vom Template vorgegeben, nicht ändern. (Ältere Item-Dateien ohne dieses Feld gelten als `mc`.) |
| `stimulus_text` | Aufgabentext; darf Markdown (z. B. Tabellen) und LaTeX (`$...$`) enthalten |
| `stimulus_image` | Optional: Bildpfad `www/...` (Datei muss in `items/www/` liegen) |
| `type_stimulus` | `text` oder `image` |

#### Multiple-Choice-Items (`answer_mode = mc`)

-   `answeroption_01` – `answeroption_05`: Antwortoptionen (Text oder Bildpfad; wenn Bilder, dann für *alle* Optionen). Mindestens 3 sind inhaltlich sinnvoll, maximal 5.
-   `answeroption_06`: Platzhalter zum Überspringen — `Frage überspringen.` (Text) bzw. `www/skip.png` (Bilder).
-   `answer_correct`: Nummer der **einen** richtigen Option, z. B. `3`.
-   `type_answer`: `text` oder `image`.
-   `if_answeroption_01` – `05`: Feedback je Option (Anzahl muss der Anzahl der Optionen entsprechen); `if_answeroption_06` bleibt unverändert.

#### Numerische Items (`answer_mode = num`)

Studierende tippen eine Zahl ein; diese wird mit den Antwortoptionen verglichen. Jede Option `XX` (01–06) besteht aus vier Feldern:

| Feld | Inhalt |
|---|---|
| `answeroption_XX` | Der Zahlenwert, z. B. `6.67` (Punkt oder Komma) |
| `lower_answeroption_XX` | Untere Grenze des akzeptierten Bereichs (inklusive) |
| `upper_answeroption_XX` | Obere Grenze des akzeptierten Bereichs (inklusive) |
| `if_answeroption_XX` | Feedback, wenn die eingegebene Zahl zu dieser Option passt |

Regeln (werden beim Pushen geprüft):

1.  **Beide Grenzen leer** → nur der exakte Wert zählt (z. B. für ganze Zahlen, Freiheitsgrade).
2.  **Beide Grenzen gesetzt** → jede Zahl von `lower` bis `upper` passt. Der Wert selbst muss im Bereich liegen; asymmetrische Bereiche sind erlaubt (z. B. `2.58`–`2.59` für 2,582, um Runden *und* Abschneiden abzudecken).
3.  **Nur eine Grenze gesetzt** → ungültig.
4.  **Bereiche verschiedener Optionen dürfen sich nicht überschneiden** (auch nicht an einem einzelnen Punkt), damit jede Eingabe zu höchstens einer Option passt.
5.  **`answer_correct`**: Nummer(n) der richtigen Option(en), mehrere mit `;` getrennt, z. B. `1;2`. Jede richtige Option hat ihr eigenes Feedback — so können z. B. die Varianz mit *n − 1* und mit *n* beide richtig sein.
6.  Es gibt **keine Überspringen-Option** (die App hat dafür einen eigenen Button). Nicht benötigte Optionen einfach leer lassen; jede ausgefüllte Option braucht Feedback.

Eine Eingabe, die zu keiner Option passt, wertet die App als „Antwort nicht erkannt“. Falsche Optionen (Distraktoren) sollten daher typische Fehler abbilden, deren Feedback den Fehler erklärt.

**Beispiel** — *„Berechne die Varianz von 2, 4, 6, 8. Runde auf zwei Nachkommastellen.“*, `answer_correct` = `1;2`:

| # | Wert | lower | upper | richtig | Feedback |
|---|---|---|---|---|---|
| 01 | 6.67 | 6.66 | 6.67 | ✓ | Stichprobenvarianz (n − 1) |
| 02 | 5 | | | ✓ | Populationsvarianz (n) |
| 03 | 20 | | | ✗ | Quadratsumme, nicht geteilt |
| 04 | 2.58 | 2.58 | 2.59 | ✗ | Standardabweichung statt Varianz |

### Schritt 3: Validieren und einpflegen

``` r
push("items/tiger_item_130.md")                  # ein Item
push("items/tiger_item_130.md", overwrite = TRUE) # bestehendes Item ersetzen (mit Rückfrage)
push_recent()                                     # alle seit dem letzten Einpflegen geänderten Dateien
push_all(overwrite = TRUE)                        # alle Item-Dateien
```

`update_db(md_file)` ist die ältere Variante: prüft ein Item und schreibt dann alle Item-Dateien neu in CSV und Datenbank.

**Wie geschrieben wird:** Die CSV-Datei wird komplett neu geschrieben. Die SQLite-Tabelle `item_db` wird dagegen nur *aktualisiert* — geänderte Items per `id_item` überschrieben, neue ergänzt, fehlende Spalten (z. B. die Grenzen für numerische Items) hinzugefügt. Spalten, die tigertools nicht selbst verwaltet (z. B. die IRT-Parameter `irt_*` aus der Kalibrierung), bleiben erhalten. Items, deren Datei gelöscht wurde, bleiben ebenfalls in der Datenbank.
