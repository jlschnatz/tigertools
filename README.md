## Das `tigertools`-Pakets

Das tigertools-R-Paket wird verwendet, um neue Items basierend auf einem standardisierten Template zu erstellen und in die bestehende Datenbank einzupflegen.

**Installation und Laden des Paket** Das Paket kann über die folgende Befehlsfolge installiert und geladen werden:

``` r
# install.packages("remotes") # falls nicht installiert
remotes::install_github("jlschnatz/tigertools")
library(tigertools)
```


Nachdem das Paket geladen wurde, können dessen Funktionen verwendet werden, um Items effizient und konsistent zu erstellen.

## Workflow

**Übersicht**:

1.  *Schritt 1*: Neues Item generieren Erstelle ein neues Item mit der Funktion `create()` aus dem Paket tigertools:

2.  *Schritt 2*: Datei bearbeiten Bearbeite die generierte Markdown-Datei gemäß den Template-Vorgaben. Falls eine zusätzliche R-Datei erstellt wurde (z. B. für Daten-Simulationen), passe diese ebenfalls an.

3.  *Schritt 3*: Überprüfung und Aktualisierung der Datenbank Überprüfe das Item und aktualisiere gleichzeitig die Datenbank mit der Funktion `update_db()`

    -   Validierung: Die Funktion überprüft automatisch, ob das Item korrekt ausgefüllt wurde.
    -   Fehlerbehandlung: Wenn Fehler auftreten, wird ein entsprechender Error ausgegeben. In diesem Fall muss das Item überarbeitet und der Schritt erneut durchgeführt werden.

4.  *Schritt 4*: Finalisierung Sobald die Überprüfung fehlerfrei abgeschlossen ist, wird das Item automatisch in die Datenbank aufgenommen.

### Schritt 1: Erstellen der Datei

Ein neues Item kannst du mit der Funktion create() erstellen:

``` r
create(item_folder = "items", open = TRUE, r_file = NULL)
```

Argumente: - `item_folder` (default: "items") Gibt den Dateipfad an, in dem die Markdown-Dateien der Items gespeichert werden. Der Standardwert sollte in der Regel nicht geändert werden.

-   `open` (default: TRUE) Bestimmt, ob die erstellte Datei nach der Erstellung automatisch in der verwendeten IDE geöffnet wird.

-   `r_file` (default: NULL) Optional: Gibt den Namen einer zusätzlichen R-Datei an, die parallel zur Markdown-Datei erstellt wird. Dies ist relevant, wenn es sich bei der Aufgabe um eine R-Programmieraufgabe handelt, für die ein Datensatz generiert und simuliert werden muss. Diese Datei kann genutzt werden, um den benötigten Datensatz zu erstellen.

**Beispiel**

``` r
#| eval: false
# Beispiel: Erstellung eines neuen Items, mit R-Datei für Datensatz `therapie`
create_new_item(open = TRUE, r_file = "therapie.R")
```

### Schritt 2: Inhalt

Muss nach einem Bestimmten Schema befüllt werden:

#### id_item

-   Eine eineindeutige Kennzeichung für jedes Item
-   Ist automatisch schon angegeben (entspricht dem Dateinamen der Markdowndatei)
-   Muss dadurch auch nicht mehr verändert werden

#### learning_area

-   Zuordnung des Item in einen Lernbereich (orientiert an der Vorlesung)
-   Mögliche Kategorien: `Deskriptivstatistik`, `Grundlagen der Inferenzstatistik`, `Wahrscheinlichkeit`, `Zusammenhangsmaße`, `Regression` oder `Poweranalyse`

##### type_item

-   Art des Item (Content items beziehen sich auf inhaltliche Items und coding Items beziehen sich auf R-Inhalte)
-   Mögliche Kategorien: Muss entweder `content` oder `coding` sein

#### bloom_taxonony

-   Einordnung des Items in die Bloom-Taxonomie
-   Mögliche Kategorien: `knowledge`, `comprehension` oder `application`

#### theo_diff

-   Einschätzung des Schwierigkeitsgrads des Items (subjektiv)
-   Mögliche Kategorien: `easy`, `medium`, `hard`

#### stimulus_text

-   Beschreibung: Der Stimulustext des Items
-   Kann reine Textform sein oder auch HTML-Code oder MD-Code (z.B. für eine kleine Tabelle oder eine Formel) enthalten

#### stimulus_image

-   Falls zusätzlich zu dem Stimulustext ein Bild mit in den Stimulus eingefügt werden soll
-   Muss ein Pfad zu einem Bild sein

#### answeroption_01 - answeroption_05

-   Felder für die möglichen Antwortoptionen des Items
-   Kann sowohl reine Textform oder ein Pfad zu einem Bild sein
-   Wenn Bilder verwendet wird, muss für *alle* Antwortoptionen Bilder verwendet werden (Text kombiniert mit Bildern ist also derzeit nicht möglich)
-   Es können maximal 5 Antwortoptionen verwendet werden (answeroption_01-05), aber jedoch auch weniger (mind. 3 sollten es jedoch sein aus inhaltlichen Gründen)

#### answeroption_06

-   Ist ein Placeholder, wenn Studierende die Aufgabe ohne Bewertung überspringen wollen
-   Kategorien: `Frage überspringen.` für Antwortoptionen mit Textinhalt oder `www/skip.png` für Antwortoptionen, die Bilder enthalten

#### answer_correct

-   Beschreibung: Die korrekte Antwort (als Zahl)
-   z.B. wenn die dritte Antwortoption korrekt ist: 3

#### type_stimulus

-   Beschreibung: Die Art des Stimulus
-   Kategorie: entweder `text`, wenn das Feld `stimulus_image` leer ist oder `stimulus_image`, wenn ein Bild verwendet wurde im Stimulus

#### type_answer

-   Beschreibung: Die Art der Antwortoptionen
-   Kategorie: entweder `text`, wenn für die Felder `answeroption_XX` nur Text verwendet wurde oder `stimulus_image`, wenn ein Bilder verwendet wurden

#### if_answeroption_01 - if_answeroption_05

-   Beschreibung: Die Feedbackblöcke für die jeweilige Antwortoption
-   Anzahl an ausgefüllten Feedbackblöcken muss Anzahl der verwendeten Antwortoptionen entsprechen

#### if_answeroption_06

-   Beschreibung: Feedbackblock, wenn Item übersprungen wurde
-   Ist bereits vorgegeben und soll nicht verändert werden

### Schritt 3: Überprüfung und Aktualisierung

Die Funktion update_db() dient dazu, ein neu generiertes Item zu validieren und in die bestehende Datenbank einzupflegen.

``` r
update_db(
    md_file, 
    item_folder = "items", 
    csv_file = "data_item_tiger.csv", 
    sqlite_file = "db_item.sqlite"
    )
```

-   `md_file` Die Markdown-Datei des neuen Items (z. B. items/tiger_item_001.md).

-   `item_folder` (default: "items") Der Speicherort der Item-Dateien. Standardwert sollte nicht geändert werden.

-   `csv_file` (default: "data_item_tiger.csv") Der Name der CSV-Datei, die die Item-Datenbank enthält. Standardwert sollte nicht geändert werden.

-   `sqlite_file` (default: "db_item.sqlite") Der Name der SQLite-Datenbankdatei. Auch hier sollte der Standardwert nicht geändert werden.-

Die Funktion prüft automatisch, ob das Item vollständig und korrekt ist. Wenn Fehler auftreten (z. B. fehlende Felder), wird ein Error ausgegeben. Im Error werden konkrete Hinweise gegeben, was an dem Item geändert werden muss. Das Item muss überarbeitet werden, bevor die Datenbank erneut aktualisiert wird.

### Schritt 4: Finalisierung

Sobald die Funktion `update_db()` erfolgreich durchläuft, wird das Item automatisch in die Datenbank aufgenommen. Der Workflow ist damit abgeschlossen und kann von vorne beginnen.