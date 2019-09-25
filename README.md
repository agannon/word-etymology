# Word Etymologies

### General Notes
This project was made to familiarize myself with Haskell as a web application language
and Vue.js. The goal is to display the 1000 most common words in the English language
(based on https://gist.github.com/deekayen/4148741), finding their language(s) of origin
(based on https://www.etymonline.com/), get translations from English to those languages
using Google Translate.

Problems that arose during project:
1. The list of words used had duplicates and did not
include parts of speech so translation was not exact in that regard
2. I am only familiar with German and French, so the Latin translations especially
are very reliant on Google search results
3. If I were to redo the lookup/translation process I would use the 1000 most common
English nouns as there is less ambiguity of meaning in a word that represents a noun

Other notes:
* Words where the language of origin is marked as English are if it appeared first in
Old English, was directly from Proto-Germanic, or was from a regional English language
such as Kentish/Saxon.
* Using etymonline.com I assumed that when it says from French, from Latin from Greek that
 they were loaned by each language in sequence unless it says from both x and y.
* The API shows words from the language they directly came from, and the first example
of that word. E.g. 'cotton' comes from French 'coton' which originally comes from Arabic 
'qatn' so to speak.

Disclaimer: I am a language hobbyist and do not make any authoritative linguistic claims 


---

### Stack

---
Haskell
 * Haskell2010
 * Framework: Spock
 * Build tool: Stack

JS
 * Framework: Vue.js

Databases
 * Sqlite3

---
### Running a local instance

---
##### Requirements
 * Haskell2010
 * Sqlite3

##### Setup Dev Environment (OS X)
1. Check out latest version of master branch
 ```
 git pull https://github.com/agannon/word-etymology
 cd word-etymology
 git checkout master
 ```
2. Install Stack
 ```
 curl -sSL https://get.haskellstack.org/ | sh
 ```

3. Build app
 ```
 cd word-etymology
 stack ghci
 main
 ```
 main will call Main.hs and compile the app, instantiate the database and
 run the server on port 8080. When that finishes, exit using Ctrl-C

4. Populate Database
 In the ghci
 ```haskell
 contents <- readFile "data/1000_most_common_words.csv"
 parsed = parseData contents
 importData parsed "api.db"
 ```

---
### Short API Guide
To view api (get only):
run `main` in ghci and go to `localhost:8080/api/words/`

Currently supported searches are by database primary key:
`8080/api/words/1`,

and 'LIKE' partial matcher in SQL
`8080/api/words?langBranch=%german%`

Current key search words are: 'langBranch', 'originLang', 'langPath', and 'branchLang'
Which are defined in Main.hs

---

### How to view completed app

Run `main` in ghci and go to `localhost:8080/home/`
Highcharts.js pie chart will be visible with options to display either the origin language
or origin language branch, and with options to filter based on origin language/branch and 
path to English

Below that is a paginated table detailing all the fields retrieved by the api.
Sortable in each column except 'Path to English'

---
### Possible Next Steps
1. Refactor JS out of views/home.html into a Vue single file
2. Refactor Main.hs to move importer functions to another module
3. Update JS sorters to account for diacritics

The app is complete as far as I am concerned but may make improvements