# Word Etymologies

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
`8080/api/words?branch=%german%`

Current key search words are: 'branch', 'origin', 'path', and 'blang'

---
### Next Steps
1. Incorporate Vue for the Single Page Home
2. Create helper functions and port them to JS using GHCJS
3. Add support for Postgres db
4. Refactor Main.hs to move importer functions to another module