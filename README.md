# Mass Terms and Parser Combinators
## by Jack Valinsky


### Files:
- src/Model          &rarr; defines individual and mass types and predicates
- src/EF2synShort.hs &rarr; syntax definition for English fragment
- src/EF3semShort.hs &rarr; how to evaluate truth value of a synactic type given a model
- src/Parser/hs      &rarr; how to parse strings to syntactic types and evaluate them
- app/Main.hs        &rarr; demo showing evaluation of different sentences


### How to build demo:
- <code> stack build && stack exec plurals-lin268-exe </code>
- Alternatively one can just load all the files in src/ in the repl using <code> stack ghci </code>




