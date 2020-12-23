# LCC Language Definition

## Expression Grammar:

BINDING = 'a' .. 'z'
EXP = VAR BINDING | ABS BINDING EXP | CURRIED [BINDING] EXP | APPLICATION 

## Reduction & Conversion

Stages: Parsing -> Reducing: Substitution -> Alpha -> Beta

(\x.\y. x (\a.\x.x))
Application (ABS 'x'(ABS 'y' (Var 'x'))) (ABS 'a' (ABS 'x' (Var 'x')))
Application needs binding deduplication, so alpha-conversion with checking all used bindings
so we 

Question #A: What symbols/bindings should be allowed? what constants/identifiers should be allowed? 

Finding #1: Currying is exlusive recursive usage of abstraction so: 
n curryable terms are: 
... M (ABS '#1' (ABS '#2' (... (ABS '#n')))) N ...
where M and N are valid lambda terms
Here we can now search lambda terms for this structure
Maybe Curried Expression Token?
Note to myself: This gonna be a huge refactor
What instance is fitting this operation? When is this operation taking place? After parsing?
Curryable instance? Type-Level Checking?

Finding #2: Term equality should be programmable with debrujin-indeces

Finding #1.1: Is application parentheses avoidance a similar case with exclusive recursive usage of application in the right-hand-side?

Finding #1.2: i should always transform the expression after parsing i think
