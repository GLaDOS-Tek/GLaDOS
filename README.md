# GLaDOS Project - Felyn

Welcome to the GLaDOS Project repository!

## The projet

This project is a compiler for a fictional language we invented, the Felyn.  
We used Haskell and Stack.


## How to code in Felyn ?

Felyn is a functional language with a syntax similar to C.

For details, we made a [BNF](Felyn.bnf) (Backus–Naur Form) documentation.

## Example

```
cat division (a b) {
  if (b == 0) {
    vomit ("division by 0 forbidden" 84)
  } else {
    a / b
  }
}

feed (a 10)
division (a 2)
```

Output :
```5```
## Credits



An Epitech project by  
Sacha TOPALOFF - Adam BENTALEB - Arthur BOURDIN - Antoine GIRARD - Stanislas COMMENGE
