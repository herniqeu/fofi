# FOFI Compiler

A high-performance, functionally-pure compiler for the FOFI language, implemented in Haskell.

## Synopsis

This project implements a full-fledged compiler for the FOFI language, employing cutting-edge techniques in compiler design and functional programming. It utilizes monadic parser combinators, algebraic data types, and lazy evaluation to achieve a highly modular and efficient compilation process.

## Features

- Lexical analysis using deterministic finite automata (DFA)
- Recursive descent parsing with monadic combinators
- Advanced semantic analysis with polymorphic type inference
- Intermediate representation using continuation-passing style (CPS)
- Optimized code generation targeting JavaScript

## Installation

Ensure you have GHC (Glasgow Haskell Compiler) and Cabal installed. Then run:

```bash
cabal update
cabal install --only-dependencies
cabal build