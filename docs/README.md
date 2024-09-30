# FOFI Language Documentation

## Table of Contents

1. [Introduction](#introduction)
2. [Data Types](#data-types)
3. [Variables](#variables)
4. [Operators](#operators)
5. [Control Structures](#control-structures)
6. [Built-in Functions](#built-in-functions)
7. [Program Structure](#program-structure)
8. [Comments](#comments)
9. [Examples](#examples)

## Introduction

FOFI (Framework Orientado para Facilitação Inclusiva) is a simple, statically-typed programming language designed for educational purposes. It features a straightforward syntax with elements from both imperative and functional programming paradigms.

## Data Types

FOFI supports three basic data types:

1. `numero`: Represents integer values
2. `binario`: Represents boolean values (true or false)
3. `texto`: Represents string values

Example:
```fofi
numero x;
binario isValid;
texto message;
```

## Variables

Variables in FOFI must be declared before use. The syntax for variable declaration is:

```fofi
<type> <variable_name>[, <variable_name>]*;
```

Variables are assigned values using the `:` operator.

Example:
```fofi
numero count, max;
count : 0;
max : 100;

texto greeting;
greeting : "Hello, FOFI!";

binario flag;
flag : v;  // 'v' represents true, 'f' represents false
```

## Operators

### Arithmetic Operators

- `+`: Addition
- `-`: Subtraction
- `*`: Multiplication
- `/`: Integer division
- `%`: Modulo (remainder)
- `^`: Exponentiation

Example:
```fofi
numero a, b, c;
a : 10;
b : 3;
c : a + b * 2 - (a % b) ^ 2;
```

### Relational Operators

- `=`: Equal to
- `!=`: Not equal to
- `>`: Greater than
- `<`: Less than
- `>=`: Greater than or equal to
- `<=`: Less than or equal to

Example:
```fofi
binario result;
result : (a > b) e (c <= d);
```

### Logical Operators

- `e`: Logical AND
- `ou`: Logical OR
- `nao`: Logical NOT

Example:
```fofi
binario x, y, z;
z : (x ou y) e (nao x);
```

## Control Structures

### If-Else Statement

Syntax:
```fofi
se (<condition>) {
    <statements>
} senao {
    <statements>
}
```

The `senao` block is optional.

Example:
```fofi
se (x > 0) {
    mostrar("Positive");
} senao {
    mostrar("Non-positive");
}
```

### While Loop

Syntax:
```fofi
enquanto (<condition>) {
    <statements>
}
```

Example:
```fofi
numero i;
i : 0;
enquanto (i < 5) {
    mostrar(i);
    i : i + 1;
}
```

### Repeat Loop

Syntax:
```fofi
repita (<count>) {
    <statements>
}
```

Example:
```fofi
repita (3) {
    mostrar("Hello");
}
```

## Built-in Functions

FOFI provides several built-in functions for input, output, and other operations.

### Input Functions

1. `ler_numero(msg)`: Reads an integer from user input
2. `ler_binario(msg)`: Reads a boolean from user input
3. `ler()`: Reads input from a special input device
4. `consultar()`: Non-blocking function to check for input

Example:
```fofi
numero age;
age : ler_numero("Enter your age: ");

binario consent;
consent : ler_binario("Do you agree? ");

numero key;
key : ler();

numero lastInput;
lastInput : consultar();
```

### Output Functions

1. `mostrar(value)`: Displays a value on the screen

Example:
```fofi
mostrar("Hello, World!");
mostrar(42);
mostrar(v);
```

### Graphics and Animation Functions

1. `criar_figura(tipo, cor, x, y, tamanho)`: Creates a shape
2. `criar_imagem(arq, x, y)`: Creates an image
3. `colidiu(ref1, ref2)`: Checks if two objects collide
4. `mover(ref, dx, dy)`: Moves an object
5. `destacar(ref)`: Highlights an object
6. `reverter_destaque()`: Removes highlighting

Example:
```fofi
numero circle;
circle : criar_figura("circulo", "#FF0000", 100, 100, 50);

numero img;
img : criar_imagem("background.png", 0, 0);

binario collision;
collision : colidiu(circle, img);

mover(circle, 10, 20);
destacar(circle);
reverter_destaque();
```

### Miscellaneous Functions

1. `aleatorio(min, max)`: Generates a random number
2. `limpar()`: Clears the screen
3. `inicializar_com_cor(cor)`: Sets background color
4. `inicializar_com_imagem(arq)`: Sets background image
5. `tocar(arq)`: Plays an audio file
6. `esperar(t)`: Pauses execution for t milliseconds

Example:
```fofi
numero rand;
rand : aleatorio(1, 10);

limpar();
inicializar_com_cor("#FFFFFF");
tocar("music.mp3");
esperar(1000);
```

## Program Structure

A FOFI program consists of three main parts:

1. Program description
2. Variable declarations
3. Main code block

Example:
```fofi
programa
"This is a simple FOFI program"

var
numero x, y;
texto message;

{
    x : 5;
    y : 10;
    message : "Hello, FOFI!";
    mostrar(message);
    mostrar(x + y);
}
```

## Comments

FOFI supports two types of comments:

1. Single-line comments: Start with `#`
2. Multi-line comments: Enclosed between `/# #/`

Example:
```fofi
# This is a single-line comment

/# This is a
   multi-line comment #/
```

## Examples

### Example 1: Simple Calculator

```fofi
programa
"Simple Calculator"

var
numero a, b, result;
texto operation;

{
    a : ler_numero("Enter first number: ");
    b : ler_numero("Enter second number: ");
    operation : ler_texto("Enter operation (+, -, *, /): ");

    se (operation = "+") {
        result : a + b;
    } senao se (operation = "-") {
        result : a - b;
    } senao se (operation = "*") {
        result : a * b;
    } senao se (operation = "/") {
        se (b != 0) {
            result : a / b;
        } senao {
            mostrar("Error: Division by zero");
            result : 0;
        }
    } senao {
        mostrar("Error: Invalid operation");
        result : 0;
    }

    mostrar("Result: ");
    mostrar(result);
}
```

### Example 2: Simple Game

```fofi
programa
"Simple Game: Catch the Circle"

var
numero player, target, score;
binario gameOver;

{
    inicializar_com_cor("#000000");
    player : criar_figura("quadrado", "#00FF00", 200, 200, 20);
    target : criar_figura("circulo", "#FF0000", 100, 100, 10);
    score : 0;
    gameOver : f;

    enquanto (nao gameOver) {
        numero input;
        input : consultar();

        se (input = 1) { mover(player, 0, -10); }  # Up
        se (input = 2) { mover(player, 0, 10); }   # Down
        se (input = 3) { mover(player, -10, 0); }  # Left
        se (input = 4) { mover(player, 10, 0); }   # Right

        se (colidiu(player, target)) {
            score : score + 1;
            mostrar("Score: ");
            mostrar(score);
            mover(target, aleatorio(0, 400), aleatorio(0, 400));
        }

        se (score >= 10) {
            gameOver : v;
        }

        esperar(50);
    }

    mostrar("Game Over! Final Score: ");
    mostrar(score);
}
```

This documentation provides a comprehensive overview of the FOFI language, including its syntax, data types, control structures, and built-in functions. The examples demonstrate how to use various language features in practical scenarios.