# Lenguajes-De-Programacion

## Rust

Rust utiliza módulos y crates (cajas).

## Variables

Existen distintos tipos de datos. Se pueden ver en la [documentación.](https://doc.rust-lang.org/book/ch03-02-data-types.html "Rust Data Type Documentation")

```rust
let num = 1;
```

```rust
let enumerate = Color::Red;
```

```rust
let person = Person {
        age: 25,
        name: String::from("Alice"),
    };
```

```rust
let person_weight = Measurement::Weight(70.5);
```

```rust
let vec = vec![1, 2, 3];
```

```rust
let operacion: fn(i32, i32) -> i32;
```

```rust
let y = {
        let x = 3;
        x + 1
    };
```

También puede usarse la palabra reservada `return`

```rust
fn ejemplo_condicion(x: i32) -> i32 {
    if x > 10 {
        return x * 2; // Devuelve tempranamente si x es mayor que 10
    }
    x + 1 // Esta expresión también es un valor de retorno
}
```

Estas variables son inmutables. Una vez que se les asigna un valor, no se las puede sobreescribir. Para poder hacer hacer eso las variables hay que declararlas con el prefijo `mut`.

Este tipo de variables puede o no tener una asignación a continuación. Pero si o si no puede modificarse luego de asignarse.

```rust
let mut num = 1;
```

Por otro lado existen las constantes. Las constantes deben asignarse en el momento en que se declaran y su valor no puede ser el del resultado de una función en runtime. Su alcance o scope puede ser global, por lo que es util para utilizarlos a lo largo de todo el programa. Las constantes deben tener declarado el tipo.

```rust
const THREE_HOURS_IN_SECONDS: u32 = 60 * 60 * 3;
```

## Eclipsamiento (Shadowing)

Existe eclipsamiento, por lo que una variable de un scope de más alto nivel, puede ser eclipsada por una variable de más bajo nivel.

```rust
fn main() {
    let x = 5;

    let x = x + 1;

    {
        let x = x * 2;
        println!("The value of x in the inner scope is: {x}");
    }

    println!("The value of x is: {x}");
}
```

Una variable puede ser sobreescrita cambiando su tipo en un mismo scope, pero no es recomendable. En el siguiente ejemplo se transforma una variable que es un string a una variable que es un número.

```rust

    let spaces = "   ";
    let spaces = spaces.len();
```

## Tipado

El tipado de Rust es estático. Esto quiere decir que el compilador debe conocer el tamaño de todas las variables en momento de compilación. El compilador aveces puede inferir tipos. Aveces hay que indicarselo o darle más datos en caso de que no pueda inferirlo.

```rust
let guess: u32 = "42".parse().expect("Not a number!");
```

## Tipos de Datos

### Integer

| Length  | Signed | Unsigned |
| ------- | ------ | -------- |
| 8-bit   | i8     | u8       |
| 16-bit  | i16    | u16      |
| 32-bit  | i32    | u32      |
| 64-bit  | i64    | u64      |
| 128-bit | i128   | u128     |
| arch    | isize  | usize    |

| Number literals | Example     |
| --------------- | ----------- |
| Decimal         | 98_222      |
| Hex             | 0xff        |
| Octal           | 0o77        |
| Binary          | 0b1111_0000 |
| Byte (u8 only)  | b'A'        |

### Floats

```rust

    let x = 2.0; // f64

    let y: f32 = 3.0; // f32
```

### Numeric Operations

```rust
    // addition
    let sum = 5 + 10;

    // subtraction
    let difference = 95.5 - 4.3;

    // multiplication
    let product = 4 * 30;

    // division
    let quotient = 56.7 / 32.2;
    let truncated = -5 / 3; // Results in -1

    // remainder
    let remainder = 43 % 5;
```

### Boolean

```rust
let t = true;

    let f: bool = false; // with explicit type annotation
```

### Char

```rust
let c = 'z';
    let z: char = 'ℤ'; // with explicit type annotation
    let heart_eyed_cat = '😻';
```

## Compound Types

### Tuple

```rust
let tup: (i32, f64, u8) = (500, 6.4, 1);
```

Para obtener los valores de una tupla podemos usar un pattern maching destructive.

```rust
let tup = (500, 6.4, 1);
let (x, y, z) = tup;
```

O acceder por el índice

```rust
let x: (i32, f64, u8) = (500, 6.4, 1);
let five_hundred = x.0;
let six_point_four = x.1;
let one = x.2;
```

### Arrays

```rust
let a = [1, 2, 3, 4, 5];
```

Se puede definir el tamaño del array

```rust
let a: [i32; 5] = [1, 2, 3, 4, 5];
```

```rust
let a = [3; 5];
// Es análogo a:
let a = [3, 3, 3, 3, 3];
```

Se pueden acceder a las posiciones del arreglo con

```rust

    let first = a[0];
    let second = a[1];
```

## Macros

Existen ciertas funciones que son propias de Rust. Algunos que existen son:


* **assert!** : Verifica una expresión booleana y muestra un mensaje de error si la expresión es falsa.
* **assert_eq!** : Verifica si dos valores son iguales y muestra un mensaje de error si no lo son.
* **assert_ne!** : Verifica si dos valores no son iguales y muestra un mensaje de error si son iguales.
* **dbg!** : Imprime el valor de una expresión junto con su ubicación en el código para ayudar en la depuración.
* **format!** : Formatea y crea una cadena de texto utilizando argumentos similares a `println!`.
* **println!** : Imprime una cadena de texto formateada en la consola.
* **eprintln!** : Similar a `println!`, pero imprime en la salida de error estándar.
* **panic!** : Genera un pánico y muestra un mensaje de error personalizado.
* **unreachable!** : Marca una parte del código como inalcanzable, lo que generará un pánico si se ejecuta.
* **vec!** : Crea un vector con elementos proporcionados.
* **hash_map!** : Crea un HashMap con pares clave-valor proporcionados.
* **cfg!** : Evalúa condiciones de configuración en tiempo de compilación.
* **file!** : Devuelve el nombre del archivo actual en tiempo de compilación.
* **line!** : Devuelve el número de línea actual en tiempo de compilación.
* **concat!** : Concatena literales de cadena en una sola cadena.
* **concat_idents!** : Concatena identificadores para crear uno nuevo.
* **include!** : Incluye contenido de un archivo en el código fuente.
* **include_str!** : Incluye el contenido de un archivo como una cadena.
* **env!** : Accede a variables de entorno en tiempo de compilación.
* **stringify!** : Convierte un token en su representación de cadena.

## Expresiones de Control

```rust
    if number > 5 && number < 10 {
        println!("El número está entre 5 y 10.");
    } else {
        println!("El número no cumple la condición.");
    }
```

## Iteraciones

### For

```rust
let a = [10, 20, 30, 40, 50];

    for element in a {
        println!("the value is: {element}");
    }
```

```rust
let mut sum = 0;
for &num in numbers {
        sum += num;
        if sum >= 10 {
            break;
        }
    }
```

> En este código se hace referencia a `num` por un puntero a esa variable.

En este código se hace un for de un arreglo de numeros del 4 al 1. Porque la función rev invierte el arreglo.

```rust
for number in (1..4).rev() {
        println!("{number}!");
    }
```

### While

```rust

    while count < 5 {
        println!("El contador es: {}", count);
        count += 1; // Incrementa el contador en 1 en cada iteración
    }
```

### Loop (While true)

```rust
    loop {
        println!("El contador es: {}", count);
        count += 1;

        if count >= 5 {
            break; // Salir del bucle cuando el contador alcance 5
        }
    }
```

```rust
let mut counter = 0;

    let result = loop {
        counter += 1;

        if counter == 10 {
            break counter * 2;
        }
    };
```

Se pueden usar etiquetas o labels para los loops. En este caso podemos usar palabras reservadas como `break` y `continue` sobre cada uno de los loops de forma independiente. Por defecto aplica sobre los loops internos.

```rust
let mut count = 0;
    'counting_up: loop {
        println!("count = {count}");
        let mut remaining = 10;

        loop {
            println!("remaining = {remaining}");
            if remaining == 9 {
                break;
            }
            if count == 2 {
                break 'counting_up;
            }
            remaining -= 1;
        }

        count += 1;
    }
    println!("End count = {count}");
```

### Match (Switch Case)

```rust
    match numero {
        1 => println!("Es uno"),
        2 => println!("Es dos"),
        3 => println!("Es tres"),
        _ => println!("No es uno, dos ni tres"),
    }
```

## Funciones

```rust
// Definición de una función sin parámetros ni valor de retorno
fn saludar() {
    println!("¡Hola!");
}

// Definición de una función con parámetros y valor de retorno
fn sumar(a: i32, b: i32) -> i32 {
    a + b
}

// Definición de una función con funciónes como parámetros 
fn suma(a: i32, b: i32) -> i32 {
    a + b
}

fn resta(a: i32, b: i32) -> i32 {
    a - b
}

fn operar(func: fn(i32, i32) -> i32, a: i32, b: i32) -> i32 {
    func(a, b)
}


```

## Struct

El struct es la declaración de un tipo.

```rust
struct Persona {
    nombre: String,
    edad: u32,
}
```

El tipo luego puede implementarse.

También puede instanciarse como las implementaciones.

```rust
let persona1 = Persona {
        nombre: String::from("Alice"),
        edad: 30,
    };
```

### Implementations

La implementación se hace sobre un tipo struct. La implementación contiene las funciones (métodos) que puede tener una struct.

```rust
impl Persona {
    fn nueva(nombre: &str, edad: u32) -> Persona {
        Persona {
            nombre: nombre.to_string(),
            edad,
        }
    }

    fn presentarse(&self) {
        println!("Hola, soy {} y tengo {} años.", self.nombre, self.edad);
    }
}
```

Podría utilizarse de la siguiente manera

```rust
let persona = Persona::nueva("Alice", 30);
persona.presentarse();
```

## Traits

Son como interfaces para declarar las funciones.

```rust
trait Saludable {
    fn saludar(&self);
}
```

## Modulos

Los módulos se exportan y se indica que función se pueden acceder desde fuera.

```rust
pub mod my_module {
    pub fn suma(a: i32, b: i32) -> i32 {
        a + b
    }
}

pub use self::my_module::suma;
```

Acá se exporta el módulo `my_module` y se indica que se puede utilizar la función `suma`. Se pueden exportar varias funciones y agrupar varias funciones dentro de un módulo. Las funciones expuestas deben tener el perfijo `pub` de publico.

```rust
pub mod my_module {
    pub fn suma(a: i32, b: i32) -> i32 {
        a + b
    }

    pub fn resta(a: i32, b: i32) -> i32 {
        a - b
    }
}

pub use self::my_module::{suma,resta};
```

## Packaging

### Module

Es un conjunto de funcionalidades agrupadas.

```rust
// src/mi_modulo.rs
pub mod mi_submodulo {
    pub fn mi_funcion() {
        // Implementación de la función
    }
}
```

### Crates

```rust
// src/mi_biblioteca/mod.rs
pub mod mi_submodulo {
    pub fn mi_funcion() {
        // Implementación de la función
    }
}
```

Es un conjunto de código. Un crate puede ser una librería o no. Cuando tengo una carpeta, tengo un crate. Puedo tener un crate dentro de otro crate. Existe la palabra reservada `crate` para el crate por defecto del workspace.

### Independent Libibrary

La idea es crear una librería que pueda ser compartida con otros proyectos.

```rust
// mi_libreria/Cargo.toml
[package]
name = "mi_libreria"
version = "0.1.0"

[dependencies]

// mi_libreria/src/lib.rs
pub mod mi_modulo {
    pub fn mi_funcion() {
        // Implementación de la función
    }
}
```

### Packages o **Workspace**

Los paquetes se crean cuando un crate está asociado a un Cargo.toml. Es decir

```rust
my_workspace/
├── Cargo.toml
├── crate1/
│   ├── Cargo.toml
│   ├── src/
│   │   └── ...
├── crate2/
│   ├── Cargo.toml
│   ├── src/
│   │   └── ...
└── ...
```

## Dependencias

Para gestionar las dependencias se usa una herramienta llamada Cargo. Que es como Maven para java.

## Testing

### Unit Testing

Las pruebas unitarias por convensión van dentro de los mismos archivos que prueban. Esto es por convensión de Rust.

```rust
pub mod my_module {
    pub fn suma(a: i32, b: i32) -> i32 {
        a + b
    }
}

pub use self::my_module::suma;

#[cfg(test)]
mod unit_tests {
    use super::*;

    #[test]
    fn internal() {
        assert_eq!(4, suma(2, 2));
    }
}
```

Existen ciertas macros que funcionan para el testing.


* **assert!** : Verifica que una expresión sea verdadera. Si la expresión es falsa, la prueba fallará.
  * ```rust

    ```
* **assert_eq!** : Verifica que dos valores sean iguales. Si no son iguales, la prueba fallará.
  * ```rust
    fn suma(a: i32, b: i32) -> i32 {
        a + b
    }

    #[test]
    fn test_suma() {
        assert_eq!(suma(2, 3), 5);
        assert_eq!(suma(-2, 2), 0);
    }
    ```
* **assert_ne!** : Verifica que dos valores no sean iguales. Si son iguales, la prueba fallará.
  * ```rust
    fn es_par(numero: i32) -> bool {
        numero % 2 == 0
    }

    #[test]
    fn test_es_par() {
        assert_ne!(es_par(5), true);
        assert_ne!(es_par(4), false);
    }
    ```
* **should_panic** : Anota una prueba para que se considere exitosa si una función "panica" (genera un pánico). Puedes usar atributos como `expected` para verificar que el mensaje de pánico coincida con lo esperado. Verifica una excepción en una sentencia.
  * ```rust
    fn dividir(a: i32, b: i32) -> i32 {
        if b == 0 {
            panic!("No se puede dividir por cero");
        }
        a / b
    }

    #[test]
    #[should_panic(expected = "No se puede dividir por cero")]
    fn test_dividir_por_cero() {
        dividir(10, 0);
    }
    ```
* **#[test]** : Atributo que marca una función como una prueba unitaria. Las funciones marcadas con este atributo son ejecutadas cuando ejecutas las pruebas.
  * ```rust
    fn es_mayor_de_edad(edad: u32) -> bool {
        edad >= 18
    }

    #[test]
    fn test_es_mayor_de_edad() {
        assert!(es_mayor_de_edad(20));
        assert!(!es_mayor_de_edad(15));
    }
    ```
* **#[cfg(test)]** : Anota un módulo como disponible solo cuando se ejecutan pruebas. Puedes colocar pruebas en módulos con este atributo. Ej: Pruebas unitarias.
  * ```rust
    #[cfg(test)]
    mod tests {
        #[test]
        fn test_multiplicar() {
            assert_eq!(multiplicar(3, 4), 12);
        }
    }
    ```
* **#[should_panic]** : Atributo que marca una prueba para que se considere exitosa si la función "panica". Puedes usar atributos como `expected` para verificar el mensaje de pánico. Para verificar Excepciones en un bloque de codigo.
  * ```rust
    #[test]
    #[should_panic(expected = "Este mensaje debe aparecer")]
    fn test_panic_customizado() {
        panic!("Este mensaje debe aparecer");
    }
    ```
* **#[test_case]** : Atributo utilizado para parametrizar pruebas. Permite ejecutar la misma prueba con diferentes entradas y expectativas.
  * ```rust
    #[test_case(2, 3, 5)]
    #[test_case(0, 0, 0)]
    #[test_case(-2, 2, 0)]
    fn test_suma_variaciones(a: i32, b: i32, resultado_esperado: i32) {
        assert_eq!(suma(a, b), resultado_esperado);
    }
    ```

### Integration Testing

Las pruebas unitarias importan lo que se quiere testear y se prueban.

```rust
use first_proyect::suma;

#[test]
fn integration_test() {
    assert_eq!(suma(2, 3), 5);
}
```
