# Lenguajes-De-Programacion

## Rust

Rust utiliza módulos y crates (cajas).

## Tipos

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

## Control

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
let mut sum = 0;
for &num in numbers {
        sum += num;
        if sum >= 10 {
            break;
        }
    }
```

> En este código se hace referencia a `num` por un puntero a esa variable.

### While

```rust

    while count < 5 {
        println!("El contador es: {}", count);
        count += 1; // Incrementa el contador en 1 en cada iteración
    }
```

### Loop (Do While)

```rust
    loop {
        println!("El contador es: {}", count);
        count += 1;

        if count >= 5 {
            break; // Salir del bucle cuando el contador alcance 5
        }
    }
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

## Dependencias

Para gestionar las dependencias se usa una herramienta llamada Cargo. Que es como Maven para java.

## Testing
