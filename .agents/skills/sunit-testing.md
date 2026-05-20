# Skill: sunit-testing

## Cuándo cargar este skill

Cargar cuando el trabajo involucra:
- Escribir o mantener tests en Cuis con SUnit.
- Organizar suites de tests.
- Diagnosticar tests que fallan o son frágiles.
- Agregar cobertura a código existente.

---

## Estructura básica de SUnit en Cuis

### Crear una TestCase

```smalltalk
TestCase subclass: #MiClaseTest
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'MiPaquete-Tests'
```

### Métodos de ciclo de vida

```smalltalk
setUp
    "Se ejecuta antes de cada test. Inicializar estado compartido aquí."
    objeto := MiClase new.

tearDown
    "Se ejecuta después de cada test. Limpiar recursos si es necesario."
```

### Escribir un test

```smalltalk
testSumaDeDosMasDosDaCuatro
    | resultado |
    resultado := objeto sumar: 2 con: 2.
    self assert: resultado equals: 4.
```

---

## Aserciones disponibles

| Aserción | Uso |
|---|---|
| `self assert: condicion` | Verifica que la condición es true |
| `self assert: valor equals: esperado` | Verifica igualdad |
| `self deny: condicion` | Verifica que la condición es false |
| `self should: [ bloque ] raise: ExcepcionClass` | Verifica que se lanza una excepción |
| `self assertEmpty: coleccion` | Verifica que la colección está vacía |
| `self assert: coleccion notEmpty` | Verifica que no está vacía |

---

## Correr tests en Cuis

```smalltalk
"Correr toda la suite de una clase"
MiClaseTest suite run.

"Correr un test específico"
MiClaseTest run: #testAlgo.

"Correr desde el browser: seleccionar la clase y usar el menú SUnit"
```

Desde la terminal (CI):
```bash
 Run the CI test script in headless mode:

 `"$CUIS_VM" $CUIS_VM_ARGS CuisImage/<your-image>.image -s .github/scripts/run-tests.st`
```

---

## Organización de suites

- Una clase `TestCase` por clase de producción (convención: `NombreClaseTest`).
- Agrupar tests relacionados en la misma `TestCase`.
- Si la TestCase crece demasiado, dividir por comportamiento o escenario.
- Los tests deben ser independientes entre sí: el orden de ejecución no debe importar.

---

## Tests buenos vs. frágiles

**Tests buenos:**
- Tienen un nombre que describe el comportamiento esperado.
- Testean comportamiento observable, no implementación interna.
- Son rápidos (milisegundos).
- No dependen de estado global o del orden de ejecución.

**Señales de tests frágiles:**
- El test falla cuando se cambia código no relacionado.
- El test accede a variables de instancia privadas directamente.
- El test requiere un orden específico de ejecución.
- El test es lento porque accede a disco, red, o la imagen completa.

---

## Diagnosticar un test que falla

1. Leer el mensaje de error completo.
2. Verificar si falla por la **razón correcta** (comportamiento ausente) o por un error de setup.
3. Correr el test en aislamiento para descartar interferencia de otros tests.
4. Si el fallo es en `setUp`, revisar el estado inicial.
5. Agregar temporalmente `Transcript show: ...` para inspeccionar valores si es necesario.

---

## Convenciones en este repo

- Los tests viven junto al código que testean, en la misma categoría con sufijo `-Tests`.
- Los nombres de método siguen `test<Comportamiento><Escenario>`.
- Cada test tiene exactamente una razón para fallar.
