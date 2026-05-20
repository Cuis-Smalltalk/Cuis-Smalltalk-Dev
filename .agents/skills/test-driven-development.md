# Skill: test-driven-development

## Cuándo cargar este skill

Cargar cuando el trabajo implica un cambio de comportamiento, nueva funcionalidad, o corrección de un bug.

---

## Flujo

### 1. Tidy First check

Antes de escribir cualquier test, revisar el código existente:

- ¿Hay nombres confusos, código duplicado, métodos demasiado largos?
- Si hay limpieza obvia y de bajo riesgo → hacerla primero en un commit separado.
- Si no hay limpieza → declararlo explícitamente: "No hay cleanup pendiente, continúo con TDD."

### 2. Red — Escribir el test que falla

- Escribir el test más pequeño y específico que capture el comportamiento deseado.
- Ejecutar el test y verificar que falla **por la razón correcta** (no por un error de setup).
- En Cuis: crear o abrir la TestCase correspondiente, agregar el método `test...`, correr con SUnit.

### 3. Green — Implementar el mínimo

- Escribir el mínimo código necesario para que el test pase.
- No agregar comportamiento extra que no esté cubierto por un test.
- Verificar que **todos** los tests existentes siguen pasando.

### 4. Refactor — Limpiar sin cambiar comportamiento

- Con los tests en verde, mejorar la estructura del código.
- Renombrar, extraer métodos, eliminar duplicación.
- Correr los tests después de cada cambio estructural.

### 5. Repetir

- Volver al paso 2 para el siguiente comportamiento.

---

## Reglas invariantes

- Nunca escribir código de producción sin un test rojo previo.
- Nunca refactorizar con tests en rojo.
- Si un test es difícil de escribir, es una señal de diseño, no un problema del test.
- Commits frecuentes: al menos uno por ciclo Red/Green/Refactor completado.

---

## En Cuis Smalltalk

- Los tests viven en clases que heredan de `TestCase`.
- Cada método de test comienza con `test`.
- Correr una suite: seleccionar la clase y usar el menú de SUnit o `MyTestCase suite run`.
- Para tests de un método específico: `MyTestCase run: #testAlgo`.
- El setup compartido va en `setUp`; el teardown en `tearDown`.
