# Skill: smalltalk-refactoring

## Cuándo cargar este skill

Cargar cuando el trabajo es refactoring de código Smalltalk en Cuis:
renombrar, extraer métodos, mover responsabilidades, simplificar jerarquías.

---

## Principios

- **Tidy First siempre:** antes de refactorizar, verificar que los tests existen y pasan.
- **Un refactoring a la vez:** no mezclar varios cambios estructurales.
- **Tests como red de seguridad:** si no hay tests, escribirlos antes de refactorizar.
- **Commits atómicos:** un commit por refactoring aplicado.

---

## Catálogo de refactorings comunes en Smalltalk

### Rename Method
- Renombrar un mensaje para que comunique mejor su intención.
- Crear el nuevo método que delega al viejo, actualizar todos los senders, eliminar el viejo.
- En Cuis: usar el browser para buscar senders (`Browse senders of...`).

### Extract Method
- Extraer un fragmento de código en un nuevo método con nombre descriptivo.
- El nuevo método debe tener una responsabilidad única y clara.
- Verificar que los tests siguen pasando después de la extracción.

### Move Method
- Mover un método a la clase que tiene los datos que usa.
- Identificar el "feature envy" (el método usa más datos de otra clase que de la propia).

### Extract Class
- Cuando una clase tiene demasiadas responsabilidades, extraer una nueva clase.
- Guía: si el nombre de la clase requiere "y" para describirse, probablemente son dos clases.

### Inline Method
- Cuando un método es tan simple que su cuerpo es más claro que su nombre.
- Reemplazar el envío de mensaje por el cuerpo directamente.

### Replace Temp with Query
- Reemplazar una variable temporal por un método que la calcula.
- Mejora la legibilidad y permite reutilización.

### Introduce Parameter Object
- Cuando varios parámetros siempre viajan juntos, encapsularlos en un objeto.

---

## Proceso paso a paso

1. **Identificar** el smell o la oportunidad de mejora.
2. **Verificar** que hay tests que cubren el código a refactorizar.
3. **Declarar** el refactoring a aplicar (nombre del refactoring).
4. **Aplicar** el cambio mínimo.
5. **Correr** los tests.
6. **Commit** si verde, revert si rojo.
7. **Repetir** para el siguiente refactoring.

---

## Smells comunes en Smalltalk

- Métodos largos (más de 7-10 líneas): candidato a Extract Method.
- Clases con muchos métodos de acceso a datos de otras clases: Feature Envy.
- Nombres como `doIt`, `process`, `handle`: Rename Method.
- Métodos con muchos parámetros: Introduce Parameter Object.
- Duplicación entre clases hermanas: Extract Superclass o Extract Method + Move.

---

## Herramientas en Cuis

- **System Browser**: navegar clases y métodos.
- **Senders / Implementors**: encontrar todos los usos de un mensaje.
- **Refactoring Browser** (si disponible): automatizar algunos refactorings.
- **SUnit**: verificar que los tests pasan después de cada cambio.
