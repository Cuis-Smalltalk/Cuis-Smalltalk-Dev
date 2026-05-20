# Skill: tcr-practice

## Cuándo cargar este skill

Cargar **solo** cuando:
- Se pide explícitamente TCR o TRC.
- El trabajo es refactoring en pasos muy pequeños.
- Los tests son rápidos (segundos) y de total confianza.

**No usar** para exploración, trabajo con tests lentos, o cuando el comportamiento es incierto.

---

## Qué es TCR

`test && commit || revert`

Si los tests pasan → commit automático del cambio.
Si los tests fallan → revert automático, volver al último estado verde.

La variante TRC invierte el orden: `test || revert && commit` (commit solo si falla, útil para escribir tests primero).

---

## Flujo

### Antes de empezar

1. Verificar que todos los tests pasan en el estado actual.
2. Identificar el refactoring objetivo: debe ser un paso pequeño y reversible.
3. Tener el comando de tests listo y medido (debe ser rápido).

### Ciclo TCR

1. Hacer **un solo cambio pequeño** (renombrar, extraer método, mover código).
2. Correr los tests.
   - Verde → commit con mensaje descriptivo.
   - Rojo → revert inmediato, repensar el paso.
3. Repetir.

### Comando base (adaptar según el runner)

```bash
# Si los tests pasan, commit; si no, revert
<run-tests> && git add -A && git commit -m "<mensaje>" || git checkout -- .
```

---

## Reglas invariantes

- Cada paso debe ser tan pequeño que el revert no duele.
- No acumular varios cambios antes de correr los tests.
- Si el revert ocurre más de dos veces en el mismo punto → el paso es demasiado grande, dividirlo.
- Los mensajes de commit deben describir el cambio estructural, no el resultado ("Extract #calculateTotal" no "Fix tests").

---

## En Cuis Smalltalk

- Los tests se corren desde la imagen o desde scripts CI en `.ContinuousIntegrationScripts/`.
- Para TCR interactivo en la imagen, correr la suite manualmente y hacer commit/revert desde la terminal según el resultado.
- Preferir un script shell que encapsule el ciclo cuando sea posible.
