---
name: exportar-paquete
description: Use when the user says "exporta", "finalizá", "andá cerrando", "cerrá", "terminá" or similar closing/export signals. Exports the current Smalltalk package as .pck.st AND commits the Tonel .st files to git. Two outputs: the .pck.st fileout (for loading into Cuis) and the Tonel files (for git versioning).
---

# Skill: exportar-paquete

## Cuándo activar

Palabras clave que disparan este skill:
- "exporta", "exportá", "exportar"
- "finalizá", "finaliza", "finalizando"
- "andá cerrando", "vamos cerrando", "cerrando"
- "terminá", "termina", "listo por hoy"
- "guardá", "guardalo"

## Qué hacer (en orden)

### 1. Tidy First check

Antes de exportar, revisar el estado del trabajo:
- ¿Hay código sin commitear en cuis-tonel?
- ¿Los tests pasaron en la última ejecución?
- Si hay trabajo a medias, pedirle al usuario que confirme antes de exportar.

### 2. Exportar el paquete como .pck.st

El `.pck.st` es el formato fileout de Cuis (Monticello). Se genera desde la imagen.

Desde la imagen Cuis (script headless o instrucción manual):
```smalltalk
| package |
package := CodePackage named: 'Tonel-Support'.
package fileOutOn: FileDirectory default / 'cuis-tonel' / 'dist'.
```

O desde terminal si hay script CI disponible:
```bash
# Desde el repo cuis-tonel
./scripts/export-pck.sh Tonel-Support
```

El archivo resultante va en: `/Users/gaston/Code/cuis-tonel/dist/Tonel-Support.pck.st`

### 3. Verificar los archivos Tonel en packages/

Los `.st` en `packages/` son la fuente de verdad para git. Verificar que estén al día:
```bash
git -C /Users/gaston/Code/cuis-tonel status packages/
```

Si hay archivos modificados o nuevos, deben estar listos para commit.

### 4. Commit de los archivos Tonel

```bash
cd /Users/gaston/Code/cuis-tonel
git add packages/
git status  # revisar qué se está committeando
git diff --cached  # ver el diff
git commit -m "export: update Tonel package files"
```

El mensaje de commit puede ser más específico según el trabajo realizado.

### 5. (Opcional) Push

Solo si el usuario lo pidió explícitamente o si hay un remote configurado.

---

## Estructura de archivos esperada

```
cuis-tonel/
  packages/
    .properties              ← { #format : #tonel }
    Tonel-Support.package/
      TonelWriter.class.st
      TonelStonWriter.class.st
      ...
    Tonel-Support-Tests.package/
      TonelWriterTest.class.st
      ...
  dist/
    Tonel-Support.pck.st     ← fileout para cargar en Cuis
```

---

## Qué se versiona en Git

**En git van los archivos Tonel (`.st` en `packages/`)**, NO la imagen ni el `.pck.st`.

- Los `.st` son texto plano → diffs legibles → revisión de código posible
- El `.pck.st` se regenera desde la imagen cuando se necesita
- La imagen `.image` va en `.gitignore`

---

## Notas

- Si el usuario dice "exportá y pusheá", hacer el push también.
- Si el usuario dice "exportá sin commit", solo generar el `.pck.st`.
- Si hay tests rojos al momento de exportar, advertir al usuario antes de proceder.
- Cada exportación es un punto de integración: los tests deben estar en verde.
