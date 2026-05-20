# AGENTS.md — Cuis-Smalltalk-Dev

Este es el archivo canónico de instrucciones para agentes de IA en este repositorio.
`CLAUDE.md` y archivos similares por compatibilidad apuntan aquí.

---

## Contexto del proyecto

Este repositorio es el entorno de desarrollo de **Cuis Smalltalk**, un sistema Smalltalk limpio y minimalista.
El trabajo principal es desarrollo en Smalltalk: código fuente en `.st`, imágenes Cuis, paquetes, scripts de CI.

---

## Reglas esenciales

- **Tidy First y TDD son el núcleo.** Todo cambio de comportamiento pasa por TDD. Toda sesión empieza con un chequeo Tidy First.
- **Verificar afirmaciones técnicas antes de acordar.** No confirmar sin evidencia.
- **Antes de tocar código, hacer el chequeo Tidy First:**
  - Si hay limpieza pendiente, hacerla primero y mantenerla en un commit separado.
  - Si no hay limpieza, decirlo explícitamente y continuar.
- **Separar cambios estructurales de cambios de comportamiento** siempre que sea posible.
- **Preferir cambios pequeños, reversibles y bien verificados.**
- **Declarar claramente qué se verificó y qué no.**

---

## Cuándo usar cada skill

| Situación | Skill a cargar |
|---|---|
| Cambio de comportamiento o corrección de bug | `test-driven-development` |
| Refactoring paso a paso con tests rápidos y confiables | `tcr-practice` (solo si se pide explícitamente o el trabajo es de pasos muy pequeños) |
| Inicio de feature aislado o antes de ejecutar un plan de implementación | `using-git-worktrees` |
| Refactoring en Smalltalk/Cuis | `smalltalk-refactoring` |
| Trabajo con SUnit, suites de tests, cobertura | `sunit-testing` |

---

## Metodologías

### Tidy First
- Revisar siempre si el código necesita limpieza estructural antes de agregar comportamiento.
- Cleanup va en un commit propio, nunca mezclado con cambios funcionales.
- Si no hay cleanup, decirlo en voz alta antes de continuar.

### Test-Driven Development (TDD)
- Red → Green → Refactor.
- Escribir el test primero, hacerlo fallar por la razón correcta, luego implementar el mínimo necesario.
- Los tests son la especificación; el código los cumple.

### TCR / TRC
- `test && commit || revert` para pasos de refactoring muy pequeños.
- Usar solo cuando los tests son rápidos y de confianza total.
- No usar para exploración o trabajo con tests lentos.

### Trunk Based Development
- Integrar frecuentemente a `main` / `master`.
- Branches de corta duración o features aislados via worktrees.
- No acumular trabajo no integrado.

---

## Skills disponibles

Los skills portables viven en `.agents/skills/`.

- [`test-driven-development`](.agents/skills/test-driven-development.md)
- [`tcr-practice`](.agents/skills/tcr-practice.md)
- [`using-git-worktrees`](.agents/skills/using-git-worktrees.md)
- [`smalltalk-refactoring`](.agents/skills/smalltalk-refactoring.md)
- [`sunit-testing`](.agents/skills/sunit-testing.md)

---

## Convenciones de Git

- Commits atómicos con mensaje conciso en inglés o español (consistente con el historial del repo).
- Nunca mezclar cleanup con comportamiento en el mismo commit.
- Nunca hacer force-push a `master` sin consenso explícito.
- Revisar `git status` y `git diff` antes de hacer commit.

---

## Notas sobre el entorno

- **OS**: macOS (darwin), Shell: zsh
- **Smalltalk**: Cuis Smalltalk (imagen `.image` en `CuisImage/`)
- **VM**: `CuisVM.app/` para macOS
- **Scripts de inicio**: `RunCuisOnMac.sh`, `RunCuisOnLinux.sh`, `RunCuisOnWindows.bat`
- **CI**: `.ContinuousIntegrationScripts/`, `.travis.yml`, `.github/`
