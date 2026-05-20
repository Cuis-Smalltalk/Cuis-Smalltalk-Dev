# AGENTS.md — Cuis-Smalltalk-Dev

Este es el archivo canónico de instrucciones para agentes de IA en este repositorio.
`CLAUDE.md` y archivos similares por compatibilidad apuntan aquí.

---

## Contexto del proyecto

Este repositorio es un fork de **[Cuis Smalltalk](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev)** mantenido por Gastón Caruso, con foco en **reducir la brecha de entrada para potenciales usuarios de Smalltalk**.

Cuis Smalltalk es un sistema Smalltalk limpio y minimalista. El trabajo principal es desarrollo en Smalltalk: código fuente en `.st`, imágenes Cuis, paquetes, scripts de CI.

La VM utilizada es **OpenSmalltalk VM** (https://github.com/OpenSmalltalk/opensmalltalk-vm). Los binarios incluidos en este repo están en `CuisVM.app/`.

Este repo apunta a **simplificar la experiencia** (especialmente para gente nueva). En cada cambio, sostener dos ideas a la vez:

- Ser respetuosos con el upstream y su historia.
- Dudar activamente si algo pertenece acá: si es mejor upstream, mejor wiki, o directamente no aporta, entonces no va.

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

## Automatización (OpenCode)

Además de los skills portables en `.agents/skills/`, este repo incluye agentes/skills específicos para OpenCode bajo `.opencode/`.

- Agente driver: `.opencode/agents/cuis-tcr-tdd-driver.md` (Tidy First → TCR cuando sea seguro → objetos de dominio → planificación "zombies" → TDD → export/eject).
- Subagente planner: `.opencode/agents/zombie-test-planner.md` (solo propone un plan de tests estilo zombies).
- Skill de workflow: `.opencode/skills/cuis-tidy-tcr-tdd-zombies/SKILL.md`.

---

## Convenciones de Git

- Commits atómicos con mensaje conciso en inglés o español (consistente con el historial del repo).
- Nunca mezclar cleanup con comportamiento en el mismo commit.
- Nunca hacer force-push a `master` sin consenso explícito.
- Revisar `git status` y `git diff` antes de hacer commit.

---

## Convención de Pull Requests

**Todo PR debe tener una descripción completa en castellano.** Esta descripción es documentación pública del proyecto open source: tiene que ser legible por alguien que no participó en la conversación que originó el cambio.

### Estructura obligatoria

```
## ¿Qué hace este PR?

Descripción clara en 2-4 oraciones de qué cambia y por qué.
Sin jerga de la sesión, sin referencias a conversaciones privadas.

## ¿Por qué lo queremos mergear?

El valor concreto que aporta: qué problema resuelve, qué mejora,
qué deuda técnica elimina. Enfocado en el "para qué", no solo el "qué".

## Cambios incluidos

Lista puntual de archivos o componentes modificados con una línea
explicando qué hace cada uno.

## Qué NO incluye (si aplica)

Lo que quedó fuera del scope y por qué. Evita confusión sobre lo
que no se tocó intencionalmente.

## Verificación

Cómo se verificó que funciona: tests, CI verde, prueba manual, etc.
```

### Reglas adicionales

- Escribir como si el lector no sabe nada del contexto previo.
- No mencionar nombres de sesiones, fechas de conversación ni "como dijimos antes".
- Si el PR es grande, dividirlo. Cada PR debe poder entenderse solo.
- El título del PR también en castellano, conciso (máximo 72 caracteres).
- Si hay screenshots, logs o evidencia de CI, incluirlos.

---

## Entorno técnico

- **OS**: macOS (darwin), Shell: zsh
- **Smalltalk**: Cuis Smalltalk (imagen `.image` en `CuisImage/`)
- **VM**: `CuisVM.app/` — binarios basados en [OpenSmalltalk VM](https://github.com/OpenSmalltalk/opensmalltalk-vm)
- **Scripts de inicio**: `RunCuisOnMac.sh`, `RunCuisOnLinux.sh`, `RunCuisOnWindows.bat`
- **CI**: GitHub Actions con runners amd64 (Docker) y arm64 (bare-metal)
  - Workflows: `.github/workflows/ci.yml` (tests), `.github/workflows/release.yml` (tags `v*`)
  - Scripts: `.github/scripts/`
  - Dockerfiles: `docker/Dockerfile` (multi-stage, amd64 + arm64)

---

## Formato de código Smalltalk

- **Packages**: formato Monticello, archivos `.pck.st`
- **Core updates**: changesets numerados `.cs.st` en `CoreUpdates/`
- **No hay Tonel** en este repositorio
