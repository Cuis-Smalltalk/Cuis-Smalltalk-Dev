---
name: cuis-tidy-tcr-tdd-zombies
description: Use when working on Cuis Smalltalk changes and you want the workflow Tidy First → (safe) TCR → domain objects → zombie-style test plan → TDD → export/eject.
---

# Cuis Tidy/TCR/TDD (Zombies)

This skill documents the intended workflow for making changes in this repo.

## Trigger

Use this when the user asks to implement/fix something in Cuis and also asks for: "tidy first", "tcr", "zombies", "plan tests", "exportar", "eject".

## Workflow

1. **Tidy First**
   - Before behavior changes, scan for obvious cleanup in the touched area.
   - Keep cleanup separate from behavior changes.
   - Check the "catalogs": existing packages/classes/protocols/updates/tests. Reuse concepts.

2. **TCR (when safe)**
   - Use TCR only when tests are fast/trustworthy and the step is tiny.
   - If a failing test would require a destructive revert, ask for explicit approval.

3. **Domain objects**
   - List the main objects, invariants, and collaborations.

4. **Plan tests with zombies**
   - Use a subagent (recommended name: `zombie-test-planner`) to propose a zombie-ladder test plan.
   - Treat it as a hypothesis; adjust as you learn.

5. **TDD**
   - Red → Green → Refactor.
   - Prefer incremental tests; implement the minimum.
   - Refactor only when the catalogs indicate duplication/misalignment.

6. **Export/eject**
   - If shipping as a package deliverable: use skill `exportar-paquete`.
   - If unloading after export is required: use skill `eject`.

## Agent pairing

Recommended:
- Use agent `.opencode/agents/cuis-tcr-tdd-driver.md` as the change driver.
- Use agent `.opencode/agents/zombie-test-planner.md` only for planning tests.
