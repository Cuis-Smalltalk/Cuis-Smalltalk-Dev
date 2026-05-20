# Skill: using-git-worktrees

## Cuándo cargar este skill

Cargar cuando:
- Se va a iniciar trabajo aislado en una feature o experimento.
- Se va a ejecutar un plan de implementación que puede afectar `master`.
- Se necesita tener dos estados del repo disponibles simultáneamente.

---

## Qué son los worktrees

`git worktree` permite tener múltiples checkouts del mismo repositorio en directorios separados, sin clonar.
Cada worktree tiene su propio directorio de trabajo y puede estar en un branch diferente.

---

## Flujo

### Crear un worktree para una feature

```bash
# Desde el repo principal
git worktree add ../Cuis-feature-nombre -b feature/nombre

# Verificar
git worktree list
```

### Trabajar en el worktree

```bash
# Moverse al worktree
cd ../Cuis-feature-nombre

# Trabajar normalmente: editar, testear, commitear
```

### Integrar a master (Trunk Based)

```bash
# En el worktree, asegurarse que los tests pasan
# Luego en el repo principal:
git checkout master
git merge --no-ff feature/nombre
# O rebase si el historial debe ser lineal:
git rebase feature/nombre
```

### Limpiar el worktree al terminar

```bash
git worktree remove ../Cuis-feature-nombre
git branch -d feature/nombre
```

---

## Reglas invariantes

- `master` siempre debe estar en estado verde (tests pasando).
- Los worktrees son de corta duración; no acumular trabajo no integrado.
- Nunca hacer force-push a `master`.
- Un worktree por feature o experimento; no reutilizar worktrees para trabajo no relacionado.

---

## En Cuis Smalltalk

- Cada worktree puede tener su propia imagen Cuis si es necesario, copiando desde `CuisImage/`.
- Los archivos `.st` son texto plano y se mergean normalmente con Git.
- Las imágenes `.image` son binarias: no se mergean, se regeneran desde los `.st`.
- Verificar que el worktree incluye la imagen y la VM necesarias antes de trabajar.

---

## Notas

- `git worktree list` muestra todos los worktrees activos.
- Un worktree no puede estar en el mismo branch que otro worktree activo.
- Para experimentos desechables usar `git worktree add --detach ../experimento`.
