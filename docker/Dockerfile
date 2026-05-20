# Cuis Smalltalk — Multi-arch VM image
#
# Supported platforms: linux/amd64, linux/arm64
# Both VMs require GLIBC >= 2.34 → Ubuntu 22.04 (glibc 2.35)
#
# This image provides the OS + VM binary + all repo files.
# The Squeak JIT requires mprotect(PROT_READ|PROT_EXEC) which Docker
# blocks in RUN steps. Therefore, all VM execution (install-updates,
# run-tests) happens outside the build, via:
#   docker run --security-opt seccomp=unconfined
#
# Stages:
#   vm       — OS + VM binary for TARGETARCH. Stable, cached.
#   workspace — extends vm, adds image + changesets + scripts.
#               Ready to receive 'docker run' commands.
#   runtime  — minimal image after updates are applied externally
#               and copied back in. Built in CI after tests pass.
#
# Usage (CI):
#   1. docker build --target workspace --tag cuis-workspace .
#   2. docker run --rm --security-opt seccomp=unconfined \
#        cuis-workspace bash .github/scripts/install-updates.sh
#   3. docker run --rm --security-opt seccomp=unconfined \
#        cuis-workspace bash .github/scripts/run-tests.sh
#
# Build runtime (after tests pass, with updated image extracted):
#   docker build --target runtime --tag cuis:latest .

# ── Stage 1: vm ──────────────────────────────────────────────────────────────
FROM ubuntu:22.04 AS vm

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      libssl3 \
      libstdc++6 \
      libgcc-s1 \
      ca-certificates \
 && rm -rf /var/lib/apt/lists/*

ARG TARGETARCH
COPY CuisVM.app/Contents/Linux-x86_64/ /opt/cuis-vm/amd64/
COPY CuisVM.app/Contents/Linux-arm64/  /opt/cuis-vm/arm64/

RUN ln -s /opt/cuis-vm/${TARGETARCH} /opt/cuis-vm/current \
 && chmod +x /opt/cuis-vm/current/squeak \
 && chmod +x /opt/cuis-vm/current/lib/squeak/*/squeak

# Verify the binary exists and is the right arch (no execution, just stat)
RUN test -x /opt/cuis-vm/current/lib/squeak/*/squeak

ENV CUIS_VM=/opt/cuis-vm/current/squeak
ENV CUIS_VM_ARGS="-vm-sound-null -vm-display-null"

WORKDIR /workspace

# ── Stage 2: workspace ───────────────────────────────────────────────────────
FROM vm AS workspace

COPY CuisImage/       /workspace/CuisImage/
COPY CoreUpdates/     /workspace/CoreUpdates/
COPY Packages/        /workspace/Packages/
COPY .github/scripts/ /workspace/.github/scripts/
RUN chmod +x /workspace/.github/scripts/*.sh

# ── Stage 3: runtime ─────────────────────────────────────────────────────────
# Minimal image to run Cuis.
# The updated CuisImage/ must be injected via --build-context or
# by rebuilding after 'docker cp' extracts the updated image.
# See .github/workflows/release.yml for the full pipeline.
FROM vm AS runtime

COPY CuisImage/   /workspace/CuisImage/
COPY TrueTypeFonts/ /workspace/TrueTypeFonts/

CMD ["sh", "-c", \
  "exec $CUIS_VM $CUIS_VM_ARGS \
   $(ls /workspace/CuisImage/*.image | head -1)"]
