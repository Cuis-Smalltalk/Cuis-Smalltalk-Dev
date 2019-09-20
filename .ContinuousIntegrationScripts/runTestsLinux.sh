#!/bin/bash

set -euo pipefail

.ContinuousIntegrationScripts/installUpdates.sh
.ContinuousIntegrationScripts/runTests.sh