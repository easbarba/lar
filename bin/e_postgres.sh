#!/usr/bin/env bash
set -euo pipefail

sudo su - postgres -c "createuser kylo"
sudo su - postgres -c "createdb kylodb"

sudo -u postgres psql

# GRANT ALL PRIVILEGES ON DATABASE kylodb TO kylo;
