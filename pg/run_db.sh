#!/usr/bin/env bash
docker stop cntx-logger-dev
docker rm cntx-logger-dev
docker run --name cntx-logger-dev \
       --publish 30000:5432 \
       --network custom-bridge \
       -e POSTGRES_PASSWORD=dev \
       pg-contextual-logger:latest
