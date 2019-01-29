#!/usr/bin/env bash
docker stop envestlog-postgres
docker rm envestlog-postgres
docker run --name envestlog-postgres --publish 30000:5432 -e POSTGRES_PASSWORD=dev evnestlog:latest
