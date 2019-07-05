#!/usr/bin/env bash
docker exec postgres pg_dump --no-owner --no-acl -s -Upostgres --dbname logging-dev > out.sql

