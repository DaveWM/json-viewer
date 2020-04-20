#!/bin/bash

sbt fullOptJS::webpack && \
cp target/scala-2.13/scalajs-bundler/main/jsonviewer-opt-bundle.js public/main.js && \
cp assets/main.css public/