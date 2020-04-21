#!/bin/bash

if ! which sbt; then
  curl -o sbt.zip https://sbt-downloads.cdnedge.bluemix.net/releases/v1.3.10/sbt-1.3.10.zip
  unzip sbt.zip
  export PATH="$PATH:$(pwd)/sbt/bin"
fi


sbt fullOptJS::webpack
cp target/scala-2.13/scalajs-bundler/main/jsonviewer-opt-bundle.js public/main.js
cp assets/main.css public/