#!/bin/bash

if ! which sbt; then
  curl -o sbt.zip https://sbt-downloads.cdnedge.bluemix.net/releases/v1.2.8/sbt-1.2.8.zip
  unzip sbt.zip
  export PATH="$PATH:$(pwd)/sbt/bin"
fi

npm i -g yarn

sbt fullOptJS::webpack
cp target/scala-2.13/scalajs-bundler/main/repo-opt-bundle.js public/main.js
cp assets/main.css public/