#!/bin/sh +x

java -fullversion
lein --version
lein do cloverage --codecov, test-cljs
