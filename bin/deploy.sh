#!/bin/bash

export GPG_TTY=$(tty)
lein deploy clojars
