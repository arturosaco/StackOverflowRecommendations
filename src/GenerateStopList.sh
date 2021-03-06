#!/bin/bash

TOKEN_REGEX='[vV]?[cC]\+\+(\p{N}[\p{N}xX])?|\.[nN][eE][tT][\p{N}]*|[%#-]?[\p{L}\p{M}_][\p{L}\p{M}\p{N}\p{Pc}\p{Pd}\p{Pf}\p{Pi}\.:'\''_-]*[\p{L}\p{M}\p{N}_%]|[cCfF]#|<[^<>,\p{Z}]+>'

if [ "$#" -ne 0 ]; then
  echo "usage: ${0}" >&2
  exit 1
fi

if [ -z "${MALLET_HOME}" ]; then
  MALLET=mallet
else
  MALLET="${MALLET_HOME}"/bin/mallet
fi

IMPORT_DIR_ARGS="\
  --keep-sequence \
  --stoplist-file reference/en.txt \
  --token-regex ${TOKEN_REGEX} \
  --token-pipe uk.me.conradscott.StemmingPipe
  --prune-count 10
  --prune-doc-frequency 0.50"

${MALLET} run uk.me.conradscott.GenerateStopList \
  --input test train \
  --output data/stoplist.txt \
  ${IMPORT_DIR_ARGS}
