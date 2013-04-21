#!/bin/bash

if [ "$#" -ne 0 ]; then
  echo "usage: ${0}" >&2
  exit 1
fi

if [ -d /cygdrive ]; then
    CYGWIN_PATH="cygpath -w"
    MAHOUT_ROOT=/cygdrive/c/Users/Public/Packages/mahout-SNAPSHOT/
else
    CYGWIN_PATH="echo"
    MAHOUT_ROOT=/home/packages/mahout-SNAPSHOT
fi

\cp "${MAHOUT_ROOT}"/core/target/mahout-core-0.8-SNAPSHOT-job.jar "${HOME}"/tmp
jar uf $(${CYGWIN_PATH} "${HOME}"/tmp/mahout-core-0.8-SNAPSHOT-job.jar) -C $(${CYGWIN_PATH} "${HOME}"/SOLDA/CollaborativeFiltering/bin) uk/ac/ucl/csml/ir/Recommend.class
hadoop jar $(${CYGWIN_PATH} "${HOME}"/tmp/mahout-core-0.8-SNAPSHOT-job.jar) uk.ac.ucl.csml.ir.Recommend
