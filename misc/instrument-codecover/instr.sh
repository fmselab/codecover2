#!/bin/sh

# NOTE: several paths are hardcoded, they need to be adjusted
#       according to your environment
#
# input: codecover tree (eclipse projects)
# output: instrumented codecover tree + tsc
#
# export from svn
# merge sources
# instrument
# move sources back
# move protocol to proper location -> model/src
# adjust manifest -> export org.codeover.instrumentation...
# build with eclipse

# see org.codecover.instrumentation.java15.JavaInstrumentationHelper
# for files which need to be excluded
#
# at the moment this are:
#   CoverageCounterLog.java
#   CoverageResultLog.java
#   MeasurementConstants.java
#   and all files of the instrumentation-java-measurement project

# NOTE: for now the following step needs to be performed manually
#
# the manifest of the model needs to export the following packages:
#   org.codecover (for CodeCoverInfo.java)
#   org.codecover.instrumentation.measurement
#   org.codecover.instrumentation.java.measurement
#   org.codecover.instrumentation.java.measurement.jmx

set -x
rm -rf code
svn export ../trunk/code/ code
cd code

mkdir all_src
mv instrumentation-java-measurement/src/ instrumentation-java-measurement/src2
cp -a */src/* all_src/
/home/t/codecover/trunk/code/release/codecover.sh in -v -r all_src/ -l java -d all_instr -c tsc.xml -x../excl -a utf-8

for f in */src/; do cd "$f"; find . -name \*.java -not -name CoverageCounterLog.java -not -name CoverageResultLog.java -not -name MeasurementConstants.java -exec mv ../../all_instr/'{}' '{}' \; ; cd ../../; done
mv instrumentation-java-measurement/src2/ instrumentation-java-measurement/src

mv all_instr/org/codecover/instrumentation model/src/org/codecover

rm -rf all_src
rm -rf all_instr
set +x