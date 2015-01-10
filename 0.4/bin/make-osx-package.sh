#!/bin/sh

PROJECT_DIR=/Users/mikel/Workshop/programming/bard/0.4
JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_05.jdk/Contents/Home

BUILD_DIR=/Users/mikel/Desktop/bard_0.4.2/macosx
PACKAGER=${JAVA_HOME}/bin/javafxpackager
PRODUCT=bard
JARFILE=bard.jar

${PACKAGER} -deploy -v -native -outdir ${BUILD_DIR} -outfile ${PRODUCT} -srcdir ${PROJECT_DIR} -srcfiles ${JARFILE} -appclass bard -name "bard"

