# ***********************************************************************
# FILE IDENTIFICATION
#
# Name:          Makefile
# Project:       Bard
# Purpose:       the Bard Makefile
# Author:        mikel evins
# Copyright:     2009 by mikel evins
#
# ***********************************************************************

# Which Clozure Common Lisp? (edit for your system)
CCL_DIR=/usr/local/ccl/trunk/darwinx86/ccl
LISP=${CCL_DIR}/dx86cl64


APPNAME=Bard
BUNDLE=${APPNAME}.app

EXECUTABLE=${APPNAME}.image

all: bundle

clean:
	rm -rf ${BUNDLE}
	find . -name "*.dfsl" -delete
	find . -name "*~" -exec rm -rf {} \;
	find . -name "*.dx??fsl" -delete

tidy:
	find . -name "*.dfsl" -delete
	find . -name "*~" -exec rm -rf {} \;
	find . -name "*.dx??fsl" -delete

bundle: image
	mkdir -p ./${BUNDLE}/Contents/Resources/English.lproj/
	mkdir -p ./${BUNDLE}/Contents/MacOS
	ibtool ./Contents/Resources/English.lproj/MainMenu.xib --compile ./${BUNDLE}/Contents/Resources/English.lproj/MainMenu.nib
	ibtool ./Contents/Resources/English.lproj/BardDocument.xib --compile ./${BUNDLE}/Contents/Resources/English.lproj/BardDocument.nib
	rsync -r --exclude '.svn' --exclude '*.xib' ./Contents ./${BUNDLE}
	mv ./${EXECUTABLE} ./${BUNDLE}/Contents/MacOS/${APPNAME}
	find . -name "*.dfsl" -delete
	find . -name "*.dx??fsl" -delete

image: 
	${LISP} --batch -l "Bard.lisp" -e "(build-bard \"${EXECUTABLE}\")"


