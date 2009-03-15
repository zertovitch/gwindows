PACKAGES = svg_demo

ADAOPTS = -I../../framework/

include ../../makefile.inc

svg_demo: adobe_svg.ads
	gnatmake ${ADAOPTS} svg_demo ${POSTOPTS}

adobe_svg.ads:
	bindcom {B2ADAF70-94C9-11D4-9064-00C04F78ACF9} 2 0 Adobe_SVG

cleanall: clean
	-del adobe_svg*.*
