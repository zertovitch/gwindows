Include with GWindows in the gwindows\redist directory are precompiled
versions of the tclcontrol from Farzad Pezeshkpour -
http://www.sys.uea.ac.uk/~fuzz/TclControl/default.html

You register the version of the control that matches your version of
TCL installed. Note that the TCL dlls must be in the path at the time
of registering the DLL and when your application is running to work
properly.

To register a version use (replacing 83 with the version you will be using):

cd gwindows\redist
regsvr32 tclcontrol83.dll

