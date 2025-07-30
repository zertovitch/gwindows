# GWindows & GNAVI

GWindows is a free and open-source Ada programming framework for
MS Windows. GWindows is used for many years in professional, industrial,
commercial, or open-source applications. GWindows is in pure Ada and has
no dependencies other than the Windows API itself that is available on
every computer running the MS Windows operating system or even a system
that provides a Windows subsystem like WINE.
The GWindows framework is updated regularly.

The GNAVI application is an integrated, interactive visual development
environment. The GNAVI application is so far unfinished.

For graphical interface development, an alternative to GNAVI is
the combination of a resource editor (for instance the freeware ResEdit,
shipped with GWindows installation) for editing resource scripts (.rc files),
and GWenerator, a code generator that translates a .rc file into Ada files
that use GWindows. GWenerator is also included in this distribution.

For more information, read the other "readme"'s, mainly:

  * `gwindows\readme.txt              `-  for the GWindows framework
  * `gwenerator\gwenerator_info.html  `-  for the GWenerator code generator
  * `gnatcom\readme.txt               `-  for the GNATCOM framework

But if you are in a hurry, jump to one of the following project files
and start playing!

  * `gwindows\gwindows_tutorials.gpr  `(executables are created in `gwindows\tutorials\`)
  * `gwindows\gwindows_samples.gpr    `(executables are created in `gwindows\samples\`)
  * `gwindows\gwindows_contrib.gpr    `(executables are created in `gwindows\contrib\`)
  * `gwindows\gwindows_test.gpr       `(executables are created in `gwindows\test\`)
