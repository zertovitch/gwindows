makeinfo --html -o %1.html %1.texi --force
makeinfo --hpj-output %1.hpj -o %1.rtf %1.texi --force
"D:\Program Files\Microsoft Visual Studio\Common\Tools\hcw" %1.hpj
