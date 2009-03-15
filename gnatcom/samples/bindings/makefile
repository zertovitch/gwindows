all: ado msmq ie mshtml sgc

ado:
	-../../tools/bindcom {00000200-0000-0010-8000-00AA006D2EA4} 2 0 ADO

msmq:
	-../../tools/bindcom {D7D6E071-DCCD-11D0-AA4B-0060970DEBAE} 1 0 MSMQ

ie:
	-../../tools/bindcom {EAB22AC0-30C1-11CF-A7EB-0000C05BAE0B} 1 0 IE

mshtml:
	-../../tools/bindcom {3050F1C5-98B5-11CF-BB82-00AA00BDCE0B} 4 0 MSHTML

sgc:
	-../../tools/bindcom {139DD5D1-D6E2-11D0-89D4-00A0C90833E6} 1 0 \
          StructuredGraphicControl

clean:
	-del *.ali
	-del *.o
	-del *~
	-del *.ad?
