// client.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#import "\work\gnatcom\test\cpp\beep.tlb" no_namespace

int main(int argc, char* argv[])
{
	CoInitialize(NULL);

	//IBeepPtr beep(__uuidof(BeepClass), NULL, CLSCTX_LOCAL_SERVER);

	IDispatchPtr beep(__uuidof(BeepClass), NULL, CLSCTX_LOCAL_SERVER);

	printf("Hello World!\n");
	
	long ID;
	LPOLESTR member = L"beep";

	beep->GetIDsOfNames(IID_NULL, &member, 1, 0, &ID);

	printf("ID = %i", ID);

	DISPPARAMS none = {NULL, NULL, 0, 0};

	beep->Invoke(ID, IID_NULL, 0, DISPATCH_METHOD, &none, NULL, NULL, NULL);

	CoUninitialize();

	return 0;
}

