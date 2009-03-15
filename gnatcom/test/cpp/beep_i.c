/* this file contains the actual definitions of */
/* the IIDs and CLSIDs */

/* link this file in with the server and any clients */


/* File created by MIDL compiler version 5.01.0164 */
/* at Mon Jan 10 18:05:51 2000
 */
/* Compiler settings for F:\Work\gnatcom\test\cpp\beep.idl:
    Os (OptLev=s), W1, Zp8, env=Win32, ms_ext, c_ext
    error checks: allocation ref bounds_check enum stub_data 
*/
//@@MIDL_FILE_HEADING(  )
#ifdef __cplusplus
extern "C"{
#endif 


#ifndef __IID_DEFINED__
#define __IID_DEFINED__

typedef struct _IID
{
    unsigned long x;
    unsigned short s1;
    unsigned short s2;
    unsigned char  c[8];
} IID;

#endif // __IID_DEFINED__

#ifndef CLSID_DEFINED
#define CLSID_DEFINED
typedef IID CLSID;
#endif // CLSID_DEFINED

const IID IID_IBeep = {0x0FE0EE22,0x8AA2,0x11d2,{0x81,0xAA,0x44,0x45,0x53,0x54,0x00,0x01}};


const IID LIBID_BeepLibrary = {0x0FE0EE20,0x8AA2,0x11d2,{0x81,0xAA,0x44,0x45,0x53,0x54,0x00,0x01}};


const CLSID CLSID_BeepClass = {0x0FE0EE21,0x8AA2,0x11d2,{0x81,0xAA,0x44,0x45,0x53,0x54,0x00,0x01}};


#ifdef __cplusplus
}
#endif

