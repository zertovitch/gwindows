------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--           G N A T C O M . I T Y P E L I B _ I N T E R F A C E            --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2006 David Botton                   --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. It is distributed in the hope that it will be useful,  but WITHOUT --
-- ANY WARRANTY;  without  even the  implied warranty of MERCHANTABILITY or --
-- FITNESS FOR A PARTICULAR PURPOSE.    See the GNU General  Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with this;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- More information about GNATCOM and the most current version can          --
-- be located on the web at http://www.gnavi.org/gnatcom                    --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with System;
with Interfaces.C;

with GNATCOM.Iinterface;
with GNATCOM.Types;

package GNATCOM.ITypeLib_Interface is

   type ITypeLib_Type is new GNATCOM.Iinterface.Interface_Type
     with null record;

   procedure Initialize (This : in out ITypeLib_Type);

   function Pointer (This : ITypeLib_Type)
     return GNATCOM.Types.Pointer_To_ITypeLib;

   procedure Open (This        : in out ITypeLib_Type;
                   Source_Name : in     String);
   --  This procedure loads a type library from Source_Name. The Source_Name
   --  may take the form of a type library file, or any Windows file that
   --  contains a TYPELIB resource. Additionally, if Source_Name is neither
   --  type it will be parsed as a moniker.
   --
   --  To Access additional TYPELIB resources place a slash followed by the
   --  index of the resource.
   --
   --  Example:
   --
   --   Create (My_Library, Open("C:\Work\beep.dll"));
   --
   --   To access the third TYPELIB resource in the dll
   --
   --   Create (My_Library, Open("C:\Work\beep.dll\3"));

   function To_Pointer_To_ITypeLib is new Ada.Unchecked_Conversion
     (System.Address, GNATCOM.Types.Pointer_To_ITypeLib);

   procedure Attach
     (This    : in out ITypeLib_Type;
      Pointer :        GNATCOM.Types.Pointer_To_ITypeLib);

   function GetTypeInfoCount
     (This : ITypeLib_Type)
     return Interfaces.C.unsigned;

   function GetTypeInfo
     (This  : ITypeLib_Type;
      index : Interfaces.C.int)
     return GNATCOM.Types.Pointer_To_ITypeInfo;

   function GetTypeInfoType
     (This  : ITypeLib_Type;
      index : Interfaces.C.int)
     return GNATCOM.Types.TYPEKIND;

   function GetTypeInfoOfGuid
     (This : ITypeLib_Type;
      guid : GNATCOM.Types.Pointer_To_GUID)
     return GNATCOM.Types.Pointer_To_ITypeInfo;

   function GetLibAttr
     (This : ITypeLib_Type)
     return GNATCOM.Types.Pointer_To_TLIBATTR;

   function GetTypeComp
     (This : ITypeLib_Type)
     return GNATCOM.Types.Pointer_To_ITypeComp;

   procedure GetDocumentation
     (This           : ITypeLib_Type;
      index          : Interfaces.C.int;
      pBstrName      : GNATCOM.Types.Pointer_To_BSTR;
      pBstrDocString : GNATCOM.Types.Pointer_To_BSTR;
      pdwHelpContext : GNATCOM.Types.Pointer_To_unsigned_long;
      pBstrHelpFile  : GNATCOM.Types.Pointer_To_BSTR);

   function IsName (This      : ITypeLib_Type;
                    szNameBuf : GNATCOM.Types.BSTR;
                    lHashVal  : Interfaces.C.unsigned_long;
                    Clear     : Boolean                       := True)
     return GNATCOM.Types.bool;

   procedure FindName
     (This      : ITypeLib_Type;
      szNameBuf : GNATCOM.Types.BSTR;
      lHashVal  : Interfaces.C.unsigned_long;
      ppTInfo   : GNATCOM.Types.Pointer_To_Pointer_To_ITypeInfo;
      rgMemId   : GNATCOM.Types.Pointer_To_long;
      pcFound   : GNATCOM.Types.Pointer_To_int;
      Clear     : Boolean                                       := True);

   procedure ReleaseTLibAttr (This      : ITypeLib_Type;
                              pTLibAttr : GNATCOM.Types.Pointer_To_TLIBATTR);

end GNATCOM.ITypeLib_Interface;
