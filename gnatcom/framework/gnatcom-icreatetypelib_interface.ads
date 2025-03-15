------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--     G N A T C O M . I C R E A T E T Y P E L I B _ I N T E R F A C E      --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2006 David Botton                   --
--                                                                          --
-- MIT License                                                              --
--                                                                          --
-- Permission is hereby granted, free of charge, to any person obtaining    --
-- a copy of this software and associated documentation files (the          --
-- "Software"), to deal in the Software without restriction, including      --
-- without limitation the rights to use, copy, modify, merge, publish,      --
-- distribute, sublicense, and/or sell copies of the Software, and to       --
-- permit persons to whom the Software is furnished to do so, subject to    --
-- the following conditions:                                                --
--                                                                          --
-- The above copyright notice and this permission notice shall be included  --
-- in all copies or substantial portions of the Software.                   --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,          --
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF       --
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   --
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY     --
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,     --
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE        --
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                   --
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

package GNATCOM.ICreateTypeLib_Interface is

   type ICreateTypeLib_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out ICreateTypeLib_Type);

   function Pointer (This : ICreateTypeLib_Type)
     return GNATCOM.Types.Pointer_To_ICreateTypeLib;

   function To_Pointer_To_ICreateTypeLib is new Ada.Unchecked_Conversion
     (System.Address, GNATCOM.Types.Pointer_To_ICreateTypeLib);

   procedure Attach (This    : in out ICreateTypeLib_Type;
                     Pointer : in     GNATCOM.Types.Pointer_To_ICreateTypeLib);

   procedure Create_Type_Library
     (This      : in out ICreateTypeLib_Type;
      File_Name : in     String);
   --  This function creates an implementation for ICreateTypeLib that will
   --  write out a Type Library to File_Name

   procedure CreateTypeInfo
     (This     : ICreateTypeLib_Type;
      szName   : GNATCOM.Types.LPWSTR;
      tkind    : GNATCOM.Types.TYPEKIND;
      ppCTInfo : GNATCOM.Types.Pointer_To_Pointer_To_ICreateTypeInfo);

   procedure SetName
     (This   : ICreateTypeLib_Type;
      szName : GNATCOM.Types.LPWSTR);

   procedure SetVersion
     (This         : ICreateTypeLib_Type;
      wMajorVerNum : Interfaces.C.unsigned_short;
      wMinorVerNum : Interfaces.C.unsigned_short);

   procedure SetGuid
     (This : ICreateTypeLib_Type;
      guid : GNATCOM.Types.Pointer_To_GUID);

   procedure SetDocString
     (This  : ICreateTypeLib_Type;
      szDoc : GNATCOM.Types.LPWSTR);

   procedure SetHelpFileName
     (This           : ICreateTypeLib_Type;
      szHelpFileName : GNATCOM.Types.LPWSTR);

   procedure SetHelpContext
     (This          : ICreateTypeLib_Type;
      dwHelpContext : Interfaces.C.unsigned_long);

   procedure SetLcid
     (This : ICreateTypeLib_Type;
      lcid : Interfaces.C.unsigned_long);

   procedure SetLibFlags
     (This      : ICreateTypeLib_Type;
      uLibFlags : Interfaces.C.unsigned);

   procedure SaveAllChanges
     (This : ICreateTypeLib_Type);

end GNATCOM.ICreateTypeLib_Interface;
