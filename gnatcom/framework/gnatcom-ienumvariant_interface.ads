------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--       G N A T C O M . I E N U M V A R I A N T _ I N T E R F A C E        --
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

--  Thick binding to IEnumVariant

with Ada.Unchecked_Conversion;

with GNATCOM.Iinterface;
with GNATCOM.Types;

package GNATCOM.IEnumVARIANT_Interface is

   type Array_Of_Variants is
     array (Natural range <>) of aliased GNATCOM.Types.VARIANT;

   type IEnumVARIANT_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   function To_Pointer_To_IEnumVARIANT is
      new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, GNATCOM.Types.Pointer_To_IEnumVARIANT);

   procedure Initialize (This : in out IEnumVARIANT_Type);

   function Pointer (This : IEnumVARIANT_Type)
     return GNATCOM.Types.Pointer_To_IEnumVARIANT;

   procedure Attach (This    : in out IEnumVARIANT_Type;
                     Pointer : in     GNATCOM.Types.Pointer_To_IEnumVARIANT);

   function Next
     (This         : IEnumVARIANT_Type;
      celt         : Integer)
     return Array_Of_Variants;

   procedure Skip
     (This : IEnumVARIANT_Type;
      celt : Integer);

   procedure Reset
     (This : IEnumVARIANT_Type);

   function Clone
     (This   : IEnumVARIANT_Type)
     return GNATCOM.Types.Pointer_To_IEnumVARIANT;

end GNATCOM.IEnumVARIANT_Interface;
