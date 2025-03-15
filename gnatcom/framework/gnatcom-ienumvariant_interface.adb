------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--       G N A T C O M . I E N U M V A R I A N T _ I N T E R F A C E        --
--                                                                          --
--                                B o d y                                   --
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

with GNATCOM.Errors;
with Interfaces.C;

package body GNATCOM.IEnumVARIANT_Interface is

   procedure Initialize (This : in out IEnumVARIANT_Type) is
   begin
      Set_IID (This, GNATCOM.Types.IID_IEnumVARIANT);
   end Initialize;

   function Pointer (This : IEnumVARIANT_Type)
     return GNATCOM.Types.Pointer_To_IEnumVARIANT
   is
   begin
      return To_Pointer_To_IEnumVARIANT (Address (This));
   end Pointer;

   procedure Attach (This    : in out IEnumVARIANT_Type;
                     Pointer : in     GNATCOM.Types.Pointer_To_IEnumVARIANT)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   function Next
     (This         : IEnumVARIANT_Type;
      celt         : Integer)
     return Array_Of_Variants
   is
      Var_Array : aliased Array_Of_Variants (1 .. celt);
      pceltFetched : aliased Interfaces.C.long;
   begin
      if celt < 1 then
         return Var_Array;
      end if;

      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Next
         (Pointer (This),
          Interfaces.C.long (celt),
          Var_Array (1)'Access,
          pceltFetched'Unchecked_Access));

      return Var_Array (1 .. Natural (pceltFetched));
   end Next;

   procedure Skip
     (This : IEnumVARIANT_Type;
      celt : Integer)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Skip
         (Pointer (This),
          Interfaces.C.long (celt)));

   end Skip;

   -- Reset --

   procedure Reset
     (This : IEnumVARIANT_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Reset
         (Pointer (This)));

   end Reset;

   -- Clone --

   function Clone
     (This   : IEnumVARIANT_Type)
     return GNATCOM.Types.Pointer_To_IEnumVARIANT
   is
      ppenum : aliased GNATCOM.Types.Pointer_To_IEnumVARIANT;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Clone
         (Pointer (This),
          ppenum'Unchecked_Access));

      return ppenum;
   end Clone;

end GNATCOM.IEnumVARIANT_Interface;
