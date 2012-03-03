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
