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
