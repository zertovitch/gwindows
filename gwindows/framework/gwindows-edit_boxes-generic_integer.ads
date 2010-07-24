------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
-- G W I N D O W S . E D I T _ B O X E S .G E N E R I C _ I N T E G E R     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2005 David Botton                   --
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
-- More information about GWindows and the latest current release can       --
-- be located on the web at http://www.gnavi.org/gwindows                   --
--                                                                          --
------------------------------------------------------------------------------

generic
   type Integer_Type is (<>);
package GWindows.Edit_Boxes.Generic_Integer is

   -------------------------------------------------------------------------
   --  Edit_Box_Type
   -------------------------------------------------------------------------

   type Edit_Box_Type is new GWindows.Edit_Boxes.Edit_Box_Type
     with private;
   type Edit_Box_Access is access all Edit_Box_Type;
   type Pointer_To_Edit_Box_Class is access all Edit_Box_Type'Class;

   -------------------------------------------------------------------------
   --  Edit_Box_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Edit       : in out Edit_Box_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Initial    : in     Integer_Type
      := Integer_Type'First;
      Left       : in     Integer                              := 0;
      Top        : in     Integer                              := 0;
      Width      : in     Integer                              := 0;
      Height     : in     Integer                              := 0;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);
   --  Create Edit Box

   -------------------------------------------------------------------------
   --  Edit_Box_Type - Override Event Methods
   -------------------------------------------------------------------------

   procedure On_Update (Edit : in out Edit_Box_Type);
   --  Validate user's entry

   -------------------------------------------------------------------------
   --  New Edit_Box_Type operations
   -------------------------------------------------------------------------

   function Current (Edit : in Edit_Box_Type) return Integer_Type;

   procedure Current (Edit : in out Edit_Box_Type; Item  : in Integer_Type);

private
   type Edit_Box_Type is new GWindows.Edit_Boxes.Edit_Box_Type with record
      Value : Integer_Type;
   end record;

end GWindows.Edit_Boxes.Generic_Integer;
