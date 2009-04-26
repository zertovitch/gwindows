------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--           G W I N D O W S . D A T A B A S E S . C O N T R O L S          --
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

with GWindows.Windows;
with GWindows.Buttons;
with GWindows.Base;

package GWindows.Databases.Controls is

   -------------------------------------------------------------------------
   --  Binding_Recordset_Type
   -------------------------------------------------------------------------
   --  Recordset type that supports binding to controls and windows

   type Binding_Recordset_Type is
     new GWindows.Databases.Recordset_Type with private;

   -------------------------------------------------------------------------
   --  Binding_Recordset_Type - Methods
   -------------------------------------------------------------------------

   type Permissions_Type is (Read_Only, Write_Only, Read_Write);

   procedure Bind_Text_Control
     (Recordset   : in out Binding_Recordset_Type;
      Field_Name  : in     GString;
      Control     : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Permissions : in     Permissions_Type := Read_Write);
   --  Binds control to the recordset through the Text property to
   --  Field_Name

   procedure Fill_Bindings (Recordset  : in out Binding_Recordset_Type);
   --  Manually fill all bindings

   procedure Clear_Bindings (Recordset : in out Binding_Recordset_Type);
   --  Clears all bindings

   procedure Fill_Record   (Recordset  : in out Binding_Recordset_Type);
   --  Manually fill record with values from binding

   -------------------------------------------------------------------------
   --  Database_Control_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure Finalize (Object : in out Binding_Recordset_Type);
   --  Cleans up Binding Maps

   procedure On_Before_Update (Recordset : in out Binding_Recordset_Type;
                               Change    :    out Boolean);
   --  do a Fill_Record

   procedure On_Change (Recordset : in out Binding_Recordset_Type);
   --  Do a Fill_Bindings

   -------------------------------------------------------------------------
   --  Database_Control_Type
   -------------------------------------------------------------------------
   --  Simple command bar for database navigation

   type Database_Control_Type is
     new GWindows.Windows.Window_Type with
      record
         Database        : GWindows.Databases.Database_Type;
         Recordset       : Binding_Recordset_Type;
         First_Button    : GWindows.Buttons.Button_Type;
         Previous_Button : GWindows.Buttons.Button_Type;
         Add_Button      : GWindows.Buttons.Button_Type;
         Update_Button   : GWindows.Buttons.Button_Type;
         Delete_Button   : GWindows.Buttons.Button_Type;
         Next_Button     : GWindows.Buttons.Button_Type;
         Last_Button     : GWindows.Buttons.Button_Type;
      end record;

   type Database_Control_Access is access all Database_Control_Type;
   type Pointer_To_Database_Control_Class is
     access all Database_Control_Type'Class;

   -------------------------------------------------------------------------
   --  Database_Control_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Create (Window : in out Database_Control_Type);

   procedure On_Destroy (Window : in out Database_Control_Type);

   procedure Do_First
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Previous
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Add
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Update
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Delete
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Next
     (Window : in out GWindows.Base.Base_Window_Type'Class);

   procedure Do_Last
     (Window : in out GWindows.Base.Base_Window_Type'Class);

private
   type Text_Bind_Record;
   type Pointer_To_Text_Bind_Record is access all Text_Bind_Record;

   type Text_Bind_Record is
      record
         Field_Name  : GString_Unbounded;
         Control     : GWindows.Base.Pointer_To_Base_Window_Class := null;
         Permissions : Permissions_Type := Read_Write;
         Next        : Pointer_To_Text_Bind_Record := null;
      end record;

   type Binding_Recordset_Type is
     new GWindows.Databases.Recordset_Type with
      record
           Text_Bind_First : Pointer_To_Text_Bind_Record := null;
      end record;

end GWindows.Databases.Controls;
