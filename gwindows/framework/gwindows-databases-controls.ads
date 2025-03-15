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
