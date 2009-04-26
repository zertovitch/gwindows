------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--           G W I N D O W S . D A T A B A S E S . C O N T R O L S          --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Unchecked_Deallocation;

with GWindows.Base; use GWindows.Base;
with GWindows.Message_Boxes; use GWindows.Message_Boxes;
with GWindows.GStrings; use GWindows.GStrings;
with GNATCOM.IErrorInfo;

package body GWindows.Databases.Controls is

   ------------
   -- Do_Add --
   ------------

   procedure Do_Add
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      P : Pointer_To_Database_Control_Class :=
        Pointer_To_Database_Control_Class (Parent (Window));
   begin
      Add_New (P.Recordset);
      On_Change (P.Recordset);
   exception
      when others =>
         Message_Box
           (Window, "Database Error",
            To_GString_From_String (GNATCOM.IErrorInfo.Get_IErrorInfo),
            Icon => Error_Icon);
   end Do_Add;

   ---------------
   -- Do_Delete --
   ---------------

   procedure Do_Delete
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      P : Pointer_To_Database_Control_Class :=
        Pointer_To_Database_Control_Class (Parent (Window));
   begin
      begin
         Delete (P.Recordset);
      exception
         when others =>
            Message_Box
              (Window, "Database Error",
               To_GString_From_String (GNATCOM.IErrorInfo.Get_IErrorInfo),
               Icon => Error_Icon);

            return;
      end;

      Clear_Bindings (P.Recordset);

      begin
         Move_First (P.Recordset);
      exception
         when others =>
            null;
      end;
   end Do_Delete;

   -------------
   -- Do_Next --
   -------------

   procedure Do_Next
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      P : Pointer_To_Database_Control_Class :=
        Pointer_To_Database_Control_Class (Parent (Window));
   begin
      if not EOF (P.Recordset) then
         Move_Next (P.Recordset);

         if EOF (P.Recordset) then
            Move_Last (P.Recordset);
         end if;
      end if;
   exception
      when others =>
         Message_Box
           (Window, "Datavase Error",
            To_GString_From_String (GNATCOM.IErrorInfo.Get_IErrorInfo),
            Icon => Error_Icon);
   end Do_Next;

   -----------------
   -- Do_Previous --
   -----------------

   procedure Do_Previous
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      P : Pointer_To_Database_Control_Class :=
        Pointer_To_Database_Control_Class (Parent (Window));
   begin
      if not BOF (P.Recordset) then
         Move_Previous (P.Recordset);

         if BOF (P.Recordset) then
            Move_First (P.Recordset);
         end if;
      end if;
   exception
      when others =>
         Message_Box
           (Window, "Datavase Error",
            To_GString_From_String (GNATCOM.IErrorInfo.Get_IErrorInfo),
            Icon => Error_Icon);
   end Do_Previous;

   --------------
   -- Do_First --
   --------------

   procedure Do_First
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      P : Pointer_To_Database_Control_Class :=
        Pointer_To_Database_Control_Class (Parent (Window));
   begin
      if not BOF (P.Recordset) then
         Move_First (P.Recordset);
      end if;
   exception
      when others =>
         Message_Box
           (Window, "Datavase Error",
            To_GString_From_String (GNATCOM.IErrorInfo.Get_IErrorInfo),
            Icon => Error_Icon);
   end Do_First;

   -------------
   -- Do_Last --
   -------------

   procedure Do_Last
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      P : Pointer_To_Database_Control_Class :=
        Pointer_To_Database_Control_Class (Parent (Window));
   begin
      if not EOF (P.Recordset) then
         Move_Last (P.Recordset);
      end if;
   exception
      when others =>
         Message_Box
           (Window, "Datavase Error",
            To_GString_From_String (GNATCOM.IErrorInfo.Get_IErrorInfo),
            Icon => Error_Icon);
   end Do_Last;

   ---------------
   -- Do_Update --
   ---------------

   procedure Do_Update
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      P : Pointer_To_Database_Control_Class :=
        Pointer_To_Database_Control_Class (Parent (Window));
   begin
      Update (P.Recordset);
   exception
      when others =>
         Message_Box
           (Window, "ADO Error",
            To_GString_From_String (GNATCOM.IErrorInfo.Get_IErrorInfo),
            Icon => Error_Icon);
   end Do_Update;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Window : in out Database_Control_Type) is
      use GWindows.Buttons;
   begin
      Keyboard_Support (Window);

      Create (Window.First_Button, Window, "<<",
              0, 0, 20, 25);
      On_Click_Handler (Window.First_Button,
                        Do_First'Unrestricted_Access);

      Create (Window.Previous_Button, Window, "<",
              20, 0, 20, 25);
      On_Click_Handler (Window.Previous_Button,
                        Do_Previous'Unrestricted_Access);

      Create (Window.Add_Button, Window, "+",
              40, 0, 20, 25);
      On_Click_Handler (Window.Add_Button,
                        Do_Add'Unrestricted_Access);

      Create (Window.Update_Button, Window, "U",
              60, 0, 20, 25);
      On_Click_Handler (Window.Update_Button,
                        Do_Update'Unrestricted_Access);

      Create (Window.Delete_Button, Window, "-",
              80, 0, 20, 25);
      On_Click_Handler (Window.Delete_Button,
                        Do_Delete'Unrestricted_Access);

      Create (Window.Next_Button, Window, ">",
              100, 0, 20, 25);
      On_Click_Handler (Window.Next_Button,
                        Do_Next'Unrestricted_Access);

      Create (Window.Last_Button, Window, ">>",
              120, 0, 20, 25);
      On_Click_Handler (Window.Last_Button,
                        Do_Last'Unrestricted_Access);
   end On_Create;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Window : in out Database_Control_Type)
   is
   begin
      Close (Window.Recordset);
      Close (Window.Database);
   exception
      when others =>
         null;
   end On_Destroy;

   -----------------------
   -- Bind_Text_Control --
   -----------------------

   procedure Bind_Text_Control
     (Recordset   : in out Binding_Recordset_Type;
      Field_Name  : in     GString;
      Control     : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Permissions : in     Permissions_Type := Read_Write)
   is
      Rec : Pointer_To_Text_Bind_Record := new Text_Bind_Record;
   begin
      Rec.Field_Name  := To_GString_Unbounded (Field_Name);
      Rec.Control     := Control;
      Rec.Permissions := Permissions;
      Rec.Next        := Recordset.Text_Bind_First;
      Recordset.Text_Bind_First := Rec;
   end Bind_Text_Control;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Binding_Recordset_Type)
   is
      procedure Free is
         new Ada.Unchecked_Deallocation (Text_Bind_Record,
                                         Pointer_To_Text_Bind_Record);
      Rec : Pointer_To_Text_Bind_Record;
   begin
      while Object.Text_Bind_First /= null loop
         Rec := Object.Text_Bind_First;
         Object.Text_Bind_First := Rec.Next;

         Free (Rec);
      end loop;
   end Finalize;

   ----------------------
   -- On_Before_Update --
   ----------------------

   procedure On_Before_Update (Recordset : in out Binding_Recordset_Type;
                               Change    :    out Boolean)
   is
      Rec : Pointer_To_Text_Bind_Record := Recordset.Text_Bind_First;
   begin
      while Rec /= null loop
         if Rec.Permissions = Read_Write or Rec.Permissions = Write_Only then
            Field_Value (Recordset,
                         To_GString_From_Unbounded (Rec.Field_Name),
                         Text (Rec.Control.all));
         end if;

         Rec := Rec.Next;
      end loop;

      On_Before_Update (Recordset_Type (Recordset), Change);
   exception
      when others =>
         Message_Box
           ("Datavase Error",
            To_GString_From_String (GNATCOM.IErrorInfo.Get_IErrorInfo),
            Icon => Error_Icon);
   end On_Before_Update;

   ---------------
   -- On_Change --
   ---------------

   procedure On_Change (Recordset : in out Binding_Recordset_Type)
   is
      Rec : Pointer_To_Text_Bind_Record := Recordset.Text_Bind_First;
   begin
      if  BOF (Recordset) or EOF (Recordset) then
         Clear_Bindings (Recordset);
      else
         while Rec /= null loop
            if Rec.Permissions = Read_Write or Rec.Permissions = Read_Only then
               Text (Rec.Control.all,
                     Field_Value (Recordset,
                                  To_GString_From_Unbounded (Rec.Field_Name)));
            end if;

            Rec := Rec.Next;
         end loop;
      end if;

      On_Change (Recordset_Type (Recordset));
   exception
      when others =>
         Message_Box
           ("Datavase Error",
            To_GString_From_String (GNATCOM.IErrorInfo.Get_IErrorInfo),
            Icon => Error_Icon);
   end On_Change;

   -------------------
   -- Fill_Bindings --
   -------------------

   procedure Fill_Bindings (Recordset  : in out Binding_Recordset_Type)
   is
   begin
      On_Change (Binding_Recordset_Type'Class (Recordset));
   end Fill_Bindings;

   -----------------
   -- Fill_Record --
   -----------------

   procedure Fill_Record   (Recordset  : in out Binding_Recordset_Type)
   is
      Change : Boolean;
   begin
      On_Before_Update (Binding_Recordset_Type'Class (Recordset), Change);
   end Fill_Record;

   --------------------
   -- Clear_Bindings --
   --------------------

   procedure Clear_Bindings (Recordset : in out Binding_Recordset_Type)
   is
      Rec : Pointer_To_Text_Bind_Record := Recordset.Text_Bind_First;
   begin
      while Rec /= null loop
         if Rec.Permissions = Read_Write or Rec.Permissions = Read_Only then
            Text (Rec.Control.all, "");
         end if;

         Rec := Rec.Next;
      end loop;
   end Clear_Bindings;

end GWindows.Databases.Controls;
