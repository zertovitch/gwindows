------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada Framework for Windows Development             --
--                                                                          --
--                   O F F I C E   A P P L I C A T I O N S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 2022 - 2023 Gautier de Montmollin          --
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
-- be located on the web at one of the following places:                    --
--   http://sf.net/projects/gnavi/                                          --
--   https://github.com/zertovitch/gwindows                                 --
--                                                                          --
------------------------------------------------------------------------------

with GWindows.Application,
     GWindows.Base,
     GWindows.GStrings;

with Ada.Wide_Characters.Handling;

package body Office_Applications is

   overriding procedure On_Button_Select
      (Control : in out Classic_Main_Tool_Bar_Type;
       Item    : in     Integer)
   is
       Parent : constant Access_To_Classic_Main_Window_Class :=
          Access_To_Classic_Main_Window_Class (Controlling_Parent (Control));
   begin
      --  A click simulates a menu entry selection.
      On_Menu_Select (Parent.all, Item);
   end On_Button_Select;

   function Shorten_File_Name
      (S : GWindows.GString; Max_Length : Positive) return GWindows.GString
   is
      prefix : constant := 15;
      shortcut : constant GWindows.GString := " ... ";
   begin
      pragma Assert (Max_Length >= prefix + shortcut'Length);
      if S'Length < Max_Length then
         return S;
      else
         return
            --  Length of this chunk: prefix.
            S (S'First .. S'First + prefix - 1) &
            --  Length of this chunk: shortcut'Length.
            shortcut &
            --  Length of this chunk: Max_Length - prefix - shortcut'Length:
            S (S'Last - Max_Length + prefix + shortcut'Length + 1 .. S'Last);
            ------> Total length: Max_Length
      end if;
   end Shorten_File_Name;

   procedure Add_MRU
      (MRU            : in out MRU_Info;
       Name           :        GWindows.GString;
       Line           :        Integer := -1;
       Add_To_Desktop :        Boolean := True)
   is
      x : Integer := MRU_Range'First - 1;
      upper_name : GWindows.GString := Name;
      best_line : Natural := 0;
      use GWindows, GWindows.GStrings;
      use type GWindows.GString_Unbounded;
   begin
      if Add_To_Desktop then
         --  Add name to the list in task bar or
         --  elsewhere in Windows Explorer or Desktop.
         GWindows.Application.Add_To_Recent_Documents (Name);
      end if;

      To_Upper (upper_name);

      --  Search for `Name` in the list.
      for m in MRU_Range loop
         if Ada.Wide_Characters.Handling.To_Upper
            (To_GString_From_Unbounded (MRU.Item (m).Name)) = upper_name
         then
            --  Case insensitive comparison (Jan-2007)
            x := m;
            best_line := MRU.Item (m).Line;
            exit;
         end if;
      end loop;

      if Line > -1 then
         best_line := Line;
      end if;

      if x = MRU_Range'First then
         --  `Name` was found and is already on the top of the list.
         --  We only need to adjust the line mumber.
         MRU.Item (MRU_Range'First).Line := best_line;
      else
         --  General case.
         if x > MRU_Range'First then
            --  `Name` was found at position x.
            null;
         else
            --  `Name` is new (was not found.
            --  We will shift down the full list.
            x := MRU_Range'Last;
         end if;
         --  Shift down items before x, erasing item #x.
         for i in reverse MRU_Range'First + 1 .. x loop
            MRU.Item (i) := MRU.Item (i - 1);
         end loop;
         --  Put `Name` as first item in the list.
         MRU.Item (MRU_Range'First) :=
            (Name => To_GString_Unbounded (Name),
             Line => best_line);
      end if;
   end Add_MRU;

   procedure Update_MRU_Menu
      (MRU  : MRU_Info;
       Menu : GWindows.Menus.Menu_Type)
   is
      use GWindows, GWindows.Menus, GWindows.GStrings;
   begin
      for i in MRU.ID_Menu'Range loop
         declare
            img : GString := Image (i);
            sfn : constant GString :=
               Shorten_File_Name
                  (To_GString_From_Unbounded (MRU.Item (i).Name), 50);
         begin
            img (img'First) := '&';
            Text (Menu      => Menu,
                  Locate_By => Command,
                  Locate_At => MRU.ID_Menu (i),
                  New_Text  => img & ' ' & sfn);
         end;
      end loop;
   end Update_MRU_Menu;

   procedure Close_Initial_Document
      (Main_Window : in out Classic_Main_Window_Type)
   is
      use GWindows.Base;

      procedure Try_Closing_Initial_Document
         (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class)
      is
      begin
         if Any_Window /= null
            and then Any_Window.all in Classic_Document_Window_Type'Class
         then
            declare
               W : Classic_Document_Window_Type renames
                  Classic_Document_Window_Type (Any_Window.all);
            begin
               if W.Extra_First_Doc
                  and then not
                     Is_Document_Modified
                        (Classic_Document_Window_Type'Class (W))
               then
                  --  This situation happens only if the startup (usually
                  --  blank) document is at its initial state.
                  --  Contents are either untouched, or with all
                  --  modifications undone.
                  Close (Any_Window.all);
               end if;
            end;
         end if;
      end Try_Closing_Initial_Document;

   begin
      Enumerate_Children
         (MDI_Client_Window (Main_Window).all,
          Try_Closing_Initial_Document'Unrestricted_Access);
   end Close_Initial_Document;

end Office_Applications;
