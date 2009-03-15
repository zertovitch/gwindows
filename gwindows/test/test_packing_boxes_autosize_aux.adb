--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2005 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

with GWindows.Events;
with GWindows.Types.AUnit;
package body Test_Packing_Boxes_Autosize_Aux is

   procedure On_Create (Window : in out Top_Window_Type) is
   begin
      On_Destroy_Handler (Window, GWindows.Events.Do_End_Loop'Access);

      Packing_Direction (Window, GWindows.Packing_Boxes.Vertical);
      Padding (Window, 5);
      Insets (Window, (Top => 5, Left => 10, Bottom => 15, Right => 20));
      Fill_Height (Window, False);

      GWindows.Buttons.Create
        (Window.Button_1,
         Parent => Window,
         Text   => "Button_1");
      --  Recommended_Size (Button_1) => (64, 22)

      GWindows.Packing_Boxes.Create
        (Window.Hbox,
         Parent    => Window,
         Direction => GWindows.Packing_Boxes.Horizontal);
      GWindows.Packing_Boxes.Padding (Window.Hbox, 3);
      GWindows.Packing_Boxes.Insets
        (Window.Hbox,
         (Top => 3, Left => 6, Bottom => 9, Right => 12));
      GWindows.Packing_Boxes.Fill_Width (Window.Hbox, False);

      GWindows.Buttons.Create
        (Window.Button_2,
         Parent => Window.Hbox,
         Text   => "Button_2");
      --  Recommended_Size (Button_2) => (64, 22)

      GWindows.Buttons.Create
        (Window.Button_3,
         Parent => Window.Hbox,
         Text   => "Button_3 foo"); --  longer caption than button_2
      --  Recommended_Size (Button_3) => (88, 22)

      declare
         New_Size : constant GWindows.Types.Size_Type :=
           GWindows.Packing_Boxes.Recommended_Size (Window.Hbox);
      begin
         GWindows.Packing_Boxes.Size
           (Window.Hbox, Height => New_Size.Height, Width => New_Size.Width);
      end;
      declare
         New_Size : constant GWindows.Types.Size_Type :=
           Recommended_Size (Window);
      begin
         Size
           (Window, Height => New_Size.Height, Width => New_Size.Width);
      end;
   end On_Create;

   procedure Check
     (Label                : in String;
      Computed             : in Top_Window_Type;
      Expected_Client_Size : in GWindows.Types.Size_Type;
      Expected_Button_1    : in GWindows.Types.Rectangle_Type;
      Expected_Hbox        : in GWindows.Types.Rectangle_Type;
      Expected_Button_2    : in GWindows.Types.Rectangle_Type;
      Expected_Button_3    : in GWindows.Types.Rectangle_Type)
   is
      use GWindows.Packing_Boxes;
      use GWindows.Buttons;
      use GWindows.Types.AUnit;
   begin
      Check
        (Label & ".Client_Size",
         Client_Area_Size (Computed),
         Expected_Client_Size);
      Check
        (Label & ".Button_1",
         Location (Computed.Button_1),
         Expected_Button_1);
      Check (Label & ".Hbox", Location (Computed.Hbox), Expected_Hbox);
      Check
        (Label & ".Button_2",
         Location (Computed.Button_2),
         Expected_Button_2);
      Check
        (Label & ".Button_3",
         Location (Computed.Button_3),
         Expected_Button_3);
   end Check;

end Test_Packing_Boxes_Autosize_Aux;
