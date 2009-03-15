--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2004, 2005 Stephen Leake.  All Rights Reserved.
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
package body Test_Packing_Boxes_Aux is

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
         Text   => "Button_1",
         Top    => 0,
         Left   => 0,
         Width  => 50,
         Height => 20);

      GWindows.Packing_Boxes.Create
        (Window.Hbox,
         Parent    => Window,
         Top       => 0,
         Left      => 0,
         Width     => 130,
         Height    => 20,
         Direction => GWindows.Packing_Boxes.Horizontal);
      GWindows.Packing_Boxes.Padding (Window.Hbox, 3);
      GWindows.Packing_Boxes.Insets
        (Window.Hbox,
         (Top => 3, Left => 6, Bottom => 9, Right => 12));
      GWindows.Packing_Boxes.Fill_Width (Window.Hbox, False);

      GWindows.Buttons.Create
        (Window.Button_2,
         Parent => Window.Hbox,
         Text   => "Button_2",
         Top    => 0,
         Left   => 0,
         Width  => 50,
         Height => 20);

      GWindows.Buttons.Create
        (Window.Button_3,
         Parent => Window.Hbox,
         Text   => "Button_3 foo", --  longer caption than Button_2
         Top    => 0,
         Left   => 0,
         Width  => 60,
         Height => 20);

      GWindows.Packing_Boxes.Pack (Window.Hbox);
      Pack (Window);

   end On_Create;

   procedure Check
     (Label              : in String;
      Computed           : in Top_Window_Type;
      Expected_Vbox_Size : in GWindows.Types.Size_Type;
      Expected_Button_1  : in GWindows.Types.Rectangle_Type;
      Expected_Hbox      : in GWindows.Types.Rectangle_Type;
      Expected_Button_2  : in GWindows.Types.Rectangle_Type;
      Expected_Button_3  : in GWindows.Types.Rectangle_Type)
   is
      use GWindows.Packing_Boxes;
      use GWindows.Buttons;
      use GWindows.Types.AUnit;
   begin
      Check
        (Label & ".Vbox_Size",
         Client_Area_Size (Computed),
         Expected_Vbox_Size);
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

end Test_Packing_Boxes_Aux;
