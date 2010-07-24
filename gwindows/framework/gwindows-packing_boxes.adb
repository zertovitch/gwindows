------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                G W I N D O W S . P A C K I N G _ B O X E S               --
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

package body GWindows.Packing_Boxes is
   use GWindows.Types;

   -----------------
   -- Fill_Height --
   -----------------

   procedure Fill_Height
     (Window : in out Packing_Box_Type;
      State  : in     Boolean            := True)
   is
   begin
      Window.Fill_Height := State;
   end Fill_Height;

   -----------------
   -- Fill_Height --
   -----------------

   function Fill_Height (Window : in Packing_Box_Type) return Boolean is
   begin
      return Window.Fill_Height;
   end Fill_Height;

   ----------------
   -- Fill_Width --
   ----------------

   procedure Fill_Width
     (Window : in out Packing_Box_Type;
      State  : in     Boolean            := True)
   is
   begin
      Window.Fill_Width := State;
   end Fill_Width;

   ----------------
   -- Fill_Width --
   ----------------

   function Fill_Width (Window : in Packing_Box_Type) return Boolean is
   begin
      return Window.Fill_Width;
   end Fill_Width;

   ------------
   -- Insets --
   ------------

   procedure Insets
     (Window : in out Packing_Box_Type;
      Rect   : in     GWindows.Types.Rectangle_Type)
   is
   begin
      Window.Insets := Rect;
   end Insets;

   ------------
   -- Insets --
   ------------

   function Insets
     (Window : in Packing_Box_Type)
      return GWindows.Types.Rectangle_Type
   is
   begin
      return Window.Insets;
   end Insets;

   -------------
   -- On_Size --
   -------------

   procedure On_Size
     (Window : in out Packing_Box_Type;
      Width  : in     Integer;
      Height : in     Integer)
   is
   begin
      GWindows.Windows.On_Size
        (GWindows.Windows.Window_Type (Window), Width, Height);
      Pack (Packing_Box_Type'Class (Window));
   end On_Size;

   ----------
   -- Pack --
   ----------

   procedure Pack (Window : in out Packing_Box_Type) is
      use GWindows.Base;
      use type Interfaces.C.long;

      Control_Width  : Natural;
      Control_Height : Natural;
      Child_Count    : Natural := 0;
      Total_Width    : Natural := 0;
      Total_Height   : Natural := 0;
      Current        : Natural;

      procedure Count_Children (Child : Pointer_To_Base_Window_Class);
      --  Call back enumeration Count number of children

      procedure Size_Children (Child : Pointer_To_Base_Window_Class);
      --  Call back enumeration function for sizing children

      procedure Pack_Children (Child : Pointer_To_Base_Window_Class);
      --  Call back enumeration function for packing children

      procedure Count_Children (Child : Pointer_To_Base_Window_Class) is
      begin
         if
           Handle (Parent (Child.all).all) = Handle (Window)
         then
            Child_Count := Child_Count + 1;

            if Child.all in Packing_Box_Type'Class then
               Pack (Packing_Box_Type (Child.all));
            end if;

            Total_Width := Total_Width + Client_Area_Width (Child.all);
            Total_Height := Total_Height + Client_Area_Height (Child.all);
         end if;
      end Count_Children;

      procedure Size_Children (Child : Pointer_To_Base_Window_Class) is
      begin
         if
           Handle (Parent (Child.all).all) = Handle (Window)
         then
            if Window.Fill_Height then
               Height (Child.all, Control_Height);
            end if;

            if Window.Fill_Width then
               Width (Child.all, Control_Width);
            end if;
         end if;

      exception
         when others =>
            null; -- out of bounds windows ignored
      end Size_Children;

      procedure Pack_Children (Child : Pointer_To_Base_Window_Class) is
      begin
         if
           Handle (Parent (Child.all).all) = Handle (Window)
         then
            if
              Window.Direction = Horizontal or
              Window.Direction = Horizontal_From_Center or
              Window.Direction = Horizontal_From_Right
            then
               Move (Child.all, Current, Window.Insets.Top);

               Current := Current + Width (Child.all) + Window.Padding;
            else
               Move (Child.all, Window.Insets.Left, Current);

               Current := Current + Height (Child.all) + Window.Padding;
            end if;
         end if;

      exception
         when others =>
            null; -- out of bounds windows ignored
      end Pack_Children;

   begin
      if Window.Auto_Size_To_Contents then
         declare
            New_Size : GWindows.Types.Size_Type          :=
              Content_Bounds (Window);
            Old_Size : constant GWindows.Types.Size_Type := Size (Window);
            Dock_At  : constant GWindows.Base.Dock_Type  := Dock (Window);
         begin
            if Dock_At = At_Top or Dock_At = At_Bottom then
               New_Size.Width := Old_Size.Width;
            elsif Dock_At = At_Left or Dock_At = At_Right then
               New_Size.Height := Old_Size.Height;
            end if;

            if New_Size /= Old_Size and Dock_At /= Fill then
               Size (Window, New_Size);
            end if;
         end;
      end if;

      if not (Iconic (Window)) then
         Enumerate_Children (Window, Count_Children'Unrestricted_Access);

         if Window.Fill_Width or Window.Fill_Height then

            if
              Window.Direction = Horizontal or
              Window.Direction = Horizontal_From_Center or
              Window.Direction = Horizontal_From_Right
            then
               if Window.Fill_Height then
                  Control_Height := Client_Area_Height (Window) -
                    Window.Insets.Top -
                    Window.Insets.Bottom;
               end if;

               if Window.Fill_Width then
                  Control_Width := (Client_Area_Width (Window) -
                                    (Window.Padding * (Child_Count - 1)) -
                                    Window.Insets.Left -
                                    Window.Insets.Right) / Child_Count;
               end if;
            else
               if  Window.Fill_Width then
                  Control_Width := Client_Area_Width (Window) -
                    Window.Insets.Left -
                    Window.Insets.Right;
               end if;

               if Window.Fill_Height then
                  Control_Height := (Client_Area_Height (Window) -
                                     (Window.Padding * (Child_Count - 1)) -
                                     Window.Insets.Top -
                                     Window.Insets.Bottom) / Child_Count;
               end if;
            end if;

            Enumerate_Children (Window, Size_Children'Unrestricted_Access);
         end if;

         case Window.Direction is
            when Horizontal =>
               Current := Window.Insets.Left;
            when Vertical =>
               Current := Window.Insets.Top;
            when Horizontal_From_Center =>
               Current := (Client_Area_Width (Window) -
                 Window.Insets.Left -
                 Window.Insets.Right -
                 Total_Width) / 2 + Window.Insets.Left;
            when Vertical_From_Center =>
               Current := (Client_Area_Height (Window) -
                 Window.Insets.Top -
                 Window.Insets.Bottom -
                 Total_Height) / 2 + Window.Insets.Top;
            when Horizontal_From_Right =>
               Current := Client_Area_Width (Window) -
                 Window.Insets.Left -
                 Window.Insets.Right -
                 Total_Width;
            when Vertical_From_Bottom =>
               Current := Client_Area_Height (Window) -
                 Window.Insets.Top -
                 Window.Insets.Bottom -
                 Total_Height;
         end case;

         Enumerate_Children (Window, Pack_Children'Unrestricted_Access);
      end if;
   exception
      when others =>
         null; -- Window too small
   end Pack;

   ------------------------
   --  Recommended_Size  --
   ------------------------

   function Recommended_Size (Window : in Packing_Box_Type)
                             return GWindows.Types.Size_Type
   is
      use GWindows.Base;

      Total_Size     : Size_Type := (0, 0);
      Max_Child_Size : Size_Type := (0, 0);
      Count          : Integer   := 0;

      procedure Size_Children (Child : Pointer_To_Base_Window_Class);
      --  Call back enumeration function for accumulating sizes of
      --  children

      procedure Size_Children (Child : Pointer_To_Base_Window_Class)
      is
         Child_Size : Size_Type;
         use type Interfaces.C.long;
      begin
         if Handle (Parent (Child.all).all) /= Handle (Window) then
            return;
         end if;

         Count := Count + 1;

         Child_Size := Recommended_Size (Child.all);

         Max_Child_Size := Max (Child_Size, Max_Child_Size);

         case Window.Direction is
            when Horizontal | Horizontal_From_Center | Horizontal_From_Right =>
               Total_Size.Width  := Total_Size.Width + Child_Size.Width;

               Total_Size.Height := Integer'Max
                 (Total_Size.Height, Child_Size.Height);

            when Vertical | Vertical_From_Center | Vertical_From_Bottom =>
               Total_Size.Height := Total_Size.Height + Child_Size.Height;

               Total_Size.Width  := Integer'Max
                 (Total_Size.Width, Child_Size.Width);
         end case;
      end Size_Children;

   begin
      Enumerate_Children (Window, Size_Children'Unrestricted_Access);

      case Window.Direction is
         when Horizontal | Horizontal_From_Center | Horizontal_From_Right =>
            if Window.Fill_Width then
               Total_Size.Width := Count * Max_Child_Size.Width;
            end if;

            Total_Size.Width :=
              Total_Size.Width + (Count - 1) * Window.Padding;

         when Vertical | Vertical_From_Center | Vertical_From_Bottom =>
            if Window.Fill_Height then
               Total_Size.Height := Count * Max_Child_Size.Height;
            end if;

            Total_Size.Height :=
              Total_Size.Height + (Count - 1) * Window.Padding;
      end case;

      Total_Size := Total_Size +
        (Width  => Window.Insets.Left + Window.Insets.Right,
         Height => Window.Insets.Top + Window.Insets.Bottom);

      return Calculate_New_Window_Size (Window, Total_Size);
   end Recommended_Size;

   ----------------------
   --  Content_Bounds  --
   ----------------------

   function Content_Bounds (Window : in Packing_Box_Type)
                           return GWindows.Types.Size_Type is
      use GWindows.Base;

      Total_Size : Size_Type := (0, 0);
      Count      : Integer   := 0;

      procedure Size_Children (Child : Pointer_To_Base_Window_Class);
      --  Call back enumeration function for accumulating sizes of
      --  children

      procedure Size_Children (Child : Pointer_To_Base_Window_Class)
      is
         Child_Size : Size_Type;
         use type Interfaces.C.long;
      begin
         if Handle (Parent (Child.all).all) /= Handle (Window) then
            return;
         end if;

         Count := Count + 1;

         if Child.all in Packing_Box_Type'Class then
            Pack (Packing_Box_Type (Child.all));
         end if;

         Child_Size := Size (Child.all);

         case Window.Direction is
            when Horizontal | Horizontal_From_Center | Horizontal_From_Right =>
               Total_Size.Width  := Total_Size.Width + Child_Size.Width;

               Total_Size.Height := Integer'Max
                 (Total_Size.Height, Child_Size.Height);

            when Vertical | Vertical_From_Center | Vertical_From_Bottom =>
               Total_Size.Height := Total_Size.Height + Child_Size.Height;

               Total_Size.Width  := Integer'Max
                 (Total_Size.Width, Child_Size.Width);
         end case;
      end Size_Children;

   begin
      Enumerate_Children (Window, Size_Children'Unrestricted_Access);

      case Window.Direction is
         when Horizontal | Horizontal_From_Center | Horizontal_From_Right =>
            Total_Size.Width :=
              Total_Size.Width + (Count - 1) * Window.Padding;

         when Vertical | Vertical_From_Center | Vertical_From_Bottom =>
            Total_Size.Height :=
              Total_Size.Height + (Count - 1) * Window.Padding;
      end case;

      Total_Size := Total_Size +
        (Width  => Window.Insets.Left + Window.Insets.Right,
         Height => Window.Insets.Top + Window.Insets.Bottom);

      return Calculate_New_Window_Size (Window, Total_Size);
   end Content_Bounds;

   -----------------------
   -- Packing_Direction --
   -----------------------

   procedure Packing_Direction
     (Window    : in out Packing_Box_Type;
      Direction : in     Packing_Direction_Type)
   is
   begin
      Window.Direction := Direction;
   end Packing_Direction;

   -----------------------
   -- Packing_Direction --
   -----------------------

   function Packing_Direction
     (Window : in Packing_Box_Type)
      return Packing_Direction_Type
   is
   begin
      return Window.Direction;
   end Packing_Direction;

   -------------
   -- Padding --
   -------------

   procedure Padding
     (Window : in out Packing_Box_Type;
      Amount : in     Natural)
   is
   begin
      Window.Padding := Amount;
   end Padding;

   -------------
   -- Padding --
   -------------

   function Padding (Window : in Packing_Box_Type) return Natural is
   begin
      return Window.Padding;
   end Padding;

   ------------
   -- Create --
   ------------

   procedure Create
     (Window     : in out Packing_Box_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer                              := 0;
      Top        : in     Integer                              := 0;
      Width      : in     Integer                              := 0;
      Height     : in     Integer                              := 0;
      Direction  : in     Packing_Direction_Type               := Horizontal;
      All_Keys   : in     Boolean                              := False;
      Container  : in     Boolean                              := True;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is
   begin
      Window.Direction := Direction;
      Create_As_Control (Window, Parent, "",
                         Left, Top, Width, Height,
                         All_Keys, Container, ID, 0, Show, Is_Dynamic);
      Pack (Window);
   end Create;

   ----------------
   -- Inset_Left --
   ----------------

   procedure Inset_Left (Window : in out Packing_Box_Type;
                         Value  : in     Natural)
   is
   begin
      Window.Insets.Left := Value;
   end Inset_Left;

   function Inset_Left (Window : in Packing_Box_Type)
                       return Natural
   is
   begin
      return Window.Insets.Left;
   end Inset_Left;

   ---------------
   -- Inset_Top --
   ---------------

   procedure Inset_Top (Window : in out Packing_Box_Type;
                        Value  : in     Natural)
   is
   begin
      Window.Insets.Top := Value;
   end Inset_Top;

   function Inset_Top (Window : in Packing_Box_Type)
                      return Natural
   is
   begin
      return Window.Insets.Top;
   end Inset_Top;

   -----------------
   -- Inset_Right --
   -----------------

   procedure Inset_Right (Window : in out Packing_Box_Type;
                          Value  : in     Natural)
   is
   begin
      Window.Insets.Right := Value;
   end Inset_Right;

   function Inset_Right (Window : in Packing_Box_Type)
                        return Natural
   is
   begin
      return Window.Insets.Right;
   end Inset_Right;

   ------------------
   -- Inset_Bottom --
   ------------------

   procedure Inset_Bottom (Window : in out Packing_Box_Type;
                           Value  : in     Natural)
   is
   begin
      Window.Insets.Bottom := Value;
   end Inset_Bottom;

   function Inset_Bottom (Window : in Packing_Box_Type)
                         return Natural
   is
   begin
      return Window.Insets.Bottom;
   end Inset_Bottom;

   --------------
   -- Run_Mode --
   --------------

   procedure Run_Mode (Window : in out Packing_Box_Type;
                       Value  : in     GWindows.Base.Run_Mode_Type)
   is
      use GWindows.Base;
      use GWindows.Windows;
   begin
      GWindows.Windows.Run_Mode (Window_Type (Window), Value);

      if
        Value = Development_Create_Complete or
        Value = Development_Running
      then
         Pack (Window);
      end if;
   end Run_Mode;

   --------------------------
   -- Auto_Size_To_Contents --
   --------------------------

   procedure Auto_Size_To_Contents (Window : in out Packing_Box_Type;
                                   Value  : in     Boolean   := True)
   is
   begin
      Window.Auto_Size_To_Contents := Value;
   end Auto_Size_To_Contents;

   function Auto_Size_To_Contents (Window : in Packing_Box_Type) return Boolean
   is
   begin
      return Window.Auto_Size_To_Contents;
   end Auto_Size_To_Contents;

end GWindows.Packing_Boxes;
