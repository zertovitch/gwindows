------------------------------------------------------------------------------
--                                                                          --
--                      gwindows.buttons.ex_buttons                         --
--                                                                          --
--                                 B o d y                                  --
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
-- More information about GWindows and the most current public version can  --
-- be located on the web at http://www.adapower.com/GWindows                --
--                                                                          --
------------------------------------------------------------------------------

with GWindows.Base; use GWindows.Base;
with Ada.Unchecked_Deallocation;

package body GWindows.Buttons.Ex_buttons is

   use type Interfaces.C.unsigned;

   BM_SETIMAGE            : constant := 247;
--   BM_GETIMAGE            : constant := 246;
   IMAGE_BITMAP           : constant := 0;
   BS_BITMAP              : constant := 128;
   GWL_STYLE              : constant := -16;

   procedure Set_Image (Button : in Extended_Button_Type;
                        Image  : in Bitmap_Type);

--   function Get_Image (Button : in Extended_Button_Type)
--                      return GWindows.Types.Handle;

   procedure Free is new Ada.Unchecked_Deallocation (Flash_Task_Type,
                                                     Flash_Task_Access);

   ------------------------
   -- imported functions --
   ------------------------

   -------------------
   -- SetWindowLong --
   -------------------

   procedure SetWindowLong (hwnd    : GWindows.Types.Handle;
                            nIndex  : Interfaces.C.int := GWL_STYLE;
                            newLong : Interfaces.C.unsigned);
   pragma Import (StdCall, SetWindowLong,
                    "SetWindowLong" & Character_Mode_Identifier);

   -------------------
   -- GetWindowLong --
   -------------------

   function GetWindowLong (hwnd   : GWindows.Types.Handle;
                           nIndex : Interfaces.C.int   := GWL_STYLE)
                          return Interfaces.C.unsigned;
   pragma Import (StdCall, GetWindowLong,
                    "GetWindowLong" & Character_Mode_Identifier);

   ----------------------
   -- public functions --
   ----------------------

   ------------
   -- Create --
   ------------

   procedure Create
    (Button     : in out Extended_Button_Type;
     Parent     : in out GWindows.Base.Base_Window_Type'Class;
     Left       : in     Integer;
     Top        : in     Integer;
     Width      : in     Integer;
     Height     : in     Integer;
     Image      : in     Bitmap_Type                          := Null_Bitmap;
     ID         : in     Integer                              := 0;
     Show       : in     Boolean                              := True;
     Is_Dynamic : in     Boolean                              := False)
   is
   begin
      Create_Control (Button, Parent, "Button", "", Left, Top,
                      Width, Height, ID, Is_Dynamic => Is_Dynamic);
      Button.Id := ID;
      Handle (Button.Static_Image, Handle (Image));
      Set_Image (Button, Image);

      Tab_Stop (Button);

      if Show then
         GWindows.Buttons.Show (Button_Type (Button));
      end if;

   end Create;

   -------------------
   -- set_button_id --
   -------------------

   procedure Set_Button_Id (Button : in out Extended_Button_Type;
                            Id     : in Integer)
   is
   begin
      Button.Id := Id;
   end Set_Button_Id;

   -------------------
   -- get_button_id --
   -------------------

   function Get_Button_Id (Button : in Extended_Button_Type) return Integer is
   begin
      return Button.Id;
   end Get_Button_Id;

   ----------------
   -- init_flash --
   ----------------

   procedure Init_Flash (Button       : in out Extended_Button_Type;
                         Interval     : in Float       := 0.5;
                         Silent_Image : in Bitmap_Type := Null_Bitmap;
                         Activ_Image  : in Bitmap_Type := Null_Bitmap)
   is
   begin
      Handle (Button.Flash_activ_Image, Handle (Activ_Image));
      Handle (Button.Flash_silent_Image, Handle (Silent_Image));
      Button.Flash_Interval := Standard.Duration (Interval);
   end Init_Flash;

   -----------------
   -- start_flash --
   -----------------

   procedure Start_Flash (Button : in out Extended_Button_Type) is

   begin
      if Button.Flash_Task_Ptr /= null then -- task is
         return;
      end if;
      Button.Flash_Task_Ptr := new Flash_Task_Type;
      Button.Flash_Task_Ptr.Init (Button);
      Button.Flash_Task_Ptr.Start;
   end Start_Flash;

   ----------------
   -- stop_flash --
   ----------------

   procedure Stop_Flash (Button : in out Extended_Button_Type) is
   begin
      if Button.Flash_Task_Ptr = null then -- task is not
         return;
      end if;
      Button.Flash_Task_Ptr.Stop;
      Free (Button.Flash_Task_Ptr);
      Button.Flash_Task_Ptr := null;
   end Stop_Flash;

   --------------------
   -- event-handling --
   --------------------

   ---------------
   -- on_create --
   ---------------

   procedure On_Create (Window : in out Extended_Button_Type) is
   begin
      --  setting new bitmap_style
      SetWindowLong (Handle (Window),
                     newLong => GetWindowLong (Handle (Window)) or BS_BITMAP);
   end On_Create;

   --------------
   -- finalize --
   --------------

   procedure finalize (Window : in out Extended_Button_Type) is
   begin
      Stop_Flash (Window);
      Finalize (Button_Type (Window));
   end finalize;

   --------------------
   -- body functions --
   --------------------

   ---------------
   -- task_body --
   ---------------

   task body Flash_Task_Type is
      type Flash_Status_Type is (Activ, Silent);
      Flash_Status : Flash_Status_Type     := Silent;
      T_Button_Ptr : Extended_Button_Access;
   begin
      accept init (Button : in Extended_Button_Type) do
            T_Button_Ptr := Extended_Button_Access
                              (Window_From_Handle (Handle (Button)));
      end init;

      accept Start;

      loop
         select
            accept Stop;
            Set_Image (T_Button_Ptr.all, T_Button_Ptr.Static_Image);
            exit;
         or
            delay T_Button_Ptr.Flash_Interval;
            if Flash_Status = Silent then
               Flash_Status := Activ;
               Set_Image (T_Button_Ptr.all, T_Button_Ptr.Flash_activ_Image);
            else
               Flash_Status := Silent;
               Set_Image (T_Button_Ptr.all, T_Button_Ptr.Flash_silent_Image);
            end if;
         end select;
      end loop;

   exception
      when others =>
         null;
   end Flash_Task_Type;

   ---------------
   -- Set_image --
   ---------------

   procedure Set_Image (Button : in Extended_Button_Type;
                        Image  : in Bitmap_Type)
   is
      procedure Sendmessage (Hwnd   : GWindows.Types.Handle := Handle (Button);
                             Umsg   : Interfaces.C.int  := BM_SETIMAGE;
                             Wparam : Integer           := IMAGE_BITMAP;
                             Lparam : GWindows.Types.Handle := Handle (Image));
      pragma Import (Stdcall, Sendmessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      Sendmessage;
   end Set_Image;

   ---------------
   -- Get_Image --
   ---------------

--   function Get_Image (Button : in Extended_Button_Type)
--                      return GWindows.Types.handle
--   Is
--      function Sendmessage (Hwnd   : Interfaces.C.Long := Handle (button);
--                            Umsg   : Interfaces.C.Int  := BM_GETIMAGE;
--                            Wparam : Integer           := IMAGE_BITMAP;
--                            Lparam : integer           := 0)
--                           return GWindows.Types.handle;
--      pragma Import (Stdcall, Sendmessage,
--                       "SendMessage" & Character_Mode_Identifier);
--   begin
--      return Sendmessage;
--   end get_Image;

end GWindows.Buttons.Ex_buttons;
