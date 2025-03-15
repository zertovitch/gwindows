------------------------------------------------------------------------------
--                                                                          --
--                      gwindows.buttons.ex_buttons                         --
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
-- More information about GWindows and the most current public version can  --
-- be located on the web at http://www.adapower.com/GWindows                --
--                                                                          --
------------------------------------------------------------------------------
--  Features of Extended_Button_Type:
--  -> You may set an Id to an Extended_Button
--  -> The Button may carry an Image
--  -> The Button may blink showing two Images toggling
------------------------------------------------------------------------------

with GWindows.Drawing_Objects; use GWindows.Drawing_Objects;
pragma Elaborate_All (GWindows.Drawing_Objects);

package GWindows.Buttons.Ex_buttons is

   type Extended_Button_Type is new Button_Type with private;
   type Extended_Button_Access is access all Extended_Button_Type;
   type Pointer_To_Extended_Button_Class
    is access all Extended_Button_Type'Class;

   Null_Bitmap : Bitmap_Type;

   procedure Create
      (Button         : in out Extended_Button_Type;
       Parent         : in out GWindows.Base.Base_Window_Type'Class;
       Left           : in     Integer;
       Top            : in     Integer;
       Width          : in     Integer;
       Height         : in     Integer;
       Image          : in     Bitmap_Type                  := Null_Bitmap;
       ID             : in     Integer                      := 0;
       Show           : in     Boolean                      := True;
       Is_Dynamic     : in     Boolean                      := False);
   --  creates an Extended_button

   procedure Set_Button_Id (Button : in out Extended_Button_Type;
                            Id : in Integer);
   function Get_Button_Id (Button : in Extended_Button_Type) return Integer;
   --  get and set a button-id

   procedure Init_Flash (Button       : in out Extended_Button_Type;
                         Interval     : in Float       := 0.5; -- seconds
                         Silent_Image : in Bitmap_Type := Null_Bitmap;
                         Activ_Image  : in Bitmap_Type := Null_Bitmap);
   --  initialize the flash-option
   --  must be called before Start_Flash

   procedure Start_Flash (Button : in out Extended_Button_Type);
   --  start the flash

   procedure Stop_Flash (Button : in out Extended_Button_Type);
   --  stop the flash

   --------------------
   -- event-handling --
   --------------------

   procedure On_Create (Window : in out Extended_Button_Type);

   procedure finalize (Window : in out Extended_Button_Type);

private

   task type Flash_Task_Type is
      entry init (Button : in Extended_Button_Type);
      entry Start;
      entry Stop;
   end Flash_Task_Type;

   type Flash_Task_Access is access Flash_Task_Type;

   type Extended_Button_Type is new Button_Type with
      record
         Id                 : Integer;
         Static_Image       : Bitmap_Type;
         Flash_activ_Image  : Bitmap_Type;
         Flash_silent_Image : Bitmap_Type;
         Flash_Interval     : Standard.Duration := 0.5;
         Flash_Task_Ptr     : Flash_Task_Access := null;
      end record;

end GWindows.Buttons.Ex_buttons;
