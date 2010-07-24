------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                 G W I N D O W S . I M A G E _ L I S T S                  --
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

with GWindows.GStrings;
with GWindows.Internal;

package body GWindows.Image_Lists is

   -------------------------------------------------------------------------
   --  Package Body
   -------------------------------------------------------------------------

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (List : in out Image_List_Type)
   is
      pragma Warnings (Off, List);
   begin
      null;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (List : in out Image_List_Type)
   is
   begin
      Destroy (List);
   end Finalize;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (List : in out Image_List_Type)
   is
      procedure ImageList_Destroy
        (HIML : GWindows.Types.Handle := List.HIMAGELIST);
      pragma Import (StdCall, ImageList_Destroy, "ImageList_Destroy");
   begin
      ImageList_Destroy;
   end Destroy;

   ------------
   -- Handle --
   ------------

   procedure Handle (List   : in out Image_List_Type;
                     Handle : in     GWindows.Types.Handle)
   is
   begin
      List.HIMAGELIST := Handle;
   end Handle;

   function Handle (List : in Image_List_Type) return GWindows.Types.Handle
   is
   begin
      return List.HIMAGELIST;
   end Handle;

   ------------
   -- Create --
   ------------

   procedure Create (List          : in out Image_List_Type;
                     Width, Height : in     Positive;
                     Initial_Size  : in     Positive;
                     Grow_By       : in     Natural         := 1)
   is
      function ImageList_Create
        (CX    : Positive := Width;
         XY    : Positive := Height;
         FLAGS : Natural  := 0;
         Init  : Positive := Initial_Size;
         Grow  : Natural  := Grow_By)
        return GWindows.Types.Handle;
      pragma Import (StdCall, ImageList_Create, "ImageList_Create");
   begin
      Handle (List, ImageList_Create);
   end Create;

   procedure Create (List    : in out Image_List_Type;
                     Name    : in     GString;
                     Width   : in     Positive;
                     Grow_By : in     Natural         := 1)
   is
      C_Text : Interfaces.C.char_array :=
        Interfaces.C.To_C (GWindows.GStrings.To_String (Name));

      LR_CREATEDIBSECTION : constant := 16#2000#;

      function ImageList_LoadImage
        (HINST : GWindows.Types.Handle :=
           GWindows.Internal.Current_hInstance;
         Name  : access Interfaces.C.char      := C_Text (C_Text'First)'Access;
         CX    : in     Positive               := Width;
         Grow  : in     Natural                := Grow_By;
         CREF  : in     Interfaces.C.unsigned  := 16#FF000000#;
         UT    : in     Integer                := 0;
         FLAGS : in     Natural                := LR_CREATEDIBSECTION)
        return GWindows.Types.Handle;
      pragma Import (StdCall, ImageList_LoadImage, "ImageList_LoadImage");
   begin
      Handle (List, ImageList_LoadImage);
   end Create;

   ----------------------
   -- Create_From_File --
   ----------------------

   procedure Create_From_File (List      : in out Image_List_Type;
                               File_Name : in     GString;
                               Width     : in     Positive;
                               Grow_By   : in     Natural         := 1)
   is
      pragma Warnings (Off, List);

      C_Text : Interfaces.C.char_array :=
        Interfaces.C.To_C (GWindows.GStrings.To_String (File_Name));
      LR_LOADFROMFILE : constant := 16;

      function ImageList_LoadImage
        (HINST : GWindows.Types.Handle := GWindows.Internal.Current_hInstance;
         Name  : access Interfaces.C.char     := C_Text (C_Text'First)'Access;
         CX    : in     Positive              := Width;
         Grow  : in     Natural               := Grow_By;
         CREF  : in     Interfaces.C.unsigned := 16#FF000000#;
         UT    : in     Integer               := 0;
         FLAGS : in     Natural               := LR_LOADFROMFILE)
        return GWindows.Types.Handle;
      pragma Import (StdCall, ImageList_LoadImage, "ImageList_LoadImage");
   begin
      Handle (List, ImageList_LoadImage);
   end Create_From_File;

   --------------
   -- Add_Icon --
   --------------

   procedure Add_Icon (List : in out Image_List_Type;
                       Icon : in     GWindows.Drawing_Objects.Icon_Type)
   is
      procedure ImageList_ReplaceIcon
        (HIML  : GWindows.Types.Handle := List.HIMAGELIST;
         Pos   : Integer               := -1;
         HICON : GWindows.Types.Handle :=
           GWindows.Drawing_Objects.Handle (Icon));
      pragma Import (StdCall, ImageList_ReplaceIcon, "ImageList_ReplaceIcon");
   begin
      ImageList_ReplaceIcon;
   end Add_Icon;

   ------------------
   -- Replace_Icon --
   ------------------

   procedure Replace_Icon (List  : in out Image_List_Type;
                           Index : in     Natural;
                           Icon  : in     GWindows.Drawing_Objects.Icon_Type)
   is
      procedure ImageList_ReplaceIcon
        (HIML  : GWindows.Types.Handle := List.HIMAGELIST;
         Pos   : Integer               := Index;
         HICON : GWindows.Types.Handle :=
           GWindows.Drawing_Objects.Handle (Icon));
      pragma Import (StdCall, ImageList_ReplaceIcon, "ImageList_ReplaceIcon");
   begin
      ImageList_ReplaceIcon;
   end Replace_Icon;

   ---------
   -- Add --
   ---------

   procedure Add (List   : in out Image_List_Type;
                  Bitmap : in     GWindows.Drawing_Objects.Bitmap_Type)
   is
      procedure ImageList_Add
        (HIML : GWindows.Types.Handle := List.HIMAGELIST;
         HBMP : GWindows.Types.Handle :=
           GWindows.Drawing_Objects.Handle (Bitmap);
         Mask : Integer               := 0);
      pragma Import (StdCall, ImageList_Add, "ImageList_Add");
   begin
      ImageList_Add;
   end Add;

   -----------
   -- Count --
   -----------

   function Count (List : in Image_List_Type) return Integer
   is
      function ImageList_GetImageCount
        (HIML : GWindows.Types.Handle := List.HIMAGELIST)
        return Integer;
      pragma Import (StdCall,
                     ImageList_GetImageCount,
                     "ImageList_GetImageCount");
   begin
      return ImageList_GetImageCount;
   end Count;

   ----------------
   -- Begin_Drag --
   ----------------

   procedure Begin_Drag (List  : in out Image_List_Type;
                         Index : in     Integer         := 0;
                         X, Y  : in     Integer         := 0)
   is
      procedure ImageList_BeginDrag
        (HIML : GWindows.Types.Handle := List.HIMAGELIST;
         Pos  : Integer               := Index;
         DX   : Integer               := X;
         DY   : Integer               := Y);
      pragma Import (StdCall, ImageList_BeginDrag, "ImageList_BeginDrag");
   begin
      ImageList_BeginDrag;
   end Begin_Drag;

   --------------
   -- End_Drag --
   --------------

   procedure End_Drag
   is
      procedure ImageList_EndDrag;
      pragma Import (StdCall,
                     ImageList_EndDrag,
                     "ImageList_EndDrag");
   begin
      ImageList_EndDrag;
   end End_Drag;

   -----------------------
   -- Drag_Cursor_Image --
   -----------------------

   procedure Drag_Cursor_Image (List  : in out Image_List_Type;
                                Index : in     Integer         := 0;
                                X, Y  : in     Integer         := 0)
   is
      procedure ImageList_SetDragCursorImage
        (HIML : GWindows.Types.Handle := List.HIMAGELIST;
         Pos  : Integer               := Index;
         DX   : Integer               := X;
         DY   : Integer               := Y);
      pragma Import (StdCall,
                     ImageList_SetDragCursorImage,
                     "ImageList_SetDragCursorImage");
   begin
      ImageList_SetDragCursorImage;
   end Drag_Cursor_Image;

   ----------------
   -- Drag_Enter --
   ----------------

   procedure Drag_Enter (Window : in out GWindows.Base.Base_Window_Type'Class;
                         X, Y   : in     Integer)
   is
      procedure ImageList_DragEnter
        (HWIN : GWindows.Types.Handle := GWindows.Base.Handle (Window);
         DX   : Integer               := X;
         DY   : Integer               := Y);
      pragma Import (StdCall, ImageList_DragEnter, "ImageList_DragEnter");
   begin
      ImageList_DragEnter;
   end Drag_Enter;

   ----------------
   -- Drag_Leave --
   ----------------

   procedure Drag_Leave (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      procedure ImageList_DragLeave
        (HWIN : GWindows.Types.Handle := GWindows.Base.Handle (Window));
      pragma Import (StdCall, ImageList_DragLeave, "ImageList_DragLeave");
   begin
      ImageList_DragLeave;
   end Drag_Leave;

   ---------------
   -- Drag_Move --
   ---------------

   procedure Drag_Move (Window : in out GWindows.Base.Base_Window_Type'Class;
                        X, Y   : in     Integer)
   is
      pragma Warnings (Off, Window);

      procedure ImageList_DragMove
        (DX : Integer := X;
         DY : Integer := Y);
      pragma Import (StdCall, ImageList_DragMove, "ImageList_DragMove");
   begin
      ImageList_DragMove;
   end Drag_Move;

   ---------------------
   -- Drag_Show_Image --
   ---------------------

   procedure Drag_Show_Image
   is
      procedure ImageList_DragShowNolock
        (Show : Boolean := True);
      pragma Import (StdCall,
                     ImageList_DragShowNolock,
                     "ImageList_DragShowNolock");
   begin
      ImageList_DragShowNolock;
   end Drag_Show_Image;

   ---------------------
   -- Drag_Hide_Image --
   ---------------------

   procedure Drag_Hide_Image
   is
      procedure ImageList_DragShowNolock
        (Show : Boolean := False);
      pragma Import (StdCall,
                     ImageList_DragShowNolock,
                     "ImageList_DragShowNolock");
   begin
      ImageList_DragShowNolock;
   end Drag_Hide_Image;

   ---------------
   -- Duplicate --
   ---------------

   procedure Duplicate (In_List  : in     Image_List_Type;
                        Out_List :    out Image_List_Type)
   is
      pragma Warnings (Off, In_List);
      --       function ImageList_Duplicate
      --         (HIML : GWindows.Types.Handle := In_List.HIMAGELIST)
      --         return GWindows.Types.Handle;
      --       pragma Import
      --  (StdCall, ImageList_Duplicate, "ImageList_Duplicate");
   begin
      Handle (Out_List, GWindows.Types.Null_Handle);
      null;
   end Duplicate;

   --------------
   -- Get_Icon --
   --------------

   procedure Get_Icon (List  : in     Image_List_Type;
                       Index : in     Natural;
                       Icon  :    out GWindows.Drawing_Objects.Icon_Type)
   is
      function ImageList_GetIcon
        (HIML  : GWindows.Types.Handle := List.HIMAGELIST;
         I     : Natural               := Index;
         Flags : Natural               := 0)
        return GWindows.Types.Handle;
      pragma Import (StdCall, ImageList_GetIcon, "ImageList_GetIcon");
   begin
      GWindows.Drawing_Objects.Handle (Icon, ImageList_GetIcon);
   end Get_Icon;

   ----------
   -- Draw --
   ----------

   procedure Draw (List   : in out Image_List_Type;
                   Canvas : in out GWindows.Drawing.Canvas_Type'Class;
                   Index  : in     Natural;
                   X, Y   : in     Integer)
   is
      procedure ImageList_Draw
        (HIML  : GWindows.Types.Handle := List.HIMAGELIST;
         Pos   : Integer               := Index;
         HDC   : GWindows.Types.Handle := GWindows.Drawing.Handle (Canvas);
         DX    : Integer               := X;
         DY    : Integer               := Y;
         Style : Integer               := 0);
      pragma Import (StdCall, ImageList_Draw, "ImageList_Draw");
   begin
      ImageList_Draw;
   end Draw;

end GWindows.Image_Lists;
