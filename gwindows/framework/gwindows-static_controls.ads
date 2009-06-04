------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--              G W I N D O W S . S T A T I C _ C O N T R O L S             --
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

with GWindows.Base;
with GWindows.Drawing_Objects;
with GWindows.Types;

package GWindows.Static_Controls is

   -------------------------------------------------------------------------
   --  Label_Type
   -------------------------------------------------------------------------

   type Label_Type is new GWindows.Base.Base_Window_Type with private;
   type Label_Access is access all Label_Type;
   type Pointer_To_Label_Class is access all Label_Type'Class;

   -------------------------------------------------------------------------
   --  Label_Type - Creation Methods
   -------------------------------------------------------------------------

   type Alignment_Type is
     (Left, Right, Center, Left_No_Word_Wrap, Static_Size);
   --  Alignment/Sizing of text or graphic
   --  If Static_Size is set and the static control is a graphical type
   --  then the control will not grow to fit the graphic

   type Border_Type is
     (None, Simple, Half_Sunken);

   procedure Create
     (Static     : in out Label_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Left       : in     Integer                              := 0;
      Top        : in     Integer                              := 0;
      Width      : in     Integer                              := 0;
      Height     : in     Integer                              := 0;
      Alignment  : in     Alignment_Type                       :=
        GWindows.Static_Controls.Left;
      Border     : in     Border_Type                          := None;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);
   --  Create Label

   procedure Create_Label
     (Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Left       : in     Integer                              := 0;
      Top        : in     Integer                              := 0;
      Width      : in     Integer                              := 0;
      Height     : in     Integer                              := 0;
      Alignment  : in     Alignment_Type                       :=
        GWindows.Static_Controls.Left;
      Border     : in     Border_Type                          := None;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True);
   --  Create Label with no variable

   function Recommended_Size (Static : in Label_Type)
                             return GWindows.Types.Size_Type;

   -------------------------------------------------------------------------
   --  Label_Type - Event Handlers
   -------------------------------------------------------------------------
   --  See Event Methods for details on each event

   procedure On_Click_Handler (Static  : in out Label_Type;
                               Handler : in GWindows.Base.Action_Event);
   procedure Fire_On_Click (Static : in out Label_Type);

   procedure On_Double_Click_Handler (Static  : in out Label_Type;
                               Handler : in GWindows.Base.Action_Event);
   procedure Fire_On_Double_Click (Static : in out Label_Type);

   procedure On_Enable_Handler (Static  : in out Label_Type;
                               Handler : in GWindows.Base.Action_Event);
   procedure Fire_On_Enable (Static : in out Label_Type);

   procedure On_Disable_Handler (Static  : in out Label_Type;
                               Handler : in GWindows.Base.Action_Event);
   procedure Fire_On_Disable (Static : in out Label_Type);

   -------------------------------------------------------------------------
   --  Label_Type - Event Methods
   -------------------------------------------------------------------------

   procedure On_Click (Static : in out Label_Type);
   --  Item clicked

   procedure On_Double_Click (Static : in out Label_Type);
   --  Item double clicked

   procedure On_Enable (Static : in out Label_Type);
   --  Item enabled

   procedure On_Disable (Static : in out Label_Type);
   --  Item disabled

   -------------------------------------------------------------------------
   --  Label_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  When overiding events, to insure that the event handlers will still
   --  be executed when set by users, call the base class or fire the event
   --  handler directly.

   procedure On_Command (Window  : in out Label_Type;
                         Code    : in     Integer;
                         ID      : in     Integer;
                         Control : in
                           GWindows.Base.Pointer_To_Base_Window_Class);
   --  Receives command messages from parent window

   -------------------------------------------------------------------------
   --  Icon_Type
   -------------------------------------------------------------------------

   type Icon_Type is new Label_Type with private;
   type Icon_Access is access all Icon_Type;
   type Pointer_To_Icon_Class is access all Icon_Type'Class;

   -------------------------------------------------------------------------
   --  Icon_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Static     : in out Icon_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Alignment  : in     Alignment_Type                       :=
        GWindows.Static_Controls.Left;
      Border     : in     Border_Type                          := None;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);

   --  Create Icon
   --  Text is name of Icon in resource file
   --  For a numeric resource use #XXXX where XXXX is the resource ID

   procedure Create_Icon
     (Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Alignment  : in     Alignment_Type                       :=
        GWindows.Static_Controls.Left;
      Border     : in     Border_Type                          := None;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True);
   --  Create Icon with no variable

   -------------------------------------------------------------------------
   --  Bitmap_Type
   -------------------------------------------------------------------------

   type Bitmap_Type is new Label_Type with private;
   type Bitmap_Access is access all Bitmap_Type;
   type Pointer_To_Bitmap_Class is access all Bitmap_Type'Class;

   -------------------------------------------------------------------------
   --  Bitmap_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Static     : in out Bitmap_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Alignment  : in     Alignment_Type                       :=
        GWindows.Static_Controls.Left;
      Border     : in     Border_Type                          := None;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);

   --  Create Bitmap
   --  Text is name of Bitmap in resource file
   --  For a numeric resource use #XXXX where XXXX is the resource ID

   procedure Create_Bitmap
     (Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Alignment  : in     Alignment_Type                       :=
        GWindows.Static_Controls.Left;
      Border     : in     Border_Type                          := None;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True);
   --  Create Bitmap with no Variable

   procedure Set_Bitmap (Static : in out Bitmap_Type;
                         Bitmap : in GWindows.Drawing_Objects.Bitmap_Type);
   --  Set bitmap for static

   -------------------------------------------------------------------------
   --  Meta_File_Type
   -------------------------------------------------------------------------

   type Meta_File_Type is new Label_Type with private;
   type Meta_File_Access is access all Meta_File_Type;
   type Pointer_To_Meta_File_Class is access all Meta_File_Type'Class;

   -------------------------------------------------------------------------
   --  Meta_File_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Static     : in out Meta_File_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Alignment  : in     Alignment_Type                       :=
        GWindows.Static_Controls.Left;
      Border     : in     Border_Type                          := None;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);

   --  Create Meta File
   --  Text is name of Meta File in resource file
   --  For a numeric resource use #XXXX where XXXX is the resource ID

   procedure Create_Meta_File
     (Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Alignment  : in     Alignment_Type                       :=
        GWindows.Static_Controls.Left;
      Border     : in     Border_Type                          := None;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True);
   --  Create Meta File with no variable

private
   type Label_Type is new GWindows.Base.Base_Window_Type with
      record
         On_Click_Event        : GWindows.Base.Action_Event := null;
         On_Double_Click_Event : GWindows.Base.Action_Event := null;
         On_Enable_Event       : GWindows.Base.Action_Event := null;
         On_Disable_Event      : GWindows.Base.Action_Event := null;
      end record;

   type Icon_Type is new Label_Type with null record;

   type Bitmap_Type is new Label_Type with null record;

   type Meta_File_Type is new Label_Type with null record;

end GWindows.Static_Controls;
