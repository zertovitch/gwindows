------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--              G W I N D O W S . E D I T _ B O X E S . R I C H             --
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

with TOM;

package GWindows.Edit_Boxes.Rich is
   pragma Elaborate_Body;

   -------------------------------------------------------------------------
   --  Rich_Edit_Box_Type
   -------------------------------------------------------------------------

   type Rich_Edit_Box_Type is
     new Multi_Line_Edit_Box_Type with private;
   type Rich_Edit_Box_Access is access all Rich_Edit_Box_Type;

   -------------------------------------------------------------------------
   --  Rich_Edit_Box_Type - Creation Functions
   -------------------------------------------------------------------------

   procedure Create
     (Edit              : in out Rich_Edit_Box_Type;
      Parent            : in out GWindows.Base.Base_Window_Type'Class;
      Text              : in     GString;
      Left              : in     Integer;
      Top               : in     Integer;
      Width             : in     Integer;
      Height            : in     Integer;
      Horizontal_Scroll : in     Boolean                            := True;
      ID                : in     Integer                            := 0;
      Show              : in     Boolean                            := True;
      Is_Dynamic        : in     Boolean                            := False);
   --  Create a single line Rich Text Edit Box

   procedure Create_Multi_Line
     (Edit              : in out Rich_Edit_Box_Type;
      Parent            : in out GWindows.Base.Base_Window_Type'Class;
      Text              : in     GString;
      Left              : in     Integer;
      Top               : in     Integer;
      Width             : in     Integer;
      Height            : in     Integer;
      Horizontal_Scroll : in     Boolean                            := False;
      Vertical_Scroll   : in     Boolean                            := True;
      Capture_Return    : in     Boolean                            := True;
      ID                : in     Integer                            := 0;
      Show              : in     Boolean                            := True;
      Is_Dynamic        : in     Boolean                            := False);
   --  Create Rich Text Edit Box

   procedure Create
     (Edit              : in out Rich_Edit_Box_Type;
      Parent            : in out GWindows.Base.Base_Window_Type'Class;
      Text              : in     GString;
      Left              : in     Integer;
      Top               : in     Integer;
      Width             : in     Integer;
      Height            : in     Integer;
      Horizontal_Scroll : in     Boolean;
      Vertical_Scroll   : in     Boolean;
      Capture_Return    : in     Boolean;
      ID                : in     Integer;
      Show              : in     Boolean                            := True;
      Is_Dynamic        : in     Boolean                            := False);
   --  Create Rich Text Edit Box

   -------------------------------------------------------------------------
   --  Rich_Edit_Box_Type - Properties
   -------------------------------------------------------------------------

   function Line_Text (Edit : Rich_Edit_Box_Type;
                       Line : Positive)
                      return GString;

   function Get_ITextDocument (Edit : in Rich_Edit_Box_Type)
                              return TOM.Pointer_To_ITextDocument;
   --  Retrieves the Text Object Model Document Interface for the Rich Text
   --  Edit Box

   -------------------------------------------------------------------------
   --  Rich_Edit_Box_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  When overiding events, to insure that the event handlers will still
   --  be executed when set by users, call the base class or fire the event
   --  handler directly.

   procedure On_Create (Edit : in out Rich_Edit_Box_Type);
   --  Handles setting up event mask

private

   type Rich_Edit_Box_Type is new Multi_Line_Edit_Box_Type with
      record
         Multi_Line : Boolean := True;
      end record;

end GWindows.Edit_Boxes.Rich;
