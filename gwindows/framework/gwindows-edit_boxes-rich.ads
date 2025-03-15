------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada Framework for Windows Development             --
--                                                                          --
--              G W I N D O W S . E D I T _ B O X E S . R I C H             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2024 David Botton                   --
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
      Read_Only         : in     Boolean                            := False;
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
