------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                    G W I N D O W S . B U T T O N S                       --
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
with GWindows.Types;
package GWindows.Buttons is

   -------------------------------------------------------------------------
   --  Button_Type
   -------------------------------------------------------------------------

   type Button_Type is new GWindows.Base.Base_Window_Type with private;
   type Button_Access is access all Button_Type;
   type Pointer_To_Button_Class is access all Button_Type'Class;

   -------------------------------------------------------------------------
   --  Button_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Button     : in out Button_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Left       : in     Integer                              := 0;
      Top        : in     Integer                              := 0;
      Width      : in     Integer                              := 0;
      Height     : in     Integer                              := 0;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);
   --  Create Button. If Width or Height is 0, Recommended_Size is used.

   -------------------------------------------------------------------------
   --  Button_Type - Event Handlers
   -------------------------------------------------------------------------
   --  See Event Methods for details on each event

   procedure On_Click_Handler (Button  : in out Button_Type;
                               Handler : in GWindows.Base.Action_Event);
   procedure Fire_On_Click (Button : in out Button_Type);

   procedure On_Focus_Handler (Button  : in out Button_Type;
                               Handler : in GWindows.Base.Action_Event);
   procedure Fire_On_Focus (Button : in out Button_Type);

   procedure On_Lost_Focus_Handler (Button  : in out Button_Type;
                               Handler : in GWindows.Base.Action_Event);
   procedure Fire_On_Lost_Focus (Button : in out Button_Type);

   procedure On_Pushed_Handler (Button  : in out Button_Type;
                               Handler : in GWindows.Base.Action_Event);
   procedure Fire_On_Pushed (Button : in out Button_Type);

   procedure On_Released_Handler (Button  : in out Button_Type;
                               Handler : in GWindows.Base.Action_Event);
   procedure Fire_On_Released (Button : in out Button_Type);

   -------------------------------------------------------------------------
   --  Button_Type - Event Methods
   -------------------------------------------------------------------------

   procedure On_Click (Button : in out Button_Type);
   --  Button clicked

   procedure On_Focus (Button : in out Button_Type);
   --  Button received focus

   procedure On_Lost_Focus (Button : in out Button_Type);
   --  Button lost focus

   procedure On_Pushed (Button : in out Button_Type);
   --  Button is being pushed

   procedure On_Released (Button : in out Button_Type);
   --  Button was released

   -------------------------------------------------------------------------
   --  Button_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Command (Window  : in out Button_Type;
                         Code    : in     Integer;
                         ID      : in     Integer;
                         Control : in
                           GWindows.Base.Pointer_To_Base_Window_Class);
   --  Receives command messages from parent window

   function Recommended_Size (Window : in Button_Type)
                             return GWindows.Types.Size_Type;

   -------------------------------------------------------------------------
   --  Default_Button_Type
   -------------------------------------------------------------------------
   --  Default_Button_Type buttons have a thicker border.

   type Default_Button_Type is new Button_Type with private;
   type Default_Button_Access is access all Default_Button_Type;

   -------------------------------------------------------------------------
   --  Default_Button_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Create (Window : in out Default_Button_Type);
   --  Sets up styles

   -------------------------------------------------------------------------
   --  Cancel_Button_Type
   -------------------------------------------------------------------------
   --  Cancel_Button_Type buttons close their parent window when clicked
   --  Note: If an On_Click_Handler is set, it will not be called

   type Cancel_Button_Type is new Button_Type with private;
   type Cancel_Button_Access is access all Cancel_Button_Type;

   -------------------------------------------------------------------------
   --  Cancel_Button_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Click (Button : in out Cancel_Button_Type);
   --  On_Click event closes parent window

   -------------------------------------------------------------------------
   --  Default_Cancel_Button_Type
   -------------------------------------------------------------------------
   --  Cancel_Button_Type buttons close their parent window when clicked
   --  Note: If an On_Click_Handler is set, it will not be called

   type Default_Cancel_Button_Type is new Cancel_Button_Type with private;
   type Default_Cancel_Button_Access is access all Default_Cancel_Button_Type;

   -------------------------------------------------------------------------
   --  Default_Cancel_Button_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Create (Window : in out Default_Cancel_Button_Type);
   --  Sets up styles

   -------------------------------------------------------------------------
   --  Dialog_Button_Type
   -------------------------------------------------------------------------
   --  Dialog_Button_Type buttons close their parent window when clicked
   --  and set the Modal Result to the Button's ID
   --  Note: If an On_Click_Handler is set, it will not be called

   type Dialog_Button_Type is new Button_Type with private;
   type Dialog_Button_Access is access all Dialog_Button_Type;

   -------------------------------------------------------------------------
   --  Dialog_Button_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Click (Button : in out Dialog_Button_Type);
   --  On_Click event closes parent window

   -------------------------------------------------------------------------
   --  Default_Dialog_Button_Type
   -------------------------------------------------------------------------
   --  Dialog_Button_Type buttons close their parent window when clicked
   --  and set the Modal Result to the Button's ID
   --  Note: If an On_Click_Handler is set, it will not be called

   type Default_Dialog_Button_Type is new Dialog_Button_Type with private;
   type Default_Dialog_Button_Access is access all Default_Dialog_Button_Type;

   -------------------------------------------------------------------------
   --  Dialog_Button_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Create (Window : in out Default_Dialog_Button_Type);
   --  Sets up styles

   -------------------------------------------------------------------------
   --  Check_Box_Type
   -------------------------------------------------------------------------

   type Check_Box_Type is new Button_Type with private;
   type Check_Box_Access is access all Check_Box_Type;

   type Check_State_Type is (Checked, Unchecked);

   function "not" (State : in Check_State_Type) return Check_State_Type;

   function State (Button : in Check_Box_Type)
                  return Check_State_Type;

   procedure State (Button : in out Check_Box_Type;
                    State  : in     Check_State_Type);
   --  Check state property

   -------------------------------------------------------------------------
   --  Check_Box_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Create (Window : in out Check_Box_Type);
   --  Sets up styles

   procedure On_Click (Button : in out Check_Box_Type);
   --  Handles switching states

   -------------------------------------------------------------------------
   --  Two_State_Button_Type
   -------------------------------------------------------------------------
   --  Two_State_Button_Type buttons look like buttons but act like
   --  check boxes

   type Two_State_Button_Type is
     new Check_Box_Type with private;
   type Two_State_Button_Access is
     access all Two_State_Button_Type;

   -------------------------------------------------------------------------
   --  Two_State_Button_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Create (Window : in out Two_State_Button_Type);
   --  Sets up styles

   -------------------------------------------------------------------------
   --  Radio_Button_Type
   -------------------------------------------------------------------------

   type Radio_Button_Type is
     new Check_Box_Type with private;
   type Radio_Button_Access is
     access all Radio_Button_Type;
   --  Creates a radio button

   -------------------------------------------------------------------------
   --  Radio_Button_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Create (Window : in out Radio_Button_Type);
   --  Sets up styles

   procedure On_Click (Button : in out Radio_Button_Type);

   -------------------------------------------------------------------------
   --  Push_Radio_Button_Type
   -------------------------------------------------------------------------

   type Push_Radio_Button_Type is
     new Radio_Button_Type with private;
   type Push_Radio_Button_Access is
     access all Push_Radio_Button_Type;
   --  Creates a push style radio button

   -------------------------------------------------------------------------
   --  Push_Radio_Button_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Create (Window : in out Push_Radio_Button_Type);
   --  Sets up styles

   -------------------------------------------------------------------------
   --  Three_State_Box_Type
   -------------------------------------------------------------------------
   --  Check box with three states Checked, Indeterminate and Unchecked

   type Three_State_Box_Type is new Button_Type with private;
   type Three_State_Box_Access is access all Three_State_Box_Type;
   --  Creates a three state check box

   type Three_State_Type is (Checked, Indeterminate, Unchecked);

   function State (Button : in Three_State_Box_Type)
                  return Three_State_Type;

   procedure State (Button : in out Three_State_Box_Type;
                    State  : in     Three_State_Type);
   --  Button state property

   -------------------------------------------------------------------------
   --  Three_State_Box_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Create (Window : in out Three_State_Box_Type);
   --  Sets up styles

   procedure On_Click (Button : in out Three_State_Box_Type);
   --  Handles switching states

   -------------------------------------------------------------------------
   --  Three_State_Button_Type
   -------------------------------------------------------------------------
   --  Button with three states Checked, Indeterminate and Unchecked

   type Three_State_Button_Type is new Three_State_Box_Type with private;
   type Three_State_Button_Access is
     access all Three_State_Button_Type;
   --  Creates a three state push button

   -------------------------------------------------------------------------
   --  Three_State_Button_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Create (Window : in out Three_State_Button_Type);
   --  Sets up styles

   -------------------------------------------------------------------------
   --  Group_Box_Type
   -------------------------------------------------------------------------

   type Group_Box_Type is new GWindows.Base.Base_Window_Type with null record;
   type Group_Box_Access is access all Group_Box_Type;
   type Pointer_To_Group_Box_Class is access all Group_Box_Type'Class;

   -------------------------------------------------------------------------
   --  Goup_Box_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Box        : in out Group_Box_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);
   --  Create Button

private
   type Button_Type is new GWindows.Base.Base_Window_Type with
      record
         On_Click_Event      : GWindows.Base.Action_Event := null;
         On_Focus_Event      : GWindows.Base.Action_Event := null;
         On_Lost_Focus_Event : GWindows.Base.Action_Event := null;
         On_Pushed_Event     : GWindows.Base.Action_Event := null;
         On_Released_Event   : GWindows.Base.Action_Event := null;
      end record;

   type Default_Button_Type is new Button_Type with null record;

   type Cancel_Button_Type is new Button_Type with null record;

   type Default_Cancel_Button_Type is new Cancel_Button_Type with null record;

   type Dialog_Button_Type is new Button_Type with null record;

   type Default_Dialog_Button_Type is new Dialog_Button_Type with null record;

   type Check_Box_Type is new Button_Type with null record;

   type Two_State_Button_Type is new Check_Box_Type with null record;

   type Radio_Button_Type is new Check_Box_Type with null record;

   type Push_Radio_Button_Type is new Radio_Button_Type with null record;

   type Three_State_Box_Type is new Button_Type with null record;

   type Three_State_Button_Type is new Three_State_Box_Type with null record;

end GWindows.Buttons;
