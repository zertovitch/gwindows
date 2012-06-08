------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                       G W I N D O W S . B A S E                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2012 David Botton                   --
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
-- be located on the web at one of the following places:                    --
--   http://sf.net/projects/gnavi/                                          --
--   http://www.gnavi.org/gwindows                                          --
--   http://www.adapower.com/gwindows                                       --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Finalization;
with System;

with GWindows.Types;
with GWindows.Drawing;
with GWindows.Drawing_Objects;
with GWindows.Colors;
with Interfaces.C;

package GWindows.Base is
   use type Interfaces.C.unsigned;

   -------------------------------------------------------------------------
   --  Base_Window_Type
   -------------------------------------------------------------------------
   --  Base_Window_Type is the parent Class of all GWindows GUIs widgets.
   --  It is generally not used directly

   type Base_Window_Type is
     new Ada.Finalization.Limited_Controlled with private;
   type Base_Window_Access is access all Base_Window_Type;
   type Pointer_To_Base_Window_Class is access all Base_Window_Type'Class;

   procedure Finalize (Object : in out Base_Window_Type);
   --  Cleans resources for dynamicly created windows. Will also close window
   --  if it is open.

   -------------------------------------------------------------------------
   --  Base_Window_Type - Properties
   -------------------------------------------------------------------------

   -- General --

   procedure Accelerator_Table
      (Window  : in out Base_Window_Type;
       Name    : in     GString);
   --  Set accelerator table for window from resource Name
   --  To specify a numeric resource use #XXXX where XXXX is the resource ID

   procedure Border (Window : in out Base_Window_Type;
                     State  : in     Boolean          := True);
   function Border (Window : in Base_Window_Type) return Boolean;
   --  Border property

   type Dock_Type is (At_Top, At_Left, At_Right, At_Bottom, Fill, None);

   procedure Dock (Window : in out Base_Window_Type;
                   To     : in     Dock_Type);
   function Dock (Window : in Base_Window_Type) return Dock_Type;
   --  Set requested docking location of this object
   --  GWindows.Windows provides a procedure Dock_Children
   --  that is called in the default On_Size handler that
   --  will dock windows based on this property

   function Focus return Pointer_To_Base_Window_Class;
   --  Returns the currently focused control/window

   function Focus (Window : in Base_Window_Type)
                  return Pointer_To_Base_Window_Class;
   --  Returns the currently/last focused control of this window

   procedure Focus (Window : in out Base_Window_Type);
   --  Set keyboard focus to window or control

   function First_Window (Window : in Base_Window_Type)
                        return Pointer_To_Base_Window_Class;
   function Last_Window (Window : in Base_Window_Type)
                        return Pointer_To_Base_Window_Class;
   function Next_Window (Window : in Base_Window_Type)
                        return Pointer_To_Base_Window_Class;
   function Previous_Window (Window : in Base_Window_Type)
                            return Pointer_To_Base_Window_Class;
   --  Return the window in the Z-Order

   procedure Set_Font (Window : in out Base_Window_Type;
                       Font   : in     GWindows.Drawing_Objects.Font_Type);
   procedure Get_Font (Window : in     Base_Window_Type;
                       Font   :    out GWindows.Drawing_Objects.Font_Type);
   --  Window Font

   procedure Set_Standard_Font
     (Window     : in out Base_Window_Type;
      Stock_Font : in     GWindows.Drawing_Objects.Stock_Font_Type);
   --  Set window to use stock font type

   procedure Group (Window : in out Base_Window_Type;
                    State  : in     Boolean          := True);
   function Group (Window : in Base_Window_Type) return Boolean;
   --  Start of control group property

   procedure Keyboard_Support (Window : in out Base_Window_Type;
                               State  : in     Boolean          := True);
   function Keyboard_Support (Window : in Base_Window_Type) return Boolean;
   --  Support moving in/out of child window/control using the tab key, etc.

   type Order_Position_Type is (Top,
                                Bottom,
                                Always_On_Top,
                                Not_Always_On_Top,
                                No_Change);

   procedure Order (Window   : in out Base_Window_Type;
                    Position : in     Order_Position_Type);
   --  Move window to position in layout order

   procedure Order (Window       : in out Base_Window_Type;
                    After_Window : in     Base_Window_Type'Class);
   --  Move window to position After_Window in layout order

   procedure Horizontal_Scroll_Bar (Window : in out Base_Window_Type;
                                    State  : in     Boolean := True);
   function Horizontal_Scroll_Bar (Window : in Base_Window_Type)
                                  return Boolean;
   --  Horizontal Scroll Bar State

   procedure Vertical_Scroll_Bar (Window : in out Base_Window_Type;
                                  State  : in     Boolean := True);
   function Vertical_Scroll_Bar (Window : in Base_Window_Type) return Boolean;
   --  Vertical Scroll Bar State

   procedure Tab_Stop (Window : in out Base_Window_Type;
                       State  : in     Boolean     := True);
   function Tab_Stop (Window : in Base_Window_Type) return Boolean;
   --  Tab Stop property

   procedure Text (Window : in out Base_Window_Type;
                   Text   : in     GString);

   function Text  (Window : in Base_Window_Type)
                  return GString;
   --  Text/Caption property

   function Text_Length (Window : in Base_Window_Type) return Integer;
   --  Legth of Text/Caption property

   function Valid (Window : in Base_Window_Type) return Boolean;
   --  If is a valid window object, ie. if it has been created and has
   --  not been destroyed

   procedure Visible (Window : in out Base_Window_Type;
                      State  : in     Boolean := True);
   function Visible (Window : in Base_Window_Type) return Boolean;
   --  Visible state of window

   procedure Enabled (Window : in out Base_Window_Type;
                      State  : in     Boolean := True);
   function Enabled (Window : in Base_Window_Type) return Boolean;
   --  Enabled state of window

   --  Location / Size --

   procedure Location (Window : in out Base_Window_Type;
                       Value  : in     GWindows.Types.Rectangle_Type;
                       Order  : in     Order_Position_Type := No_Change);
   function Location (Window : in Base_Window_Type)
                     return GWindows.Types.Rectangle_Type;
   --  Top, left, right and bottom of Window, relative to parent.

   procedure Location (Window : in out Base_Window_Type;
                       Value  : in     GWindows.Types.Point_Type);
   function Location (Window : in Base_Window_Type)
                     return GWindows.Types.Point_Type;
   --  Top and left of Window, relative to parent.

   procedure Left (Window : in out Base_Window_Type;
                   Value  : in     Integer);
   function Left (Window : in Base_Window_Type) return Integer;
   --  Location of left side window, relative to parent window (root
   --  if top level).

   procedure Top (Window : in out Base_Window_Type;
                  Value  : in     Integer);
   function Top (Window : in Base_Window_Type) return Integer;
   --  Location of top of window

   procedure Width (Window : in out Base_Window_Type;
                    Value  : in     Natural);
   function Width (Window : in Base_Window_Type) return Natural;
   --  Width of window

   procedure Height (Window : in out Base_Window_Type;
                     Value  : in     Natural);
   function Height (Window : in Base_Window_Type) return Natural;
   --  Height of window

   procedure Size (Window : in out Base_Window_Type;
                   Value  : in     GWindows.Types.Size_Type);
   procedure Size (Window : in out Base_Window_Type;
                   Width  : in     Natural;
                   Height : in     Natural);
   function Size (Window : in Base_Window_Type)
                 return GWindows.Types.Size_Type;
   --  Window Size

   procedure Client_Area_Size (Window : in out Base_Window_Type;
                               Value  : in     GWindows.Types.Size_Type);
   procedure Client_Area_Size (Window : in out Base_Window_Type;
                               Width  : in     Natural;
                               Height : in     Natural);
   function Client_Area_Size (Window : in Base_Window_Type)
                             return GWindows.Types.Size_Type;
   --  Client area size

   procedure Client_Area_Width (Window : in out Base_Window_Type;
                                Value  : in     Natural);
   function Client_Area_Width (Window : in Base_Window_Type) return Natural;
   --  Width of window

   procedure Client_Area_Height (Window : in out Base_Window_Type;
                                 Value  : in     Natural);
   function Client_Area_Height (Window : in Base_Window_Type) return Natural;
   --  Height of window

   function Recommended_Size (Window : in Base_Window_Type)
                             return GWindows.Types.Size_Type;
   --  If Window has already been created, returns an ideal size
   --  for the control/window based on its contents. Many controls
   --  will return intelligent results based on things like text size
   --  and fonts. By default this value is the size of the control as
   --  created.

   -- Relationships --

   procedure Parent (Window        : in out Base_Window_Type;
                     Parent_Window : in out Base_Window_Type'Class);
   function Parent (Window : in Base_Window_Type)
                   return Pointer_To_Base_Window_Class;
   --  Parent property

   function Controlling_Parent (Window : in Base_Window_Type)
                               return Pointer_To_Base_Window_Class;
   --  Finds the first parenting window that is not a control

   function Child (Window : in Base_Window_Type) return Boolean;
   --  Returns true if window is a child of another window

   function Control (Window : in Base_Window_Type) return Boolean;
   --  Returns true if window is a control

   -- Dialog --

   procedure Modal_Result (Window : in out Base_Window_Type;
                           Result : in     Integer);
   function Modal_Result (Window : in Base_Window_Type) return Integer;
   --  Modal result is set automaticly by the GWindows.Events.Do_Dialog_*
   --  events

   function ID (Window : in Base_Window_Type) return Integer;
   --  Returns the ID set for the control or window at the time of creation

   --  Custom Data --

   type Base_Data_Type is tagged null record;
   type Base_Data_Access is access all Base_Data_Type;
   type Pointer_To_Base_Data_Class is access all Base_Data_Type'Class;

   procedure Free (Data : in out Pointer_To_Base_Data_Class);
   --  Deallocate memory for Data

   procedure Custom_Data
     (Window    : in out Base_Window_Type;
      Data      : in     Pointer_To_Base_Data_Class;
      Auto_Free : in     Boolean                   := True);
   function Custom_Data (Window : Base_Window_Type)
                        return Pointer_To_Base_Data_Class;
   --  Custom Data Property. Allows the assocation of a type derived
   --  from Base_Data_Type with the window or control. If Auto_Free
   --  is true when the window is destroyed the Custom_Data memory
   --  will be deallocated with Free

   -------------------------------------------------------------------------
   --  Base_Window_Type - Methods
   -------------------------------------------------------------------------

   -- General --

   procedure Show (Window : in out Base_Window_Type);
   --  Show Window

   procedure Hide (Window : in out Base_Window_Type);
   --  Hide Window

   procedure Close (Window : in out Base_Window_Type);
   --  Close and destroy Window

   procedure Enable (Window : in out Base_Window_Type);
   --  Enable window

   procedure Disable (Window : in out Base_Window_Type);
   --  Disable window

   procedure Redraw (Window     : in out Base_Window_Type;
                     Erase      : in     Boolean          := False;
                     Redraw_Now : in     Boolean          := False);
   --  Async. requests of a redraw of window

   procedure Capture_Mouse (Window : in out Base_Window_Type);
   --  Capture mouse so that all mouse messages are sent to this window

   procedure Release_Mouse;
   --  Release capture on mouse

   procedure Freeze (Window : in out Base_Window_Type);
   --  Prevents changes to the window from being drawn

   procedure Thaw (Window : in out Base_Window_Type);
   --  Allows changes to the window to be drawn

   procedure Center (Window : in out Base_Window_Type);
   --  Center window to parent

   procedure Center (Window : in out Base_Window_Type;
                     Other  : in     Base_Window_Type'Class);
   --  Center window relative to another window

   procedure Move (Window : in out Base_Window_Type;
                   Left   : in     Integer;
                   Top    : in     Integer);
   --  Move location of window

   function Next_Tab_Stop (Window  : in Base_Window_Type;
                           Control : in Base_Window_Type'Class)
                          return Pointer_To_Base_Window_Class;
   --  Find the next control with a tab stop after control

   function Previous_Tab_Stop (Window  : in Base_Window_Type;
                               Control : in Base_Window_Type'Class)
                          return Pointer_To_Base_Window_Class;
   --  Find the next control with a tab stop before control

   type Enumerate_Function is access
     procedure (Window : Pointer_To_Base_Window_Class);

   procedure Enumerate_Children (Window : in Base_Window_Type;
                                 Proc   : in Enumerate_Function);
   --  Enumerates through children of window calling Proc

   function Point_To_Client (Window : in Base_Window_Type;
                             Point  : in GWindows.Types.Point_Type)
                            return GWindows.Types.Point_Type;
   --  Converts a location relative to the desktop to be relative to the
   --  client area of the Window

   function Point_To_Desktop (Window : in Base_Window_Type;
                              Point  : in GWindows.Types.Point_Type)
                             return GWindows.Types.Point_Type;
   --  Converts a location relative to the client area of the Window to
   --  be relative to the desktop

   procedure End_Dialog (Window : in out Base_Window_Type;
                         Result : in     Integer);
   --  Ends a dialog or modal window and sets modal result
   --  Should be called in the On_Destroy message of any
   --  windows to be displayed as a dialog

   procedure Get_Canvas
     (Window : in     Base_Window_Type;
      Canvas :    out GWindows.Drawing.Canvas_Type);
   --  Returns a canvas for drawing in the client area

   procedure Get_Full_Window_Canvas
     (Window : in     Base_Window_Type;
      Canvas :    out GWindows.Drawing.Canvas_Type);
   --  Returns a canvas for the entire window including frame, menu, etc.

   function Calculate_New_Window_Size
     (Window           : in Base_Window_Type;
      Client_Area_Size : in GWindows.Types.Size_Type)
     return GWindows.Types.Size_Type;
   --  Calculates the new size of a window given Client_Area_Size;
   --  adds the frame size.

   type Mouse_Wheel_Target_Type is (Focus_Window, Mouse_Window);

   Mouse_Wheel_Target : Mouse_Wheel_Target_Type := Focus_Window;

   -------------------------------------------------------------------------
   --  Base_Window_Type - Event Types
   -------------------------------------------------------------------------

   type Scroll_Request_Type is (End_Scroll,
                                First,
                                Last,
                                Previous_Unit,
                                Next_Unit,
                                Previous_Page,
                                Next_Page,
                                Thumb_Set,
                                Thumb_Drag);

   type Action_Event is access
     procedure (Window : in out GWindows.Base.Base_Window_Type'Class);

   type Location_Action_Event is access
     procedure (Window : in out GWindows.Base.Base_Window_Type'Class;
                X      : in     Integer;
                Y      : in     Integer);

   type Raw_Input_Event is access
     procedure (Window   : in out GWindows.Base.Base_Window_Type'Class;
                WParam   : in     Integer;
                RawData  : in     GWindows.Types.Handle;
                Continue :    out Integer);

   type Scroll_Event is access
     procedure (Window  : in out GWindows.Base.Base_Window_Type'Class;
                Request : in     Scroll_Request_Type);

   type Pre_Create_Event is access
     procedure (Window    : in out GWindows.Base.Base_Window_Type'Class;
                dwStyle   : in out Interfaces.C.unsigned;
                dwExStyle : in out Interfaces.C.unsigned);

   -------------------------------------------------------------------------
   --  Base_Window_Type - Event Handlers
   -------------------------------------------------------------------------
   --  See Event Methods for details on each event

   procedure On_Create_Handler (Window  : in out Base_Window_Type;
                                Handler : in Action_Event);
   procedure Fire_On_Create (Window : in out Base_Window_Type);

   procedure On_Create_Start_Handler (Window  : in out Base_Window_Type;
                                      Handler : in Action_Event);
   procedure Fire_On_Create_Start (Window : in out Base_Window_Type);

   procedure On_Pre_Create_Handler (Window  : in out Base_Window_Type;
                                    Handler : in Pre_Create_Event);
   procedure Fire_On_Pre_Create (Window     : in out Base_Window_Type;
                                 dwStyle    : in out Interfaces.C.unsigned;
                                 dwExStyle  : in out Interfaces.C.unsigned);

   procedure On_Destroy_Handler (Window  : in out Base_Window_Type;
                                 Handler : in Action_Event);
   procedure Fire_On_Destroy (Window : in out Base_Window_Type);

   procedure On_Context_Menu_Handler (Window  : in out Base_Window_Type;
                                      Handler : in Location_Action_Event);
   procedure Fire_On_Context_Menu (Window : in out Base_Window_Type;
                                   X      : in     Integer;
                                   Y      : in     Integer);

   procedure On_Input_Handler (Window  : in out Base_Window_Type;
                               Handler : in Raw_Input_Event);
   procedure Fire_On_Input (Window   : in out Base_Window_Type;
                            WParam   : in     Integer;
                            RawData  : in     GWindows.Types.Handle;
                            Continue :    out Integer);

   procedure On_Horizontal_Scroll_Handler
     (Window  : in out Base_Window_Type;
      Handler : in     Scroll_Event);
   procedure Fire_On_Horizontal_Scroll
     (Window  : in out Base_Window_Type;
      Request : in     Scroll_Request_Type);

   procedure On_Vertical_Scroll_Handler
     (Window  : in out Base_Window_Type;
      Handler : in     Scroll_Event);
   procedure Fire_On_Vertical_Scroll
     (Window  : in out Base_Window_Type;
      Request : in     Scroll_Request_Type);

   -------------------------------------------------------------------------
   --  Base_Window_Type - Event Methods
   -------------------------------------------------------------------------
   --  When overiding events, to insure that the event handlers will still
   --  be executed when set by users, call the base class or fire the event
   --  handler directly.

   procedure On_Create (Window : in out Base_Window_Type);
   --  Called on creation of window

   procedure On_Create_Start (Window : in out Base_Window_Type);
   --  Called before on create and therefore before control
   --  creation in the On_Create of GNAVI applications

   procedure On_Pre_Create (Window    : in out Base_Window_Type;
                            dwStyle   : in out Interfaces.C.unsigned;
                            dwExStyle : in out Interfaces.C.unsigned);
   --  Called before creation of window to allow customizaion of the
   --  generic GWindows styles for creating a window.
   --  The GUI element of Window has not yet been created by the OS
   --  so most GUI related methods will have no effect on it.

   procedure On_Destroy (Window : in out Base_Window_Type);
   --  Called on destruction of window before deallocating Base_Window_Type

   procedure On_Context_Menu (Window : in out Base_Window_Type;
                              X      : in     Integer;
                              Y      : in     Integer);
   --  Called when the right mouse button is clicked on a window, or menu
   --  key on 102 keyboard is hit or Shift+F10
   --  X and Y are -1 if not generated by a mouse click
   --  NOTE: Coordinates are relative to desktop

   procedure On_Input
     (Window   : in out Base_Window_Type;
      WParam   : in     Integer;
      RawData  : in     GWindows.Types.Handle;
      Control  : in     Pointer_To_Base_Window_Class;
      Continue :    out Integer);
   --  Called with a raw input device
   --  If control /= null then the message will be routed down to control
   --  handler is fired only from target control/window

   procedure On_Horizontal_Scroll
     (Window  : in out Base_Window_Type;
      Request : in     Scroll_Request_Type;
      Control : in     Pointer_To_Base_Window_Class);
   --  Called when a horizontal scroll bar is moved
   --  If control /= null then the message will be routed down to control
   --  handler is fired only from target control/window

   procedure On_Vertical_Scroll
     (Window  : in out Base_Window_Type;
      Request : in     Scroll_Request_Type;
      Control : in     Pointer_To_Base_Window_Class);
   --  Called when a vertical scroll bar is moved
   --  If control /= null then the message will be routed down to control
   --  handler is fired only from target control/window

   -------------------------------------------------------------------------
   --  Base_Window_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Command (Window  : in out Base_Window_Type;
                         Code    : in     Integer;
                         ID      : in     Integer;
                         Control : in     Pointer_To_Base_Window_Class);
   --  Called when a control is pushed, menu selected, or accelerator keyed
   --  Default implementation redirects message to control and if a dialog
   --  box process default buttons, ie. those with control IDs between
   --  1 and 9

   procedure On_Draw_Item
     (Window          : in out Base_Window_Type;
      Canvas          : in out GWindows.Drawing.Canvas_Type;
      Item_ID         : in     Integer;
      Item_Action     : in     Interfaces.C.unsigned;
      Item_State      : in     Interfaces.C.unsigned;
      Item_Rect       : in     GWindows.Types.Rectangle_Type;
      Item_Data       : in     Integer;
      Control         : in     Pointer_To_Base_Window_Class);
   --  Used to support Owner Drawn items

   type Notification is
      record
         Handle : GWindows.Types.Handle;
         ID     : GWindows.Types.Wparam;
         Code   : Integer;
      end record;
   type Pointer_To_Notification is access all Notification;
   pragma No_Strict_Aliasing (Pointer_To_Notification);

   procedure On_Notify (Window       : in out Base_Window_Type;
                        Message      : in     Pointer_To_Notification;
                        Control      : in     Pointer_To_Base_Window_Class;
                        Return_Value : in out GWindows.Types.Lresult);
   --  On message from common controls
   --  If control /= null then the message will be routed down to control

   procedure On_Filter_Message
     (Window       : in out Base_Window_Type;
      message      : in     Interfaces.C.unsigned;
      wParam       : in     GWindows.Types.Wparam;
      lParam       : in     GWindows.Types.Lparam;
      Return_Value : in out GWindows.Types.Lresult;
      Continue     : out    Boolean);
   --  Called to allow pre processing of messages

   procedure On_Message
     (Window       : in out Base_Window_Type;
      message      : in     Interfaces.C.unsigned;
      wParam       : in     GWindows.Types.Wparam;
      lParam       : in     GWindows.Types.Lparam;
      Return_Value : in out GWindows.Types.Lresult);
   --  Called for all unhandled messages

   -------------------------------------------------------------------------
   --  Base_Window_Type - Win32 Interface Members
   -------------------------------------------------------------------------
   --  These should be used with caution and only with a full understanding
   --  of the internals of the entire GWindows framework

   function Handle (Window : in Base_Window_Type)
                   return GWindows.Types.Handle;
   --  Win32 Handle

   function Window_From_Handle (Handle : GWindows.Types.Handle)
                               return Pointer_To_Base_Window_Class;
   --  If Handle is for a GWindows window return a pointer to the window

   function MDI_Client_Window (Window : in Base_Window_Type)
                              return Base_Window_Access;
   procedure MDI_Client_Window (Window      : in out Base_Window_Type;
                                Client_HWND : in     GWindows.Types.Handle);
   --  MDI Client Window property, the MDI client window is a special
   --  window that is created inside an MDI_Top window to handle MDI child
   --  windows

   function Accelerator_Handle (Window : in Base_Window_Type)
                               return GWindows.Types.Handle;
   procedure Accelerator_Handle (Window : in out Base_Window_Type;
                                 HACCEL : in     GWindows.Types.Handle);
   --  Handle to accelerator table

   procedure Is_Dialog (Window : in out Base_Window_Type;
                        Value  : in     Boolean);
   function Is_Dialog (Window : in Base_Window_Type) return Boolean;
   --  Returns true if window is being displayed as a dialog

   procedure Is_Modal (Window          : in out Base_Window_Type;
                       Value           : in     Boolean;
                       Disabled_Parent : in     Pointer_To_Base_Window_Class);
   function Is_Modal (Window : in Base_Window_Type) return Boolean;
   --  Returns true if window is being displayed using show_modal

   type Run_Mode_Type is (Normal,
                          Development_Create_Start,
                          Development_Create_Complete,
                          Development_Running);

   procedure Run_Mode (Window : in out Base_Window_Type;
                       Value  : in     Run_Mode_Type);
   function Run_Mode (Window : in Base_Window_Type) return Run_Mode_Type;
   --  Run_Mode property used to tell window/control if it is in a normal
   --  running environment or in development stages

   procedure Attach
     (Window     : in out Base_Window_Type;
      HWND       : in     GWindows.Types.Handle;
      Is_Dynamic : in     Boolean               := False;
      Procedures : in     Boolean               := True);           --  * AnSp
   --  Attach a window created outside of GWindows to GWindows
   --  * AnSp:  Added parameter Procedures to be able to create only
   --  *        a link between a Windows handle and GWindows object.

   procedure Attach_Control
     (Window     : in out Base_Window_Type;
      HWND       : in     GWindows.Types.Handle;
      Is_Dynamic : in     Boolean               := False;
      Procedures : in     Boolean               := True);           --  * AnSp
   --  Attach control created outside of GWindows to GWindows
   --  * AnSp:  Added parameter Procedures to be able to create only
   --  *        a link between a Windows handle and GWindows object.

   procedure Attach_Dialog
     (Window     : in out Base_Window_Type;
      HWND       : in     GWindows.Types.Handle;
      Is_Dynamic : in     Boolean               := False;
      Procedures : in     Boolean               := True);           --  * AnSp
   --  Attachs a Created dialog to a GWindow
   --  * AnSp:  Added parameter Procedures to be able to create only
   --  *        a link between a Windows handle and GWindows object.

   procedure Attach_Dialog_Item
     (Window     : in out Base_Window_Type;
      Parent     : in     Base_Window_Type'Class;
      ID         : in     Integer;
      Is_Dynamic : in     Boolean               := False;
      Procedures : in     Boolean               := True);           --  * AnSp
   --  Attach to dialog item in parent window
   --  * AnSp:  Added parameter Procedures to be able to create only
   --  *        a link between a Windows handle and GWindows object.

   type Link_Type_Type is
     (Desktop_Link, Window_Link, MDI_Child_Link, Control_Link);

   procedure Link
     (Window     : in out Base_Window_Type'Class;
      HWND       : in     GWindows.Types.Handle;
      Is_Dynamic : in     Boolean;
      Link_Type  : in     Link_Type_Type        := Window_Link;
      Procedures : in     Boolean               := True);           --  * AnSp
   --  Links an HWND to a Window
   --  Note: This procedure should only be used to link together windows
   --        created with Win32 windows of the "GWindows_Class"
   --  * AnSp:  Added parameter Procedures to be able to create only
   --  *        a link between a Windows handle and GWindows object.

   procedure Create_Control
     (Window     : in out Base_Window_Type;
      Parent     : in out Base_Window_Type'Class;
      Win_Class  : in     GString;
      Text       : in     GString;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      ID         : in     Integer                := 0;
      Styles     : in     Interfaces.C.unsigned  := 0;
      Is_Dynamic : in     Boolean                := False);
   --  Create a Win32 child control window
   --  Note: Win32 controls should generally be created through bindings to
   --        the various win32 controls.

   procedure Use_Mouse_Wheel (Window : in out Base_Window_Type);

   -------------------------------------------------------------------------
   --  Advanced Specs for Customizing GWindows
   -------------------------------------------------------------------------

   CS_VREDRAW                 : constant := 1;
   --  Class style: Redraw for vertical change
   CS_HREDRAW                 : constant := 2;
   --  Class style: Redraw for horizontal change
   CS_KEYCVTWINDOW            : constant := 4;
   CS_DBLCLKS                 : constant := 8;
   --  Class style: Send double click message to window
   CS_OWNDC                   : constant := 32;
   --  Class style: DC for each window instance
   CS_CLASSDC                 : constant := 64;
   --  Class style: DC shared by class
   CS_PARENTDC                : constant := 128;
   --  Class style: Use DC of parent
   CS_NOKEYCVT                : constant := 256;
   CS_NOCLOSE                 : constant := 512;
   --  Class style: Inhibit close on system menu
   CS_SAVEBITS                : constant := 2048;
   --  Class style: Save screen image behind window
   CS_BYTEALIGNCLIENT         : constant := 4096;
   --  Class style: Align client area on x byte boundary
   CS_BYTEALIGNWINDOW         : constant := 8192;
   --  Class style: Align window on x byte boundary
   CS_GLOBALCLASS             : constant := 16384;
   --  Class style: Application global
   CS_IME                     : constant := 65536;

   function WndProc
     (hwnd    : GWindows.Types.Handle;
      message : Interfaces.C.unsigned;
      wParam  : GWindows.Types.Wparam;
      lParam  : GWindows.Types.Lparam)
     return GWindows.Types.Lresult;
   pragma Convention (Stdcall, WndProc);
   --  GWindows Implementation of Win32 Procedure Call Back
   --  Procedure and Message Dispatch

   function WndProc_Control
     (hwnd    : GWindows.Types.Handle;
      message : Interfaces.C.unsigned;
      wParam  : GWindows.Types.Wparam;
      lParam  : GWindows.Types.Lparam)
     return GWindows.Types.Lresult;
   pragma Convention (Stdcall, WndProc_Control);
   --  GWindows Implementation of Win32 Procedure Call Back
   --  Procedure and Message Dispatch for controls

   type WNDCLASS is
      record
         style         : Interfaces.C.unsigned :=
           CS_HREDRAW or CS_VREDRAW or CS_DBLCLKS or CS_OWNDC;
         lpfnWndProc   : System.Address        := WndProc'Address;
         cbClsExtra    : Interfaces.C.int      := 0;
         cbWndExtra    : Interfaces.C.int      := 0;
         hInstance     : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         hIcon         : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         hCursor       : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         hbrBackground : GWindows.Types.Handle := GWindows.Types.To_Handle
            (GWindows.Types.Lparam (GWindows.Colors.COLOR_HIGHLIGHTTEXT));
         lpszMenuName  : Pointer_To_GChar_C    := null;
         lpszClassName : Pointer_To_GChar_C    := null;
      end record;
   --  Win32 WNDCLASS Structure

   procedure Register_Class (Custom_Class : WNDCLASS);
   --  Registers a custom Win32 Widnows Class. The class name can then be
   --  passed in to create procedures that accept a custom class name.
   --  Note: In order to insure that the GWindows frame work will handle
   --  messages for Windows created with your custom class, you must use
   --  either WndProc or WndProc_Control for lpfnWndProc

   type Exception_Event is access
     procedure (Window : in out GWindows.Base.Base_Window_Type'Class;
                E : Ada.Exceptions.Exception_Occurrence);

   procedure On_Exception_Handler (Handler : Exception_Event);

private
   type Windproc_Access is access
     function (hwnd    : GWindows.Types.Handle;
               message : Interfaces.C.unsigned;
               wParam  : GWindows.Types.Wparam;
               lParam  : GWindows.Types.Lparam)
              return GWindows.Types.Lresult;
   pragma Convention (Stdcall, Windproc_Access);

   type Base_Window_Type is
     new Ada.Finalization.Limited_Controlled with
      record
         HWND            : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         ParentWindowProc : Windproc_Access              := null;
         haccel          : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         MDI_Client       : Base_Window_Access           := null;
         Keyboard_Support : Boolean                      := False;
         Is_Control       : Boolean                      := False;
         Last_Focused    : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         Is_Dynamic       : Boolean                      := False;
         Modal_Result     : Integer                      := 0;
         In_Dialog        : Boolean                      := False;
         Is_Modal         : Boolean                      := False;
         Disabled_Parent  : Pointer_To_Base_Window_Class := null;
         Run_Mode         : Run_Mode_Type                := Normal;
         Dock             : Dock_Type                    := None;
         Custom_Data      : Pointer_To_Base_Data_Class   := null;
         Free_Custom_Data : Boolean                      := False;
         Use_Mouse_Wheel  : Boolean                      := False;
         Is_Linked        : Boolean                      := False;  --  * AnSp
         --  * AnSp:  Added parameter Procedures to be able to create only
         --  *        a link between a Windows handle and GWindows object.

         --  Event Handlers
         On_Create_Event             : Action_Event          := null;
         On_Create_Start_Event       : Action_Event          := null;
         On_Pre_Create_Event         : Pre_Create_Event      := null;
         On_Destroy_Event            : Action_Event          := null;
         On_Context_Menu_Event       : Location_Action_Event := null;
         On_Input_Event              : Raw_Input_Event       := null;
         On_Horizontal_Scroll_Event  : Scroll_Event          := null;
         On_Vertical_Scroll_Event    : Scroll_Event          := null;
      end record;

end GWindows.Base;
