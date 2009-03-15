--  Access to window XML specs

with DOM.Core;
with GWindows;
with GNAVI_XML;
with GNAVI_Controls;

package GNAVI_Window is

   type GNAVI_Window_Type is new GNAVI_XML.GNAVI_XML_Type with private;
   type GNAVI_Window_Access is access all GNAVI_Window_Type;
   type Pointer_To_GNAVI_Window_Class is access all GNAVI_Window_Type'Class;

   subtype Control_Element is DOM.Core.Element;

   Null_Control_Element : Control_Element := null;

   -------------------------------------------------------------------------
   --  Window
   -------------------------------------------------------------------------

   function Window_Element (Window : GNAVI_Window_Type)
                           return Control_Element;
   --  Return window element

   function Window_Name (Window : GNAVI_Window_Type) return GWindows.GString;
   --  Return name of window

   function Window_Type (Window : GNAVI_Window_Type) return GWindows.GString;
   --  Return type of window

   -------------------------------------------------------------------------
   --  Controls in Window
   -------------------------------------------------------------------------
   --  The window itself may also be treated as a control

   function Has_Controls (Window : GNAVI_Window_Type) return Boolean;
   --  Returns true if the window has controls

   function Controls (Window : GNAVI_Window_Type) return Control_Element;
   --  Returns Controls node on Window

   function Control (Window : GNAVI_Window_Type;
                     Index  : Positive)
                    return Control_Element;
   --  Returns control number Index under window where all controls
   --  are seen as a single flat list under window

   function Has_Child_Controls (Control : Control_Element)
                               return Boolean;
   --  Returns true if this control has child controls

   function Child_Controls (Control : Control_Element)
                           return Control_Element;
   --  Returns Controls node of a control

   function First_Control (Controls : Control_Element)
                          return Control_Element;
   --  Returns first control in controls block

   function Previous_Control (Control : Control_Element)
                             return Control_Element;
   --  Returns the previous control before this control or null

   function Next_Control (Control : Control_Element)
                         return Control_Element;
   --  Returns the next control after this control or null if no more

   function Parent_Control (Control : Control_Element)
                           return Control_Element;
   --  Returns the parent control (or window) of control

   procedure Add_Control (Window        : in out GNAVI_Window_Type;
                          Parent        : in     Control_Element;
                          Name          : in     GWindows.GString;
                          Control_Index : in     Positive);
   --  Add control of type Control_Index from GNAVI_Controls

   procedure Delete_Control (Window  : in out GNAVI_Window_Type;
                             Control : in     Control_Element);
   --  Delete control

   function Copy_Control (Control : in Control_Element)
                         return Control_Element;
   --  Creates a copy of a control and its children but does not insert
   --  it in to the window. Use insert to put the control in or
   --  Free_Control to release the copy from memory.

   procedure Cut_Control (Window  : in out GNAVI_Window_Type;
                          Control : in     Control_Element;
                          Result  :    out Control_Element);
   --  Cuts control out of window, but does not delete it. Use
   --  Free_Control to release the control from memory or
   --  insert to put it in

   procedure Free_Control (Control : in out Control_Element);
   --  Release control from memory. Use Delete_Control to remove
   --  a control from the window and release its memory at the same time

   procedure Insert_Control_Before
     (Window  : in out GNAVI_Window_Type;
      Control : in     Control_Element;
      Parent  : in     Control_Element;
      Before  : in     Control_Element := Null_Control_Element);
   --  Insert control currently  as a child of Parent and
   --  before Before in sibling list. If Before is null then add
   --  as last child. If Control is in window it is removed first
   --  and then moved to new location

   function Control_Name (Control : Control_Element)
                         return GWindows.GString;
   --  Returns name of control

   function Control_Type (Control : Control_Element)
                         return GWindows.GString;
   --  Returns type of control

   -------------------------------------------------------------------------
   --  Controls in Window - Handlers
   -------------------------------------------------------------------------

   function Handler_Count (Control : Control_Element)
                          return Natural;

   function Handler_Name (Control : Control_Element;
                          Index   : Positive)
                         return GWindows.GString;

   function Handler_Type (Control : Control_Element;
                          Index   : Positive)
                         return GWindows.GString;

   function Handler_Event (Control : Control_Element;
                           Index   : Positive)
                          return GWindows.GString;

   procedure Set_Handler (Window       : in out GNAVI_Window_Type;
                          Control      : in     Control_Element;
                          Name         : in     GWindows.GString;
                          Event_Type   : in     GWindows.GString;
                          Handler_Type : in     GWindows.GString);
   --  Will set a hander of name and handler_type. It will replace
   --  a handler that already exists of the same event type.

   -------------------------------------------------------------------------
   --  Controls in Window - Create properties
   -------------------------------------------------------------------------

   function Create_Property_Count (Control : Control_Element)
                                  return Natural;

   function Create_Property_Name (Control : Control_Element;
                                  Index   : Positive)
                                 return GWindows.GString;

   function Create_Property_Value (Control : Control_Element;
                                   Index   : Positive)
                                  return GWindows.GString;

   procedure Set_Create_Property (Window  : in out GNAVI_Window_Type;
                                  Control : in     Control_Element;
                                  Name    : in     GWindows.GString;
                                  Value   : in     GWindows.GString);

   function Get_Create_Property (Control : Control_Element;
                                 Name    : GWindows.GString)
                                return GWindows.GString;

   function Get_Create_Property (Control : Control_Element;
                                 Name    : GWindows.GString)
                                return Integer;

   function Get_Create_Property (Control : Control_Element;
                                 Name    : GWindows.GString)
                                return Boolean;

   -------------------------------------------------------------------------
   --  Controls in Window - Init properties
   -------------------------------------------------------------------------

   function Init_Property_Count (Control : Control_Element)
                                return Natural;

   function Init_Property_Name (Control : Control_Element;
                                Index   : Positive)
                               return GWindows.GString;

   function Init_Property_Value (Control : Control_Element;
                                 Index   : Positive)
                                return GWindows.GString;

   procedure Set_Init_Property (Window  : in out GNAVI_Window_Type;
                                Control : in     Control_Element;
                                Name    : in     GWindows.GString;
                                Value   : in     GWindows.GString);

   function Get_Init_Property (Control : Control_Element;
                               Name    : GWindows.GString)
                              return GWindows.GString;

   function Get_Init_Property (Control : Control_Element;
                               Name    : GWindows.GString)
                              return Integer;

   function Get_Init_Property (Control : Control_Element;
                               Name    : GWindows.GString)
                              return Boolean;

   -------------------------------------------------------------------------
   --  Controls in Window - All Properties in one view
   -------------------------------------------------------------------------
   --  This includes properties that have not been set. Value returns
   --  the default value for properties that have not been set

   function All_Properties (Control : Control_Element)
                           return GNAVI_Controls.Detail_Array;

   function All_Property_Count (Control : Control_Element)
                               return Natural;

   function All_Property_Name (Control : Control_Element;
                               Index   : Positive)
                              return GWindows.GString;

   function All_Property_Value (Control : Control_Element;
                                Index   : Positive)
                               return GWindows.GString;

   procedure Set_All_Property (Window  : in out GNAVI_Window_Type;
                               Control : in     Control_Element;
                               Name    : in     GWindows.GString;
                               Value   : in     GWindows.GString);

   -------------------------------------------------------------------------
   --  Controls in Window - All Handlers in one view
   -------------------------------------------------------------------------
   --  This includes handlers that have not been set. Value and Type return
   --  empty strings for unset handlers

   function All_Handlers (Control : Control_Element)
                         return GNAVI_Controls.Detail_Array;

   function All_Handler_Count (Control : Control_Element)
                              return Natural;

   function All_Handler_Name (Control : Control_Element;
                              Index   : Positive)
                             return GWindows.GString;

   function All_Handler_Type (Control : Control_Element;
                              Index   : Positive)
                             return GWindows.GString;

   function All_Handler_Event (Control : Control_Element;
                               Index   : Positive)
                              return GWindows.GString;

   procedure Set_All_Handler (Window       : in out GNAVI_Window_Type;
                              Control      : in     Control_Element;
                              Name         : in     GWindows.GString;
                              Event_Type   : in     GWindows.GString;
                              Handler_Type : in     GWindows.GString);

   -------------------------------------------------------------------------
   --  Additional Utilities
   -------------------------------------------------------------------------

   function Strip_Type (Property_Name : GWindows.GString) return String;
   --  Strips type information and returns an Ada String of the enum value

   function Trim_Ends (Text : GWindows.GString) return GWindows.GString;
   --  Remove first and last character which should be quotes

   Property_Out_Of_Bounds : exception;

private
     type GNAVI_Window_Type is new GNAVI_XML.GNAVI_XML_Type with
        record
           null;
        end record;

end GNAVI_Window;
