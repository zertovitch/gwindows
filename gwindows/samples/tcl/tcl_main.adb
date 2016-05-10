with Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

--  with GNATCOM.Utility;
with GNATCOM.BSTR; use GNATCOM.BSTR;
with GNATCOM.Create.COM_Interface; use GNATCOM.Create.COM_Interface;
with GNATCOM.Events;

with TclControl.ITclControl_Interface; use TclControl.ITclControl_Interface;

with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.ActiveX; use GWindows.ActiveX;
--  with GWindows.Events;
with GWindows.Static_Controls; use GWindows.Static_Controls;
with GWindows.Application;
with GWindows.GStrings;

package body Tcl_Main is

   Main    : Main_Window_Type;
   Contain : ActiveX_Type;
   Label   : Label_Type;

   A_Event_Object : aliased TCL_Event_Type;
   A_Event        : constant Pointer_To_COM_Interface_Type :=
     TclControl.uITclControlEvents_Events.Create
     (A_Event_Object'Unchecked_Access);
   A_Events       : GNATCOM.Events.IConnectionPoint_Type;

   procedure Go is
      Result  : GNATCOM.Types.VARIANT_BOOL;
      pragma Unreferenced (Result);
   begin
      Create (Main, "TCL ActiveX Control Example",
              Width => 400, Height=> 300);

      Create (Contain, Main, TclControl.CLSID_TclControl, 10, 10, 246, 140);
      Create (Label, Main, "", 10, 150, 60, 30);

      Show (Main);

      Query (Control, Interfac (Contain));

      Result := SetVar (Control, To_BSTR ("a"), To_BSTR ("0"),
                        TclControl.GLOBAL_ONLY);

      Result := Eval (Control,
                      To_BSTR ("pack [button .b -text PressMe -command" &
                               " {focus .b; incr a} -width 10 -bd 1]"));
      Result := Eval
        (Control,
         To_BSTR ("pack [scale .s -orient horizontal -variable a" &
                  " -from 0 -to 360 -bd 1 -width 10]"));

      Result := Eval (Control,
                      To_BSTR ("pack [label .l -text {Tcl Window} -fg blue" &
                               " -font {Arial 12 bold}]"));

      Result := TraceVar (Control, To_BSTR ("a"),
                          TclControl.TRACE_WRITES);

      TclControl.uITclControlEvents_Events.Set_Events
        (A_Events,
         For_Object => Interfac (Contain),
         Event_Interface => A_Event);

      GWindows.Application.Message_Loop;

   end Go;

   procedure OnTraceVar
     (This  : TCL_Event_Type;
      name1 : GNATCOM.Types.VARIANT;
      Flags : GNATCOM.Types.VARIANT)
   is
   pragma Unreferenced (Flags, name1, This);
      Value  : GNATCOM.Types.BSTR;
      Result : GNATCOM.Types.VARIANT_BOOL;
      pragma Unreferenced (Result);
   begin
      Value := GetVar (Control, To_BSTR ("a"), TclControl.GLOBAL_ONLY);

      declare
         Val : constant Integer := Integer'Value (To_Ada (Value));
      begin
         Result := Eval
           (Control,
            To_BSTR (".b config -text {a is" & Val'Img & "}"));

         Left (Label, Val);

         Top (Label,
              Integer (60.0 *
                       Sin (Float (Val) * Ada.Numerics.Pi / 180.0) +
                       150.0));

         Text (Label, GWindows.GStrings.Image (Val));
      end;
   end OnTraceVar;

end Tcl_Main;
