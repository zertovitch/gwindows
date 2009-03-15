with GNATCOM.Types;

with TclControl.ITclControl_Interface;
with TclControl.UITclControlEvents_Events;

package Tcl_Main is

   Control : TclControl.ITclControl_Interface.ITclControl_Type;

   procedure Go;

   type TCL_Event_Type is
     new TclControl.UITclControlEvents_Events.uITclControlEvents_Event
     with null record;

   procedure OnTraceVar
     (This  : TCL_Event_Type;
      name1 : GNATCOM.Types.VARIANT;
      Flags : GNATCOM.Types.VARIANT);


end Tcl_Main;
