with GNATCOM.Types;

--  The package tree TclControl is obtained by running
--    bindcom tclcontrol83.dll TclControl
--  in the gwindows\redist directory (see readme.txt in this directory).
--
with TclControl.ITclControl_Interface;
with TclControl.uITclControlEvents_Events;

package Tcl_Main is

   Control : TclControl.ITclControl_Interface.ITclControl_Type;

   procedure Go;

   type TCL_Event_Type is
     new TclControl.uITclControlEvents_Events.uITclControlEvents_Event
     with null record;

   procedure OnTraceVar
     (This  : TCL_Event_Type;
      name1 : GNATCOM.Types.VARIANT;
      Flags : GNATCOM.Types.VARIANT);


end Tcl_Main;
