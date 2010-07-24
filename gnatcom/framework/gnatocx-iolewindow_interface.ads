with GNATCOM.Iinterface;

package GNATOCX.IOleWindow_Interface is

   type IOleWindow_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out IOleWindow_Type);

   function Pointer (This : IOleWindow_Type)
     return Pointer_To_IOleWindow;

   procedure Attach (This    : in out IOleWindow_Type;
                     Pointer : in     Pointer_To_IOleWindow);

   procedure GetWindow
     (This  : IOleWindow_Type;
      phwnd : Pointer_To_wireHWND);

   procedure ContextSensitiveHelp
     (This       : IOleWindow_Type;
      fEnterMode : Interfaces.C.long);

end GNATOCX.IOleWindow_Interface;
