with GNATCOM.Errors;

package body GNATOCX.IOleWindow_Interface is

   procedure Initialize (This : in out IOleWindow_Type) is
   begin
      Set_IID (This, IID_IOleWindow);
   end Initialize;

   function Pointer (This : IOleWindow_Type)
     return Pointer_To_IOleWindow
   is
   begin
      return To_Pointer_To_IOleWindow (Address (This));
   end Pointer;

   procedure Attach (This    : in out IOleWindow_Type;
                     Pointer : in     Pointer_To_IOleWindow)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure GetWindow
     (This  : IOleWindow_Type;
      phwnd : Pointer_To_wireHWND)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetWindow
         (Pointer (This),
          phwnd));

   end GetWindow;

   procedure ContextSensitiveHelp
     (This       : IOleWindow_Type;
      fEnterMode : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ContextSensitiveHelp
         (Pointer (This),
          fEnterMode));

   end ContextSensitiveHelp;

end GNATOCX.IOleWindow_Interface;
