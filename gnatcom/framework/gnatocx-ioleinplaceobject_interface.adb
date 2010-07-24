with GNATCOM.Errors;

package body GNATOCX.IOleInPlaceObject_Interface is

   procedure Initialize (This : in out IOleInPlaceObject_Type) is
   begin
      Set_IID (This, IID_IOleInPlaceObject);
   end Initialize;

   function Pointer (This : IOleInPlaceObject_Type)
     return Pointer_To_IOleInPlaceObject
   is
   begin
      return To_Pointer_To_IOleInPlaceObject (Address (This));
   end Pointer;

   procedure Attach (This    : in out IOleInPlaceObject_Type;
                     Pointer : in     Pointer_To_IOleInPlaceObject)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure GetWindow
     (This  : IOleInPlaceObject_Type;
      phwnd : Pointer_To_wireHWND)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetWindow
         (Pointer (This),
          phwnd));

   end GetWindow;

   procedure ContextSensitiveHelp
     (This       : IOleInPlaceObject_Type;
      fEnterMode : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ContextSensitiveHelp
         (Pointer (This),
          fEnterMode));

   end ContextSensitiveHelp;

   procedure InPlaceDeactivate
     (This : IOleInPlaceObject_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.InPlaceDeactivate
         (Pointer (This)));

   end InPlaceDeactivate;

   procedure UIDeactivate
     (This : IOleInPlaceObject_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.UIDeactivate
         (Pointer (This)));

   end UIDeactivate;

   procedure SetObjectRects
     (This         : IOleInPlaceObject_Type;
      lprcPosRect  : Pointer_To_RECT;
      lprcClipRect : Pointer_To_RECT)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetObjectRects
         (Pointer (This),
          lprcPosRect,
          lprcClipRect));

   end SetObjectRects;

   procedure ReactivateAndUndo
     (This : IOleInPlaceObject_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ReactivateAndUndo
         (Pointer (This)));

   end ReactivateAndUndo;

end GNATOCX.IOleInPlaceObject_Interface;
