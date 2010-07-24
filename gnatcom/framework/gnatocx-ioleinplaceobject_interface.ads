with GNATCOM.Iinterface;

package GNATOCX.IOleInPlaceObject_Interface is

   type IOleInPlaceObject_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out IOleInPlaceObject_Type);

   function Pointer (This : IOleInPlaceObject_Type)
     return Pointer_To_IOleInPlaceObject;

   procedure Attach (This    : in out IOleInPlaceObject_Type;
                     Pointer : in     Pointer_To_IOleInPlaceObject);

   procedure GetWindow
     (This  : IOleInPlaceObject_Type;
      phwnd : Pointer_To_wireHWND);

   procedure ContextSensitiveHelp
     (This       : IOleInPlaceObject_Type;
      fEnterMode : Interfaces.C.long);

   procedure InPlaceDeactivate
     (This : IOleInPlaceObject_Type);

   procedure UIDeactivate
     (This : IOleInPlaceObject_Type);

   procedure SetObjectRects
     (This         : IOleInPlaceObject_Type;
      lprcPosRect  : Pointer_To_RECT;
      lprcClipRect : Pointer_To_RECT);

   procedure ReactivateAndUndo
     (This : IOleInPlaceObject_Type);

end GNATOCX.IOleInPlaceObject_Interface;
