with GNATCOM.Errors;

package body GNATOCX.IOleClientSite_Interface is

   procedure Initialize (This : in out IOleClientSite_Type) is
   begin
      Set_IID (This, IID_IOleClientSite);
   end Initialize;

   function Pointer (This : IOleClientSite_Type)
     return Pointer_To_IOleClientSite
   is
   begin
      return To_Pointer_To_IOleClientSite (Address (This));
   end Pointer;

   procedure Attach (This    : in out IOleClientSite_Type;
                     Pointer : in     Pointer_To_IOleClientSite)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure SaveObject
     (This : IOleClientSite_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SaveObject
         (Pointer (This)));

   end SaveObject;

   procedure GetMoniker
     (This           : IOleClientSite_Type;
      dwAssign       : Interfaces.C.unsigned_long;
      dwWhichMoniker : Interfaces.C.unsigned_long;
      ppmk           : Pointer_To_Pointer_To_IMoniker)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetMoniker
         (Pointer (This),
          dwAssign,
          dwWhichMoniker,
          ppmk));

   end GetMoniker;

   procedure GetContainer
     (This        : IOleClientSite_Type;
      ppContainer : Pointer_To_Pointer_To_IOleContainer)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetContainer
         (Pointer (This),
          ppContainer));

   end GetContainer;

   procedure ShowObject
     (This : IOleClientSite_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ShowObject
         (Pointer (This)));

   end ShowObject;

   procedure OnShowWindow
     (This  : IOleClientSite_Type;
      fShow : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.OnShowWindow
         (Pointer (This),
          fShow));

   end OnShowWindow;

   procedure RequestNewObjectLayout
     (This : IOleClientSite_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RequestNewObjectLayout
         (Pointer (This)));

   end RequestNewObjectLayout;

end GNATOCX.IOleClientSite_Interface;
