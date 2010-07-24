with GNATCOM.Errors;

package body GNATOCX.IPersist_Interface is

   procedure Initialize (This : in out IPersist_Type) is
   begin
      Set_IID (This, IID_IPersist);
   end Initialize;

   function Pointer (This : IPersist_Type)
     return Pointer_To_IPersist
   is
   begin
      return To_Pointer_To_IPersist (Address (This));
   end Pointer;

   procedure Attach (This    : in out IPersist_Type;
                     Pointer : in     Pointer_To_IPersist)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure GetClassID
     (This     : IPersist_Type;
      pClassID : GNATCOM.Types.Pointer_To_GUID)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetClassID
         (Pointer (This),
          pClassID));

   end GetClassID;

end GNATOCX.IPersist_Interface;
