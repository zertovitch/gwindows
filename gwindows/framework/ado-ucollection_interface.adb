with GNATCOM.Iinterface;

with GNATCOM.Errors;

package body ADO.uCollection_Interface is

   procedure Initialize (This : in out uCollection_Type) is
   begin
      Set_IID (This, IID_uCollection);
   end Initialize;

   function Pointer (This : uCollection_Type)
     return Pointer_To_uCollection
   is
   begin
      return To_Pointer_To_uCollection (Address (This));
   end Pointer;

   procedure Attach (This    : in out uCollection_Type;
                     Pointer : in     Pointer_To_uCollection)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   function Get_Count
     (This : uCollection_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Count
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Count;

   function uNewEnum
     (This      : uCollection_Type)
     return GNATCOM.Types.Pointer_To_IUnknown
   is
       RetVal : aliased GNATCOM.Types.Pointer_To_IUnknown;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.uNewEnum
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end uNewEnum;

   procedure Refresh
     (This : uCollection_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Refresh
         (Pointer (This)));

   end Refresh;

end ADO.uCollection_Interface;
