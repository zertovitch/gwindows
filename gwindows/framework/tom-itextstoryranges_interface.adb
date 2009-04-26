with GNATCOM.Iinterface;

with GNATCOM.Errors;

package body TOM.ITextStoryRanges_Interface is

   procedure Initialize (This : in out ITextStoryRanges_Type) is
   begin
      Set_IID (This, IID_ITextStoryRanges);
   end Initialize;

   function Pointer (This : ITextStoryRanges_Type)
     return Pointer_To_ITextStoryRanges
   is
   begin
      return To_Pointer_To_ITextStoryRanges (Address (This));
   end Pointer;

   procedure Attach (This    : in out ITextStoryRanges_Type;
                     Pointer : in     Pointer_To_ITextStoryRanges)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   function uNewEnum
     (This      : ITextStoryRanges_Type)
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

   function Item
     (This    : ITextStoryRanges_Type;
      Index   : Interfaces.C.long)
     return Pointer_To_ITextRange
   is
       RetVal : aliased Pointer_To_ITextRange;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Item
         (Pointer (This),
          Index,
          RetVal'Unchecked_Access));

      return RetVal;
   end Item;

   function Get_Count
     (This   : ITextStoryRanges_Type)
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

end TOM.ITextStoryRanges_Interface;
