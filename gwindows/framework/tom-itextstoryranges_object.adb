package body TOM.ITextStoryRanges_Object is

   function uNewEnum
     (This : ITextStoryRanges_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, ITextStoryRanges_uNewEnum);
   end uNewEnum;

   function Item
     (This  : ITextStoryRanges_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ITextStoryRanges_Item,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Index),
         Free);
   end Item;

   function Get_Count
     (This : ITextStoryRanges_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, ITextStoryRanges_Get_Count);
   end Get_Count;

end TOM.ITextStoryRanges_Object;
