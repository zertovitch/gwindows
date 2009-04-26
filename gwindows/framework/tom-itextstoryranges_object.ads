with GNATCOM.Dispinterface;

package TOM.ITextStoryRanges_Object is

   type ITextStoryRanges_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function uNewEnum
     (This : ITextStoryRanges_Type)
     return GNATCOM.Types.VARIANT;

   function Item
     (This  : ITextStoryRanges_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function Get_Count
     (This : ITextStoryRanges_Type)
     return GNATCOM.Types.VARIANT;

end TOM.ITextStoryRanges_Object;
