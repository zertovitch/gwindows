with GNATCOM.Dispinterface;

package TOM.ITextStoryRanges_Interface is

   type ITextStoryRanges_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out ITextStoryRanges_Type);

   function Pointer (This : ITextStoryRanges_Type)
     return Pointer_To_ITextStoryRanges;

   procedure Attach (This    : in out ITextStoryRanges_Type;
                     Pointer : in     Pointer_To_ITextStoryRanges);

   function uNewEnum
     (This      : ITextStoryRanges_Type)
     return GNATCOM.Types.Pointer_To_IUnknown;

   function Item
     (This    : ITextStoryRanges_Type;
      Index   : Interfaces.C.long)
     return Pointer_To_ITextRange;

   function Get_Count
     (This   : ITextStoryRanges_Type)
     return Interfaces.C.long;

end TOM.ITextStoryRanges_Interface;
