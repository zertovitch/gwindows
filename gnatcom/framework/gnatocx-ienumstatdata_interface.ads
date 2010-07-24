with GNATCOM.Iinterface;

package GNATOCX.IEnumSTATDATA_Interface is

   type IEnumSTATDATA_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out IEnumSTATDATA_Type);

   function Pointer (This : IEnumSTATDATA_Type)
     return Pointer_To_IEnumSTATDATA;

   procedure Attach (This    : in out IEnumSTATDATA_Type;
                     Pointer : in     Pointer_To_IEnumSTATDATA);

   procedure RemoteNext
     (This         : IEnumSTATDATA_Type;
      celt         : Interfaces.C.unsigned_long;
      rgelt        : Pointer_To_STATDATA;
      pceltFetched : GNATCOM.Types.Pointer_To_unsigned_long);

   procedure Skip
     (This : IEnumSTATDATA_Type;
      celt : Interfaces.C.unsigned_long);

   procedure Reset
     (This : IEnumSTATDATA_Type);

   procedure Clone
     (This   : IEnumSTATDATA_Type;
      ppenum : Pointer_To_Pointer_To_IEnumSTATDATA);

end GNATOCX.IEnumSTATDATA_Interface;
