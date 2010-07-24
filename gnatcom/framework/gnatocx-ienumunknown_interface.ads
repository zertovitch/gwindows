with GNATCOM.Iinterface;

package GNATOCX.IEnumUnknown_Interface is

   type IEnumUnknown_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out IEnumUnknown_Type);

   function Pointer (This : IEnumUnknown_Type)
     return Pointer_To_IEnumUnknown;

   procedure Attach (This    : in out IEnumUnknown_Type;
                     Pointer : in     Pointer_To_IEnumUnknown);

   procedure RemoteNext
     (This         : IEnumUnknown_Type;
      celt         : Interfaces.C.unsigned_long;
      rgelt        : GNATCOM.Types.Pointer_To_Pointer_To_IUnknown;
      pceltFetched : GNATCOM.Types.Pointer_To_unsigned_long);

   procedure Skip
     (This : IEnumUnknown_Type;
      celt : Interfaces.C.unsigned_long);

   procedure Reset
     (This : IEnumUnknown_Type);

   procedure Clone
     (This   : IEnumUnknown_Type;
      ppenum : Pointer_To_Pointer_To_IEnumUnknown);

end GNATOCX.IEnumUnknown_Interface;
