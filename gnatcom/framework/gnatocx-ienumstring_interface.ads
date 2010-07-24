with GNATCOM.Iinterface;

package GNATOCX.IEnumString_Interface is

   type IEnumString_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out IEnumString_Type);

   function Pointer (This : IEnumString_Type)
     return Pointer_To_IEnumString;

   procedure Attach (This    : in out IEnumString_Type;
                     Pointer : in     Pointer_To_IEnumString);

   procedure RemoteNext
     (This         : IEnumString_Type;
      celt         : Interfaces.C.unsigned_long;
      rgelt        : GNATCOM.Types.Pointer_To_LPWSTR;
      pceltFetched : GNATCOM.Types.Pointer_To_unsigned_long);

   procedure Skip
     (This : IEnumString_Type;
      celt : Interfaces.C.unsigned_long);

   procedure Reset
     (This : IEnumString_Type);

   procedure Clone
     (This   : IEnumString_Type;
      ppenum : Pointer_To_Pointer_To_IEnumString);

end GNATOCX.IEnumString_Interface;
