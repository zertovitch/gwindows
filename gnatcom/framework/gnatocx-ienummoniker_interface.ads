with GNATCOM.Iinterface;

package GNATOCX.IEnumMoniker_Interface is

   type IEnumMoniker_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out IEnumMoniker_Type);

   function Pointer (This : IEnumMoniker_Type)
     return Pointer_To_IEnumMoniker;

   procedure Attach (This    : in out IEnumMoniker_Type;
                     Pointer : in     Pointer_To_IEnumMoniker);

   procedure RemoteNext
     (This         : IEnumMoniker_Type;
      celt         : Interfaces.C.unsigned_long;
      rgelt        : Pointer_To_Pointer_To_IMoniker;
      pceltFetched : GNATCOM.Types.Pointer_To_unsigned_long);

   procedure Skip
     (This : IEnumMoniker_Type;
      celt : Interfaces.C.unsigned_long);

   procedure Reset
     (This : IEnumMoniker_Type);

   procedure Clone
     (This   : IEnumMoniker_Type;
      ppenum : Pointer_To_Pointer_To_IEnumMoniker);

end GNATOCX.IEnumMoniker_Interface;
