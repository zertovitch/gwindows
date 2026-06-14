with GNATCOM.Iinterface;
with Win32_Types;

package GNATOCX.IEnumOLEVERB_Interface is

   type IEnumOLEVERB_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out IEnumOLEVERB_Type);

   function Pointer (This : IEnumOLEVERB_Type)
     return Pointer_To_IEnumOLEVERB;

   procedure Attach (This    : in out IEnumOLEVERB_Type;
                     Pointer : in     Pointer_To_IEnumOLEVERB);

   procedure RemoteNext
     (This         : IEnumOLEVERB_Type;
      celt         : Win32_Types.Unsigned_Long;
      rgelt        : Pointer_To_OLEVERB;
      pceltFetched : GNATCOM.Types.Pointer_To_unsigned_long);

   procedure Skip
     (This : IEnumOLEVERB_Type;
      celt : Win32_Types.Unsigned_Long);

   procedure Reset
     (This : IEnumOLEVERB_Type);

   procedure Clone
     (This   : IEnumOLEVERB_Type;
      ppenum : Pointer_To_Pointer_To_IEnumOLEVERB);

end GNATOCX.IEnumOLEVERB_Interface;
