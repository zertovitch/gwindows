with GNATCOM.Iinterface;

package GNATOCX.IPersist_Interface is

   type IPersist_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out IPersist_Type);

   function Pointer (This : IPersist_Type)
     return Pointer_To_IPersist;

   procedure Attach (This    : in out IPersist_Type;
                     Pointer : in     Pointer_To_IPersist);

   procedure GetClassID
     (This     : IPersist_Type;
      pClassID : GNATCOM.Types.Pointer_To_GUID);

end GNATOCX.IPersist_Interface;
