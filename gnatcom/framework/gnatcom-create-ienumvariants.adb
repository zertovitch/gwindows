with Ada.Unchecked_Conversion;

package body GNATCOM.Create.IEnumVARIANTs is

   function IEnumVARIANT_Next
     (This         : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      celt         : GNATCOM.Types.ULONG;
      rgVar        : VARIANT_ARRAY_Access;
      pCeltFetched : GNATCOM.Types.Pointer_To_ULONG)
      return GNATCOM.Types.HRESULT
   is
      use GNATCOM.Types;
      Obj : constant Pointer_To_EnumVARIANT_Type :=
        Pointer_To_EnumVARIANT_Type (This.CoClass);
      I   : Natural := 0;
   begin
      while GNATCOM.Types.ULONG (I) < celt
        and then Has_Element (Obj.Position)
      loop
         rgVar (I) := To_VARIANT (Obj.Position);
         Next (Obj.Position);
         I := I + 1;
      end loop;
      if pCeltFetched /= null then
         pCeltFetched.all := GNATCOM.Types.ULONG (I);
      end if;
      if Has_Element (Obj.Position) or else GNATCOM.Types.ULONG (I) = celt then
         return GNATCOM.S_OK;
      else
         return GNATCOM.S_FALSE;
      end if;
   exception
      when others =>
         return GNATCOM.E_UNEXPECTED;
   end IEnumVARIANT_Next;

   function IEnumVARIANT_Skip
     (This : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      celt : GNATCOM.Types.ULONG)
      return GNATCOM.Types.HRESULT
   is
      use GNATCOM.Types;
      Obj : constant Pointer_To_EnumVARIANT_Type :=
        Pointer_To_EnumVARIANT_Type (This.CoClass);
      I : GNATCOM.Types.ULONG := 0;
   begin
      while I < celt and then Has_Element (Obj.Position) loop
         Next (Obj.Position);
         I := I + 1;
      end loop;
      if Has_Element (Obj.Position) or else I = celt then
         return GNATCOM.S_OK;
      else
         return GNATCOM.S_FALSE;
      end if;
   exception
      when others =>
         return GNATCOM.E_UNEXPECTED;
   end IEnumVARIANT_Skip;

   function IEnumVARIANT_Reset
     (This : access GNATCOM.Create.COM_Interface.COM_Interface_Type)
     return GNATCOM.Types.HRESULT
   is
      Obj : constant Pointer_To_EnumVARIANT_Type :=
        Pointer_To_EnumVARIANT_Type (This.CoClass);
   begin
      Obj.Position := Obj.First;
      return GNATCOM.S_OK;
   exception
      when others =>
         return GNATCOM.E_UNEXPECTED;
   end IEnumVARIANT_Reset;

   function IEnumVARIANT_Clone
     (This   : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      ppEnum : GNATCOM.Types.Pointer_To_Pointer_To_IEnumVARIANT)
      return GNATCOM.Types.HRESULT
   is
      use GNATCOM.Types;
      function To_Pointer_To_Pointer_To_Void is new Ada.Unchecked_Conversion
        (GNATCOM.Types.Pointer_To_Pointer_To_IEnumVARIANT,
         GNATCOM.Types.Pointer_To_Pointer_To_Void);
      Obj    : constant Pointer_To_EnumVARIANT_Type :=
        Pointer_To_EnumVARIANT_Type (This.CoClass);
      Object : Pointer_To_EnumVARIANT_Type := null;
      That : GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type :=
        null;
   begin
      if ppEnum = null then
         return GNATCOM.E_POINTER;
      else
         Object := new EnumVARIANT_Type;
         Object.First    := Obj.First;
         Object.Position := Obj.Position;
         That := GNATCOM.Create.COM_Interface.Create_Object
           (GNATCOM.Create.COM_Interface.Pointer_To_CoClass (Object));
         return GNATCOM.Create.COM_Interface.QueryInterface
           (That,
            IID_IEnumVARIANT'Access,
            To_Pointer_To_Pointer_To_Void (ppEnum));
      end if;
   exception
      when others =>
         return GNATCOM.E_UNEXPECTED;
   end IEnumVARIANT_Clone;

   function Create (First : Cursor)
           return GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type
   is
      Object : constant Pointer_To_EnumVARIANT_Type := new EnumVARIANT_Type;
   begin
      Object.First    := First;
      Object.Position := First;
      return GNATCOM.Create.COM_Interface.Create_Object
        (GNATCOM.Create.COM_Interface.Pointer_To_CoClass (Object));
   end Create;

end GNATCOM.Create.IEnumVARIANTs;
