with Ada.Unchecked_Conversion;
with GNATCOM.Create.IConnectionPoint;
with GNATCOM.Errors;

package body GNATCOM.Create.IConnectionPointContainer is

   function FindConnectionPoint
     (Connections : Pointer_To_COM_Interface_Array;
      riid        : GNATCOM.Types.Pointer_To_GUID;
      ppCP        : GNATCOM.Types.Pointer_To_Pointer_To_IConnectionPoint)
      return GNATCOM.Types.HRESULT
   is
      use type GNATCOM.Types.Pointer_To_Pointer_To_IConnectionPoint;
      use type GNATCOM.Types.GUID;

      function To_Pointer_To_Pointer_To_Void is new Ada.Unchecked_Conversion
        (GNATCOM.Types.Pointer_To_Pointer_To_IConnectionPoint,
         GNATCOM.Types.Pointer_To_Pointer_To_Void);

      Result : GNATCOM.Types.HRESULT;
   begin
      if ppCP = null then
         Result := GNATCOM.E_POINTER;
      else
         Result := GNATCOM.CONNECT_E_NOCONNETION;
         for Connection of Connections loop
            if riid.all = IConnectionPoint.Pointer_To_ConnectionPoint_Type
              (Connection.CoClass).Event_IID.all
            then
               Result := COM_Interface.QueryInterface
                 (Connection, GNATCOM.Types.IID_IConnectionPoint'Access,
                  To_Pointer_To_Pointer_To_Void (ppCP));
               if GNATCOM.Errors.SUCCEEDED (Result) then
                  Result := GNATCOM.S_OK;
               else
                  Result := GNATCOM.E_UNEXPECTED;
               end if;
               exit;
            end if;
         end loop;
      end if;
      return Result;
   end FindConnectionPoint;

   type af_IEnumConnectionPoints_Clone is access
     function (This   : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
               ppEnum :
               GNATCOM.Types.Pointer_To_Pointer_To_IEnumConnectionPoints)
               return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type Pointer_To_IConnectionPoint_Array is array
     (Interfaces.C.unsigned_long range <>) of
     GNATCOM.Types.Pointer_To_IConnectionPoint;

   type af_IEnumConnectionPoints_Next is access
     function (This   : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
               cConnections : Interfaces.C.unsigned_long;
               ppCP         : out Pointer_To_IConnectionPoint_Array;
               pcFetched    : access Interfaces.C.unsigned_long)
               return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type af_IEnumConnectionPoints_Reset is access
     function (This : access GNATCOM.Create.COM_Interface.COM_Interface_Type)
               return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type af_IEnumConnectionPoints_Skip is access
     function (This : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
               cConnections : Interfaces.C.unsigned_long)
               return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   function IEnumConnectionPoints_Clone
     (This   : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      ppEnum : GNATCOM.Types.Pointer_To_Pointer_To_IEnumConnectionPoints)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   pragma Warnings (Off,
"type of argument ""IEnumConnectionPoints_Next.ppCP"" is unconstrained array");

   function IEnumConnectionPoints_Next
     (This         : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      cConnections : Interfaces.C.unsigned_long;
      ppCP         : out Pointer_To_IConnectionPoint_Array;
      pcFetched    : access Interfaces.C.unsigned_long)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   function IEnumConnectionPoints_Reset
     (This : access GNATCOM.Create.COM_Interface.COM_Interface_Type)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   function IEnumConnectionPoints_Skip
     (This : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      cConnections : Interfaces.C.unsigned_long)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type IEnumConnectionPoints_Vtbl_Record is
      record
         IUnknown : GNATCOM.Create.COM_Interface.IUnknown_Vtbl_Record;
         Next     : af_IEnumConnectionPoints_Next :=
           IEnumConnectionPoints_Next'Access;
         Skip     : af_IEnumConnectionPoints_Skip :=
           IEnumConnectionPoints_Skip'Access;
         Reset    : af_IEnumConnectionPoints_Reset :=
           IEnumConnectionPoints_Reset'Access;
         Clone    : af_IEnumConnectionPoints_Clone :=
           IEnumConnectionPoints_Clone'Access;
      end record
     with Convention => C_Pass_By_Copy;

   IEnumConnectionPoints_Vtbl : aliased IEnumConnectionPoints_Vtbl_Record;

   GUID_Map : aliased GNATCOM.Create.COM_Interface.GUID_Record_Array :=
     (1 => (GNATCOM.Types.IID_IEnumConnectionPoints,
            IEnumConnectionPoints_Vtbl'Address));

   type EnumConnectionPoints_Type (Last : Positive) is
     new GNATCOM.Create.COM_Interface.CoClass_Type (GUID_Map'Access) with
      record
         Connections : Pointer_To_COM_Interface_Array (1 .. Last);
         I           : Positive := 1;
      end record;

   type Pointer_To_EnumConnectionPoints_Type is
     access all EnumConnectionPoints_Type;

   function To_Pointer_To_Pointer_To_Void is new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Pointer_To_IEnumConnectionPoints,
      GNATCOM.Types.Pointer_To_Pointer_To_Void);

   function IEnumConnectionPoints_Clone
     (This   : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      ppEnum : GNATCOM.Types.Pointer_To_Pointer_To_IEnumConnectionPoints)
      return GNATCOM.Types.HRESULT
   is
      Obj    : constant Pointer_To_EnumConnectionPoints_Type :=
        Pointer_To_EnumConnectionPoints_Type (This.CoClass);
      Result : GNATCOM.Types.HRESULT;
   begin
      Result := COM_Interface.QueryInterface
        (COM_Interface.Create_Object (new EnumConnectionPoints_Type'(Obj.all)),
         GNATCOM.Types.IID_IEnumConnectionPoints'Access,
         To_Pointer_To_Pointer_To_Void (ppEnum));
      if GNATCOM.Errors.SUCCEEDED (Result) then
         return GNATCOM.S_OK;
      else
         return GNATCOM.E_UNEXPECTED;
      end if;
   exception
      when others =>
         return GNATCOM.E_UNEXPECTED;
   end IEnumConnectionPoints_Clone;

   function IEnumConnectionPoints_Next
     (This         : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      cConnections : Interfaces.C.unsigned_long;
      ppCP         : out Pointer_To_IConnectionPoint_Array;
      pcFetched    : access Interfaces.C.unsigned_long)
      return GNATCOM.Types.HRESULT
   is
      use type Interfaces.C.unsigned_long;

      function To_Pointer_To_Pointer_To_Void is new Ada.Unchecked_Conversion
        (GNATCOM.Types.Pointer_To_Pointer_To_IConnectionPoint,
         GNATCOM.Types.Pointer_To_Pointer_To_Void);

      Obj : constant Pointer_To_EnumConnectionPoints_Type :=
        Pointer_To_EnumConnectionPoints_Type (This.CoClass);
      Fetched : constant Interfaces.C.unsigned_long :=
        Interfaces.C.unsigned_long'Min
          (cConnections, Interfaces.C.unsigned_long (Obj.Last - Obj.I + 1));
      Result : GNATCOM.Types.HRESULT;
      CP     : aliased GNATCOM.Types.Pointer_To_IConnectionPoint;
   begin
      for J in 0 .. Fetched - 1 loop
         CP     := null;
         Result := COM_Interface.QueryInterface
           (Obj.Connections (Obj.I),
            GNATCOM.Types.IID_IConnectionPoint'Access,
            To_Pointer_To_Pointer_To_Void (CP'Access));
         ppCP (ppCP'First + J) := CP;
         Obj.I := Obj.I + 1;
      end loop;
      if pcFetched /= null then
         pcFetched.all := Fetched;
      end if;
      if cConnections > Fetched then
         return GNATCOM.S_FALSE;
      else
         return GNATCOM.S_OK;
      end if;
   end IEnumConnectionPoints_Next;

   function IEnumConnectionPoints_Reset
     (This : access GNATCOM.Create.COM_Interface.COM_Interface_Type)
      return GNATCOM.Types.HRESULT
   is
      Obj : constant Pointer_To_EnumConnectionPoints_Type :=
        Pointer_To_EnumConnectionPoints_Type (This.CoClass);
   begin
      Obj.I := Obj.Connections'First;
      return GNATCOM.S_OK;
   end IEnumConnectionPoints_Reset;

   function IEnumConnectionPoints_Skip
     (This         : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      cConnections : Interfaces.C.unsigned_long)
      return GNATCOM.Types.HRESULT
   is
      use type Interfaces.C.unsigned_long;

      Obj : constant Pointer_To_EnumConnectionPoints_Type :=
        Pointer_To_EnumConnectionPoints_Type (This.CoClass);
   begin
      Obj.I := Positive (Interfaces.C.unsigned_long (Obj.I) + cConnections);
      if Obj.I > Obj.Connections'Last then
         Obj.I := Obj.Connections'Last;
         return S_FALSE;
      else
         return S_OK;
      end if;
   end IEnumConnectionPoints_Skip;

   function EnumConnectionPoints
     (Connections : Pointer_To_COM_Interface_Array;
      ppEnum      : GNATCOM.Types.Pointer_To_Pointer_To_IEnumConnectionPoints)
      return GNATCOM.Types.HRESULT
   is
      use type GNATCOM.Types.Pointer_To_Pointer_To_IEnumConnectionPoints;

      Enumerator : Pointer_To_EnumConnectionPoints_Type;
      Result     : GNATCOM.Types.HRESULT;
   begin
      if ppEnum = null then
         Result := GNATCOM.E_POINTER;
      else
         Enumerator := new EnumConnectionPoints_Type (Connections'Length);
         Enumerator.Connections := Connections;
         Result := COM_Interface.QueryInterface
           (COM_Interface.Create_Object
              (COM_Interface.Pointer_To_CoClass (Enumerator)),
            GNATCOM.Types.IID_IEnumConnectionPoints'Access,
            To_Pointer_To_Pointer_To_Void (ppEnum));
         if GNATCOM.Errors.SUCCEEDED (Result) then
            Result := GNATCOM.S_OK;
         else
            Result := GNATCOM.E_UNEXPECTED;
         end if;
      end if;
      return Result;
   exception
      when others =>
         return GNATCOM.E_UNEXPECTED;
   end EnumConnectionPoints;

end GNATCOM.Create.IConnectionPointContainer;
