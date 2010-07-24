with GNATCOM.Errors;

package body GNATOCX.IRunningObjectTable_Interface is

   procedure Initialize (This : in out IRunningObjectTable_Type) is
   begin
      Set_IID (This, IID_IRunningObjectTable);
   end Initialize;

   function Pointer (This : IRunningObjectTable_Type)
     return Pointer_To_IRunningObjectTable
   is
   begin
      return To_Pointer_To_IRunningObjectTable (Address (This));
   end Pointer;

   procedure Attach (This    : in out IRunningObjectTable_Type;
                     Pointer : in     Pointer_To_IRunningObjectTable)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure Register
     (This          : IRunningObjectTable_Type;
      grfFlags      : Interfaces.C.unsigned_long;
      punkObject    : GNATCOM.Types.Pointer_To_IUnknown;
      pmkObjectName : Pointer_To_IMoniker;
      pdwRegister   : GNATCOM.Types.Pointer_To_unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Register
         (Pointer (This),
          grfFlags,
          punkObject,
          pmkObjectName,
          pdwRegister));

   end Register;

   procedure Revoke
     (This       : IRunningObjectTable_Type;
      dwRegister : Interfaces.C.unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Revoke
         (Pointer (This),
          dwRegister));

   end Revoke;

   procedure IsRunning
     (This          : IRunningObjectTable_Type;
      pmkObjectName : Pointer_To_IMoniker)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.IsRunning
         (Pointer (This),
          pmkObjectName));

   end IsRunning;

   procedure GetObject
     (This          : IRunningObjectTable_Type;
      pmkObjectName : Pointer_To_IMoniker;
      ppunkObject   : GNATCOM.Types.Pointer_To_Pointer_To_IUnknown)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetObject
         (Pointer (This),
          pmkObjectName,
          ppunkObject));

   end GetObject;

   procedure NoteChangeTime
     (This       : IRunningObjectTable_Type;
      dwRegister : Interfaces.C.unsigned_long;
      pfiletime  : Pointer_To_uFILETIME)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.NoteChangeTime
         (Pointer (This),
          dwRegister,
          pfiletime));

   end NoteChangeTime;

   procedure GetTimeOfLastChange
     (This          : IRunningObjectTable_Type;
      pmkObjectName : Pointer_To_IMoniker;
      pfiletime     : Pointer_To_uFILETIME)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetTimeOfLastChange
         (Pointer (This),
          pmkObjectName,
          pfiletime));

   end GetTimeOfLastChange;

   procedure EnumRunning
     (This          : IRunningObjectTable_Type;
      ppenumMoniker : Pointer_To_Pointer_To_IEnumMoniker)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.EnumRunning
         (Pointer (This),
          ppenumMoniker));

   end EnumRunning;

end GNATOCX.IRunningObjectTable_Interface;
