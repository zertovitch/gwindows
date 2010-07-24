with GNATCOM.Iinterface;

package GNATOCX.IRunningObjectTable_Interface is

   type IRunningObjectTable_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out IRunningObjectTable_Type);

   function Pointer (This : IRunningObjectTable_Type)
     return Pointer_To_IRunningObjectTable;

   procedure Attach (This    : in out IRunningObjectTable_Type;
                     Pointer : in     Pointer_To_IRunningObjectTable);

   procedure Register
     (This          : IRunningObjectTable_Type;
      grfFlags      : Interfaces.C.unsigned_long;
      punkObject    : GNATCOM.Types.Pointer_To_IUnknown;
      pmkObjectName : Pointer_To_IMoniker;
      pdwRegister   : GNATCOM.Types.Pointer_To_unsigned_long);

   procedure Revoke
     (This       : IRunningObjectTable_Type;
      dwRegister : Interfaces.C.unsigned_long);

   procedure IsRunning
     (This          : IRunningObjectTable_Type;
      pmkObjectName : Pointer_To_IMoniker);

   procedure GetObject
     (This          : IRunningObjectTable_Type;
      pmkObjectName : Pointer_To_IMoniker;
      ppunkObject   : GNATCOM.Types.Pointer_To_Pointer_To_IUnknown);

   procedure NoteChangeTime
     (This       : IRunningObjectTable_Type;
      dwRegister : Interfaces.C.unsigned_long;
      pfiletime  : Pointer_To_uFILETIME);

   procedure GetTimeOfLastChange
     (This          : IRunningObjectTable_Type;
      pmkObjectName : Pointer_To_IMoniker;
      pfiletime     : Pointer_To_uFILETIME);

   procedure EnumRunning
     (This          : IRunningObjectTable_Type;
      ppenumMoniker : Pointer_To_Pointer_To_IEnumMoniker);

end GNATOCX.IRunningObjectTable_Interface;
