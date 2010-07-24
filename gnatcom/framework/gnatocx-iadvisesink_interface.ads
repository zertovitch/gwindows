with GNATCOM.Iinterface;

package GNATOCX.IAdviseSink_Interface is

   type IAdviseSink_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out IAdviseSink_Type);

   function Pointer (This : IAdviseSink_Type)
     return Pointer_To_IAdviseSink;

   procedure Attach (This    : in out IAdviseSink_Type;
                     Pointer : in     Pointer_To_IAdviseSink);

   procedure RemoteOnDataChange
     (This       : IAdviseSink_Type;
      pformatetc : Pointer_To_FORMATETC;
      pStgmed    : Pointer_To_wireASYNC_STGMEDIUM);

   procedure RemoteOnViewChange
     (This     : IAdviseSink_Type;
      dwAspect : Interfaces.C.unsigned_long;
      lindex   : Interfaces.C.long);

   procedure RemoteOnRename
     (This : IAdviseSink_Type;
      pmk  : Pointer_To_IMoniker);

   procedure RemoteOnSave
     (This : IAdviseSink_Type);

   procedure RemoteOnClose
     (This : IAdviseSink_Type);

end GNATOCX.IAdviseSink_Interface;
