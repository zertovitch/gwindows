package body IE.DShellWindowsEvents_Events is

   procedure Invoke
     (dispidMember : in Interfaces.C.long;
      wFlags       : in Interfaces.C.unsigned_short;
      pdispparams  : in GNATCOM.Types.Pointer_To_DISPPARAMS;
      Event_Object : in GNATCOM.Events.Event_Object.Event_Pointer)
   is
      use type Interfaces.C.long;
   begin
      case dispidMember is
         when DShellWindowsEvents_WindowRegistered=>
            WindowRegistered
              (DShellWindowsEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when DShellWindowsEvents_WindowRevoked=>
            WindowRevoked
              (DShellWindowsEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (0));
         when others =>
            null;
      end case;
   end Invoke;

   function Create (From : in GNATCOM.Events.Event_Object.Event_Pointer)
     return GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type
   is
   begin
      return GNATCOM.Events.Event_Object.Create
        (Invoke'Access,
         IID_DShellWindowsEvents,
         From);
   end Create;

   procedure Set_Events
     (This            : in out GNATCOM.Events.IConnectionPoint_Type;
      For_Object      : in     GNATCOM.IInterface.Interface_Type'Class;
      Event_Interface :
        access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      Free            : Boolean := True)
   is
   begin
      GNATCOM.Events.Set_Events
        (This,
         For_Object,
         IID_DShellWindowsEvents,
         Event_Interface,
         Free);
   end Set_Events;

   procedure WindowRegistered
     (This    : DShellWindowsEvents_Event;
      lCookie : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end WindowRegistered;

   procedure WindowRevoked
     (This    : DShellWindowsEvents_Event;
      lCookie : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end WindowRevoked;

end IE.DShellWindowsEvents_Events;

