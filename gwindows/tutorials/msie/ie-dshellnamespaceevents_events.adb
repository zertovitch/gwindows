package body IE.DShellNameSpaceEvents_Events is

   procedure Invoke
     (dispidMember : in Interfaces.C.long;
      wFlags       : in Interfaces.C.unsigned_short;
      pdispparams  : in GNATCOM.Types.Pointer_To_DISPPARAMS;
      Event_Object : in GNATCOM.Events.Event_Object.Event_Pointer)
   is
      use type Interfaces.C.long;
   begin
      case dispidMember is
         when DShellNameSpaceEvents_FavoritesSelectionChange=>
            FavoritesSelectionChange
              (DShellNameSpaceEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (6),
               pdispparams.rgvarg (5),
               pdispparams.rgvarg (4),
               pdispparams.rgvarg (3),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when DShellNameSpaceEvents_SelectionChange=>
            SelectionChange
              (DShellNameSpaceEvents_Event'Class (Event_Object.all));
         when DShellNameSpaceEvents_DoubleClick=>
            DoubleClick
              (DShellNameSpaceEvents_Event'Class (Event_Object.all));
         when DShellNameSpaceEvents_Initialized=>
            Initialized
              (DShellNameSpaceEvents_Event'Class (Event_Object.all));
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
         IID_DShellNameSpaceEvents,
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
         IID_DShellNameSpaceEvents,
         Event_Interface,
         Free);
   end Set_Events;

   procedure FavoritesSelectionChange
     (This              : DShellNameSpaceEvents_Event;
      cItems            : GNATCOM.Types.VARIANT;
      hItem             : GNATCOM.Types.VARIANT;
      strName           : GNATCOM.Types.VARIANT;
      strUrl            : GNATCOM.Types.VARIANT;
      cVisits           : GNATCOM.Types.VARIANT;
      strDate           : GNATCOM.Types.VARIANT;
      fAvailableOffline : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end FavoritesSelectionChange;

   procedure SelectionChange
     (This : DShellNameSpaceEvents_Event)
   is
   begin
      null;
   end SelectionChange;

   procedure DoubleClick
     (This : DShellNameSpaceEvents_Event)
   is
   begin
      null;
   end DoubleClick;

   procedure Initialized
     (This : DShellNameSpaceEvents_Event)
   is
   begin
      null;
   end Initialized;

end IE.DShellNameSpaceEvents_Events;

