with GNATCOM.Events.Event_Object;
with GNATCOM.Create.COM_Interface;
with GNATCOM.IInterface;

package IE.DShellNameSpaceEvents_Events is

   type DShellNameSpaceEvents_Event is
     new GNATCOM.Events.Event_Object.Event_Type with null record;

   function Create (From : in GNATCOM.Events.Event_Object.Event_Pointer)
     return GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type;

   procedure Set_Events
     (This            : in out GNATCOM.Events.IConnectionPoint_Type;
      For_Object      : in     GNATCOM.IInterface.Interface_Type'Class;
      Event_Interface :
        access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      Free            : Boolean := True);

   procedure FavoritesSelectionChange
     (This              : DShellNameSpaceEvents_Event;
      cItems            : GNATCOM.Types.VARIANT;
      hItem             : GNATCOM.Types.VARIANT;
      strName           : GNATCOM.Types.VARIANT;
      strUrl            : GNATCOM.Types.VARIANT;
      cVisits           : GNATCOM.Types.VARIANT;
      strDate           : GNATCOM.Types.VARIANT;
      fAvailableOffline : GNATCOM.Types.VARIANT);

   procedure SelectionChange
     (This : DShellNameSpaceEvents_Event);

   procedure DoubleClick
     (This : DShellNameSpaceEvents_Event);

   procedure Initialized
     (This : DShellNameSpaceEvents_Event);

end IE.DShellNameSpaceEvents_Events;

