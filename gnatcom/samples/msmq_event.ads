with GNATCOM.Types;
with GNATCOM.Create.COM_Interface;
with GNATCOM.Events;
with GNATCOM.Types;
with GNATCOM.VARIANT;

with MSMQ.uDMSMQEventEvents_Events;
with MSMQ.IMSMQEvent_Interface;

package MSMQ_Event is

   type MSMQ_Event_Type is
     new MSMQ.UDMSMQEventEvents_Events.UDMSMQEventEvents_Event with
     null record;

   procedure Arrived
     (This   : MSMQ_Event_Type;
      Queue  : GNATCOM.Types.VARIANT;
      Cursor : GNATCOM.Types.VARIANT);

   procedure ArrivedError
     (This      : MSMQ_Event_Type;
      Queue     : GNATCOM.Types.VARIANT;
      ErrorCode : GNATCOM.Types.VARIANT;
      Cursor    : GNATCOM.Types.VARIANT);

   Event_Object  : aliased MSMQ_Event_Type;
   Events        :
     GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type :=
     MSMQ.UDMSMQEventEvents_Events.Create (Event_Object'Unchecked_Access);
   Event_Connect : GNATCOM.Events.IConnectionPoint_Type;
   MSMQ_Events   : MSMQ.IMSMQEvent_Interface.IMSMQEvent_Type;
   Time_Out      : aliased GNATCOM.Types.VARIANT :=
     GNATCOM.VARIANT.To_VARIANT (10000);

end MSMQ_Event;
