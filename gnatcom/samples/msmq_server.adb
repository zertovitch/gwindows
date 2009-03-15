with GNAT.IO; use GNAT.IO;

with GNATCOM.Types;
with GNATCOM.BSTR; use GNATCOM.BSTR;
with GNATCOM.VARIANT; use GNATCOM.VARIANT;
with GNATCOM.Create.COM_Interface;
with GNATCOM.Utility;
with GNATCOM.Initialize;
with GNATCOM.Events;

with MSMQ.IMSMQQueueInfo_Interface; use MSMQ.IMSMQQueueInfo_Interface;
with MSMQ.IMSMQQueue_Interface; use MSMQ.IMSMQQueue_Interface;
with MSMQ.uDMSMQEventEvents_Events; use MSMQ.uDMSMQEventEvents_Events;
with MSMQ.IMSMQEvent_Interface; use MSMQ.IMSMQEvent_Interface;

with MSMQ_Event; use MSMQ_Event;

procedure MSMQ_Server is
   QueueInfo       : IMSMQQueueInfo_Type;
   Queue           : IMSMQQueue_Type;
   T               : aliased GNATCOM.Types.VARIANT := To_VARIANT (True);
begin
   GNATCOM.Initialize.Initialize_COM;

   Put_Line ("Create Queue");

   Create(QueueInfo, MSMQ.CLSID_MSMQQueueInfo);

   Put_Label(QueueInfo, To_BSTR ("Test Queue"));

   Put_PathName(QueueInfo, TO_BSTR (".\TestQueue"));

   begin
      Create (QueueInfo,
              IsWorldReadable => T'Unchecked_Access);
   exception
      when others =>
         --  Queue already exists, so lets just keep going
         null;
   end;

   Put_Line ("Open Queue");
   Attach (Queue,
           Open (QueueInfo,
                 uAccess   => MSMQ.MQ_RECEIVE_ACCESS,
                 ShareMode => MSMQ.MQ_DENY_RECEIVE_SHARE));


   Put_Line ("Enable Receive");
   Create (MSMQ_Events, MSMQ.CLSID_MSMQEvent);

   Set_Events (Event_Connect,
               For_Object      => MSMQ_Events,
               Event_Interface => Events);

   EnableNotification (Queue,
                       Event          => Pointer (MSMQ_Events),
                       ReceiveTimeout => Time_Out'Access);

   Put_Line ("Wait for messages");
   GNATCOM.Utility.Message_Loop;
end MSMQ_Server;
