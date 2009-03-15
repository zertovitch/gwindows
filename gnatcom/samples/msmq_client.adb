with GNAT.IO; use GNAT.IO;

with GNATCOM.Types;
with GNATCOM.BSTR; use GNATCOM.BSTR;
with GNATCOM.VARIANT; use GNATCOM.VARIANT;
with GNATCOM.Initialize;


with MSMQ.IMSMQQuery_Interface; use MSMQ.IMSMQQuery_Interface;
with MSMQ.IMSMQQueueInfos_Interface; use MSMQ.IMSMQQueueInfos_Interface;
with MSMQ.IMSMQQueueInfo_Interface; use MSMQ.IMSMQQueueInfo_Interface;
with MSMQ.IMSMQQueue_Interface; use MSMQ.IMSMQQueue_Interface;
with MSMQ.IMSMQMessage_Interface; use MSMQ.IMSMQMessage_Interface;

procedure MSMQ_Client is
   Query      : IMSMQQuery_Type;
   QueueInfos : IMSMQQueueInfos_Type;
   QueueInfo  : IMSMQQueueInfo_Type;
   Queue      : IMSMQQueue_Type;
   Message    : IMSMQMessage_Type;
   QueueName  : aliased GNATCOM.Types.VARIANT := To_VARIANT ("Test Queue");
begin
   GNATCOM.Initialize.Initialize_COM;

   Put_Line ("Create Query Object");
   Create (Query, MSMQ.CLSID_MSMQQuery);

   Put_Line ("Lookup Queue");
   Attach (QueueInfos,
           LookupQueue (Query,
                        Label => QueueName'Unchecked_Access));

   Reset (QueueInfos);

   Attach (QueueInfo, Next (QueueInfos));

   Put_Line ("Open Queue");

   Attach (Queue,
           Open (QueueInfo,
                 uAccess   => MSMQ.MQ_SEND_ACCESS,
                 ShareMode => MSMQ.MQ_DENY_NONE));

   Put_Line ("Create Message");
   Create (Message, MSMQ.CLSID_MSMQMessage);

   Put_Delivery (Message, MSMQ.MQMSG_DELIVERY_EXPRESS);
   Put_Label (Message, To_BSTR ("Hello World!"));

   Send (Message, Pointer (Queue));

   Put_Line ("Done!");
   Free (QueueName);
end MSMQ_Client;
