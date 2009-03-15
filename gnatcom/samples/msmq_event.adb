with GNAT.IO; use GNAT.IO;

with GNATCOM.Utility;
with GNATCOM.VARIANT; use GNATCOM.VARIANT;
with GNATCOM.BSTR; use GNATCOM.BSTR;

with MSMQ.IMSMQQueue_Interface; use MSMQ.IMSMQQueue_Interface;
with MSMQ.IMSMQMessage_Interface; use MSMQ.IMSMQMessage_Interface;
with MSMQ.IMSMQEvent_Interface; use MSMQ.IMSMQEvent_Interface;

package body MSMQ_Event is

   -------------
   -- Arrived --
   -------------

   procedure Arrived
     (This   : MSMQ_Event_Type;
      Queue  : GNATCOM.Types.VARIANT;
      Cursor : GNATCOM.Types.VARIANT)
   is
      Object  : IMSMQQueue_Type;
      T       : aliased GNATCOM.Types.VARIANT := To_VARIANT (True);
      Message : IMSMQMessage_Type;
   begin
      Put_Line ("Arrived!");
      Attach (Object, Queue);

      Attach (Message,
              Receive(Object,
                      WantBody => T'Access,
                      ReceiveTimeout => To_Variant(5000)'Unrestricted_Access));

      Put_Line("Get message properties");
      Put_Line("Label : " & To_Ada (Get_Label(Message)));

      AddRef (Object);  --  Prevent auto destruction of Queue object

      EnableNotification (Object,
                          Event          =>
                            Pointer (MSMQ_Events),
                          ReceiveTimeout => Time_Out'Access);
   end Arrived;

   ------------------
   -- ArrivedError --
   ------------------

   procedure ArrivedError
     (This      : MSMQ_Event_Type;
      Queue     : GNATCOM.Types.VARIANT;
      ErrorCode : GNATCOM.Types.VARIANT;
      Cursor    : GNATCOM.Types.VARIANT)
   is
   begin
      Put_Line ("ArrivedError!");
      GNATCOM.Utility.Post_Quit;
   end ArrivedError;

end MSMQ_Event;

