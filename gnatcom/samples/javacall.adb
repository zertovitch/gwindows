with Ada.Exceptions;

with GNATCOM.Initialize; use GNATCOM.Initialize;
with GNATCOM.Dispinterface; use GNATCOM.Dispinterface;
with GNATCOM.VARIANT; use GNATCOM.VARIANT;
with GNAT.IO; use GNAT.IO;

procedure JavaCall is
   IDispatch : Dispinterface_Type;
begin
   Initialize_COM;

   --  You can access any java class that runs on MS JVM
   --  You can also specify a file name such as:
   --      "java:MyStuff.Class"
   Put_Line ("Create Dispatch Object using Java Moniker");
   CreateFromMoniker (IDispatch, "java:java.util.Date");

   --  All public members of the calls are available through Invokes
   Put_Line ("Get Date and Time from java.util.Date");
   Put_Line (To_Ada (Invoke (IDispatch, "toString")));

exception
   when E : others =>
      Put_Line (Ada.Exceptions.Exception_Name (E));
      Put_Line (Ada.Exceptions.Exception_Message (E));
end JavaCall;

