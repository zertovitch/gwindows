with GNAT.IO; use GNAT.IO;

with GNATCOM.Initialize;

procedure Initialize_Test is

   pragma Linker_Options ("-lole32");

begin
   Put_Line ("-- Start Initialize Test");
   New_Line;

   Put_Line ("Test   : Start COM environment Single Threaded");
   GNATCOM.Initialize.Initialize_COM;
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test   : Attempt to do Multi-Threaded init after");
   Put_Line ("         Single-Threaded Initialized");
   Put_Line ("Expect : Raise of CHANGED_MODE_ERROR");
   begin
      GNATCOM.Initialize.Initialize_COM_Multi_Threaded;
      Put_Line ("** FAIL : Should raise CHANGED_MODE_ERROR");
   exception
      when GNATCOM.Initialize.CHANGED_MODE_ERROR =>
         Put_Line ("++ PASS");
         New_Line;
   end;

   Put_Line ("Test   : Close COM environment Single Threaded");
   GNATCOM.Initialize.Uninitialize_COM;
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test   : Start COM environment Multi Threaded");
   GNATCOM.Initialize.Initialize_COM_Multi_Threaded;
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("Test   : Attempt to do Single-Threaded init after");
   Put_Line ("         Multi-Threaded Initialized");
   Put_Line ("Expect : Raise of CHANGED_MODE_ERRORx");
   begin
      GNATCOM.Initialize.Initialize_COM;
      Put_Line ("** FAIL : Should raise CHANGED_MODE_ERROR");
   exception
      when GNATCOM.Initialize.CHANGED_MODE_ERROR =>
         Put_Line ("++ PASS");
         New_Line;
   end;

   Put_Line ("Test   : Close COM environment Multi Threaded");
   GNATCOM.Initialize.Uninitialize_COM;
   Put_Line ("++ PASS");
   New_Line;

   Put_Line ("-- Completed Initialize Test");
   New_Line;

end Initialize_Test;

