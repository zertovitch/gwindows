with GWindows.Message_Boxes; use GWindows.Message_Boxes;
with GWindows.GStrings;

package body Tutorial23_Handle_Events is

   ----------------------
   -- DocumentComplete --
   ----------------------

   procedure DocumentComplete
     (This  : Handle_Event_Type;
      pDisp : GNATCOM.Types.VARIANT;
      URL   : GNATCOM.Types.VARIANT)
   is
   begin
      Message_Box ("Tutorial23",
                   GWindows.GStrings.To_GString_From_VARIANT
                     (URL, Clear => false) &
                     " - Document has been loaded.");
      --  Do not clear or free parameters passed in from events
   end DocumentComplete;

end Tutorial23_Handle_Events;

