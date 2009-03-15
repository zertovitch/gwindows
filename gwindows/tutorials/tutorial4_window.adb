with GWindows.Message_Boxes;

package body Tutorial4_Window is

   --------------
   -- On_Close --
   --------------

   procedure On_Close
     (Window    : in out My_Window_Type;
      Can_Close :    out Boolean)
   is
      use GWindows.Message_Boxes;
   begin
      Can_Close := Message_Box (Window, "Tutorial4", "Ok to close?",
                                Yes_No_Box, Question_Icon) =  Yes;
   end On_Close;

end Tutorial4_Window;
