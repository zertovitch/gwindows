with Ada.Exceptions; use Ada.Exceptions;
with GWindows.Base;

package GWindows.Exceptions is

   --  Create a popup dialog and show the exception information in a read-only
   --  text box in this dialog. It is possible to copy paste the traceback in
   --  this exception information

   procedure Show_Exception (E     : Exception_Occurrence;
                             Title : GString);

   procedure Show_Exception
      (Parent : in out GWindows.Base.Base_Window_Type'Class;
       E      :        Exception_Occurrence;
       Title  :        GString);

   --  Show the exception, as above, and also terminate current program

   procedure Basic_Exception_Handler
      (Parent : in out GWindows.Base.Base_Window_Type'Class;
       E      :        Exception_Occurrence);

end GWindows.Exceptions;
