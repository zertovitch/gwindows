with Ada.Strings.Fixed;    use Ada.Strings.Fixed;
with GWindows.Application; use GWindows.Application;
with GWindows.Buttons;     use GWindows.Buttons;
with GWindows.Edit_Boxes;  use GWindows.Edit_Boxes;
with GWindows.GStrings;    use GWindows.GStrings;
with GWindows.Windows;     use GWindows.Windows;
with Interfaces.C;         use Interfaces.C;

package body GWindows.Exceptions is

   procedure On_Pre_Create
      (Window    : in out GWindows.Base.Base_Window_Type'Class;
       dwStyle   : in out Interfaces.C.unsigned;
       dwExStyle : in out Interfaces.C.unsigned) is
      WS_POPUP : constant := 16#80000000#;
      WS_CAPTION : constant := 16#c00000#;
      DS_MODALFRAME : constant := 16#80#;
      pragma Unreferenced (Window);
      pragma Unreferenced (dwExStyle);
   begin
      dwStyle := WS_POPUP or WS_CAPTION or DS_MODALFRAME;
   end On_Pre_Create;

   function Lf_To_Nl (S : String) return String is
      Pos : Integer;
   begin
      Pos := Index (S, ASCII.LF & "");
      if Pos = 0 then
         return S;
      else
         return S (S'First .. Pos - 1) & ASCII.CR & ASCII.LF &
         Lf_To_Nl (S (Pos + 1 .. S'Last));
      end if;
   end Lf_To_Nl;

   procedure Show_Exception
      (Parent : in out GWindows.Base.Base_Window_Type'Class;
       E      :        Exception_Occurrence;
       Title  :        GString) is
      Quit               : Boolean := False;
      Dialog             : Window_Type;
      Exception_Edit_Box : Multi_Line_Edit_Box_Type;
      Exit_Button        : Button_Type;
      pragma Warnings (Off, Quit);

      procedure Exit_Handler
         (Window : in out GWindows.Base.Base_Window_Type'Class) is
         pragma Unreferenced (Window);
      begin
         Quit := True;
         End_Loop;
      end Exit_Handler;

   begin
      On_Pre_Create_Handler (Dialog, On_Pre_Create'Access);
      Create_As_Dialog (Window => Dialog,
                        Parent => Parent,
                        Title  => Title,
                        Left   => 1,
                        Top    => 1,
                        Width  => 500,
                        Height => 200);
      Create
         (Button => Exit_Button,
          Parent => GWindows.Base.Base_Window_Type'Class (Dialog),
          Text   => "Exit",
          Left   => 0,
          Top    => 0,
          Width  => 495,
          Height => 20);
      On_Click_Handler (Exit_Button, Exit_Handler'Unrestricted_Access);
      Create
         (Edit              => Exception_Edit_Box,
          Parent            => GWindows.Base.Base_Window_Type'Class (Dialog),
          Text              =>
             To_GString_From_String (Lf_To_Nl (Exception_Information (E))),
          Left              => 0,
          Top               => 20,
          Width             => 495,
          Height            => 175,
          Horizontal_Scroll => True);
      Read_Only (Exception_Edit_Box);
      Center (Dialog, Parent);
      while not Quit loop
         Show_Modal (GWindows.Base.Base_Window_Type'Class (Dialog), Parent);
      end loop;
   end Show_Exception;

   procedure Show_Exception (E     : Exception_Occurrence;
                             Title : GString) is
      Quit               : Boolean := False;
      Dialog             : Window_Type;
      Exception_Edit_Box : Multi_Line_Edit_Box_Type;
      Exit_Button        : Button_Type;
      pragma Warnings (Off, Quit);

      procedure Exit_Handler
         (Window : in out GWindows.Base.Base_Window_Type'Class) is
         pragma Unreferenced (Window);
      begin
         Quit := True;
         End_Loop;
      end Exit_Handler;

   begin
      On_Pre_Create_Handler (Dialog, On_Pre_Create'Access);
      Create_As_Dialog (Window => Dialog,
                        Title  => Title,
                        Left   => 1,
                        Top    => 1,
                        Width  => 500,
                        Height => 200);
      Create
         (Button => Exit_Button,
          Parent => GWindows.Base.Base_Window_Type'Class (Dialog),
          Text   => "Exit",
          Left   => 0,
          Top    => 0,
          Width  => 495,
          Height => 20);
      On_Click_Handler (Exit_Button, Exit_Handler'Unrestricted_Access);
      Create
         (Edit              => Exception_Edit_Box,
          Parent            => GWindows.Base.Base_Window_Type'Class (Dialog),
          Text              =>
             To_GString_From_String (Lf_To_Nl (Exception_Information (E))),
          Left              => 0,
          Top               => 20,
          Width             => 495,
          Height            => 175,
          Horizontal_Scroll => True);
      Read_Only (Exception_Edit_Box);
      while not Quit loop
         Show_Modal (GWindows.Base.Base_Window_Type'Class (Dialog));
      end loop;
   end Show_Exception;

   procedure Basic_Exception_Handler
      (Parent : in out GWindows.Base.Base_Window_Type'Class;
       E      :        Exception_Occurrence) is
      --  http://msdn.microsoft.com/en-us/library/ms682658(VS.85).aspx
      procedure ExitProcess (uExitCode : Integer := 1);
      pragma Import (Stdcall, ExitProcess, "ExitProcess");
   begin
      Show_Exception (Parent, E, "Exception");
      ExitProcess;
   end Basic_Exception_Handler;

end GWindows.Exceptions;
