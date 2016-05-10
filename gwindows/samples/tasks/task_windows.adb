--  Windows can only be created in a thread with a message loop
--  So either you need to create all windows in the same task that ends in
--  message loop, or each task must have its own message loop

--  Windows can be manipulated once created from any task

--  Windows with ActiveX controls MUST be created in the main task, so neither
--  of these methods will work for those types of windows

with GWindows.Windows; use GWindows.Windows;
with GWindows.Windows.Main; use GWindows.Windows.Main;
with GWindows.Application;
--  with GWindows.Events;
with GWindows.GStrings; use GWindows.GStrings;
with GWindows.Message_Boxes;

with GNAT.OS_Lib;

procedure Task_Windows is
   --  METHOD 1: Each task with own message loop

   task type Window_Task is
      entry Title (Name : GWindows.GString);
   end Window_Task;

   task body Window_Task is
      Window  : Main_Window_Type;
   begin
      Create (Window, Width => 50, Height => 50);

      accept Title (Name : GWindows.GString) do
         Text (Window, Name);
         Show (Window);
      end Title;

      GWindows.Application.Message_Loop;
   end Window_Task;

   --  Method 2: Each window is created in a single "Window Factory" task
   --            There are better ways to implement the Window Factory,
   --            This is just one way

   task Window_Factory_Task is
      entry Create (Window : Pointer_To_Window_Class);
      entry Create_Complete;
   end Window_Factory_Task;

   task body Window_Factory_Task is
      Current_Window : Pointer_To_Window_Class;
   begin
      loop
         select
            accept Create (Window : Pointer_To_Window_Class) do
               Current_Window := Window;
            end Create;

            GWindows.Windows.Create (Current_Window.all,
                                     Is_Dynamic => True);

            accept Create_Complete;
         or
            delay 0.001;
            GWindows.Application.Message_Check;
         end select;
      end loop;
   end Window_Factory_Task;

   task type Window_Builder_Task is
      entry Title (Name : GWindows.GString);
   end Window_Builder_Task;

   task body Window_Builder_Task is
      Window : constant Window_Access := new Window_Type;
   begin
      accept Title (Name : GWindows.GString) do
         Window_Factory_Task.Create (Pointer_To_Window_Class (Window));
         Window_Factory_Task.Create_Complete;
         Text (Window.all, Name);
         Size (Window.all, 50, 50);
         Show (Window.all);
      end Title;
   end Window_Builder_Task;

   Some_Windows : array (1 .. 4) of Window_Task;
   More_Windows : array (1 .. 4) of Window_Builder_Task;

begin
   for N in Some_Windows'Range loop
      Some_Windows(N).Title (To_GString_From_String (N'Img));
   end loop;

   for N in Some_Windows'Range loop
      More_Windows(N).Title (To_GString_From_String ("F -" & N'Img));
   end loop;

   GWindows.Message_Boxes.Message_Box
     ("Task_Windows", "Click to Close Application");

  GNAT.OS_Lib.OS_Exit (0);
end Task_Windows;
