with GWindows.Windows.Main;

package Tutorial4_Window is

   type My_Window_Type is
     new GWindows.Windows.Main.Main_Window_Type with private;
   type My_Window_Access is access all My_Window_Type;
   type Pointer_To_My_Window_Class is access all My_Window_Type'Class;

   procedure On_Close (Window    : in out My_Window_Type;
                       Can_Close :    out Boolean);

private
   type My_Window_Type is
     new GWindows.Windows.Main.Main_Window_Type with null record;
end Tutorial4_Window;
