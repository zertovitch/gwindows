with GWindows.Base;

package GWindows.Open_File is

   procedure Open_File
     (Window            : in out GWindows.Base.Base_Window_Type'Class;
      Dialog_Title      : in     GString;
      File_Name         : in out GString_Unbounded;
      Success           :    out Boolean);

end GWindows.Open_File;