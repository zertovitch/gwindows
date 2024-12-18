with Ada.Text_IO;
with GNAVI_Project;
with GNAVI_XML;

procedure Test_Load_Project is

  procedure Test_1 is
    p : GNAVI_Project.GNAVI_Project_Type;
  begin
    GNAVI_Project.Load_Project (p, "gnavi_ide.gnp");  --  Load a supposedly valid project.
  end Test_1;

  procedure Test_2 is
    p : GNAVI_Project.GNAVI_Project_Type;
  begin
    GNAVI_Project.Load_Project (p, "gnavi_ide.adb");  --  Load an invalid project file.
    raise Program_Error with "There should be an error here.";
  exception
     when GNAVI_Project.Invalid_Project =>
       null;  --  Expected
  end Test_2;

  procedure Test_3 is
    p : GNAVI_Project.GNAVI_Project_Type;
  begin
    GNAVI_Project.Load_Project (p, "gnavi_new_project.gnw");  --  Load an invalid project file, but a valid XML file.
    raise Program_Error with "There should be an error here.";
  exception
    when GNAVI_Project.Invalid_Project =>
      null;  --  Expected
  end Test_3;

begin
  Test_1;
  Test_2;
  Test_3;
  Ada.Text_IO.Put_Line ("All right");
end Test_Load_Project;
