

with Text_IO;

package body Rc_Error_Report is

    The_File : Text_io.File_Type;

procedure Initialize_User_Error_Report is
begin
  null;
end;

procedure Terminate_User_Error_Report is
begin
  null;
end;

procedure Report_Continuable_Error 
    (Line_Number : in Natural;
    Offset      : in Natural;
    Finish      : in Natural;
    Message     : in String;
    Error       : in Boolean)  is
begin
  null;
end;


    procedure Initialize_Output is
      begin
        Text_io.Create(The_File, Text_io.Out_File, "rc.lis");
        initialize_user_error_report;
      end Initialize_Output;

    procedure Finish_Output is
      begin
        Text_io.Close(The_File);
        terminate_user_error_report;
      end Finish_Output;

    procedure Put(S: in String) is
    begin
      Text_io.put(The_File, S);
    end Put;

    procedure Put(C: in Character) is
    begin
      Text_io.put(The_File, C);
    end Put;

    procedure Put_Line(S: in String) is
    begin
      Text_io.put_Line(The_File, S);
    end Put_Line;


end Rc_Error_Report;
