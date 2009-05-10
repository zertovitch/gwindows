package Rc_Error_Report is

    Syntax_Error : Exception;
    Syntax_Warning : Exception;
    Total_Errors : Natural := 0;   -- number of syntax errors found.
    Total_Warnings : Natural := 0; -- number of syntax warnings found.
        
    procedure Report_Continuable_Error(Line_Number : in Natural;
                                       Offset      : in Natural;
                                       Finish      : in Natural;
                                       Message     : in String;
                                       Error       : in Boolean);

    procedure Initialize_Output;

    procedure Finish_Output;

    procedure Put(S: in String);

    procedure Put(C: in Character);

    procedure Put_Line(S: in String);

end Rc_Error_Report;
