-- 15-Dec-2008: GdM: Created (some code previously in GWin_Util).

package GWindows.Static_Controls.Web is

  --------------------------------------------------------------------------
  -- For URL's there is a "pointing finger" cursor, like in Web browsers.
  -- When the mouse pointer goes over the URL, the cursor becomes a hand
  -- with a pointing finger. If the system (older Windows, or some other
  -- cause) cannot find the standard hand, this URL code tries to find
  -- in the resource a cursor with the name "Finger_cursor".
  -- Here is a convenient finger.cur to associate with, uuencoded:
  --
  --  begin 600 finger.cur
  --  M```"``$`("````8````P`0``%@```"@````@````0`````$``0````````$`
  --  M````````````````````````````____````````````````````````````
  --  M`````````````````````````````````?X```'^```#_P```_\```?_```'
  --  M_X``#_^```O_@``;_X``._^``#/]@``#;8```VT```-L```#8````P````,`
  --  M```#`````P````,`````````____________________________________
  --  M__________________P`___\`/___`#___@`?__X`'__\`!___``/__@`#__
  --  MX``__\``/_^``#__@``__X@`/__X`'__^`#___@#___X'___^'____A____X
  --  +?___^'____S_____
  --  `
  --  end
  --------------------------------------------------------------------------

  -- This mimics GWindows.Static_Controls.Create_Label

  procedure Create_URL
    (Parent     : in out GWindows.Base.Base_Window_Type'Class;
     Text       : in     GString;
     URL        : in     GString; -- Address local or on the Internet
     Left       : in     Integer;
     Top        : in     Integer;
     Width      : in     Integer;
     Height     : in     Integer;
     Alignment  : in     Alignment_Type                       :=
       GWindows.Static_Controls.Left;
     ID         : in     Integer                              := 0;
     Show       : in     Boolean                              := True);
  --  Create URL Label with no variable

  type URL_Type is new GWindows.Static_Controls.Label_Type with
    record
      URL: GWindows.GString_Unbounded;
    end record;

  -- This mimics GWindows.Static_Controls.Create(Label_Type,...)
  procedure Create
    (Static     : in out URL_Type;
     Parent     : in out GWindows.Base.Base_Window_Type'Class;
     Text       : in     GString;
     URL        : in     GString; -- Address local or on the Internet
     Left       : in     Integer;
     Top        : in     Integer;
     Width      : in     Integer;
     Height     : in     Integer;
     Alignment  : in     Alignment_Type                       :=
       GWindows.Static_Controls.Left;
     ID         : in     Integer                              := 0;
     Show       : in     Boolean                              := True;
     Is_Dynamic : in     Boolean                              := False);

  -- Overriden methods:
  procedure On_Click ( Window: in out URL_Type );
  procedure On_Message (Window       : in out URL_Type;
                        message      : in     Interfaces.C.unsigned;
                        wParam       : in     Interfaces.C.int;
                        lParam       : in     Interfaces.C.int;
                        Return_Value : in out Interfaces.C.long);

  -- Swap a label against a new URL label, at the same place
  procedure Create_and_Swap
    (To_Show    : in out URL_Type;
     To_Hide    : in out Label_Type;
     Parent     : in out GWindows.Base.Base_Window_Type'Class;
     URL        : in     GString;
     Alignment  : in     Alignment_Type:= GWindows.Static_Controls.Left;
     Is_Dynamic : in     Boolean:= False
    );

end GWindows.Static_Controls.Web;