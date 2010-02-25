--

with GWindows.Base;                     use GWindows.Base;
with GWindows.Edit_Boxes;               use GWindows.Edit_Boxes;
with GWindows.List_Boxes;               use GWindows.List_Boxes;
with GWindows.Buttons;                  use GWindows.Buttons;
with GWindows.Common_Dialogs;           use GWindows.Common_Dialogs;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.Common_Controls;          use GWindows.Common_Controls;
with GWindows.Static_Controls;          use GWindows.Static_Controls;
with GWindows.Windows;                  use GWindows.Windows;
with GWindows.Message_Boxes;            use GWindows.Message_Boxes;
with GWindows.Application;              use GWindows.Application;

with GWindows.Static_Controls.Web;      use GWindows.Static_Controls.Web;

with GWenerator_Resource_GUI;           use GWenerator_Resource_GUI;

with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Directories;                   use Ada.Directories;
with Ada.Command_Line;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;          use Ada.Text_IO.Unbounded_IO;
with Ada.Calendar;

with RC_IO, RC_Help, YYParse, Resource_Header;

with GWens.IO;

with Time_display;

with Windows_Timers;

package body GWen_Windows is

  -- NB: for an eventual version independent of ANSI / UNICODE,
  -- only GString should be used.

  function S(Source: Unbounded_String) return String
    renames Ada.Strings.Unbounded.To_String;
  function U(Source: String) return Unbounded_String
    renames Ada.Strings.Unbounded.To_Unbounded_String;

  NL: constant String:= ASCII.CR & ASCII.LF;

  -------------------------
  -- Internal procedures --
  -------------------------

  procedure Update_status_display (Window : in out GWen_Window_Type) is
    margin_x      : constant:= 25;
    margin_x_frame: constant:= 18;
    margin_y      : constant:= 15;
    title: Unbounded_String:= Window.short_name;
  begin
    --
    -- Title
    --
    if Window.proj.modified then
      title:= title & " *";
    end if;
    Window.Text("GWenerator - " & S(title));
    --
    -- Check box and the detail part
    --
    if Window.proj.show_details then
      Window.Show_Details.State(Checked);
      Window.Client_Area_Height(Window.Details_frame.Top + Window.Details_frame.Height + margin_y);
      Window.More_less_details.Set_Bitmap(Window.less_details);
    else
      Window.Show_Details.State(Unchecked);
      Window.Client_Area_Height(Window.More_less_details.Top + Window.More_less_details.Height + 4);
      Window.More_less_details.Set_Bitmap(Window.more_details);
    end if;
    --
    -- RC main part
    --
    if Window.proj.RC_listen then
      Window.Ear_RC.Set_Bitmap(Window.ear);
    else
      Window.Ear_RC.Set_Bitmap(Window.no_ear);
    end if;
    if Window.RC_new then
      Window.Newer_RC.Show;
    else
      Window.Newer_RC.Hide;
    end if;
    --
    -- Ada main part
    --
    if Window.proj.show_ada_build then
      Window.Show_Ada_build.State(Checked);
      Window.Client_Area_Width(Window.Exe_file_icon.Left + Window.Exe_file_icon.Width + margin_x);
      Window.More_less_build.Set_Bitmap(Window.less_build);
      Window.GNATMake_messages.Show;
      Window.Ada_comp_label.Show;
      Window.Ada_blue_3.Show;
      if Window.proj.Ada_listen then
        Window.Ear_Ada.Set_Bitmap(Window.ear);
      else
        Window.Ear_Ada.Set_Bitmap(Window.no_ear);
      end if;
      if Window.Ada_new then
        Window.Newer_Ada.Show;
      else
        Window.Newer_Ada.Hide;
      end if;
      if Windows_pipes.Alive(Window.build_process) then
        Window.Button_Build_permanent.Text("Stop build");
      else
        Window.Button_Build_permanent.Text("Build now");
      end if;
      if Window.proj.Ada_auto_build and Window.last_build_failed then
        Window.Auto_build_lift_ico.Show;
        Window.Auto_build_lift_msg.Show;
      else
        Window.Auto_build_lift_ico.Hide;
        Window.Auto_build_lift_msg.Hide;
      end if;
    else
      Window.Show_Ada_build.State(Unchecked);
      Window.Client_Area_Width(Window.Ada_file_icon.Left + Window.Ada_file_icon.Width + margin_x);
      Window.More_less_build.Set_Bitmap(Window.more_build);
      Window.GNATMake_messages.Hide;
      Window.Ada_comp_label.Hide;
      Window.Ada_blue_3.Hide;
    end if;
    Window.Details_frame.Width(Window.Client_Area_Width - margin_x_frame);
    --
  end Update_status_display;

  procedure On_Details_Check_Box_Click (Window : in out GWindows.Base.Base_Window_Type'Class) is
    gw: GWen_Window_Type renames GWen_Window_Type(Parent(Window).all);
  begin
    gw.proj.show_details:= gw.Show_Details.State = Checked;
    Update_status_display(gw);
  end On_Details_Check_Box_Click;

  procedure On_Details_Button_Click (Window : in out GWindows.Base.Base_Window_Type'Class) is
    gw: GWen_Window_Type renames GWen_Window_Type(Parent(Window).all);
  begin
    gw.Show_Details.State(not gw.Show_Details.State);
    On_Details_Check_Box_Click(Window);
  end On_Details_Button_Click;

  procedure On_Build_Check_Box_Click (Window : in out GWindows.Base.Base_Window_Type'Class) is
    gw: GWen_Window_Type renames GWen_Window_Type(Parent(Window).all);
  begin
    gw.proj.show_ada_build:= gw.Show_Ada_build.State = Checked;
    Update_status_display(gw);
  end On_Build_Check_Box_Click;

  procedure On_Build_Button_Click (Window : in out GWindows.Base.Base_Window_Type'Class) is
    gw: GWen_Window_Type renames GWen_Window_Type(Parent(Window).all);
  begin
    gw.Show_Ada_Build.State(not gw.Show_Ada_Build.State);
    On_Build_Check_Box_Click(Window);
  end On_Build_Button_Click;

  --------------------------------------
  -- New methods for GWen_Window_Type --
  --------------------------------------

  procedure On_Options (Window : in out GWen_Window_Type) is
    dlg    : GWen_properties_Type;
    candidate: GWen:= Window.proj;

    procedure Update_base_units ( dummy : in out GWindows.Base.Base_Window_Type'Class ) is
      pragma Warnings(off, dummy);
      use_defaults_checked: constant Boolean:= dlg.Use_Base_defs.State = Checked;
    begin
      Enabled(dlg.Basx, not use_defaults_checked);
      Enabled(dlg.Basy, not use_defaults_checked);
    end Update_base_units;

    procedure Select_RC ( dummy : in out GWindows.Base.Base_Window_Type'Class ) is
      pragma Warnings(off, dummy);
      New_File_Name : GWindows.GString_Unbounded;
      File_Title    : GWindows.GString_Unbounded;
      Success: Boolean;
    begin
      Open_File (
        Window,
        "Choose Resource file...",
         New_File_Name,
         ((U("Resource compiler files (*.rc)"), U("*.rc" )),
          (U("All files (*.*)"),                U("*.*"))
         ),
         ".rc",
         File_Title,
         Success
      );
      if Success then
        dlg.Edit_RC_File_Name.Text(S(New_File_Name));
      end if;
    end Select_RC;

    procedure Select_Ada ( dummy : in out GWindows.Base.Base_Window_Type'Class ) is
      pragma Warnings(off, dummy);
      New_File_Name : GWindows.GString_Unbounded;
      File_Title    : GWindows.GString_Unbounded;
      Success: Boolean;
    begin
      Open_File (
        Window,
        "Choose Ada main unit file...",
         New_File_Name,
         ((U("Ada files (*.adb,*.ads)"), U("*.adb;*.ads" )),
          (U("Programs (*.exe)"),        U("*.exe" )),
          (U("All files (*.*)"),         U("*.*"))
         ),
         ".adb",
         File_Title,
         Success
      );
      if Success then
        dlg.Edit_Main_Ada_File_Name.Text(S(New_File_Name));
      end if;
    end Select_Ada;

    function Img(ch: GWens.RC_compiler_choice) return String is
    begin
      case ch is
        when none =>
          return "Never";
        when windres =>
          return "Call windres";
      end case;
    end Img;

    procedure Get_Data ( dummy : in out GWindows.Base.Base_Window_Type'Class ) is
      pragma Warnings(off, dummy);
    begin
      candidate.RC_Name       := U(dlg.Edit_RC_File_Name.Text);
      candidate.RC_listen     := dlg.Listen_RC.State = Checked;
      candidate.RC_auto_trans := dlg.Auto_translate.State = Checked;
      for ch in GWens.RC_compiler_choice loop
        if dlg.RC_Compiler_list.Text = Img(ch) then
          candidate.RC_compile:= ch;
        end if;
      end loop;
      --
      candidate.separate_items      := dlg.Separate_items.State = Checked;
      candidate.base_x              := Integer'Value(dlg.Basx.Text);
      candidate.base_y              := Integer'Value(dlg.Basy.Text);
      candidate.base_defaults       := dlg.Use_Base_defs.State = Checked;
      candidate.initialize_controls := dlg.Initialize_controls.State = Checked;
      --
      candidate.Ada_Main      := U(dlg.Edit_Main_Ada_File_Name.Text);
      candidate.Ada_listen    := dlg.Listen_Ada.State = Checked;
      candidate.Ada_auto_build:= dlg.Auto_build.State = Checked;
      --
      candidate.Ada_command   := U(dlg.Ada_cmd.Text);
    end Get_Data;

    modified: Boolean;
    Bool_to_Check: constant array(Boolean) of Check_State_Type:= (Unchecked, Checked);

  begin
    dlg.Create_Full_Dialog(Window);
    dlg.Small_Icon("Tools");
    --
    -- Display the non-closing buttons
    --
    dlg.Button_Browse_RC.Hide;
    dlg.Button_Browse_RC_permanent.Show;
    dlg.Button_Browse_Ada.Hide;
    dlg.Button_Browse_Ada_permanent.Show;
    --
    -- Fill dialog's contents
    --
    -- - RC box:
    dlg.Edit_RC_File_Name.Text(S(candidate.RC_name));
    dlg.Listen_RC.State(Bool_to_Check(candidate.RC_listen));
    dlg.Auto_translate.State(Bool_to_Check(candidate.RC_auto_trans));
    for ch in GWens.RC_compiler_choice loop
      dlg.RC_Compiler_list.Add(Img(ch));
    end loop;
    dlg.RC_Compiler_list.Text(Img(Window.proj.RC_compile));
    --
    -- - Code generation box:
    --
    dlg.Separate_items.State(Bool_to_Check(candidate.separate_items));
    dlg.Basx.Text(Integer'Image(candidate.base_x));
    dlg.Basy.Text(Integer'Image(candidate.base_y));
    dlg.Use_Base_defs.State(Bool_to_Check(candidate.base_defaults));
    dlg.Initialize_controls.State(Bool_to_Check(candidate.initialize_controls));
    --
    -- - Ada background compilation box:
    --
    dlg.Edit_Main_Ada_File_Name.Text(S(candidate.Ada_main));
    dlg.Listen_Ada.State(Bool_to_Check(candidate.Ada_listen));
    dlg.Auto_build.State(Bool_to_Check(candidate.Ada_auto_build));
    --
    dlg.Ada_cmd.Text(S(candidate.Ada_command));
    --
    dlg.Center;
    --
    On_Destroy_Handler (dlg, Get_Data'Unrestricted_Access);
    On_Click_Handler (dlg.Use_Base_defs, Update_base_units'Unrestricted_Access);
    On_Click_Handler (dlg.Button_Browse_RC_permanent, Select_RC'Unrestricted_Access);
    On_Click_Handler (dlg.Button_Browse_Ada_permanent, Select_Ada'Unrestricted_Access);
    Update_base_units(Window);
    --
    case GWindows.Application.Show_Dialog (dlg, Window) is
      when IDOK =>
        --
        modified:= Window.proj /= candidate;
        -- ^ True if any option has changed, False if no change.
        --
        if modified then
          Window.proj:= candidate;
          Window.proj.modified:= True;
          Update_status_display(Window);
        end if;
        -- Message_Box("Modified ?", Boolean'Image(modified));
      when others =>
        null; -- discard changes
    end case;
  end On_Options;

  procedure Process_unsaved_changes (Window : in out GWen_Window_Type; Success: out Boolean) is
  begin
    Success:= True;
    if Window.proj.modified then
      case Message_Box(
        Window,
        "GWen is changed",
        "Save modified GWen ?",
        Yes_No_Cancel_Box,
        Question_Icon
      )
      is
        when Yes    =>
          Window.last_save_success:= False;
          Window.On_Save;
          Success:= Window.last_save_success;
          -- False e.g. if "Save as..." of a new file is cancelled
          return;
        when No     =>
          null;
        when Cancel =>
          Success:= False;
        when others =>
          null;
      end case;
    end if;
  end Process_unsaved_changes;

  procedure On_New (Window : in out GWen_Window_Type) is
    fresh_gwen: GWen; -- initialized with defaults
    Success   : Boolean;
  begin
    Process_unsaved_changes(Window, Success);
    if Success then
      -- Create a new GWen now, with defaults...
      Window.proj:= fresh_gwen;
      Window.short_name:= Window.proj.name;
      Update_status_display (Window);
    end if;
  end On_New;

  procedure On_Open (Window : in out GWen_Window_Type) is
    New_File_Name : GWindows.GString_Unbounded;
    File_Title    : GWindows.GString_Unbounded;
    Success       : Boolean;
  begin
    Process_unsaved_changes(Window, Success);
    if Success then
      Open_File (
        Window,
        "Open...",
         New_File_Name,
         ((U("GWenerator project file (*.gwen)"), U("*.gwen" )),
          (U("All files (*.*)"),                  U("*.*"))
         ),
         ".gwen",
         File_Title,
         Success
      );
      if Success then
        GWens.IO.Load(
          file_name => S(New_File_Name),
          proj      => Window.proj,
          success   => Success
        );
        if Success then
          Window.short_name:= File_Title;
          Update_status_display (Window);
        else
          Message_Box(
            Window,
            "Error", S(New_File_Name) &
            " is not a GWen project file.",
            OK_Box,
            Error_Icon
          );
        end if;
      end if;
    end if;
  end On_Open;

  procedure On_Save (Window : in out GWen_Window_Type) is
  begin
    if Window.proj.titled then
      Window.last_save_success:= False;
      GWens.IO.Save(proj => Window.proj);
      Window.last_save_success:= True;
      Update_status_display (Window);
    else
      Window.On_Save_As;
    end if;
  end On_Save;

  procedure On_Save_As (Window : in out GWen_Window_Type) is
    New_File_Name : GWindows.GString_Unbounded;
    File_Title    : GWindows.GString_Unbounded;
    Success       : Boolean;
  begin
    Window.last_save_success:= False;
    Save_File (
      Window,
      "Save as...",
       New_File_Name,
       ((U("GWenerator project file (*.gwen)"), U("*.gwen" )),
        (U("All files (*.*)"),                  U("*.*"))
       ),
       ".gwen",
       File_Title,
       Success
    );
    if Success then
      if Ada.Directories.Exists(S(New_File_Name))
      and then
        Message_Box (Window,
                      "Save as",
                      S(New_File_Name) &
                      " exists" & NL &
                      "replace ?",
                      Yes_No_Box,
                      Exclamation_Icon) = No
      then
        return;
      end if;
      Window.Short_Name:= File_Title;
      Window.proj.name := New_File_Name;
      Window.proj.titled:= True;
      GWens.IO.Save (Window.proj);
      Window.last_save_success:= True;
      Update_status_display (Window);
    end if;
  end On_Save_As;

  procedure On_About (Window : in out GWen_Window_Type) is
    box: About_Box_Type;
    url: URL_Type;
  begin
    box.Create_Full_Dialog(Window);
    GWindows.Static_Controls.Web.Create_and_Swap(
      To_Show => url,
      To_Hide => box.URL,
      Parent  => box,
      URL     => box.URL.Text -- Here the text = the URL
    );
    -- Complete the Grammar version info:
    box.RC_gramm_ver.Text( box.RC_gramm_ver.Text & RC_Help.Grammar_Version );
    -- Complete the GWenerator version info:
    box.GWen_ver.Text( box.GWen_ver.Text & Version_info.FileVersion );
    box.Center;
    if Show_Dialog (box, Window) = IDOK then
      null;
    end if;
  end On_About;

  procedure Call_windres (gw: in out GWen_Window_Type) is
    sn: constant String:= S(gw.proj.RC_name);
    on: constant String:= sn(sn'First..sn'Last-1) & "bj";
    Command : constant String :=
      "windres -v " & sn & ' ' & on;
    procedure Output_a_line(l: String) is
    begin
      Add(gw.RC_to_GWindows_messages, l);
    end;
    p: Windows_pipes.Piped_process;
  begin
    Add(gw.RC_to_GWindows_messages, "");
    Add(gw.RC_to_GWindows_messages, "Compiling resource... " & Time_display);
    Add(gw.RC_to_GWindows_messages, Command);
    Windows_pipes.Start(p, Command, ".", Output_a_line'Unrestricted_Access);
    while Windows_pipes.Alive(p) loop
      Windows_pipes.Check_progress(p);
    end loop;
    Add(gw.RC_to_GWindows_messages, "Resource compiled. " & Time_display);
  end Call_windres;

  procedure Translation (gw: in out GWen_Window_Type; generate_test: Boolean) is
    sn: constant String:= S(gw.proj.RC_name);
    fe, fo: File_Type;
    -- We derout the standard output & error - anyway, there is no terminal!
  begin
    gw.Ear_RC.Set_Bitmap(gw.wheels);
    delay 0.01;
    gw.Bar_RC.Progress_Range(0, 100);
    gw.Bar_RC.Position(5);
    delay 0.01;
    if sn="" then
      Message_Box(gw, "Ressource file", "Resource file name is empty!", OK_Box, Error_Icon);
      On_Options(gw);
    elsif not Exists(sn) then
      Message_Box(gw, "Ressource file missing", "Cannot find: [" & sn & ']', OK_Box, Error_Icon);
      On_Options(gw);
    else
      gw.Bar_RC.Position(10);
      -- Copy the translation options to RC_Help's globals variables.
      -- These variables are used by the code generated into yyparse.adb from RC.y.
      RC_Help.Reset_globals;
      RC_Help.separate_items:= gw.proj.separate_items;
      RC_Help.generate_test:= generate_test;
      if not gw.proj.base_defaults then
        RC_Help.base_unit_x:= gw.proj.base_x;
        RC_Help.base_unit_y:= gw.proj.base_y;
      end if;
      RC_Help.initialize_controls:= gw.proj.initialize_controls;
      RC_Help.source_name:= gw.proj.RC_name;
      gw.Bar_RC.Position(15);
      --
      RC_IO.Open_Input(S(gw.proj.RC_name));
      Create(fe, Out_File, ""); -- temp file
      Create(fo, Out_File, ""); -- temp file
      declare
        se: constant String:= Name(fe); -- get name of temp file
        -- so: constant String:= Name(fo); -- get name of temp file
        line: Unbounded_String;
      begin
        Set_Error(fe);
        Set_Output(fo);
        Put_Line(Current_error, "GWenerator - RC to GWindows" );
        Put_Line(Current_error, "Transcripting '" & S(gw.proj.RC_name) & "'." );
        Put_Line(Current_error, "Time now: " & Time_display );
        RC_Help.has_input:= True;
        RC_Help.Ada_Begin;
        begin
          YYParse;
        exception
          when RC_Help.Syntax_Error |
               Resource_Header.Unexpected_Syntax |
               Resource_Header.No_Define         |
               Resource_Header.Illegal_Number    =>
            null; --!!
        end;
        RC_IO.Close_Input;
        Set_Error(Standard_Error);
        Set_Error(Standard_Output);
        Close(fe);
        Close(fo);
        -- Message_Box("Std Output", so);
        -- The file fo should be empty, but something
        -- somewhere puts a new line...
        gw.Bar_RC.Position(70);
        -- Output the rc2gw messages into the box
        Open(fe, In_File, se);
        Clear(gw.RC_to_GWindows_messages);
        while not End_of_File(fe) loop
          Get_Line(fe, line);
          Add(gw.RC_to_GWindows_messages, S(line));
        end loop;
        Close(fe);
        --
        -- Now, compile the resource.
        -- Since it is a quick task, we can wait for the
        -- process to complete here
        --
        case gw.proj.RC_compile is
          when none =>
            null;
          when windres =>
            Call_windres(gw);
        end case;
      end;
      gw.Bar_RC.Position(100);
      delay 0.02;
    end if;
    gw.Bar_RC.Position(0);
    -- Restore ear logos
    -- NB: no call to main updating proc since this can be
    -- called when window is in background
    if gw.proj.RC_listen then
      gw.Ear_RC.Set_Bitmap(gw.ear);
    else
      gw.Ear_RC.Set_Bitmap(gw.no_ear);
    end if;

  end Translation;

  procedure Do_Translate (Window : in out GWindows.Base.Base_Window_Type'Class) is
  begin
    Translation(GWen_Window_Type(Parent(Window).all), generate_test => False);
  end Do_Translate;

  -- Not nice (we reasonably suppose there is only *one* main window)
  type GWen_Window_Type_Access is access all GWen_Window_Type;
  the_main: GWen_Window_Type_Access;
  --
  procedure Output_build_line(l: String) is
    x,y: Integer;
  begin
    if l'Length >= 10 and then
      l(l'First..l'First+9)="completed " and then
      Index(l, "%)") > 0
    then
      x:= Index(l, "(") + 1;
      y:= x;
      while l(y+1) /= '%' loop
        y:= y + 1;
      end loop;
      the_main.Bar_Ada.Position(
        Integer'Max(
          the_main.Bar_Ada.Position,  -- avoid going back
          Integer'Value(l(x..y))
        )
      );
    else
      the_main.GNATMake_messages.Add(l);
    end if;
  end Output_build_line;

  procedure Do_Start_Stop_Build (Window : in out GWindows.Base.Base_Window_Type'Class) is
    use Windows_pipes;
    gw: GWen_Window_Type renames GWen_Window_Type(Parent(Window).all);
  begin
    if Alive(gw.build_process) then
      Stop(gw.build_process);
      gw.GNATMake_messages.Add("Stopped! ");
      gw.GNATMake_messages.Add("Time : " & Time_display);
      gw.last_build_failed:= True;
      gw.Bar_Ada.Position(0);
      gw.last_seen_running:= False;
    else
      Clear(gw.GNATMake_messages);
      gw.last_build_failed:= False;
      gw.Bar_Ada.Progress_Range(0, 100);
      the_main:= gw'Access;
      declare
        cmd: constant String:= Trim(S(gw.proj.Ada_command), Left);
      begin
        gw.GNATMake_messages.Add("Starting build... [" & cmd & "] ");
        gw.GNATMake_messages.Add("Time : " & Time_display);
        Start(gw.build_process, cmd, ".", Output_build_line'Access);
        gw.last_seen_running:= True;
      exception
        when Cannot_create_pipe =>
          Message_Box(
            Window,
            "Process error",
            "Cannot create pipe",
            OK_Box,
            Error_Icon
          );
        when Cannot_start =>
          Message_Box(
            Window,
            "Process error",
            "Cannot start process:" & NL &
            S(gw.proj.Ada_command),
            OK_Box,
            Error_Icon
          );
          On_Options(gw);
      end;
    end if;
    Update_status_display(gw);
  end Do_Start_Stop_Build;

  --------------------------------------------
  -- Overriden methods for GWen_Window_Type --
  --------------------------------------------

  procedure On_Pre_Create (Window    : in out GWen_Window_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned)
  is
    pragma Warnings (Off, Window);
    pragma Warnings (Off, dwExStyle);
    WS_BORDER     : constant:= 16#0080_0000#;
    WS_SYSMENU    : constant:= 16#0008_0000#; -- Get the [x] closing box
    WS_MINIMIZEBOX: constant:= 16#0002_0000#;
    custom_style  : constant:= WS_BORDER + WS_SYSMENU + WS_MINIMIZEBOX;
    -- essentially, we want a window the user cannot resize
  begin
    dwStyle:= custom_style;
  end On_Pre_Create;

  timer_id: constant:= 1;

  procedure On_Create (Window : in out GWen_Window_Type) is
  --  Handles setting up icons, menus, etc.
    use Ada.Command_Line, GWindows.Buttons.Graphic;
    success: Boolean;
  begin
    Window.ear.Load_Bitmap(Num_resource(Listen_32x32));
    Window.no_ear.Load_Bitmap(Num_resource(Not_Listen_32x32));
    Window.wheels.Load_Bitmap(Num_resource(Wheels_32x32));
    Window.more_details.Load_Bitmap(Num_resource(More_Vertical));
    Window.less_details.Load_Bitmap(Num_resource(Less_Vertical));
    Window.more_build.Load_Bitmap(Num_resource(More_Horizontal));
    Window.less_build.Load_Bitmap(Num_resource(Less_Horizontal));
    Small_Icon (Window, "AAA_Main_Icon");
    Large_Icon (Window, "AAA_Main_Icon");
    Window.Create_Contents(for_dialog => False);
    Window.menus.Create_Full_menu;
    Window.Menu(Window.menus.main);
    Window.Accelerator_Table('#' & Trim(Integer'Image(Main_Menu),Left));
    if Argument_count=0 then
      Window.On_New;
    else
      GWens.IO.Load(
        file_name => Argument(1),
        proj      => Window.proj,
        success   => success
      );
      if success then
        Window.short_name:= U(Simple_Name(Argument(1)));
      else
        Message_Box(
          Window,
          "Error", Simple_Name(Argument(1)) &
          " is not a GWen project file.",
          OK_Box,
          Error_Icon
        );
        Window.short_name:= Window.proj.name; -- "Untitled"
      end if;
    end if;
    Update_status_display(Window);
    Window.Center;
    On_Click_Handler( Window.Show_Details, On_Details_Check_Box_Click'Access );
    Window.More_less_details.Set_Bitmap(Window.more_details);
    On_Click_Handler( Window.More_less_details, On_Details_Button_Click'Access );
    On_Click_Handler( Window.Show_Ada_build, On_Build_Check_Box_Click'Access );
    Window.More_less_build.Set_Bitmap(Window.more_build);
    On_Click_Handler( Window.More_less_build, On_Build_Button_Click'Access );
    On_Click_Handler( Window.Button_Translate_permanent, Do_Translate'Access );
    On_Click_Handler( Window.Button_Build_permanent, Do_Start_Stop_Build'Access );
    Windows_Timers.Set_Timer(Window, timer_id, 1000);
    --
    Window.Visible;
    --
    Window.last_seen_running:= False;
    Window.last_build_failed:= False;
  end On_Create;

  procedure On_Destroy (Window : in out GWen_Window_Type) is
  -- Method taken from GWindows.Windows.Main
  begin
    GWindows.Application.End_Loop;
    Window_Type (Window).On_Destroy;
  end On_Destroy;

  function Is_RC_newer(proj: GWen) return Boolean is
    use RC_Help, Ada.Calendar;
    rn: constant String:= S(proj.RC_name);
    an: constant String:= RC_to_Package_name(rn,True,True) & ".ads";
  begin
    return
      proj.RC_listen and then
      rn /= "" and then
      Exists(rn) and then
      ( (an="" or else not Exists(an)) or else
        Modification_Time(rn) > Modification_Time(an)
      );
  end Is_RC_newer;

  function Is_Ada_newer(proj: GWen) return Boolean is
    use RC_Help, Ada.Calendar;
    en: constant String:= S(proj.Ada_main);
    rn: constant String:= S(proj.RC_name);
    an: constant String:= RC_to_Package_name(rn,True,True) & ".ads";
  begin
    if en = "" then
      return False;
    end if;
    return
      proj.Ada_listen and then
      an /= "" and then
      Exists(an) and then
      ( (en="" or else not Exists(en)) or else
        Modification_Time(an) > Modification_Time(en)
      );
  end Is_Ada_newer;

  busy_listening: Boolean:= False;

  procedure On_Message (Window       : in out GWen_Window_Type;
                        message      : in     Interfaces.C.unsigned;
                        wParam       : in     Interfaces.C.int;
                        lParam       : in     Interfaces.C.int;
                        Return_Value : in out Interfaces.C.long)
  is
    use Interfaces.C, Windows_pipes;

    procedure Update_RC_newer_flag_and_message is
    begin
      Window.RC_new:= Is_RC_newer(Window.proj);
      if Window.RC_new then
        Window.Newer_RC.Show;
      else
        Window.Newer_RC.Hide;
      end if;
      delay 0.02;
    end Update_RC_newer_flag_and_message;

    procedure Update_Ada_newer_flag_and_message is
    begin
      Window.Ada_new:= Is_Ada_newer(Window.proj);
      if Window.Ada_new then
        Window.Newer_Ada.Show;
      else
        Window.Newer_Ada.Hide;
      end if;
      delay 0.02;
    end Update_Ada_newer_flag_and_message;

    exit_code: Integer;

  begin
    if message = Windows_Timers.WM_TIMER then
      -- Window.RC_to_GWindows_messages.Add("tick!");
      if not busy_listening then
        -- Lock the listener in case the timer ticks again during what follows
        -- Without such a lock I guess it could mess something...
        busy_listening:= True;
        ---------------
        -- Listen RC --
        ---------------
        Update_RC_newer_flag_and_message;
        -- Translate if RC new and this automatism desired
        if Window.RC_new and Window.proj.RC_auto_trans then
          Do_Translate(Window.Button_Translate_permanent);
          Update_RC_newer_flag_and_message;
        end if;
        ----------------
        -- Listen Ada --
        ----------------
        Update_Ada_newer_flag_and_message;
        if Window.Ada_new and Window.proj.Ada_auto_build
          and Window.proj.show_ada_build
          and not Window.last_build_failed
          and not Alive(Window.build_process)
          -- ^avoid stopping a running build!
        then
          Do_Start_Stop_Build(Window.Button_Build_permanent);
        end if;
        -- In case there is new messages from a running Ada build,
        -- it is the occasion to empty the pipe
        if Alive(Window.build_process) then
          Check_progress(Window.build_process);
        end if;
        if Window.last_seen_running and not Alive(Window.build_process) then
          -- Process just died
          Window.last_seen_running:= False;
          exit_code:= Last_exit_code(Window.build_process);
          if exit_code = 0 then
            Window.GNATMake_messages.Add(
              "Completed (exit code:" & Integer'Image(exit_code) & "). "
            );
            Window.GNATMake_messages.Add("Time : " & Time_display);
            Window.last_build_failed:= False;
            -- But wait, sometimes the exit code is not sufficient!
            Update_Ada_newer_flag_and_message;
            if Window.Ada_new then -- Ada code still newer
              Window.last_build_failed:= True;
            end if;
          else
            Window.GNATMake_messages.Add(
              "Build failed with exit code" & Integer'Image(exit_code) & ". "
            );
            Window.GNATMake_messages.Add("Time : " & Time_display);
            Window.last_build_failed:= True;
          end if;
          Window.Bar_Ada.Position(0);
          Update_status_display(Window);
        end if;
        -- Unlock
        busy_listening:= False;
      end if;
    end if;
    -- Call parent method
    On_Message(
      Window_type(Window),
      message,
      wPAram,
      lPAram,
      Return_Value
    );
  end On_Message;

  procedure On_Menu_Select
    (Window : in out GWen_Window_Type;
     Item   : in     Integer)
  is
  begin
    case Item is
      when New_GWen =>
        Window.On_New;
      when Open_GWen =>
        Window.On_Open;
      when Save_GWen =>
        Window.On_Save;
      when Save_GWen_as =>
        Window.On_Save_As;
      when Quit =>
        Window.Close;
      when Generate_test_app =>
        Translation(Window, generate_test => True);
      when GWen_Options =>
        Window.On_Options;
      when GWenerator_Preferences =>
        Message_Box(Window, "Main Preferences", "Not yet implemented..." );
      when About =>
        Window.On_About;
      when others =>
        Message_Box(Window, "Unknown menu item", Integer'Image(Item));
    end case;
  end On_Menu_Select;

  procedure On_Close (Window : in out GWen_Window_Type;
                      Can_Close :    out Boolean)
  is
    Success: Boolean;
    use Windows_pipes;
  begin
    --
    -- 1/ Check running processes
    --
    if Alive(Window.build_process) then
      case Message_Box(
        Window,
        "Build process active",
        "The Ada build process is still active." & NL &
        "Stop it now ?",
        Yes_No_Box,
        Question_Icon
      )
      is
        when Yes    =>
          Stop(Window.build_process);
          Success:= True;
        when others =>
          Success:= False;
      end case;
    else
      Success:= True;
    end if;
    --
    -- 2/ Check unsaved changes
    --
    if Success then
      Process_unsaved_changes(Window, Success);
      if Success then
        Windows_Timers.Kill_Timer(Window, timer_id);
      end if;
    end if;
    Can_Close:= Success;
  end On_Close;

end GWen_Windows;
