-- with GWindows.Message_Boxes;            use GWindows.Message_Boxes;

package body GWindows.Simple_Sheet is

  ------------
  -- Create --
  ------------

  procedure Create (
        Sheet          : in out Simple_Sheet_Type;
        Parent         : in out GWindows.Base.Base_Window_Type'Class; -- GdM 2011 fix: parent was first !
        Left           : in     Integer                              := GWindows.Constants.Use_Default;
        Top            : in     Integer                              := GWindows.Constants.Use_Default;
        Width          : in     Integer                              := GWindows.Constants.Use_Default;
        Height         : in     Integer                              := GWindows.Constants.Use_Default;
        Cclass         : in     Gstring                              := "";
        Rows           : in     Positive;
        Columns        : in     Positive;
        Column_Borders : in     Boolean                              := False;
        Row_Borders    : in     Boolean                              := True                            ) is

    Column_Width : constant Integer := Width / Columns;
    Row_Height   : constant Integer := Height / Rows; -- GdM 2011 fix: was Columns !

  begin
    --
    -- Create a base window as a container to hold the
    -- sheet
    --
    Create_As_Control(
      Window     => Sheet,
      Parent     => Parent,
      Left       => Left,
      Top        => Top,
      Width      => Width,
      Height     => Height,
      Is_Dynamic => False);

    --
    -- Allocate an array of columns
    --
    Sheet.Sheet_Array := new Sheet_Array_Type(1 .. Columns);

    for Column in Sheet.Sheet_Array'range loop
      --
      -- Allocate an array for the rows in this column
      --
      Sheet.Sheet_Array(Column).Rows_In_Column := new
        Rows_In_The_Column_Type(1 .. Rows);

      --
      -- Create the container that is this column
      --

      GWindows.Panels.Create_As_Control(
        Window     => Sheet.Sheet_Array (Column).Column_Window,
        Parent     => Sheet.Panel,
        Top        => 0,
        Left       => (Column - 1) * Column_Width,
        Width      => Column_Width,
        Height     => Row_Height * Rows,
        Is_Dynamic => False);

      if Column_Borders then

        --        GWindows.Windows.Border(
        --          Window => Sheet.Sheet_Array (Column).Column_Window,
        --          State  => True);

        GWindows.Panels.Border_Type
          (
          Panel  => Sheet.Sheet_Array (Column).Column_Window,
          Border => GWindows.Panels.Raised);



      else
        GWindows.Panels.Border_Type
          (
          Panel  => Sheet.Sheet_Array (Column).Column_Window,
          Border => GWindows.Panels.None);

      end if;

      --
      -- Create the row containers inside this column
      --
      for Row in Sheet.Sheet_Array(Column).Rows_In_Column'range loop
        GWindows.Panels.Create_As_Control(
          Window     => Sheet.Sheet_Array (Column).Rows_In_Column (Row).panel,
          Parent     => Sheet.Sheet_Array (Column).Column_Window,
          Top        => (Row - 1) * Row_Height,
          Left       => 0,
          Width      => Column_Width,
          Height     => Row_Height,
          Is_Dynamic => False);

        if Row_Borders then

          GWindows.Panels.Border_Type
            (
            Panel  => Sheet.Sheet_Array (Column).Rows_In_Column (Row).panel,
            Border => GWindows.Panels.Raised);

        end if;
      end loop;
    end loop;


    Panel_Size(
      Window => Sheet,
      Width  => Columns * Column_Width,
      Height => Rows * Row_Height);

  end Create;

  procedure On_Destroy (Sheet : in out Simple_Sheet_Type) is
  begin
    for Column in Sheet.Sheet_Array'range loop
      for Row in Sheet.Sheet_Array(Column).Rows_In_Column'range loop
        declare
          cell: Cell_type renames Sheet.Sheet_Array(Column).Rows_In_Column(Row);
        begin
          if cell.edit_created then
            cell.edit.Close;
          end if;
          cell.panel.Close;
        end;
      end loop;
      Sheet.Sheet_Array (Column).Column_Window.Close;
    end loop;
    -- message_box("Ciao", "Ciao");
  end On_Destroy;


  procedure Set_Cell_Text (
        Sheet     : in out Simple_Sheet_Type;
        Row       : in     Positive;
        Column    : in     Positive;
        Text      : in     GWindows.GString;
        Read_Only : in     Boolean:= False ) is
    cell: Cell_type renames Sheet.Sheet_Array(Column).Rows_In_Column(Row);
  begin
    if not cell.edit_created then
      GWindows.Edit_Boxes.Create
       (Edit   => cell.edit,
        Parent => cell.panel,
        Text   => Text,
        Left   => 0,
        Top    => 0,
        Width  => GWindows.Panels.Client_Area_Width (cell.panel),
        Height => GWindows.Panels.Client_Area_Height (cell.panel),
        Horizontal_Scroll => False);
      cell.edit_created:= True;
      GWindows.Edit_Boxes.Read_Only(cell.edit, Read_Only);
      if Read_Only then
        GWindows.Edit_Boxes.Disable(cell.edit);
      end if;
    end if;

    GWindows.Edit_Boxes.Read_Only(Window => Sheet.Sheet_Array (Column).Rows_In_Column (Row).edit,
      State   => False);

  end Set_Cell_Text;

  function Get_Cell_Text (
        Sheet  : in     Simple_Sheet_Type;
        Row    : in     Positive;
        Column : in     Positive           )
    return GWindows.Gstring is
    --
    cell: Cell_type renames Sheet.Sheet_Array(Column).Rows_In_Column(Row);
  begin
    -- return GWindows.Base.Text(Window => Get_Cell(Sheet => Sheet, Row => Row , Column => Column).all);
     if cell.edit_created then
       return GWindows.Edit_Boxes.Text(cell.edit);
     else
       return "";
     end if;
  end Get_Cell_Text;


  --
  -- Returns the child widget that lives within the cell
  --
  function Get_Item_Within_Cell (
        Sheet  : in     Simple_Sheet_Type;
        Row    : in     Positive;
        Column : in     Positive           )
    return GWindows.Base.Pointer_To_Base_Window_Class is

  begin
    return Get_Cell(Sheet => Sheet, Row => Row , Column => Column);
  end Get_Item_Within_Cell;






  function Get_Cell (
        Sheet  : in     Simple_Sheet_Type;
        Row    : in     Positive;
        Column : in     Positive           )
    return GWindows.Base.Pointer_To_Base_Window_Class is
    --
    -- This is totally wrong.. Somehow we have to keep cells from having multiple
    -- children.. (I think?)
  begin
    return Sheet.Sheet_Array (Column).Rows_In_Column (Row).panel'access;
  end Get_Cell;


  ----------------------
  -- Set_Column_Width --
  ----------------------

  procedure Set_Column_Width (
        Sheet        : in out Simple_Sheet_Type;
        Column_Width : in     Integer            ) is
  begin

    --    f
    --    Size(
    --      Window => Sheet.Sheet_Array (Column).Column_Window,
    --      Width  => Column_Width,
    --      Height => Height (Sheet.Sheet_Array (Column).Column_Window));
    null;
  end Set_Column_Width;

  --------------------
  -- Set_Row_Height --
  --------------------

  procedure Set_Row_Height (
        Sheet        : in out Simple_Sheet_Type;
        Row_Height   : in     Integer            ) is
  begin
    null;
  end Set_Row_Height;

end GWindows.Simple_Sheet;

