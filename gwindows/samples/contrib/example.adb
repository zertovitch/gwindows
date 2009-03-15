with Example_Pkg;

procedure Example is
      pragma Linker_Options ("example.coff");
--   pragma Linker_Options ("-mwindows");
begin
   Example_Pkg.Main;
end Example;
