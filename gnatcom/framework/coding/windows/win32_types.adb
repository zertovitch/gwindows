package body Win32_Types is

   function To_C
     (Item       : Wide_String;
      Append_Nul : Boolean := True) return wchar_array
   is
   begin
      return Interfaces.C.To_C (Item, Append_Nul);
   end To_C;

   function To_Native_Path (S : String) return String is
   begin
      return S;
   end To_Native_Path;

   function To_Native_Path (S : Wide_String) return Wide_String is
   begin
      return S;
   end To_Native_Path;

end Win32_Types;
