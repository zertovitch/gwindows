package body Win32_Types is

   function To_C
     (Item       : Wide_String;
      Append_Nul : Boolean := True) return wchar_array
   is
   begin
      return Interfaces.C.To_C (Item, Append_Nul);
   end To_C;

   function To_Native_Path (S : String) return String is
      R : String := S;
   begin
      for I in R'Range loop
         if R (I) = '\' then
            R (I) := '/';
         end if;
      end loop;
      if R'Length >= 3 and then R (R'First + 1) = ':' then
         return R (R'First + 2 .. R'Last);
      end if;
      return R;
   end To_Native_Path;

end Win32_Types;
