--  Win32 portable types - Winelib version.
--
--  On Linux x86_64, C 'unsigned long' is 64-bit, but Win32
--  DWORD/ULONG and LONG are always 32-bit.  Use explicit
--  32-bit types for correct ABI with Winelib.
--
--  C 'wchar_t' is 32-bit on Linux but Win32 WCHAR is always
--  16-bit.  Use char16_t which is 16-bit on all platforms.

with Interfaces.C;

package Win32_Types is
   subtype Unsigned_Long is Interfaces.Unsigned_32;
   subtype Long          is Interfaces.Integer_32;
   subtype wchar_t       is Interfaces.C.char16_t;
   subtype wchar_array   is Interfaces.C.char16_array;
   wide_nul : constant wchar_t := Interfaces.C.char16_nul;

   function To_C
     (Item       : Wide_String;
      Append_Nul : Boolean := True) return wchar_array;

   function To_Native_Path (S : String) return String;
   function To_Native_Path (S : Wide_String) return Wide_String;

   type Platform_Type is (Windows, Winelib);
   Platform : constant Platform_Type := Winelib;

end Win32_Types;
