--  Win32 portable types - Windows version.
--
--  On Windows, C 'unsigned long' and 'long' are 32-bit,
--  matching Win32 DWORD/ULONG and LONG.

with Interfaces.C;

package Win32_Types is
   subtype Unsigned_Long is Interfaces.C.unsigned_long;
   subtype Long          is Interfaces.C.long;
   subtype wchar_t       is Interfaces.C.wchar_t;
   subtype wchar_array   is Interfaces.C.wchar_array;
   wide_nul : constant wchar_t := Interfaces.C.wide_nul;
end Win32_Types;
