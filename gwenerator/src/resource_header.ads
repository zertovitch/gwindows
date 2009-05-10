-- Taken from AdaGIDE.
-- Author: Alfred Hilscher.

package Resource_Header is

  procedure Convert_Header_File (Name : String; Done : out Boolean);
  -- This procedure converts a resource header file (containing only #defines for resource ids)
  -- into an equivalent Ada spec file

end Resource_Header;