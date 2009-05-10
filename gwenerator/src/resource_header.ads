-- Taken from AdaGIDE.
-- Author: Alfred Hilscher.

package Resource_Header is

  procedure Convert_Header_File (Name : String; Done : out Boolean);
  -- This procedure converts a resource header file (containing only #defines for resource ids)
  -- into an equivalent Ada spec file


  Unexpected_Syntax : exception;
  No_Define         : exception;
  Illegal_Number    : exception;

end Resource_Header;