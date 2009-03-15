-------------------------------------------------------------------------------
--  PACKAGE BODY: Pipe_Commands
--
--  PURPOSE: Implementation of a thick Ada binding for calling the popen and
--           pclose commands imported from C.
-------------------------------------------------------------------------------

with Interfaces.C; use Interfaces.C;
with Ada.Characters.Latin_1;

package body Pipe_Commands is

   LF : constant Integer := Character'Pos (Ada.Characters.Latin_1.LF);
   --  Unix end of line

   -----------------------------------------------------------------------
   --  INTERNAL FUNCTION: Popen
   --
   --  PURPOSE: Thin binding to the C "popen" command, used by the Execute
   --           function.
   -----------------------------------------------------------------------

   function popen (Command : char_array; Mode : char_array) return Files;
   pragma Import (C, popen, "_popen");

   -----------------------------------------------------------------------
   --  INTERNAL FUNCTION: pclose
   --
   --  PURPOSE: Thin binding to the C "pclose" command, used by the Close
   --           procedure.
   -----------------------------------------------------------------------

   function pclose (FileStream : Files) return Integer;
   pragma Import (C, pclose, "_pclose");

   -----------------------------------------------------------------------
   --  INTERNAL FUNCTION: feof
   --
   --  PURPOSE: Thin binding to the C "feof" command
   -----------------------------------------------------------------------

   function feof (FileStream : Files) return Integer;
   pragma Import (C, feof);

   -----------------------------------------------------------------------
   --  INTERNAL FUNCTION: fgetc
   --
   --  PURPOSE: Thin binding to the C "fgetc" function, used by Get_Next
   --           function
   -----------------------------------------------------------------------

   function fgetc (C_Stream : in Files) return Integer;
   pragma Import (C, fgetc);

   -----------------------------------------------------------------------
   --  INTERNAL FUNCTION: fputc
   --
   --  PURPOSE: Thin binding to the C "fput" function, used by Write_Next
   --           function
   -----------------------------------------------------------------------

   function fputc (C : Integer; stream : Files) return Integer;
   pragma Import (C, fputc);

   -----------------------------------------------------------------------
   --  FUNCTION: Execute
   --
   --  PURPOSE: This command executes the process indicated in the Command
   --           parameter, setting I/O according to the IO_Type parameter.
   --
   --  RETURN VALUE: The stream corresponding to the opened pipe, including
   --                the C file pointer and the mode for which the pipe was
   --                opened.
   --  EXCEPTIONS RAISED: None
   -----------------------------------------------------------------------

   function Execute (Command : in string; IO_type : in IO_MODE)
     return stream
   is
      use type System.Address;

      Result : stream;
   begin
      case IO_type is
         when read_file  =>
            Result.FileStream := popen (To_C (Command), To_C ("r"));
         when write_file =>
            Result.FileStream := popen (To_C (Command), To_C ("w"));
      end case;

      if Result.FileStream = System.Null_Address then
         raise Open_Error;
      end if;

      Result.Mode := IO_type;
      return Result;
   end Execute;

   -----------------------------------------------------------------------
   --  FUNCTION: Read_Next
   --
   --  PURPOSE: Reads the next line from the stream indicated by the parameter
   --           FromFile, returning an unbounded string.
   --  RETURN VALUE: An unbounded string containing the line read from the
   --                stream.
   --
   --  EXCEPTIONS RAISED:
   --   Access_Error => when the stream was opened with write_file mode
   --   End_Of_File  => when the pipe is closed (the program indicated
   --                   by the parameter FromFile terminates).
   -----------------------------------------------------------------------

   function Read_Next (FromFile : in stream)
     return Unbounded_String is
      Result : Unbounded_String := Null_Unbounded_String;
      char_buf : Integer;
   begin
      if FromFile.Mode = write_file then
         raise Access_Error;
      end if;

      --------------------------------------------------------------------
      --  Read characters one at a time until a line feed character is
      --  encountered, indicating an end of line. The line feed character
      --  is NOT included in the returned unbounded string.
      --------------------------------------------------------------------

      loop
         char_buf := fgetc (FromFile.FileStream);

         if feof (FromFile.FileStream) /= 0 then
            raise End_Of_File;
         end if;

         exit when char_buf = LF;

         Result := Result & character'Val (char_buf);
      end loop;

      return Result;
   end Read_Next;

   -----------------------------------------------------------------------
   --  PROCEDURE: Write_Next
   --
   --  PURPOSE: Write a line of input to the stream indicated by the
   --           parameter ToFile.
   --
   --  EXCEPTIONS RAISED:
   --     Access_Error => when the stream was opened with mode Read_File
   -----------------------------------------------------------------------

   procedure Write_Next (ToFile : in stream; Message : in string) is
      rc : Integer;
   begin
      if ToFile.Mode = read_file then
         raise Access_Error;
      end if;

      for I in Message'Range loop
         rc := fputc (character'Pos (Message (I)), ToFile.FileStream);
      end loop;

      rc := fputc (LF, ToFile.FileStream); -- add end of line
   end Write_Next;

   -----------------------------------------------------------------------
   --  PROCEDURE: Close
   --
   --  PURPOSE: Close the stream to the parameter OpenFile
   --
   --  EXCEPTIONS RAISED: None
   -----------------------------------------------------------------------
   procedure Close (OpenFile : in stream) is
      rc : Integer;
   begin
      rc := pclose (OpenFile.FileStream);
   end Close;

end Pipe_Commands;
