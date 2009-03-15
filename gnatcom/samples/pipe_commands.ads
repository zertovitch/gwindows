-------------------------------------------------------------------------------
--  Code based on:                                                           --
--  http://www.adapower.com/reuse/pipes.html                                 --
--  Contributed by: Jim Rogers                                               --
--  Contributed on: September 29, 1999                                       --
--  License: Public Domain                                                   --
--                                                                           --
--  Note: This package only works in console mode.                           --
-------------------------------------------------------------------------------
--  PACKAGE: Pipe Commands
--
--  PURPOSE: Provide a thick Ada binding to the UNX / WIN32 popen and pclose
--           commands. This allows an Ada program to call another program and
--           either send output to that program, or read input from that
--           program.
--
--  USAGE: Execute the command with mode read_file when you want to read the
--  output of the program you start. Execute the command with write_mode when
--  you want to write to the input of the program you start.
--
--  EXCEPTIONS:
--    Access_Error => Raised when a mode violation is attempted
--    End_Of_file  => Raised when the pipe is closed upon a read
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System;

package Pipe_Commands is

   type stream is private;

   type IO_MODE is (read_file, write_file);

   function execute (Command : in string; IO_type : in IO_MODE)
     return stream;

   function read_next (FromFile : in stream)
     return Unbounded_String;

   procedure write_next (ToFile : in stream; Message : in string);

   procedure close (OpenFile : in stream);

   Open_Error     : exception;
   --  Raised when unable to open pipe
   Access_Error : exception;
   --  Raised when attempt is made to violate IO_MODE
   End_Of_File  : exception;
   --  Raised on detection of End_of_file during read

private

   subtype Files is System.Address; -- Corresponds to a C file pointer

   type stream is
         record
            FileStream : Files;
            Mode       : IO_MODE;
         end record;

end Pipe_Commands;
