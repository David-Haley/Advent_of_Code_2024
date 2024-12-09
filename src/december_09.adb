with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_09 is

   subtype File_Ids is Natural;
   subtype Extended_file_Ids is Integer range -1 .. File_Ids'Last;
   Free_Block : constant Extended_File_Ids := Extended_File_Ids'First;

   subtype Block_Addresses is Natural;
   subtype Block_Counts is Natural range 0 .. 9;

   package Discs is new
     Ada.Containers.Ordered_Maps (Block_Addresses, Extended_file_Ids);
   use Discs;

   procedure Read_Input (Disc : out Discs.Map) is

      Input_File : File_Type;
      File_Id : File_Ids := File_Ids'First;
      Block_String : String (1 .. 1);
      Block_Count : Block_Counts;
      Block_Address : Block_Addresses := Block_Addresses'First;
      Is_File : Boolean := True;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_09.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Disc);
      while not End_Of_File (Input_File) loop
         Get (Input_File, Block_String);
         Block_Count := Block_Counts'Value (Block_String);
         if Is_File then
            for F in Block_Counts range 1 .. Block_Count loop
               insert (Disc, Block_Address, File_Id);
               Block_Address := @ + 1;
            end loop; -- F in Block_Counts range 1 .. Block_Count
            File_Id := @ + 1;
         else
            for E in Block_Counts range 1 .. Block_Count loop
               insert (Disc, Block_Address, Free_Block);
               Block_Address := @ + 1;
            end loop; -- E in Block_Counts range 1 .. Block_Count
         end if; -- Is_File
         Is_File := not Is_File;
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   procedure Compact (Disc : in out Discs.Map) is

      To_Block : Block_Addresses := First_Key (Disc);
      From_Block : Block_Addresses := Last_Key (Disc);

   begin -- Compact
      loop -- Move one block
         while Disc (To_Block) /= Free_Block loop
            To_Block := @ + 1;
         end loop; -- Disc (To_Block) /= Free_Block
         while Disc (From_Block) = Free_Block loop
            From_Block := @ - 1;
         end loop; -- Disc (From_Block) = Free_Block
         exit when To_Block >= From_Block;
         Disc (To_Block) := Disc (From_Block);
         Disc (From_Block) := Free_Block;
         To_Block := @ + 1;
         From_Block := @ - 1;
      end loop; -- Move one block
   end Compact;

   procedure Defrag (Disc : in out Discs.Map) is

      File_To_Move : File_Ids;
      To_Block : Block_Addresses;
      From_Block : Block_Addresses := Last_Key (Disc);
      File_Length, Free_Length : Block_Counts;

   begin -- Defrag
      while Disc (From_Block) = Free_Block loop
         From_Block := @ - 1;
      end loop; -- Disc (From_Block) = Free_Block
      -- find last block containing a file, initially the largest File_Id
      File_To_Move := Disc (From_Block);
      loop -- until finished
         -- Find a file to move
         while Disc (From_Block) /= File_To_Move loop
            From_Block := @ - 1;
         end loop; -- Disc (From_Block) /= File_To_Move
         File_Length := 0;
         while Disc (From_Block - File_Length) = File_To_Move loop
            File_Length := @ + 1;
         end loop; -- Disc (From_Block - File_Length) = File_To_Move
         To_Block := Block_Addresses'First;
         loop -- Find first fit
            while Disc (To_Block) /= Free_Block and
              To_Block < Last_Key (Disc) loop
               To_Block := @ + 1;
            end loop; -- Disc (To_Block) /= Free_Block and ...
            Free_Length := 0;
            while Disc (To_Block + Free_Length) = Free_Block and
              Free_Length < File_Length and
              To_Block + Free_Length  < Last_Key (Disc) loop
               Free_Length := @ + 1;
            end loop; -- Disc (To_Block + Free_Length) = Free_Block and ...
            exit when Free_Length >= File_Length or
              To_Block > From_Block - File_Length;
            To_Block := @ + Free_Length;
         end loop; -- Find first fit
         if Free_Length >= File_Length and
           To_Block < From_Block - File_Length then
            -- the move actually reverses the block order but all that matters
            -- here is the File_Id
            for I in Block_Counts range 1 .. File_Length loop
               Disc (To_Block) := Disc (From_Block);
               Disc (From_Block) := Free_Block;
               To_Block := @ + 1;
               From_Block := @ - 1;
            end loop; -- I in Block_Counts range 1 .. File_Length
         end if; -- Free_Length >= File_Length and ...
         File_To_Move := @ - 1;
         exit when File_To_Move < 1;
      end loop; -- until finished;
   end Defrag;

   function Checksum (Disc : in Discs.Map) return Long_Long_Integer is

      Result :  Long_Long_Integer := 0;

   begin -- Checksum
      for B in Iterate (Disc) loop
         if Element (B) /= Free_Block then
            Result := @ + Long_Long_Integer (Element (B) * Key (B));
         end if; -- Disc (B) /= Free_Block
      end loop; -- Element (B) /= Free_Block
      return Result;
   end Checksum;

   Disc : Discs.Map;

begin -- December_09
   Read_Input (Disc);
   Compact (Disc);
   Put_Line ("Part one:" & Checksum (Disc)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Read_Input (Disc);
   Defrag (Disc);
   Put_Line ("Part two:" & Checksum (Disc)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_09;
