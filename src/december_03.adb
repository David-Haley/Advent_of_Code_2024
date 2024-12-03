with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_03 is

   package Memories is new
     Ada.Containers.Vectors (Positive, Unbounded_String);
   use Memories;

   procedure Read_Input (Memory : out Memories.Vector) is

      Input_File : File_Type;
      Text : Unbounded_String;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_03.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Memory);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Append (Memory, Text);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   function Sum (Memory : in Memories.Vector;
                 Part_Two : in Boolean := False) return Natural is

      -- Problem stated that the operands are one to three digits, for at least
      -- my input there were no missing or non-conforming operands. None of the
      -- following were found but are handled with an appropriate nessage:
      -- mul(,123)
      -- mul(123,)
      -- mul(1234,12)
      -- mul(12,1234)

      Missing_Left, Bad_Operand_1, Missing_Comma, Bad_Operand_2, Missing_Right,
      Do_Found, Donot_Found, Line_End : Exception;

      subtype Operands is Natural range 0 .. 999;

      Result : Natural := 0;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Operand_1, Operand_2 : Operands;
      Max_Digits : constant Positive := 3;
      Last, Do_Index, Donot_Index : Natural;
      Mul_Op : constant String := "mul";
      Do_Op : constant String := "do()";
      Donot_Op : constant String := "don't()";
      Enable : Boolean := True;

   begin -- Sum
      for M in Iterate (Memory) loop
         Start_At := 1;
         Text := Memory (M);
         while Start_At < Length (Text) loop
            begin -- Expression exception block
               Last := Index (Text, Mul_Op, Start_At);
               if Part_Two then
                  if Enable then
                     Donot_Index := Index (Text, Donot_Op, Start_At);
                     if Donot_Index > 0 and Donot_Index < Last then
                        raise Donot_Found;
                     end if; -- Donot_Index > 0 and Donot_Index < Last
                  else
                     Do_Index := Index (Text, Do_Op, Start_At);
                     if Do_Index > 0 then
                        raise Do_Found;
                     else
                        raise Line_End;
                     end if; -- Do_Index > 0
                  end if; -- Enable
               end if; -- Part_Two
               if Last > 0 then
                  Start_At := Last + Mul_Op'Length;
               else
                  raise Line_End;
               end if; -- Last > 0
               if Start_At < Length (Text) and then
                 Element (Text, Start_At) = '(' then
                  Start_At := Start_At + 1;
               else
                  raise Missing_Left;
               end if; -- Start_At < Length (Text) and then ...
               Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First,
                           Last);
               if (First = Start_At and Last > 0) and then
                  Last - First + 1 <= Max_Digits then
                  Operand_1 := Operands'Value (Slice (Text, First, Last));
                  Start_At := Last + 1;
               else
                  raise Bad_Operand_1;
               end if; -- (First = Start_At and Last > 0) and then ...
               if Start_At < Length (Text) and then
                 Element (Text, Start_At) = ',' then
                  Start_At := Start_At + 1;
               else
                  raise Missing_Comma;
               end if; -- Start_At < Length (Text) and then ...
               Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First,
                           Last);
               if (First = Start_At and Last > 0) and then
                  Last - First + 1 <= Max_Digits then
                  Operand_2 := Operands'Value (Slice (Text, First, Last));
                  Start_At := Last + 1;
               else
                  raise Bad_Operand_2;
               end if; -- (First = Start_At and Last > 0) and then ...
               if Start_At <= Length (Text) and then
                 Element (Text, Start_At) = ')' then
                  Start_At := Start_At + 1;
               else
                  raise Missing_Right;
               end if; -- Start_At <= Length (Text) ...
               Result := @ + Operand_1 * Operand_2;
            exception
               when Missing_Left =>
                  Put_Line ("Line:" & To_Index (M)'Img & " Character:" &
                              Start_At'Img & " '(' not found");
               when Bad_Operand_1 =>
                  Put_Line ("Line:" & To_Index (M)'Img & " Character:" &
                              Start_At'Img &" bad or missing first operand");
               when Missing_Comma =>
                  Put_Line ("Line:" & To_Index (M)'Img & " Character:" &
                              Start_At'Img & " ',' not found");
               when Bad_Operand_2 =>
                  Put_Line ("Line:" & To_Index (M)'Img & " Character:" &
                              Start_At'Img & " bad or missing second operand");
               when Missing_Right =>
                  Put_Line ("Line:" & To_Index (M)'Img & " Character:" &
                              Start_At'Img & " ')' not found");
               when Do_Found =>
                  Enable := True;
                  Start_At := Do_Index + Do_Op'length;
               when Donot_Found =>
                  Enable := False;
                  Start_At := Donot_Index + Donot_Op'length;
               when Line_End =>
                  Start_At := Length (Text);
            end; -- Expression exception block
         end loop; -- Start_At < length
      end loop; -- M in Iterate (Memory)
      return Result;
   end Sum;

   Memory : Memories.Vector;

begin -- December_03
   Read_Input (Memory);
   Put_Line ("Part one:" & Sum (Memory)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:" & Sum (Memory, True)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_03;
