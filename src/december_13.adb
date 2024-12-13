with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Numerics.Generic_Real_Arrays;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Interfaces; use Interfaces;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_13 is

   type My_Reals is digits (15);
   package My_Matrices is new Ada.Numerics.Generic_Real_Arrays (My_Reals);
   use My_Matrices;

   subtype Buttons is Real_Matrix (1 .. 2, 1 .. 2);
   subtype Prizes is Real_Vector (1 .. 2);

   type Machines is record
      Button : Buttons;
      Prize : Prizes;
   end record; -- Machines

   package Machine_Lists is new Ada.Containers.Doubly_Linked_Lists (Machines);
   use Machine_Lists;

   subtype Tokens is Long_Long_Integer range 0 .. Long_Long_Integer'Last;
   A : constant Tokens := 3;
   B : constant Tokens := 1;

   procedure Read_Input (Machine_List : out Machine_Lists.List) is

      Input_File : File_Type;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_13.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Machine_List);
      while not End_Of_File (Input_File) loop
         declare -- One machine
            Text : Unbounded_String;
            Start_At, First : Positive;
            Last : Natural;
            Machine : Machines;
         begin
            Get_Line (Input_File, Text);
            Start_At := 1;
            Find_Token (Text, Decimal_Digit_Set, Start_At,Inside, First, Last);
            Machine.Button (1, 1) :=
              My_Reals'Value (Slice (Text, First, Last) & ".0");
            Start_At := Last + 1;
            Find_Token (Text, Decimal_Digit_Set, Start_At,Inside, First, Last);
            Machine.Button (2, 1) :=
              My_Reals'Value (Slice (Text, First, Last) & ".0");
            Get_Line (Input_File, Text);
            Start_At := 1;
            Find_Token (Text, Decimal_Digit_Set, Start_At,Inside, First, Last);
            Machine.Button (1, 2) :=
              My_Reals'Value (Slice (Text, First, Last) & ".0");
            Start_At := Last + 1;
            Find_Token (Text, Decimal_Digit_Set, Start_At,Inside, First, Last);
            Machine.Button (2, 2) :=
              My_Reals'Value (Slice (Text, First, Last) & ".0");
            Get_Line (Input_File, Text);
            Start_At := 1;
            Find_Token (Text, Decimal_Digit_Set, Start_At,Inside, First, Last);
            Machine.Prize (1) :=
              My_Reals'Value (Slice (Text, First, Last) & ".0");
            Start_At := Last + 1;
             Find_Token (Text, Decimal_Digit_Set, Start_At,Inside, First, Last);
            Machine.Prize (2) :=
              My_Reals'Value (Slice (Text, First, Last) & ".0");
            if not End_Of_File (Input_File) then
               Skip_Line (Input_File);
            end if; -- not End_Of_File (Input_File)
            Append (Machine_list, Machine);
            Start_At := Last + 1;
         end; -- One machine
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   function Total_Cost (Machine_List : Machine_Lists.List) return Tokens is

      function Verify (Machine : in Machines;
                       Solution : in out Prizes) return Boolean is

         -- Has the side effect of rounding Solution. the choice of tolerance
         -- is critical for part two.

         Tolerance : constant My_Reals := 0.1;
         Test : Prizes;

      begin -- Verify
         Solution (1) := My_Reals'Machine_Rounding (Solution (1));
         Solution (2) := My_Reals'Machine_Rounding (Solution (2));
         Test := Machine.Button * Solution;
         return abs (Machine.Prize (1) - Test (1)) < Tolerance and
         abs (Machine.Prize (2) - Test (2)) < Tolerance;
      end Verify;

      Solution : Prizes;
      Result : tokens := 0;

   begin -- Total_Cost
      for M in Iterate (Machine_List) loop
         if Determinant (Element (M).Button) /= 0.0 then
            Solution := Solve (Element (M).Button, Element (M).Prize);
            if Verify (Element (M), Solution) then
               Result := @ + Tokens (Solution (1)) * A +
                 Tokens (Solution (2)) * B;
            end if; -- Solution (1) = My_Reals'Truncation (Solution (1)) and ...
         end if; -- Determinant (Element (M).Button) /= 0.0
      end loop; -- M in Iterate (Machine_List)
      return Result;
   end Total_Cost;

   Machine_List : Machine_Lists.List;

begin -- December_13
   Read_Input (Machine_List);
   Put_Line ("Part one:" & Total_Cost (Machine_List)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   -- if adding 10000000000000 was supposed to prevent the use of floating point
   -- arithmetic it did not for my input;
   for M in Iterate (Machine_List) loop
      Machine_List (M).Prize (1) :=
        Machine_List (M).Prize (1) + 10000000000000.0;
      Machine_List (M).Prize (2) :=
        Machine_List (M).Prize (2) + 10000000000000.0;
   end loop; -- M in Iterate (Machine_List)
   Put_Line ("Part two:" & Total_Cost (Machine_List)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_13;
