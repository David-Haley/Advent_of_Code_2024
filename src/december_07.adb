with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Doubly_Linked_Lists;
with Interfaces; use Interfaces;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_07 is

   subtype Values is Long_Long_Integer;

   package Operands is new Ada.Containers.Vectors (Positive, Values);
   use Operands;

   type Equations is record
      Result : Values;
      Operand : Operands.Vector := Operands.Empty_Vector;
   end record; -- Equations


   package Equation_Lists is new Ada.Containers.Doubly_Linked_Lists (Equations);
   use Equation_Lists;



   procedure Read_Input (Equation_List : out Equation_Lists.List) is

      Input_File : File_Type;
      Text : Unbounded_String;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_07.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Equation_List);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         declare -- One quation
            Start_At : Positive := 1;
            First : Positive;
            Last : Natural;
            Equation : Equations;
         begin
            Start_At := 1;
            Find_Token (Text, Decimal_Digit_Set, Start_At,Inside, First, Last);
            Equation.Result := Values'Value (Slice (Text, First, Last));
            Start_At := Last + 1;
            if Element (Text, Start_AT) /= ':' then
               raise Program_Error with "Found '" & Element (Text, Start_AT) &
                 "' expected ':'";
            end if; -- Element (Text, Start_AT) /= ':'
            loop -- Read one operand
               Find_Token (Text, Decimal_Digit_Set, Start_At,Inside, First,
                           Last);
               exit when Last = 0;
               append (Equation.Operand,
                       Values'Value (Slice (Text, First, Last)));
               Start_At := Last + 1;
            end loop; -- -- Read one operand
            Append (Equation_List, Equation);
         end; -- One equation
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   function Valid_1 (Equation : in Equations) return Boolean is

      Test_Value : Values;
      Op_Store : Unsigned_32 :=
        2 ** Natural (Length (Equation.Operand) - 1) - 1;
      -- Initally there is a 1 bit for each operator. A 1 bit represents a
      -- multiply operator and a 0 reprents an addition operator.
      Result : Boolean := False;
      Operand_Mask : Unsigned_32;

   begin -- Valid_1
      loop -- One Test
         Test_Value := First_Element (Equation.Operand);
         Operand_Mask := 1;
         for I in Positive range 2 .. Positive (Length (Equation.Operand)) loop
            if (Op_Store and Operand_Mask) = 0 then
               Test_Value := @ + Equation.Operand (I);
            else
               Test_Value := @ * Equation.Operand (I);
            end if; -- Op_Store and Operand_Mask = 0
            Operand_Mask := Shift_Left (Operand_Mask, 1);
         end loop; -- I in Positive range 2 .. Positive (Length (Equation ...
         Result := Equation.Result = Test_Value;
         exit when Result or Op_Store = 0;
         Op_Store := @ - 1;
      end loop; -- One Test
      return Result;
   end Valid_1;

   function Valid_2 (Equation : in Equations) return Boolean is

      subtype Operator_Indices is Positive range 1 ..
        Positive (Length (Equation.Operand) - 1);
      type Operators is mod 3;
      type Op_Stores is array (Operator_Indices) of Operators;
      Operator_End : constant Op_Stores := (others => Operators'Last);
      Op_Store : Op_Stores := (others => Operators'First);
      -- Initally each operator is set to 0. A 1 bit represents a
      -- multiply operator and a 0 reprents an addition operator and a 2
      -- represents a concatination operator.
      Result : Boolean := False;
      Test_Value : Values;

      function "&" (Left, Right : Values) return Values is
        (Values'Value (Trim (Left'Img, Both) & Trim (Right'Img, Both)));

      procedure Increment (Op_Store : in out Op_Stores) is

         Carry : Boolean := True; -- forces an addition on the first iteration

      begin -- Increment
         for I in Operator_Indices loop
            if Carry then
               Op_Store (I) := @ + 1;
               Carry := Op_Store (I) = Operators'First;
            end if; -- Carry
         end loop; -- I in Operator_Indices
      end Increment;

      pragma Inline_Always ("&", Increment);

   begin -- Valid_2
      loop -- One Test
         Test_Value := First_Element (Equation.Operand);
         for I in Operator_Indices loop
            case Op_Store (I) is
            when 0 =>
               Test_Value := @ + Equation.Operand (I + 1);
            when 1 =>
               Test_Value := @ * Equation.Operand (I + 1);
            when 2 =>
               Test_Value := @ & Equation.Operand (I + 1);
            end case; -- Op_Store (I)
         end loop; -- I in Operator_Indices
         Result := Equation.Result = Test_Value;
         exit when Result or Op_Store = Operator_End;
         Increment (Op_Store);
      end loop; -- One Test
      return Result;
   end Valid_2;

   Equation_List : Equation_Lists.List;
   Sum : Values := 0;

begin -- December_07
   Read_Input (Equation_List);
   for E in Iterate (Equation_List) loop
      if Valid_1 (Equation_List (E)) then
         Sum := @ + Equation_List (E).Result;
      end if; -- Valid_1 (Equation_List (E))
   end loop; -- E in Iterate (Equation_List)
   Put_Line ("Part one:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
   for E in Iterate (Equation_List) loop
      if not Valid_1 (Equation_List (E)) and then
        Valid_2 (Equation_List (E)) then
         Sum := @ + Equation_List (E).Result;
      end if; -- Valid_1 (Equation_List (E)) and then ...
   end loop; -- E in Iterate (Equation_List)
   Put_Line ("Part two:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_07;
