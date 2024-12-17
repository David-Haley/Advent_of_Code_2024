with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with Interfaces; use Interfaces;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_17 is

   type Words is mod 8;

   subtype Registers is Unsigned_64;

   Octal_Digit_Set : constant Character_Set := To_Set ("01234567");

      To_Register : constant array (Words) of Registers :=
     (0, 1, 2, 3, 4,  5, 6, 7);

   type Op_Codes is (adv, bxl, bst, jnz, bxc, out_ins, bdv, cdv);

   type Machine_States is record
      A, B, C, Ip : Registers;
   end record; -- Machine_States

   package Program_Stores is new Ada.Containers.Ordered_Maps (Registers, Words);
   use Program_Stores;

   procedure Read_Input (Machine_State : out Machine_States;
                         Program_Store : out Program_Stores.Map) is

      A_String : constant String := "Register A:";
      B_String : constant String := "Register B:";
      C_String : constant String := "Register C:";
      Prog_String : constant String := "Program:";

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Pc : Registers;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_17.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Program_Store);
      Machine_State.Ip := 0;
      Pc := 0;
      Get_Line (Input_File, Text);
      Start_At := 1;
      Last := Index (Text, A_String, Start_At);
      if Last > 0 then
         Start_At := Last + A_String'Length;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First,
                     Last);
         Machine_State.A := Registers'Value (Slice (Text, First, Last));
      else
         raise Program_Error with "No A register found";
      end if; -- Last > 0
      Get_Line (Input_File, Text);
      Start_At := 1;
      Last := Index (Text, B_String, Start_At);
      if Last > 0 then
         Start_At := Last + B_String'Length;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First,
                     Last);
         Machine_State.B := Registers'Value (Slice (Text, First, Last));
      else
         raise Program_Error with "No B register found";
      end if; -- Last > 0
      Get_Line (Input_File, Text);
      Start_At := 1;
      Last := Index (Text, C_String, Start_At);
      if Last > 0 then
         Start_At := Last + C_String'Length;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First,
                     Last);
         Machine_State.C := Registers'Value (Slice (Text, First, Last));
      else
         raise Program_Error with "No C register found";
      end if; -- Last > 0
      Skip_Line (Input_File);
      Get_Line (Input_File, Text);
      Start_At := 1;
      Last := Index (Text, Prog_String);
      if Last > 0 then
         Start_At := Last + Prog_String'Length;
         while Start_At < Length (Text) loop
            Find_Token (Text, Octal_Digit_Set, Start_At, Inside, First,
                        Last);
            Insert (Program_Store, Pc, Words'Value (Slice (Text, First, Last)));
            Pc := @ + 1;
            Start_At := Last + 1;
         end loop; -- Start_At < Length (Text)
      else
         raise Program_Error with "Program not found";
      end if; -- Last > 0
      Close (Input_File);
   end Read_Input;

   procedure Run (Machine_State : in out Machine_States;
                  Program_Store : in Program_Stores.Map;
                  Output : out Unbounded_String;
                  File_Name : in Unbounded_String) is

      function Combo_Operand (Machine_State : in Machine_States;
                              Program_Store : in Program_Stores.Map)
                              return Registers is

         Result : Registers := 0;

      begin -- Combo_Operand
         case Program_Store (Machine_State.Ip + 1) is
            when 0 | 1 | 2 | 3=>
               Result := To_Register (Program_Store (Machine_State.Ip + 1));
            when 4 =>
               Result := Machine_State.A;
            when 5 =>
               Result := Machine_State.B;
            when 6 =>
               Result := Machine_State.C;
            when 7 =>
               raise Program_Error with "Bad Combo:" & Machine_State'Img;
         end case; -- Operand
         return Result;
      end Combo_Operand;

      pragma Inline_Always (Combo_Operand);


      Output_File : File_Type;
      Trace_On : Boolean := Length (File_Name) > 0;

   begin -- Run
      if Trace_On then
         Create (Output_File, Out_File, To_String (File_Name));
      end if;
      Output := Null_Unbounded_String;
      while Contains (Program_Store, Machine_State.Ip) loop
         if Trace_On then
            Put (Output_File, "Ip:" & Machine_State.Ip'Img & ": " &
                   Op_Codes'Val (Program_Store (Machine_State.Ip))'Img & "(" &
                   Words'Image (Program_Store (Machine_State.Ip + 1)) & ")");
         end if; -- Trace_On Trace_On
         case Op_Codes'Val (Program_Store (Machine_State.Ip)) is
            when adv =>
               Machine_State.A := @
                 / 2 ** Natural (Combo_Operand (Machine_State, Program_Store));
               Machine_State.Ip := @ + 2;
            when bxl =>
               Machine_State.B := @ xor
                 To_Register (Program_Store (Machine_State.Ip + 1));
               Machine_State.Ip := @ + 2;
            when bst =>
               Machine_State.B := Combo_Operand (Machine_State, Program_Store)
                 and To_Register (Words'Last);
               Machine_State.Ip := @ + 2;
            when jnz =>
               if Machine_State.A /= 0 then
                  Machine_State.Ip :=
                    To_Register (Program_Store (Machine_State.Ip + 1));
               else
                  Machine_State.Ip := @ + 2;
               end if; -- Machine_State A /= 0
            when bxc =>
               Machine_State.B := @ xor Machine_State.C;
               Machine_State.Ip := @ + 2;
            when out_ins =>
               Append (Output, "," &
                         Trim ( Registers'Image (Combo_Operand (Machine_State,
                         Program_Store) mod Words'Modulus),Left));
               Machine_State.Ip := @ + 2;
            when bdv =>
               Machine_State.B := Machine_State.A
                 / 2 ** Natural (Combo_Operand (Machine_State, Program_Store));
               Machine_State.Ip := @ + 2;
            when cdv =>
               Machine_State.C := Machine_State.A
                 / 2 ** Natural (Combo_Operand (Machine_State, Program_Store));
               Machine_State.Ip := @ + 2;
         end case; -- Program_Store (Machine_State.Ip)
         if Trace_On then
            Put_Line (Output_File, " => A:" & Machine_State.A'Img &
                        ", B:" & Machine_State.B'Img &
                        ", C:" & Machine_State.C'Img);
         end if; -- Trace_On
      end loop; -- Contains (Program_Store, Machine_State.Ip)
      Delete (Output, 1, 1); -- remove leading ','
      If Trace_On then
         Close (Output_File);
      end if; -- Trace_On
   end Run;

   Machine_State : Machine_States;
   Program_Store : Program_Stores.Map;
   Output, Output_2 : Unbounded_String;
   File_Name : Unbounded_String;

begin -- December_17
   Read_Input (Machine_State, Program_Store);
   Put ("Enter file name for trace, Enter only for no trace: ");
   Get_Line (File_Name);
   Run (Machine_State, Program_Store, Output, File_Name);
   Put_Line ("Part one: " & Output);
   DJH.Execution_Time.Put_CPU_Time;
   Read_Input (Machine_State, Program_Store);
   Machine_State.A := 2 ** 15 * 4;
   Put ("Enter file name for trace, Enter only for no trace: ");
   Get_Line (File_Name);
   Run (Machine_State, Program_Store, Output_2, File_Name);
   Put_Line ("Part two: " & Output_2);
   DJH.Execution_Time.Put_CPU_Time;
end December_17;
