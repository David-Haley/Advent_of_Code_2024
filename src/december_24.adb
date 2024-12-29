with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Interfaces; use Interfaces;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_24 is

   type Gates is (And_Gate, Or_Gate, Xor_Gate);

   subtype Logic_Levels is Integer range -1 .. 1;
   subtype Logic_Drives is Logic_Levels range 0 .. 1;
   Undefined : constant Logic_Levels := -1;

   subtype Names is Unbounded_String;

   type Input_Indices is (In_Left, In_Right);
   type Input_Arrays is array (Input_Indices) of Logic_Levels;

   type Target_Elements is record
      Name : Names;
      Input_Index : Input_Indices;
   end record; -- Target_Elements

   package Target_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Target_Elements);
   use Target_Lists;

   type State_Variables is record
      Gate : Gates;
      Input_Array : Input_Arrays := (others => Undefined);
      Out_State : Logic_Levels := Undefined;
      Target_List : Target_Lists.List := Target_Lists.Empty_List;
   end record; -- State_Variables

   package Gate_Stores is new
     Ada.Containers.Ordered_Maps (Names, State_Variables);
   use Gate_Stores;

   type Inputs is record
      Out_State : Logic_Drives;
      Target_List : Target_Lists.List := Target_Lists.Empty_List;
   end record; -- Inputs

   package Input_Lists is new Ada.Containers.Doubly_Linked_Lists (Names);
   use Input_Lists;

   package Input_Maps is new
     Ada.Containers.Ordered_Maps (Names, Inputs);
   use Input_Maps;

   type Stimuli is record
      Logic_Drive : Logic_Drives;
      Gate_Name : Names;
      Input_Index : Input_Indices;
   end record; -- Stimuli

   package Trace_Stores is new Ada.Containers.Doubly_Linked_Lists (Stimuli);
   use Trace_Stores;

   subtype Output_Values is Unsigned_64;
   subtype Bit_Numbers is Natural range 0 .. 63;

   procedure Read_Input (Input_List : out Input_Lists.List;
                         Input_Map : out Input_Maps.Map;
                         Gate_Store : out Gate_Stores.Map) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Delimiters : constant Character_Set := To_Set (" :->");
      Input : Inputs;
      Target : array (Input_Indices) of Names;
      State_Variable : State_Variables;
      Gate : Gates;
      Target_Element : Target_Elements;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_24.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Input_List);
      Clear (Input_Map);
      Clear (Gate_Store);
      Get_Line (Input_File, Text);
      while Length (Text) > 0 loop
         Start_At := 1;
         Find_Token (Text, Delimiters, Start_At, Outside, First, Last);
         Append (Input_List, Unbounded_Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Delimiters, Start_At, Outside, First, Last);
         Input.Out_State := Logic_Drives'Value (Slice (Text, First, Last));
         Insert (Input_Map, Last_Element (Input_List), Input);
         Get_Line (Input_File, Text);
      end loop; -- Length (Text) > 0
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Delimiters, Start_At, Outside, First, Last);
         Target (In_Left) := Unbounded_Slice (Text, First, Last);
         Start_At := Last + 1;
         Find_Token (Text, Delimiters, Start_At, Outside, First, Last);
         if Slice (Text, First, Last) = "AND" then
            Gate := And_Gate;
         elsif Slice (Text, First, Last) = "OR" then
            Gate := OR_Gate;
         elsif Slice (Text, First, Last) = "XOR" then
            Gate := XOR_Gate;
         else
            raise Program_Error with "Unknown gate type """ &
              Slice (Text, First, Last) & """";
         end if; -- Slice (Text, First, Last) = "AND"
         Start_At := Last + 1;
         Find_Token (Text, Delimiters, Start_At, Outside, First, Last);
         Target (In_Right) := Unbounded_Slice (Text, First, Last);
         Start_At := Last + 1;
         Find_Token (Text, Delimiters, Start_At, Outside, First, Last);
         Target_Element.Name := Unbounded_Slice (Text, First, Last);
         if not Contains (Gate_Store, Target_Element.Name) then
            Insert (Gate_Store, Target_Element.Name, State_Variable);
         end if; -- not Contains (Gate_Store, Target_Element.Name)
         Gate_Store (Target_Element.Name).Gate := Gate;
         for I in Input_Indices loop
            Target_Element.Input_Index := I;
            if Contains (Input_Map, Target (I)) then
               Append (Input_Map (Target (I)).Target_List, Target_Element);
            elsif Contains (Gate_Store, Target (I)) then
               Append (Gate_Store (Target (I)).Target_List, Target_Element);
            else
               -- assumed gate output but gate not yet defined
               Insert (Gate_Store, Target (I), State_Variable);
               Append (Gate_Store (Target (I)).Target_List, Target_Element);
            end if; -- Contains (Input_Map, Target (I))
         end loop; -- I in Input_Indices
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   procedure Evaluate (Input_List : in Input_Lists.List;
                       Input_Map : in Input_Maps.Map;
                       Gate_Store :  in out Gate_Stores.Map;
                       Trace_Store : out Trace_Stores.List) is

      package Q_I is new
        Ada.Containers.Synchronized_Queue_Interfaces (Stimuli);
      use Q_I;

      package Queues is new Ada.Containers.Unbounded_Synchronized_Queues (Q_I);
      use Queues;

      Queue : Queues.Queue;

      Stimulus, Output : Stimuli;

   begin -- Evaluate
      for I in Iterate (Input_List) loop
         Stimulus.Logic_Drive := Input_Map (Element (I)).Out_State;
         for T in Iterate (Input_Map (Element (I)).Target_List) loop
            Stimulus.Gate_Name := Element (T).Name;
            Stimulus.Input_Index := Element (T).Input_Index;
            Queue.Enqueue (Stimulus);
         end loop; -- T in Iterate (Input_Map (Element (I)).Target_List)
         while Queue.Current_Use > 0 loop
            Queue.Dequeue (Stimulus);
            Append (Trace_Store, Stimulus);
            Gate_Store (Stimulus.Gate_Name).Input_Array (Stimulus.Input_Index)
              := Stimulus.Logic_Drive;
            if Gate_Store (Stimulus.Gate_Name).Input_Array (In_Left) /=
              Undefined and
              Gate_Store (Stimulus.Gate_Name).Input_Array (In_Right) /=
              Undefined then
               case Gate_Store (Stimulus.Gate_Name).Gate is
                  when And_Gate =>
                     if Gate_Store (Stimulus.Gate_Name).Input_Array (In_Left)
                       = 1 and
                       Gate_Store (Stimulus.Gate_Name).Input_Array (In_Right)
                       = 1 then
                        Output.Logic_Drive := 1;
                     else
                        Output.Logic_Drive := 0;
                     end if; -- Gate_Store (Stimulus.Gate_Name).Input_Array ...
                  when Or_Gate =>
                     if Gate_Store (Stimulus.Gate_Name).Input_Array (In_Left)
                       = 1 or
                       Gate_Store (Stimulus.Gate_Name).Input_Array (In_Right)
                       = 1 then
                        Output.Logic_Drive := 1;
                     else
                        Output.Logic_Drive := 0;
                     end if; -- Gate_Store (Stimulus.Gate_Name).Input_Array ...
                  when Xor_Gate =>
                     if Gate_Store (Stimulus.Gate_Name).Input_Array (In_Left)
                       = 1 xor
                       Gate_Store (Stimulus.Gate_Name).Input_Array (In_Right)
                       = 1 then
                        Output.Logic_Drive := 1;
                     else
                        Output.Logic_Drive := 0;
                     end if; -- Gate_Store (Stimulus.Gate_Name).Input_Array ...
               end case; -- Gate_Store (Stimulus.Gate_Name).Gate
               Gate_Store (Stimulus.Gate_Name).Out_State := Output.Logic_Drive;
               for T in Iterate (Gate_Store (Stimulus.Gate_Name).Target_List)
               loop
                  Output.Gate_Name := Element (T).Name;
                  Output.Input_Index := Element (T).Input_Index;
                  Queue.Enqueue (Output);
               end loop; -- T in Iterate (Gate_Store (Stimulus.Gate_Name) ...
            end if; --  G Gate_Store (Stimulus.Gate_Name).Input_Array  ...
         end loop; -- Queue.Current_Use > 0
      end loop; -- I in Iterate (Input_List)
   end Evaluate;

   function Output_Register (Gate_Store : in Gate_Stores.Map)
                             return Output_Values is

      Result : Output_Values := 0;
      Mask : constant Output_Values := 1;

   begin -- Output_Register
      for G in Iterate (Gate_Store) loop
         if Element (Key (G), 1) = 'z' and then
           Element (G).Out_State = 1 then
            Result := @ or
              Shift_Left (Mask, Bit_Numbers'Value (Slice (Key (G), 2, 3)));
         end if; -- Element (Key (G), 1) = 'z'
      end loop; -- G in Iterate (Gate_Store)
      return Result;
   end Output_Register;

   procedure Test_Adder (Input_Map : in out Input_Maps.Map;
                         Gate_Store : in out Gate_Stores.Map) is

      -- Test sixteen cases of adding Xn, Xn+1, Yn and Yn+1,
      -- checking that X + Y = Z;

      procedure Reset (Gate_Store : in out Gate_Stores.Map) is

      begin -- Reset
         for G in Iterate (Gate_Store) loop
            Gate_Store (G).Input_Array := (Undefined, Undefined);
            Gate_Store (G).Out_State := Undefined;
         end loop; -- G in Iterate (Gate_Store)
      end Reset;

      function Input_Register (Input_Map : in Input_Maps.Map;
                               Register : in Character)
                               return Output_Values is

         Result : Output_Values := 0;
         Mask : constant Output_Values := 1;

      begin -- Input_Register
         for I in Iterate (Input_Map) loop
            if Element (Key (I), 1) = Register and then
              Element (I).Out_State = 1 then
               Result := @ or
                 Shift_Left (Mask, Bit_Numbers'Value (Slice (Key (I), 2, 3)));
            end if; -- Element (Key (I), 1) = Register
         end loop; -- I in Iterate (Input_Map)
         return Result;
      end Input_Register;

      function Find_MSB (Input_Map : in Input_Maps.Map) return Bit_Numbers is

         MSB : Bit_Numbers := 0;

      begin -- Find_MSB
         for I in Iterate (Input_Map) loop
            if Element (Key (I), 1) = 'x' and then
              Bit_Numbers'Value (Slice (Key (I), 2, 3)) > MSB then
               MSB := Bit_Numbers'Value (Slice (Key (I), 2, 3));
            end if; -- Element (Key (I), 1) = 'x' and then ...
         end loop; -- I in Iterate (Input_Map)
         return MSB;
      end Find_MSB;

      function Build_Name (Base_Name : in character;
                           Number : in Bit_Numbers)
                              return Unbounded_String is

      begin -- Build_Name
         if NuMber < 10 then
            return To_Unbounded_String (Base_Name & "0" &
                                          Trim (Number'Img, Left));
         else
            return To_Unbounded_String (Base_Name & Trim (Number'Img, Left));
         end if; -- Number < 10
      end Build_Name;

      procedure Set_Registers (N : in Bit_Numbers;
                               Xn, Xn_1, Yn, Yn_1 : in Logic_Drives;
                               MSB : in Bit_Numbers;
                               Input_Map : in out Input_Maps.Map) is

      begin -- Set_Registers
         for I in Bit_Numbers range 0 .. MSB loop
            Input_Map (Build_Name ('x', I)).Out_State := 0;
            Input_Map (Build_Name ('y', I)).Out_State := 0;
         end loop; -- N in Bit_Numbers range 0 .. MSB
         -- set x and y registers to 0;
         Input_Map (Build_Name ('x', N)).Out_State := Xn;
         Input_Map (Build_Name ('x', N + 1)).Out_State := Xn_1;
         Input_Map (Build_Name ('y', N)).Out_State := Yn;
         Input_Map (Build_Name ('y', N + 1)).Out_State := Yn_1;
         -- Test bits set
      end Set_Registers;

      procedure Put (Test : in Bit_Numbers;
                     Xn, Xn_1, Yn, Yn_1 : in Logic_Drives;
                     Trace_Store : in Trace_Stores.List;
                     X, Y, Expected, Actual : in Output_Values;
                     MSB : in Bit_Numbers) is

         package Out_IO is new Ada.Text_IO.Modular_IO (Output_Values);
         use Out_IO;

         Base_Name : constant String := "december_24_test_";
         Drive_State : constant String := "_" & Trim (Xn'Img, Left) &
           Trim (Xn_1'Img, Left) & Trim (Yn'Img, Left) & Trim (Yn_1'Img, Left);
         Extension : constant String := ".txt";
         Output_File : File_Type;
         Failing_Bits : constant Output_Values := Expected xor Actual;
         Field_Width : constant Positive := MSB + 4;
         Base : constant Positive := 2;

      begin -- Put
         if Test < 10 then
            Create (Output_File, Out_File, Base_Name & "0" &
                      Trim (Test'Img, Left) & Drive_State &  Extension);
         else
            Create (Output_File, Out_File, Base_Name & Trim (Test'Img, Left) &
                      Drive_State & Extension);
         end if; -- Test < 10
         Put_Line (Output_File, "Testing x and y bits" & Test'Img & " .." &
                     Bit_Numbers'Image (Test + 1) & " z bits" &
                     Test'Img & " .." & Bit_Numbers'Image (Test + 2));
         New_Line (Output_File);
         for T in Iterate (Trace_Store) loop
            Put (Output_File, Element (T).Gate_Name & '(' &
                   Trim (Element (T).Input_Index'Img, Left) & ") :=" &
                   Element (T).Logic_Drive'Img);
            if Element (Element (T).Gate_Name, 1) = 'z' then
               New_Line (Output_File);
            else
               Put (Output_File, ", ");
            end if; -- Element (T).Gate_Name (1) = 'z'
         end loop; -- T in Iterate (Trace_Store)
         New_Line (Output_File);
         Put (Output_File, "           X");
         Put (Output_File, X, Field_Width, Base);
         New_Line (Output_File);
         Put (Output_File, "           Y");
         Put (Output_File, Y, Field_Width, Base);
         New_Line (Output_File);
         Put (Output_File, "Expected   Z");
         Put (Output_File, Expected, Field_Width, Base);
         New_Line (Output_File);
         Put (Output_File, "Actual     Z");
         Put (Output_File, Actual, Field_Width, Base);
         New_Line (Output_File);
         Put (Output_File, "Failing Bits");
         Put (Output_File, Failing_Bits, Field_Width, Base);
         New_Line (Output_File);
         Close (Output_File);
      end Put;

      MSB : constant Bit_Numbers := Find_MSB (Input_Map);
      X, Y, Z : Output_Values;
      Trace_Store : Trace_Stores.List;
      Input_List : Input_Lists.List := Input_Lists.Empty_List;
      Pass : Boolean;

   begin -- Test_Adder
      -- define order in which bits are transmitted.
      for N in Bit_Numbers range 0 .. MSB loop
         Append (Input_List, Build_Name ('x', N));
         Append (Input_List, Build_Name ('y', N));
      end loop; -- N in Bit_Numbers range 0 .. MSB
      for N in Bit_Numbers range 0 .. MSB - 1 loop
         Pass := True;
         Put ("Test:" & N'Img);
         for Yn_1 in Logic_Drives loop
            for Yn in Logic_Drives loop
               for Xn_1 in Logic_Drives loop
                  for Xn in Logic_Drives loop
                     Reset (Gate_Store);
                     Clear (Trace_Store);
                     Set_Registers (N, Xn, Xn_1, Yn, Yn_1, MSB, Input_Map);
                     Evaluate (Input_List, Input_Map, Gate_Store, Trace_Store);
                     X := Input_Register (Input_Map, 'x');
                     Y := Input_Register (Input_Map, 'y');
                     Z := X + Y;
                     if Z /= Output_Register (Gate_Store) then
                        Pass := False;
                        Put (N, Xn, Xn_1, Yn, Yn_1, Trace_Store, X, Y, Z,
                             Output_Register (Gate_Store), MSB);
                     end if; -- Z /= Output_Register (Gate_Store)
                  end loop; -- Xn in Logic_Drives
               end loop; -- Xn_1 in Logic_Drives
            end loop; -- Yn in Logic_Drives
         end loop; -- Yn_1 in Logic_Drives
         if Pass then
            Put_Line (" passed");
         else
            Put_Line (" failed");
         end if; -- Pass
      end loop; -- N in Bit_Numbers range 0 .. MSB - 1
   end Test_Adder;

   Input_List : Input_Lists.List;
   Input_Map : Input_Maps.Map;
   Gate_Store :  Gate_Stores.Map;
   Trace_Store : Trace_Stores.List := Trace_Stores.Empty_List;

begin -- December_24
   Read_Input (Input_List, Input_Map, Gate_Store);
   Evaluate (Input_List, Input_Map, Gate_Store, Trace_Store);
   Clear (Trace_Store);
   Put_Line ("Part one:" & Output_Register (Gate_Store)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Test_Adder (Input_Map, Gate_Store);
   Put_Line ("Part two:");
   DJH.Execution_Time.Put_CPU_Time;
end December_24;
