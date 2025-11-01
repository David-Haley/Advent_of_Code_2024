with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Containers; use Ada.Containers;
--  with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Interfaces; use Interfaces;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_24 is

   subtype Output_Values is Unsigned_64;
   subtype Bit_Numbers is Natural range 0 .. 63;
   Mask : constant Output_Values := 1;

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

   type Source_Arrays is array (Input_Indices) of Names;

   function "<" (Left, Right : Target_Elements) return Boolean is
     (Left.Name < Right.Name or (Left.Name = Right.Name and
          Left.Input_Index < Right.Input_Index));

   package Target_Lists is new Ada.Containers.Ordered_Sets (Target_Elements);
   use Target_Lists;

   package Target_Maps is new
     Ada.Containers.Ordered_Maps (Names, Target_Lists.Set);
   use Target_Maps;

   package Source_Maps is new
     Ada.Containers.Ordered_Maps (Names, Source_Arrays);
   use Source_Maps;

   type State_Variables is record
      Gate : Gates;
      Input_Array : Input_Arrays := (others => Undefined);
      Out_State : Logic_Levels := Undefined;
   end record; -- State_Variables

   package Gate_Stores is new
     Ada.Containers.Ordered_Maps (Names, State_Variables);
   use Gate_Stores;

   package Input_Lists is new Ada.Containers.Ordered_Maps (Names, Logic_Drives);
   use Input_Lists;

   package Output_Lists is new Ada.Containers.Ordered_Sets (Names);
   use Output_Lists;

   type Swap_Pairs is record
      Left, Right : Names;
   end record; -- Swap_Pairs

   type Input_States is record
      X, Y : Output_Values;
   end record; -- Input_States

   function "<" (Left, Right : Input_States) return Boolean is
      (Left.X < Right.X or (Left.X = Right.X and  Left.Y < Right.Y));

   package Input_Sets is new Ada.Containers.Ordered_Sets (Input_States);
   use Input_Sets;

   package Gate_Sets is new Ada.Containers.Ordered_Sets (Names);
   use Gate_Sets;

   type Tests is access function (X, Y : in Output_Values) return Output_Values;

   procedure Read_Input (Input_List : out Input_Lists.Map;
                         Gate_Store : out Gate_Stores.Map;
                         Target_Map : out Target_Maps.Map;
                         Output_List : out Output_Lists.Set;
                         Source_Map : out Source_Maps.Map) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Delimiters : constant Character_Set := To_Set (" :->");
      Name : Names;
      Input_State : Logic_Drives;
      Source : Source_Arrays;
      State_Variable : State_Variables;
      Gate : Gates;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_24.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Input_List);
      Clear (Gate_Store);
      Clear (Target_Map);
      Clear (Output_List);
      Clear (Source_Map);
      Get_Line (Input_File, Text);
      while Length (Text) > 0 loop
         Start_At := 1;
         Find_Token (Text, Delimiters, Start_At, Outside, First, Last);
         Name := Unbounded_Slice (Text, First, Last);
         Start_At := Last + 1;
         Find_Token (Text, Delimiters, Start_At, Outside, First, Last);
         Input_State := Logic_Drives'Value (Slice (Text, First, Last));
         Insert (Input_List, Name, Input_State);
         Get_Line (Input_File, Text);
      end loop; -- Length (Text) > 0
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Delimiters, Start_At, Outside, First, Last);
         Source (In_Left) := Unbounded_Slice (Text, First, Last);
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
         Source (In_Right) := Unbounded_Slice (Text, First, Last);
         Start_At := Last + 1;
         Find_Token (Text, Delimiters, Start_At, Outside, First, Last);
         Name := Unbounded_Slice (Text, First, Last);
         Insert (Gate_Store, Name, State_Variable);
         Gate_Store (Name).Gate := Gate;
         if Element (Name, 1) = 'z' then
            Insert (Output_List, Name);
         end if; -- Element (Name, 1) = 'z'
         for I in Input_Indices loop
            if not Contains (Target_Map, Source (I)) then
               Insert (Target_Map, Source (I), Target_Lists.Empty_Set);
            end if; -- Contains (Input_Map, Source (I))
            Insert (Target_Map (Source (I)), (Name, I));
         end loop; -- I in Input_Indices
         Insert (Source_Map, Name, Source);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   procedure Evaluate (Input_List : in Input_Lists.Map;
                       Gate_Store :  in out Gate_Stores.Map;
                       Target_Map : in Target_Maps.Map) is

      type Stimuli is record
         Logic_Drive : Logic_Drives;
         Gate_Name : Names;
         Input_Index : Input_Indices;
      end record; -- Stimuli

      package Q_I is new
        Ada.Containers.Synchronized_Queue_Interfaces (Stimuli);
      use Q_I;

      package Queues is new Ada.Containers.Unbounded_Synchronized_Queues (Q_I);
      use Queues;

      Queue : Queues.Queue;

      Stimulus, Output : Stimuli;

   begin -- Evaluate
      -- Enqueue input targets
      for I in Iterate (Input_List) loop
         Stimulus.Logic_Drive := Element (I);
         for T in Iterate (Target_Map (Key (I))) loop
            Stimulus.Gate_Name := Element (T).Name;
            Stimulus.Input_Index := Element (T).Input_Index;
            Queue.Enqueue (Stimulus);
         end loop; -- T in Iterate (Target_Map (Key (I)))
      end loop; -- I in Iterate (Input_List)
      -- Reset the IO state of all the gates
      for G in Iterate (Gate_Store) loop
         Gate_Store (G).Input_Array := (Undefined, Undefined);
         Gate_Store (G).Out_State := Undefined;
      end loop; -- G in Iterate (Gate_Store)
      while Queue.Current_Use > 0 loop
         Queue.Dequeue (Stimulus);
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
            if Contains (Target_Map, Stimulus.Gate_Name) then
               for T in Iterate (Target_Map (Stimulus.Gate_Name)) loop
                  Output.Gate_Name := Element (T).Name;
                  Output.Input_Index := Element (T).Input_Index;
                  Queue.Enqueue (Output);
               end loop; -- T in Iterate (Gate_Store (Stimulus.Gate_Name) ...
            end if; -- Contains (Targer_Map, Stimulus.Gate_Name)
         end if; --  G Gate_Store (Stimulus.Gate_Name).Input_Array  ...
      end loop; -- Queue.Current_Use > 0
   end Evaluate;

   function Input_Register (Input_List : in Input_Lists.Map;
                            Register : in Character)
                               return Output_Values is

      Result : Output_Values := 0;

   begin -- Input_Register
      for I in Iterate (Input_List) loop
         if Element (Key (I), 1) = Register and then Element (I) = 1 then
            Result := @ or
              Shift_Left (Mask, Bit_Numbers'Value (Slice (Key (I), 2, 3)));
         end if; -- Element (Key (I), 1) = Register and then Element (I) = 1
      end loop; -- I in Iterate (Input_List)
      return Result;
   end Input_Register;

   function Output_Register (Output_List : in Output_Lists.Set;
                             Gate_Store : in Gate_Stores.Map)
                             return Output_Values is

      Result : Output_Values := 0;

   begin -- Output_Register
      for O in Iterate (Output_List) loop
         if Gate_Store (Element (O)).Out_State = 1 then
            Result := @ or
              Shift_Left (Mask, Bit_Numbers'Value (Slice (Element (O), 2, 3)));
         end if; -- Gate_Store (Element (O)).Out_State = 1
      end loop; -- O in Iterate (Output_List)
      return Result;
   end Output_Register;

   function Build_Name (Base_Name : in character;
                        Number : in Bit_Numbers)
                           return Unbounded_String is

   begin -- Build_Name
      if Number < 10 then
         return To_Unbounded_String (Base_Name & "0" &
                                       Trim (Number'Img, Left));
      else
         return To_Unbounded_String (Base_Name & Trim (Number'Img, Left));
      end if; -- Number < 10
   end Build_Name;

   Procedure Repair (Input_List : in out Input_Lists.Map;
                     Gate_Store : in out Gate_Stores.Map;
                     Target_Map : in out Target_Maps.Map;
                     Output_List : in Output_Lists.Set;
                     Test : in Tests;
                     Source_Map : in out Source_Maps.Map;
                     Swapped_Gate_Set : out  Gate_Sets.Set) is

      procedure Tester (Input_List : in out Input_Lists.Map;
                        Gate_Store : in out Gate_Stores.Map;
                        Target_Map : in Target_Maps.Map;
                        Output_List : in Output_Lists.Set;
                        Test : in Tests;
                        Failed_Input_Set : out Input_Sets.Set;
                        Failed_Gate_Set : out Gate_Sets.Set) is

         -- Test input states of an adjacent pair of X and Y bits for each of 16
         -- input states and reports outputs which are not correct.

         function Find_MSB (Input_List : in Input_Lists.Map) return Bit_Numbers is

            MSB : Bit_Numbers := 0;

         begin -- Find_MSB
            for I in Iterate (Input_List) loop
               if Element (Key (I), 1) = 'x' and then
                 Bit_Numbers'Value (Slice (Key (I), 2, 3)) > MSB then
                  MSB := Bit_Numbers'Value (Slice (Key (I), 2, 3));
               end if; -- Element (Key (I), 1) = 'x' and then ...
            end loop; -- I in Iterate (Input_List)
            return MSB;
         end Find_MSB;

         procedure Set_Registers (N : in Bit_Numbers;
                                  Xn, Xn_1, Yn, Yn_1 : in Logic_Drives;
                                  MSB : in Bit_Numbers;
                                  Input_List : in out Input_Lists.Map) is

         begin -- Set_Registers
            for I in Bit_Numbers range 0 .. MSB loop
               Input_List (Build_Name ('x', I)) := 0;
               Input_List (Build_Name ('y', I)) := 0;
            end loop; -- N in Bit_Numbers range 0 .. MSB
            -- set x and y registers to 0;
            Input_List (Build_Name ('x', N)) := Xn;
            Input_List (Build_Name ('x', N + 1)) := Xn_1;
            Input_List (Build_Name ('y', N)) := Yn;
            Input_List (Build_Name ('y', N + 1)) := Yn_1;
            -- Test bits set
         end Set_Registers;

         MSB : constant Bit_Numbers := Find_MSB (Input_List);
         X, Y, Z, Failed_Bits : Output_Values;

      begin -- Tester
         Failed_Bits := 0;
         Clear (Failed_Input_Set);
         Clear (Failed_Gate_Set);
         for N in Bit_Numbers range 0 .. MSB - 1 loop
            for Yn_1 in Logic_Drives loop
               for Yn in Logic_Drives loop
                  for Xn_1 in Logic_Drives loop
                     for Xn in Logic_Drives loop
                        Set_Registers (N, Xn, Xn_1, Yn, Yn_1, MSB, Input_List);
                        Evaluate (Input_List, Gate_Store, Target_Map);
                        X := Input_Register (Input_List, 'x');
                        Y := Input_Register (Input_List, 'y');
                        Z := Test (X, Y);
                        if Z /= Output_Register (Output_List, Gate_Store) then
                           Include (Failed_Input_Set, (X, Y));
                           Failed_Bits := @ or
                             (Z xor Output_Register (Output_List, Gate_Store));
                        end if; -- Z /= Output_Register (Gate_Store)
                     end loop; -- Xn in Logic_Drives
                  end loop; -- Xn_1 in Logic_Drives
               end loop; -- Yn in Logic_Drives
            end loop; -- Yn_1 in Logic_Drives
         end loop; -- N in Bit_Numbers range 0 .. MSB - 1
         if Failed_Bits > 0 then
            for N in Bit_Numbers range 0 .. MSB + 1 loop
               if (Failed_Bits and Shift_Left (1, N)) /= 0 then
                  Include (Failed_Gate_Set, Build_Name ('z', N));
               end if; -- (Failed_Bits and Shift_Left (1, N)) /= 0
            end loop; -- N in Bit_Numbers range 0 .. MSB + 1
         end if; -- Failed_Bits = 0
      end Tester;

      procedure Subset_Tester (Input_List : in out Input_Lists.Map;
                               Gate_Store : in out Gate_Stores.Map;
                               Target_Map : in Target_Maps.Map;
                               Output_List : in Output_Lists.Set;
                               Test : in Tests;
                               Failed_Input_Set : in Input_Sets.Set;
                               Failed_Gate_Set : out Gate_Sets.Set) is

         procedure Set_Registers (Input_State : in Input_States;
                                  Input_List : in out Input_Lists.Map) is

         begin -- Set_Registers
            for I in Iterate (Input_List) loop
               case Element (Key (I), 1) is
               when 'x' =>
                  if (Input_State.X and
                        Shift_Left (Mask,
                                    Bit_Numbers'Value (Slice (Key (I), 2, 3))))
                      = 0 then
                     Input_List (I) := 0;
                  else
                     Input_List (I) := 1;
                  end if; -- (Input_State.X and ...
               when 'y' =>
                  if (Input_State.Y and
                        Shift_Left (Mask,
                                    Bit_Numbers'Value (Slice (Key (I), 2, 3))))
                      = 0 then
                     Input_List (I) := 0;
                  else
                     Input_List (I) := 1;
                  end if; -- (Input_State.Y and ...
                  when others =>
                     raise Program_Error with "Unknown input register '" &
                       Element (Key (I), 1) & "'";
               end case; -- Element (Key (I), 1)
            end loop; -- I in Iterate (Input_List)
         end Set_Registers;

         X, Y, Z, Failed_Bits : Output_Values;

      begin -- Subset_Tester
         Failed_Bits := 0;
         Clear (Failed_Gate_Set);
         for I in Iterate (Failed_Input_Set) loop
            Set_Registers (Element (I), Input_List);
            Evaluate (Input_List, Gate_Store, Target_Map);
            X := Input_Register (Input_List, 'x');
            Y := Input_Register (Input_List, 'y');
            Z := Test (X, Y);
            if Z /= Output_Register (Output_List, Gate_Store) then
               Failed_Bits := @ or
                 (Z xor Output_Register (Output_List, Gate_Store));
            end if; -- Z /= Output_Register (Gate_Store)
         end loop; -- I in Iterate (Failed_Input_Set)
         if Failed_Bits > 0 then
            for O in Iterate (Output_List) loop
               if (Failed_Bits and
                     Shift_Left (Mask,
                                 Bit_Numbers'Value (Slice (Element (O), 2, 3))))
                   /= 0 then
                  Include (Failed_Gate_Set, Element (O));
               end if; -- (Failed_Bits and Shift_Left (1, N)) /= 0
            end loop; -- N in Bit_Numbers range 0 .. MSB + 1
         end if; -- Failed_Bits = 0
      end Subset_Tester;

      procedure Find_Swapable (Gate_Store : in Gate_Stores.Map;
                               Source_Map : in Source_Maps.Map;
                               Failed_Gate_Set : in Gate_Sets.Set;
                               Swappable_Gate_Set : out Gate_Sets.Set) is

         -- Finds all gates which feed into any output that fails testing,
         -- searches from output to inputs. Inputs are not teeated as swapable.

         package Q_I is new
           Ada.Containers.Synchronized_Queue_Interfaces (Names);
         use Q_I;

         package Queues is new
           Ada.Containers.Unbounded_Synchronized_Queues (Q_I);
         use Queues;

         Name : Names;
         Gate_Q : Queues.Queue;

      begin -- Find_Swapable
         Clear (Swappable_Gate_Set);
         for F in Iterate (Failed_Gate_Set) loop
            Gate_Q.Enqueue (Element (F));
         end loop; -- F in Iterate (Failed_Gate_Set)
         while Gate_Q.Current_Use > 0 loop
            Gate_Q.Dequeue (Name);
            insert (Swappable_Gate_Set, Name);
            if Element (Source_Map (Name) (In_Left), 1) /= 'x' and
              Element (Source_Map (Name) (In_Left), 1) /= 'y' then
               Gate_Q.Enqueue (Source_Map (Name) (In_Left));
            end if; -- Element (Source_Map (Name) (In_Left), 1) /= 'x' and ...
            if Element (Source_Map (Name) (In_Right), 1) /= 'x' and
              Element (Source_Map (Name) (In_Right), 1) /= 'y' then
               Gate_Q.Enqueue (Source_Map (Name) (In_Right));
            end if; -- Element (Source_Map (Name) (In_Right), 1) /= 'x'
         end loop; -- Gate_Q.Current_Use > 0
      end Find_Swapable;

      procedure Swap (Swap_Pair : in Swap_Pairs;
                      Gate_Store : in out Gate_Stores.Map;
                      Target_Map : in out Target_Maps.Map;
                      Source_Map : in out Source_Maps.Map) is

         -- Swaps two Left and Right gate outputs. This is done by swapping
         -- inputs in the Target_Map and swapping the function in the
         -- Gate_Store;

         Left_Left_Input : constant Names
           := Source_Map (Swap_Pair.Left) (In_Left);
         Left_Right_Input : constant Names
           := Source_Map (Swap_Pair.Left) (In_Right);
         Right_Left_Input : constant Names
           := Source_Map (Swap_Pair.Right) (In_Left);
         Right_Right_Input : constant Names
           := Source_Map (Swap_Pair.Right) (In_Right);
         Temp : Gates;

      begin -- Swap
         -- Swap inputs, in Target_Map
         Exclude (Target_Map (Left_Left_Input), (Swap_Pair.Left, In_Left));
         Exclude (Target_Map (Left_Right_Input), (Swap_Pair.Left, In_Right));
         Exclude (Target_Map (Right_Left_Input), (Swap_Pair.Right, In_Left));
         Exclude (Target_Map (Right_Right_Input), (Swap_Pair.Right, In_Right));
         Insert (Target_Map (Left_Left_Input), (Swap_Pair.Right, In_Left));
         Insert (Target_Map (Left_Right_Input), (Swap_Pair.Right, In_Right));
         Insert (Target_Map (Right_Left_Input), (Swap_Pair.Left, In_Left));
         Insert (Target_Map (Right_Right_Input), (Swap_Pair.Left, In_Right));
         -- Swap inputs, in Source_Map
         Source_Map (Swap_Pair.Left) (In_Left) := Right_Left_Input;
         Source_Map (Swap_Pair.Left) (In_Right) := Right_Right_Input;
         Source_Map (Swap_Pair.Right) (In_Left) := Left_Left_Input;
         Source_Map (Swap_Pair.Right) (In_Right) := Left_Right_Input;
         -- Swap gate function in gate store
         Temp := Gate_Store (Swap_Pair.Left).Gate;
         Gate_Store (Swap_Pair.Left).Gate := Gate_Store (Swap_Pair.Right).Gate;
         Gate_Store (Swap_Pair.Right).Gate := Temp;
      end Swap;

      Failed_Input_Set : Input_Sets.Set;
      Failed_Gate_Set : Gate_Sets.Set;
      Test_Failed_Input_Set : Input_Sets.Set;
      Test_Failed_Gate_Set : Gate_Sets.Set;
      Swappable_Gate_Set : Gate_Sets.Set;

   begin -- Repair
      Tester (Input_List, Gate_Store, Target_Map, Output_List, Test,
              Failed_Input_Set, Failed_Gate_Set);
      Put_Line ("Failed_Input_Set:" & Failed_Input_Set'Img);
      Put_Line ("Failed_Gate_Set:" & Failed_Gate_Set'Img);
      Find_Swapable (Gate_Store, Source_Map, Failed_Gate_Set,
                     Swappable_Gate_Set);
      Clear (Swapped_Gate_Set);
      --  while not Is_Empty (Failed_Gate_Set) loop
      --  end loop; -- not Is_Empty (Failed_Gate_Set)
      Swap ((To_Unbounded_String ("z00"), To_Unbounded_String ("z05")), Gate_Store, Target_Map, Source_Map);
      Swap ((To_Unbounded_String ("z01"), To_Unbounded_String ("z02")), Gate_Store, Target_Map, Source_Map);
      Tester (Input_List, Gate_Store, Target_Map, Output_List, Test,
               Test_Failed_Input_Set, Test_Failed_Gate_Set);
      Put_Line ("Failed_Gate_Set:" & Test_Failed_Gate_Set'Img);
   end Repair;

   function Test_add (X, Y : in Output_Values) return Output_Values is
      (X + Y);

   function Test_and (X, Y : in Output_Values) return Output_Values is
      (X and Y);

   Input_List : Input_Lists.Map;
   Gate_Store : Gate_Stores.Map;
   Target_Map : Target_Maps.Map;
   Output_List : Output_Lists.Set;
   Source_Map : Source_Maps.Map;
   Failed_Input_Set : Input_Sets.Set;
   Failed_Gate_Set, Swapped_Gate_Set : Gate_Sets.Set;

begin -- December_24
   Read_Input (Input_List, Gate_Store, Target_Map, Output_List, Source_Map);
   Evaluate (Input_List, Gate_Store, Target_Map);
   Put_Line ("Part one:" & Output_Register (Output_List, Gate_Store)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   if (Argument_Count >= 2 and then Argument (2) = "2") or
     Argument_Count = 0 then
      if Argument_Count = 3 and then Argument (3) = "and" then
         Repair (Input_List, Gate_Store, Target_Map, Output_List,
                 Test_and'Access, Source_Map, Swapped_Gate_Set);
      else
         Repair (Input_List, Gate_Store, Target_Map, Output_List,
                 Test_add'Access, Source_Map, Swapped_Gate_Set);
      end if; -- Argument_Count = 3 and then Argument (2) = "and"
      Put_Line ("Part two:" & Swapped_Gate_Set'Img);
      DJH.Execution_Time.Put_CPU_Time;
   end if; -- (Argument_Count >= 2 and then Argument (2) = "2") or ..
end December_24;
