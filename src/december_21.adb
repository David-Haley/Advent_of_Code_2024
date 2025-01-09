with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_21 is

   subtype Robot_Indices is Positive range 1 .. 25;

   subtype Ordinates is Integer;
   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   function "+" (Left, Right : Coordinates) return Coordinates is
     ((Left.X + Right.X, Left.Y + Right.Y));

   function "-" (Left, Right : Coordinates) return Coordinates is
     ((Left.X - Right.X, Left.Y - Right.Y));

   Origin : constant Coordinates := (0, 0);

   subtype Door_Buttons is Character with
     Static_Predicate => Door_Buttons in 'A' | '0' .. '9';

   -- Door Pad
   -- X -2  -1   0    Y
   --  +---+---+---+
   --  | 7 | 8 | 9 | 3
   --  +---+---+---+
   --  | 4 | 5 | 6 | 2
   --  +---+---+---+
   --  | 1 | 2 | 3 | 1
   --  +---+---+---+
   --      | 0 | A | 0
   --      +---+---+

   subtype Door_X is Ordinates range -2 .. 0;
   subtype Door_Y is Ordinates range 0 .. 3;

   Illegal_Door : exception;

   type Door_Coordinates is new Coordinates;

   function "+" (Left : Door_Coordinates;
                 Right : Coordinates) return Door_Coordinates is

      Result : Door_Coordinates;

   begin -- "+"
      Result.X := Left.X + Right.X;
      Result.Y := Left.Y + Right.Y;
      -- tried to use a dynamic predicate to enforce this but could not make
      -- it work
      if not (Result.X in Door_X) then
         raise Illegal_Door with "Out of range X";
      end if; -- not (Result.X in Door_X)
      if not (Result.Y in Door_Y) then
         raise Illegal_Door with "Out of range Y";
      end if; -- not (Result.Y in Door_Y)
      if Result.X = -2 and Result.Y = 0 then
         raise Illegal_Door with "No button";
      end if; -- Result.X = -2 and Result.Y = 0
      return Result;
   end "+";

   function "-" (Left, Right : Door_Coordinates) return Coordinates is
     ((Left.X - Right.X, Left.Y - Right.Y));

   type Door_States is record
      Position : Door_Coordinates := Door_Coordinates (Origin);
      Button : Door_Buttons := 'A';
   end record; -- Door_States

   subtype Remote_Buttons is Character with
     Static_Predicate => Remote_Buttons in '^' | 'v' | '<' | '>' | 'A';

   -- X -2  -1   0    Y
   --      +---+---+
   --      | ^ | A |  0
   --  +---+---+---+
   --  | < | v | > | -1
   --  +---+---+---+

   subtype Remote_X is Ordinates range -2 .. 0;
   subtype Remote_Y is Ordinates range -1 .. 0;

   Illegal_Remote : exception;

   type Remote_Coordinates is new Coordinates;

   function "+" (Left : Remote_Coordinates;
                 Right : Coordinates) return Remote_Coordinates is

      Result : Remote_Coordinates;

   begin -- "+"
      Result.X := Left.X + Right.X;
      Result.Y := Left.Y + Right.Y;
      -- tried to use a dynamic predicate to enforce this but could not make
      -- it work
      if not (Result.X in Remote_X) then
         raise Illegal_Remote with "Out of range X";
      end if; -- not (Result.X in Remote_X)
      if not (Result.Y in Remote_Y) then
         raise Illegal_Remote with "Out of range Y";
      end if; -- not (Result.Y in Remote_Y)
      if Result.X = -2 and Result.Y = 0 then
         raise Illegal_Remote with "No button";
      end if; -- Result.X = -2 and Result.Y = 0
      return Result;
   end "+";

   function "-" (Left, Right : Remote_Coordinates) return Coordinates is
     ((Left.X - Right.X, Left.Y - Right.Y));

   type Robot_States is record
      Position : Remote_Coordinates := Remote_Coordinates (Origin);
      Button : Remote_Buttons := 'A';
   end record; -- Robot_States

   type First_Directions is (Horizintal, Vertical);

   type Button_Map_Keys is record
      Start, Finish : Character;
   end record; -- Button_Map_Keys

   function "<" (Left, Right : Button_Map_Keys) return Boolean is
     (Left.Start < Right.Start or
        (Left.Start = Right.Start and Left.Finish < Right.Finish));

   subtype Codes is Unbounded_String;

   End_Command : constant Codes := To_Unbounded_String ("End_Command");
   Stop_Command : constant Codes := To_Unbounded_String ("Stop_Command");

   type Button_Map_Elements is array (First_Directions) of Codes;
   -- Zero length code indicates illegal move.

   package Button_Maps is new
     Ada.Containers.Ordered_Maps (Button_Map_Keys, Button_Map_Elements);
   use Button_Maps;

   type Code_Elements is record
      Door_Code : Codes; -- The required door code
      My_Code : Codes := Null_Unbounded_String; -- Code required to generate
   end record; -- Code_Element

   package Code_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Code_Elements);
   use Code_lists;

   package Command_QI is new
     Ada.Containers.Synchronized_Queue_Interfaces (Codes);
   use Command_QI;

   package Command_Queues is new
     Ada.Containers.Unbounded_Synchronized_Queues (Command_QI);
   use Command_Queues;

   type Command_Queue_Access is access all Command_Queues.Queue;

   type Build_Elements is record
      Index : Natural;
      Code : Codes;
   end record; -- Build_Elements;

   Start_Build : constant Build_Elements := (Index => 0,
                                             Code => Null_Unbounded_String);

   package Build_QI is new
     Ada.Containers.Synchronized_Queue_Interfaces (Build_Elements);
   use Build_QI;

   package Build_Queues is new
     Ada.Containers.Unbounded_Synchronized_Queues (Build_QI);
   use Build_Queues;

   task type Do_Robot is
      entry Start (Remote_Map_E : in Button_Maps.Map;
                   Command_In_E, Command_out_E : in Command_Queue_Access);
   end Do_Robot;

   procedure Read_Input (Code_List : out Code_Lists.List) is

      Input_File : File_Type;
      Code_Element : Code_Elements;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_21.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Clear (Code_List);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Code_Element.Door_Code);
         Append (Code_List, Code_Element);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   procedure Build_Door_Map (Door_Map : out Button_Maps.Map) is

      -- Assumes that either stepping vertically or stepping horizontally first
      -- will always be better than alternating between vertically and
      -- horizontally. Note that locally the same number of steps is required
      -- irrespective of the direct path used but this may not be the case when
      -- global optomisation is applied.

      function Door_Coordinate (Door_Button : in Door_Buttons)
                                 return Door_Coordinates is

         -- Returns the absolute coordinates of the button

      begin -- Door_Coordinate
         case Door_Button is
            when 'A' =>
               return (0, 0);
            when '0' =>
               return (-1, 0);
            when '1' =>
               return (-2, 1);
            when '2' =>
               return (-1, 1);
            when '3' =>
               return (0, 1);
            when '4' =>
               return (-2, 2);
            when '5' =>
               return (-1, 2);
            when '6' =>
               return (0, 2);
            when '7' =>
               return (-2, 3);
            when '8' =>
               return (-1, 3);
            when '9' =>
               return (0, 3);
         end case; -- Door_Button
      end Door_Coordinate;

      Door_Code : Button_Map_Elements;
      Difference : Coordinates;
      Dir : First_Directions;
      Door_Robot : Door_States;

   begin -- Build_Door_Map
      Clear (Door_Map);
      for S in Door_Buttons loop
         for F in Door_Buttons loop
            Door_Code := (others => Null_Unbounded_String);
            Difference := Door_Coordinate (F) - Door_Coordinate (S);
            if Difference.X /= 0 then
               Dir := Horizintal;
               Door_Robot := (Door_Coordinate (S), S);
               begin -- H exception block
                  for L in Ordinates range Difference.X .. -1 loop
                     Door_Code (Dir) := @ & '<';
                     Door_Robot.Position := @ + (-1, 0);
                  end loop; -- L in Ordinates range Difference.X .. -1
                  for R in Ordinates range 1 .. Difference.X loop
                     Door_Code (Dir) := @ & '>';
                     Door_Robot.Position := @ + (1, 0);
                  end loop; -- R in Ordinates range 1 .. Difference.X
                  for U in Ordinates range 1 .. Difference.Y loop
                     Door_Code (Dir) := @ & '^';
                     Door_Robot.Position := @ + (0, 1);
                  end loop; -- U in Ordinates range 1 .. Difference.Y
                  for D in Ordinates range Difference.Y .. -1 loop
                     Door_Code (Dir) := @ & 'v';
                     Door_Robot.Position := @ + (0, -1);
                  end loop; -- D in Ordinates range Difference.Y .. -1
                  Door_Code (Dir) := @ & 'A';
               exception
                  when Illegal_Door =>
                     Door_Code (Dir) := Null_Unbounded_String;
               end; -- H exception block
            end if; -- Difference.X /= 0
            if Difference.Y /= 0 then
               Dir := Vertical;
               Door_Robot := (Door_Coordinate (S), S);
               begin -- V exception block
                  for U in Ordinates range 1 .. Difference.Y loop
                     Door_Code (Dir) := @ & '^';
                     Door_Robot.Position := @ + (0, 1);
                  end loop; -- U in Ordinates range 1 .. Difference.Y
                  for D in Ordinates range Difference.Y .. -1 loop
                     Door_Code (Dir) := @ & 'v';
                     Door_Robot.Position := @ + (0, -1);
                  end loop; -- D in Ordinates range Difference.Y .. -1
                  for L in Ordinates range Difference.X .. -1 loop
                     Door_Code (Dir) := @ & '<';
                     Door_Robot.Position := @ + (-1, 0);
                  end loop; -- L in Ordinates range Difference.X .. -1
                  for R in Ordinates range 1 .. Difference.X loop
                     Door_Code (Dir) := @ & '>';
                     Door_Robot.Position := @ + (1, 0);
                  end loop; -- R in Ordinates range 1 .. Difference.X
                  Door_Code (Dir) := @ & 'A';
               exception
                  when Illegal_Door =>
                     Door_Code (Dir) := Null_Unbounded_String;
               end; -- V exception block
            end if; -- Difference.Y /= 0;
            if S = F then
               Door_Code (Horizintal) := To_Unbounded_String ("A");
               -- same button pressed
            end if; -- S = F
            Insert (Door_Map, (S, F), Door_Code);
         end loop; -- F in Door_Buttons
      end loop; -- S in Door_Buttons
   end Build_Door_Map;

   procedure Build_Remote_Map (Remote_Map : out Button_Maps.Map) is

      -- Assumes that either stepping vertically or stepping horizontally first
      -- will always be better than alternating between vertically and
      -- horizontally. Note that locally the same number of steps is required
      -- irrespective of the direct path used but this may not be the case when
      -- global optomisation is applied.

      function Remote_Coordinate (Remote_Button : in Remote_Buttons)
                                  return Remote_Coordinates is

         -- Returns the absolute coordinates of the robot remote control buttons

      begin -- Remote_Coordinate
         case Remote_Button is
            when 'A' =>
               return (0, 0);
            when '^' =>
               return (-1, 0);
            when '>' =>
               return (0, -1);
            when 'v' =>
               return (-1, -1);
            when '<' =>
               return (-2, -1);
         end case; -- Remote_Button
      end Remote_Coordinate;

      Remote_Code : Button_Map_Elements;
      Difference : Coordinates;
      Dir : First_Directions;
      Remote_Robot : Robot_States;

   begin -- Build_Remote_Map
      Clear (Remote_Map);
      for S in Remote_Buttons loop
         for F in Remote_Buttons loop
            Remote_Code := (others => Null_Unbounded_String);
            Difference := Remote_Coordinate (F) - Remote_Coordinate (S);
            if Difference.X /= 0 then
               Dir := Horizintal;
               Remote_Robot := (Remote_Coordinate (S), S);
               begin -- H exception block
                  for L in Ordinates range Difference.X .. -1 loop
                     Remote_Code (Dir) := @ & '<';
                     Remote_Robot.Position := @ + (-1, 0);
                  end loop; -- L in Ordinates range Difference.X .. -1
                  for R in Ordinates range 1 .. Difference.X loop
                     Remote_Code (Dir) := @ & '>';
                     Remote_Robot.Position := @ + (1, 0);
                  end loop; -- R in Ordinates range 1 .. Difference.X
                  for U in Ordinates range 1 .. Difference.Y loop
                     Remote_Code (Dir) := @ & '^';
                     Remote_Robot.Position := @ + (0, 1);
                  end loop; -- U in Ordinates range 1 .. Difference.Y
                  for D in Ordinates range Difference.Y .. -1 loop
                     Remote_Code (Dir) := @ & 'v';
                     Remote_Robot.Position := @ + (0, -1);
                  end loop; -- D in Ordinates range Difference.Y .. -1
                  Remote_Code (Dir) := @ & 'A';
               exception
                  when Illegal_Remote =>
                     Remote_Code (Dir) := Null_Unbounded_String;
               end; -- H exception block
            end if; -- Difference.X /= 0
            if Difference.Y /= 0 then
               Dir := Vertical;
               Remote_Robot := (Remote_Coordinate (S), S);
               begin -- V exception block
                  for U in Ordinates range 1 .. Difference.Y loop
                     Remote_Code (Dir) := @ & '^';
                     Remote_Robot.Position := @ + (0, 1);
                  end loop; -- U in Ordinates range 1 .. Difference.Y
                  for D in Ordinates range Difference.Y .. -1 loop
                     Remote_Code (Dir) := @ & 'v';
                     Remote_Robot.Position := @ + (0, -1);
                  end loop; -- D in Ordinates range Difference.Y .. -1
                  for L in Ordinates range Difference.X .. -1 loop
                     Remote_Code (Dir) := @ & '<';
                     Remote_Robot.Position := @ + (-1, 0);
                  end loop; -- L in Ordinates range Difference.X .. -1
                  for R in Ordinates range 1 .. Difference.X loop
                     Remote_Code (Dir) := @ & '>';
                     Remote_Robot.Position := @ + (1, 0);
                  end loop; -- R in Ordinates range 1 .. Difference.X
                  Remote_Code (Dir) := @ & 'A';
               exception
                  when Illegal_Remote =>
                     Remote_Code (Dir) := Null_Unbounded_String;
               end; -- V exception block
            end if; -- Difference.Y /= 0
            if S = F then
               Remote_Code (Horizintal) := To_Unbounded_String ("A");
               -- same button pressed
            end if; -- S = F
            Insert (Remote_Map, (S, F), Remote_Code);
         end loop; -- F in Remote_Buttons
      end loop; -- S in Remote_Buttons
   end Build_Remote_Map;

   procedure Sequence (Door_Map : in Button_Maps.Map;
                       Door_Command_Q, Human_Command_Q : in out
                         Command_Queues.Queue;
                       Code_Element : in out Code_Elements) is

      Build_Q : Build_Queues.Queue;
      Current, Next : Build_Elements;
      Button_Map_Key : Button_Map_Keys;
      My_Code : Codes;

   begin -- Sequence
      Build_Q.Enqueue (Start_Build);
      while Build_Q.Current_Use > 0 loop
         Build_Q.Dequeue (Current);
         if Current.Index >= Length (Code_Element.Door_Code) then
            Door_Command_Q.Enqueue (Current.Code);
         else
            Next.Index := Current.Index + 1;
            if Current.Index = 0 then
               Button_Map_Key.Start := 'A';
            else
               Button_Map_Key.Start :=
                 Element (Code_Element.Door_Code, Current.Index);
            end If; -- Current.Index = 0
            Button_Map_Key.Finish :=
              Element (Code_Element.Door_Code, Next.Index);
            for F in First_Directions loop
               if Length (Door_Map (Button_Map_Key) (F)) > 0 then
                  Next.Code := Current.Code & Door_Map (Button_Map_Key) (F);
                  Build_Q.Enqueue (Next);
               end if; -- Length (Door_Map (Button_Map_Key) (F)) > 0
            end loop; -- F in First_Directions
         end if; -- Current.Index >= Length (Code_Element.Door_Code)
      end loop; -- Build_Q.Current_Use > 0
      Door_Command_Q.Enqueue (End_Command);
      Code_Element.My_Code := Null_Unbounded_String;
      loop -- process one command sequence
         Human_Command_Q.Dequeue (My_Code);
         exit when My_Code = End_Command;
         if Length (My_Code) < Length (Code_Element.My_Code) or else
           Length (Code_Element.My_Code) = 0 then
            Code_Element.My_Code := My_Code;
         end if; -- Length (My_Code) < Length (Code_Element.My_Code) or else ...
      end loop; -- process one command sequence
   end Sequence;

   task body Do_Robot is

      Build_Q : Build_Queues.Queue;
      Current, Next : Build_Elements;
      Remote_Map_Key : Button_Map_Keys;
      Code : Codes;
      Remote_Map : Button_Maps.Map;
      Command_In, Command_out : Command_Queue_Access;

   begin -- Do_Robot
      accept Start (Remote_Map_E : in Button_Maps.Map;
                    Command_In_E, Command_out_E : in Command_Queue_Access) do
         Remote_Map := Remote_Map_E;
         Command_In := Command_In_E;
         Command_Out := Command_out_E;
      end Start;
      loop -- process one command sequence
         Command_In.Dequeue (Code);
         if Code = End_Command then
            Command_Out.Enqueue (End_Command);
         elsif  Code = Stop_Command then
            Command_Out.Enqueue (Stop_Command);
            exit;
         else
            Build_Q.Enqueue (Start_Build);
            while Build_Q.Current_Use > 0 loop
               Build_Q.Dequeue (Current);
               if Current.Index >= Length (Code) then
                  Command_Out.Enqueue (Current.Code);
               else
                  Next.Index := Current.Index + 1;
                  if Current.Index = 0 then
                     Remote_Map_Key.Start := 'A';
                  else
                     Remote_Map_Key.Start := Element (Code, Current.Index);
                  end If; -- Current.Index = 0
                  Remote_Map_Key.Finish := Element (Code, Next.Index);
                  for F in First_Directions loop
                     if Length (Remote_Map (Remote_Map_Key) (F)) > 0 then
                        Next.Code := Current.Code &
                          Remote_Map (Remote_Map_Key) (F);
                        Build_Q.Enqueue (Next);
                     end if; -- Length (Remote_Map (Button_Map_Key) (F)) > 0
                  end loop; -- F in First_Directions
               end if; -- Current.Index >= Length (Code)
            end loop; -- Build_Q.Current_Use > 0
         end if; -- Code = End_Command
      end loop; -- process one command sequence
   end Do_Robot;

   function Complexity (Code_Element : in Code_Elements) return Natural is

      First : Positive;
      Last, Code_Number, Sequence_Length : Natural;

   begin -- Complexity
      Find_Token (Code_Element.Door_Code, Decimal_Digit_Set, Inside, First,
                  Last);
      Code_Number := Natural'Value (Slice (Code_Element.Door_Code, First,
                                    Last));
      Sequence_Length := Length (Code_Element.My_Code);
      return Code_Number * Sequence_Length;
   end Complexity;

   Code_List : Code_Lists.List;
   Door_Map, Remote_Map : Button_Maps.Map;
   Door_Command_Q : aliased Command_Queues.Queue;
   Robot_Array : array (Robot_Indices) of Do_Robot;
   Link_Array : array (Robot_Indices) of aliased Command_Queues.Queue;
   Sum : Natural := 0;

begin -- December_21
   Read_Input (Code_List);
   Build_Door_Map (Door_Map);
   Build_Remote_Map (Remote_Map);
   Robot_Array (1).Start (Remote_Map, Door_Command_Q'Access,
                          Link_Array (1)'Access);
   Robot_Array (2).Start (Remote_Map, Link_Array (1)'Access,
                          Link_Array (2)'Access);
   for C in Iterate (Code_List) loop
      Sequence (Door_Map, Door_Command_Q, Link_Array (2), Code_List (C));
      Sum := @ + Complexity (Element (C));
   end loop; -- C in Iterate (Code_List)
   Put_Line ("Part one:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Sum := 0;
   for R in Robot_Indices range 3 .. Robot_Indices'Last loop
      Robot_Array (R).Start (Remote_Map, Link_Array (R - 1)'Access,
                             Link_Array (R)'Access);
   end loop; -- R in Robot_Indices range 3 .. Robot_Indices'Last
   for C in Iterate (Code_List) loop
      Sequence (Door_Map, Door_Command_Q, Link_Array (Robot_Indices'Last),
                Code_List (C));
      Sum := @ + Complexity (Element (C));
   end loop; -- C in Iterate (Code_List)
   Put_Line ("Part two:");
   DJH.Execution_Time.Put_CPU_Time;
   Door_Command_Q.Enqueue (Stop_Command);
end December_21;
