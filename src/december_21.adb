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
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_21 is

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
   Door_Check : constant array (Door_X, Door_Y) of Character :=
     (-2 => ( 3 => '7', 2 => '4', 1 => '1', 0 => 'X'),
      -1 => ( 3 => '8', 2 => '5', 1 => '2', 0 => '0'),
      0 => ( 3 => '9', 2 => '6', 1 => '3', 0 => 'A'));

   subtype Door_Coordinates is Coordinates with
     Dynamic_Predicate=> Door_Coordinates.X in Door_X and
     Door_Coordinates.Y in Door_Y and
     not (Door_Coordinates.X = -2 and Door_Coordinates.Y = 0); -- Door_States;

   type Door_States is record
      Position : Door_Coordinates := Origin;
      Button : Door_Buttons := 'A';
   end record; -- Door_States

   type Code_Elements is record
      Code : Unbounded_String;
      Sequence_Length : Natural := 0;
   end record; -- Code_Element

   package Code_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Code_Elements);
   use Code_lists;

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
         Get_Line (Input_File, Code_Element.Code);
         Append (Code_List, Code_Element);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   function Door_Press (Door_Button : in Door_Buttons;
                        State : in out Door_States) return Coordinates is

      -- Returns difference between the current state and the required button

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

      Result : Coordinates;

   begin -- Door_Press
      Result :=
        Door_Coordinate (Door_Button) - Door_Coordinate (State.Button);
      State.Button := Door_Button;
      return Result;
   end Door_Press;

   subtype Robot_Buttons is Character with
     Static_Predicate => Robot_Buttons in '^' | 'v' | '<' | '>' | 'A';

   -- X -2  -1   0    Y
   --      +---+---+
   --      | ^ | A |  0
   --  +---+---+---+
   --  | < | v | > | -1
   --  +---+---+---+

   subtype Robot_X is Ordinates range -2 .. 0;
   subtype Robot_Y is Ordinates range -1 .. 0;

   Robot_Check : constant array (Robot_X, Robot_Y) of Character :=
     (-2 => (0 => 'X', -1 => '<'),
      -1 => (0 => '^', -1 => 'v'),
      0 => (0 => 'A', -1 => '>'));

   subtype Remote_Coordinates is Coordinates with
     Dynamic_Predicate => Remote_Coordinates.X in Robot_X and
     Remote_Coordinates.Y in Robot_Y and
     not (Remote_Coordinates.X = -2 and Remote_Coordinates.Y = 0);

   type Robot_States is record
      Position : Remote_Coordinates := Origin;
      Button : Robot_Buttons := 'A';
   end record; -- Robot_States

   function Remote_Press (Robot_Button : in Robot_Buttons;
                          State : in out Robot_States) return Coordinates is

      function Remote_Coordinate (Robot_Button : in Robot_Buttons)
                                  return Remote_Coordinates is

         -- Returns the absolute coordinates of the robot remote control buttons

      begin -- Remote_Coordinate
         case Robot_Button is
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
         end case; -- Robot_Button
      end Remote_Coordinate;

      Result : Coordinates;

   begin -- Remote_Press
      Result :=
        Remote_Coordinate (Robot_Button) - Remote_Coordinate (State.Button);
      State.Button := Robot_Button;
      return Result;
   end Remote_Press;

   procedure Sequence (Code_Element : in out Code_Elements) is

      function Door_Sequence (Key : in Door_Buttons;
                              State : in out Door_States)
                              return Unbounded_String is

         function To_Presses (Difference : in Coordinates;
                              Update : in out Door_Coordinates)
                              return Unbounded_String is

            -- Has side effecr of updating coordinates

            Result : Unbounded_String := Null_Unbounded_String;

         begin -- To_Presses
            for R in Ordinates range 1 .. Difference.x loop
               Result := @ & '>';
               Update := @ + (1, 0);
            end loop; -- R in Ordinates range 1 .. Difference.x
            for D in Ordinates range Difference.Y .. -1 loop
               Result := @ & 'v';
               Update := @ + (0, -1);
            end loop; -- D in Ordinates range Difference.Y .. -1
            for U in Ordinates range 1 .. Difference.Y loop
               Result := @ & '^';
               Update := @ + (0, 1);
            end loop; -- U in Ordinates range 1 .. Difference.Y
            for L in Ordinates range Difference.X .. -1 loop
               Result := @ & '<';
               Update := @ + (-1, 0);
            end loop; -- L in Ordinates range Difference.X .. -1
            return Result & 'A';
         end To_Presses;

         Result : Unbounded_String;

      begin -- Door_Sequence
         Result := To_Presses (Door_Press (Key, State), State.Position);
         if State.Button /= Door_Check (State.Position.X, State.Position.Y) then
            raise Program_Error with "Mismatched buttons expected '"
              & State.Button & "' was '" &
              Door_Check (State.Position.X, State.Position.Y) & "'";
         end if; -- State.Button /= Door_Check (State.Position.X, ...
         return Result;
      end Door_Sequence;

      function Robot_Sequence (Key : in Robot_Buttons;
                               State : in out Robot_States)
                               return Unbounded_String is

         function To_Presses (Difference : in Coordinates;
                              Update : in out Remote_Coordinates)
                              return Unbounded_String is

            -- Has side effecr of updating coordinates

            Result : Unbounded_String := Null_Unbounded_String;

         begin -- To_Presses
            for R in Ordinates range 1 .. Difference.x loop
               Result := @ & '>';
               Update := @ + (1, 0);
            end loop; -- R in Ordinates range 1 .. Difference.x
            for U in Ordinates range 1 .. Difference.Y loop
               Result := @ & '^';
               Update := @ + (0, 1);
            end loop; -- U in Ordinates range Difference.Y .. -1
            for D in Ordinates range Difference.Y .. -1 loop
               Result := @ & 'v';
               Update := @ + (0, -1);
            end loop; -- D in Ordinates range 1 .. Difference.Y
            for L in Ordinates range Difference.X .. -1 loop
               Result := @ & '<';
               Update := @ + (-1, 0);
            end loop; -- L in Ordinates range Difference.X .. -1
            return Result & 'A';
         end To_Presses;

        Result : Unbounded_String;

      begin -- Robot_Sequence
         Result := To_Presses(Remote_Press (Key, State), State.Position);
         if State.Button /=
           Robot_Check (State.Position.X, State.Position.Y) then
            raise Program_Error with "Mismatched buttons expected '"
              & State.Button & "' was '" &
              Door_Check (State.Position.X, State.Position.Y) & "'";
         end if; -- State.Button /= ...
         return Result;
      end Robot_Sequence;

      function Verify_Door (Code : in Unbounded_String)
                            return Unbounded_String is

         Door : Door_Coordinates := Origin;
         Result : Unbounded_String := Null_Unbounded_String;
         Press : Boolean;
         Command : Robot_Buttons;

      begin -- Verify_Door
         Door := Origin;
         for D in Positive range 1 .. Length (Code) loop
            Command := Element (Code, D);
            Press := False;
            case Command is
               when '^' =>
                 Door := @ + (0, 1);
               when 'v' =>
                 Door := @ + (0, -1);
               when '<' =>
                 Door := @ + (-1, 0);
               when '>' =>
                 Door := @ + (1, 0);
               when 'A' =>
                 Press := True;
            end case; -- Command
            Put (Command & " => " & Door_Check (Door.X, Door.Y));
            if Press then
               Append (Result, Door_Check (Door.X, Door.Y));
               Put_Line (" Press");
            else
               New_Line;
            end if; -- Press
         end loop; -- D
         return Result;
      end Verify_Door;

      function Verify_Robot (Code : in Unbounded_String)
                             return Unbounded_String is

         Remote : Remote_Coordinates;
         Press : Boolean;
         Result : Unbounded_String := Null_Unbounded_String;
         Command : Robot_Buttons;

      begin -- Verify_Robot
         Remote := Origin;
         for D in Positive range 1 .. Length (Code) loop
            Command := Element (Code, D);
            Press := False;
            case Command is
               when '^' =>
                 Remote := @ + (0, 1);
               when 'v' =>
                 Remote := @ + (0, -1);
               when '<' =>
                 Remote := @ + (-1, 0);
               when '>' =>
                 Remote := @ + (1, 0);
               when 'A' =>
                 Press := True;
            end case; -- Command
            Put (Command & " => " & Robot_Check (Remote.X, Remote.Y));
            if Press then
               Append (Result, Robot_Check (Remote.X, Remote.Y));
               Put_Line (" Press");
            else
               New_Line;
            end if; -- Press
         end loop; -- D
         return Result;
      end Verify_Robot;

      Door_Robot : Door_States := (Origin, 'A');
      Vacuum_Robot, Frozen_Robot : Robot_States := (Origin, 'A');
      In_Keys, Out_Keys : Unbounded_String;

   begin -- Sequence
      Out_Keys := Null_Unbounded_String;
      Put_Line ("Code " & Code_Element.Code);
      for K in Positive range 1 .. Length (Code_Element.Code) loop
         Out_Keys := @ &
           Door_Sequence (Element (Code_Element.Code, K), Door_Robot);
      end loop; -- K in Positive range 1 .. Length (In_Keys)
      Put_Line (Out_Keys);
      if Code_Element.Code /= Verify_Door (Out_Keys) then
         raise Program_Error with "Doot missmatch";
      end if; -- Code_Element.Code /= Verify_Door (Out_Keys)
      In_Keys := Out_Keys;
      Out_Keys := Null_Unbounded_String;
      for K in Positive range 1 .. Length (In_Keys) loop
         Out_Keys := @ & Robot_Sequence (Element (In_Keys, K), Vacuum_Robot);
      end loop; -- K in Positive range 1 .. Length (In_Keys)
      Put_Line (Out_Keys);
      if In_Keys /= Verify_Robot (Out_Keys) then
         raise Program_Error with "Vacuum robot missmatch";
      end if; -- In_Keys /= Verify_Robot (Out_Keys)
      In_Keys := Out_Keys;
      Out_Keys := Null_Unbounded_String;
      for K in Positive range 1 .. Length (In_Keys) loop
         Out_Keys := @ & Robot_Sequence (Element (In_Keys, K), Frozen_Robot);
      end loop; -- K in Positive range 1 .. Length (In_Keys)
      Put_Line (Out_Keys);
      if In_Keys /= Verify_Robot (Out_Keys) then
         raise Program_Error with "Frozen robot missmatch";
      end if; -- In_Keys /= Verify_Robot (Out_Keys)
      Code_Element.Sequence_Length := Length (Out_Keys);
      Out_Keys := To_Unbounded_String
        ("<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A");
      Put_Line ("Example 379A");
      Put_Line (Out_Keys);
      In_Keys := Verify_Robot (Out_Keys);
      Out_Keys := In_Keys;
      Put_Line (Out_Keys);
      In_Keys := Verify_Robot (Out_Keys);
      Out_Keys := In_Keys;
      Put_Line (Out_Keys);
      In_Keys := Verify_Door (Out_Keys);
      Put_Line (In_Keys);
   end Sequence;

   function Complexity (Code_Element : in Code_Elements) return Natural is

      First : Positive;
      Last, Code_Number : Natural;

   begin -- Complexity
      Find_Token (Code_Element.Code, Decimal_Digit_Set, Inside, First, Last);
      Code_Number := Natural'Value (Slice (Code_Element.Code, First, Last));
      Put_Line ("Complexity" & Code_Number'Img & Code_Element.Sequence_Length'Img);
      return Code_Number * Code_Element.Sequence_Length;
   end Complexity;

   Code_List : Code_Lists.List;
   Sum : Natural := 0;

begin -- December_21
   Read_Input (Code_List);
   for C in Iterate (Code_List) loop
      Sequence (Code_List (C));
      Sum := Sum + Complexity (Element (C));
   end loop; -- C in Iterate (Code_List)
   Put_Line ("Part one:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:");
   DJH.Execution_Time.Put_CPU_Time;
end December_21;
