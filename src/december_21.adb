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
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_21 is

   subtype Robot_Indices is Positive range 1 .. 26;

   subtype Ordinates is Integer;
   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   subtype Door_Buttons is Character with
     Static_Predicate => Door_Buttons in 'A' | '0' .. '9';

   subtype Door_X is Ordinates range -2 .. 0;
   subtype Door_Y is Ordinates range 0 .. 3;

   Illegal_Door : exception;

   type Door_Coordinates is record
      X : Door_X;
      Y : Door_Y;
   end record; -- Door_Coordinates

   type Door_States is record
      Position : Door_Coordinates := (0, 0);
      Button : Door_Buttons := 'A';
   end record; -- Door_States

   subtype Remote_Buttons is Character with
     Static_Predicate => Remote_Buttons in '^' | 'v' | '<' | '>' | 'A';

   subtype Remote_X is Ordinates range -2 .. 0;
   subtype Remote_Y is Ordinates range -1 .. 0;

   Illegal_Remote : exception;

   type Remote_Coordinates is record
      X : Remote_X;
      Y : Remote_Y;
   end record; -- Remote_Coordinates

   type Button_Map_Keys is record
      Start, Finish : Character;
   end record; -- Button_Map_Keys

   function "<" (Left, Right : Button_Map_Keys) return Boolean is
     (Left.Start < Right.Start or
        (Left.Start = Right.Start and Left.Finish < Right.Finish));

   subtype Codes is Unbounded_String;

   Empty : constant Codes := Null_Unbounded_String;

   package Button_Maps is new
     Ada.Containers.Ordered_Maps (Button_Map_Keys, Codes);
   use Button_Maps;

   subtype Big_Natural is Long_Long_Integer
   range 0 .. Long_Long_Integer'Last;

   package Frequency_Maps is new
     Ada.Containers.Ordered_Maps (Codes, Big_Natural);
   use Frequency_Maps;

   type Code_Elements is record
      Door_Code : Codes; -- The required door code
      My_Code : Codes := Empty; -- Code required to generate
      Frequency_Map : Frequency_Maps.Map := Frequency_Maps.Empty_Map;
   end record; -- Code_Element

   package Code_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Code_Elements);
   use Code_lists;

   package Expanded_Codes is new Ada.Containers.Ordered_Sets (Codes);
   use Expanded_Codes;

   package Expansion_Tables is new
     Ada.Containers.Ordered_Maps (Codes, Expanded_Codes.Set);
   -- Initially the element type was Frequency_Maps.Map this allowed for more
   -- than one instance of an expanded code. All possible codes did not produce
   -- any repeats, so this was changed to a set.
   use Expansion_Tables;

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
      -- horizontally. Empirically <v is better than v<.

      function Door_Coordinate (Door_Button : in Door_Buttons)
                                return Door_Coordinates is

         -- Returns the absolute coordinates of each door button.
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

      function Vertical (S, F : in Door_Buttons) return Codes is

         Door_Code : Codes := Empty;

      begin -- Vertical
         if Door_Coordinate (S).Y < Door_Coordinate (F).Y then
            for U in Ordinates range
              Door_Coordinate (S).Y .. Door_Coordinate (F).Y - 1 loop
               Door_Code := @ & '^';
            end loop; -- U in Ordinates range ..
         elsif Door_Coordinate (F).Y < Door_Coordinate (S).Y then
            for D in Ordinates range
              Door_Coordinate (F).Y .. Door_Coordinate (S).Y - 1 loop
               Door_Code := @ & 'v';
            end loop; -- D in Ordinates range ..
         end if; -- Door_Coordinate (S).Y < Door_Coordinate (F).Y
         -- Does nothing if Door_Coordinate (S).Y = Door_Coordinate (F).Y
         return Door_Code;
      end Vertical;

      function Horizontal (S, F : in Door_Buttons) return Codes is

         Door_Code : Codes := Empty;

      begin -- Horizontal
         if Door_Coordinate (S).X < Door_Coordinate (F).X then
            for R in Ordinates range
              Door_Coordinate (S).X .. Door_Coordinate (F).X - 1 loop
               Door_Code := @ & '>';
            end loop; -- R in Ordinates range ...
         elsif Door_Coordinate (F).X < Door_Coordinate (S).X then
            for L in Ordinates range
              Door_Coordinate (F).X .. Door_Coordinate (S).X - 1 loop
               Door_Code := @ & '<';
            end loop; -- L in Ordinates range ...
         end if; -- Door_Coordinate (S).X < Door_Coordinate (F).X
         -- Does nothing if Door_Coordinate (S).X = Door_Coordinate (F).X
         return Door_Code;
      end Horizontal;

      Door_Code : Codes;

   begin -- Build_Door_Map
      Clear (Door_Map);
      for S in Door_Buttons loop
         for F in Door_Buttons loop
            if Door_Coordinate (F).X = -2 and
              Door_Coordinate (S).Y = 0 then
               Door_Code := Vertical (S, F) & Horizontal (S, F) & 'A';
            elsif Door_Coordinate (F).Y = 0 and
              Door_Coordinate (S).X = -2 then
               Door_Code := Horizontal (S, F) & Vertical (S, F) & 'A';
            elsif Door_Coordinate (F).X < Door_Coordinate (S).X then
               -- Either horozontal or vertical movement first is acceptamle.
               -- Empirically <v is better than v<
               Door_Code := Horizontal (S, F) & Vertical (S, F) & 'A';
            else
               Door_Code := Vertical (S, F) & Horizontal (S, F) & 'A';
            end if; -- Door_Coordinate (F).X = -2 and ...
            Insert (Door_Map, (S, F), Door_Code);
         end loop; -- F in Door_Buttons
      end loop; -- S in Door_Buttons
   end Build_Door_Map;

   procedure Build_Remote_Map (Remote_Map : out Button_Maps.Map) is

      -- Assumes that either stepping vertically or stepping horizontally first
      -- will always be better than alternating between vertically and
      -- horizontally. Empirically <v is better than v<.

      function Remote_Coordinate (Remote_Button : in Remote_Buttons)
                                  return Remote_Coordinates is

         -- Returns the absolute coordinates of each robot remote control
         -- button.
         -- X -2  -1   0    Y
         --      +---+---+
         --      | ^ | A |  0
         --  +---+---+---+
         --  | < | v | > | -1
         --  +---+---+---+

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

      function Vertical (S, F : in Remote_Buttons) return Codes is

         Remote_Code : Codes := Empty;

      begin -- Vertical
         if Remote_Coordinate (S).Y < Remote_Coordinate (F).Y then
            for U in Ordinates range
              Remote_Coordinate (S).Y .. Remote_Coordinate (F).Y - 1 loop
               Remote_Code := @ & '^';
            end loop; -- U in Ordinates range ..
         elsif Remote_Coordinate (F).Y < Remote_Coordinate (S).Y then
            for D in Ordinates range
              Remote_Coordinate (F).Y .. Remote_Coordinate (S).Y - 1 loop
               Remote_Code := @ & 'v';
            end loop; -- D in Ordinates range ..
         end if; -- Remote_Coordinate (S).Y < Remote_Coordinate (F).Y
         -- Does nothing if Remote_Coordinate (S).Y = Remote_Coordinate (F).Y
         return Remote_Code;
      end Vertical;

      function Horizontal (S, F : in Remote_Buttons) return Codes is

         Remote_Code : Codes := Empty;

      begin -- Horizontal
         if Remote_Coordinate (S).X < Remote_Coordinate (F).X then
            for R in Ordinates range
              Remote_Coordinate (S).X .. Remote_Coordinate (F).X - 1 loop
               Remote_Code := @ & '>';
            end loop; -- R in Ordinates range ...
         elsif Remote_Coordinate (F).X < Remote_Coordinate (S).X then
            for L in Ordinates range
              Remote_Coordinate (F).X .. Remote_Coordinate (S).X - 1 loop
               Remote_Code := @ & '<';
            end loop; -- L in Ordinates range ...
         end if; -- Remote_Coordinate (S).X < Remote_Coordinate (F).X
         return Remote_Code;
      end Horizontal;

      Remote_Code : Codes;

   begin -- Build_Remote_Map
      Clear (Remote_Map);
      for S in Remote_Buttons loop
         for F in Remote_Buttons loop
            if Remote_Coordinate (S).Y = 0 and
              Remote_Coordinate (F).X = -2 then
               Remote_Code := Vertical (S, F) & Horizontal (S, F) & 'A';
            elsif Remote_Coordinate (F).Y = 0 and
              Remote_Coordinate (S).X = -2 then
               Remote_Code := Horizontal (S, F) & Vertical (S, F) & 'A';
            elsif Remote_Coordinate (F).X < Remote_Coordinate (S).X then
               -- Either horozontal or vertical movement first is acceptamle.
               -- Empirically v< is better than <v
               Remote_Code := Horizontal (S, F) & Vertical (S, F) & 'A';
            else
               Remote_Code := Vertical (S, F) & Horizontal (S, F) & 'A';
            end if; -- Remote_Coordinate (S).Y = 0 and ...
            if not ((S = '<' and F = '>') or (S = '>' and F = '<') or
                      (S = '^' and F = 'v') or (S = 'v' and F = '^')) then
               Insert (Remote_Map, (S, F), Remote_Code);
            end if; -- not ((S = '<' and F = '>') or (S = '>' and F = '<') ...
         end loop; -- F in Remote_Buttons
      end loop; -- S in Remote_Buttons
   end Build_Remote_Map;

   procedure Door_Sequence (Door_Map : in Button_Maps.Map;
                            Code_Element : in out Code_Elements) is

      -- Produce key sequence for robot pressing door key pad.

      Previous_Button : Door_Buttons := 'A';

   begin -- Door_Sequence
      Code_Element.My_Code := Empty;
      for D in Positive range 1 .. Length (Code_Element.Door_Code) loop
         Code_Element.My_Code := @ &
           Door_Map ((Previous_Button, Element (Code_Element.Door_Code, D)));
         Previous_Button :=Element (Code_Element.Door_Code, D);
      end loop; -- D in Positive range 1 .. Length (Code_Element.Door_Code)
   end Door_Sequence;

   procedure Robot_Sequence (Remote_Map : in Button_Maps.Map;
                             Code_Element : in out Code_Elements) is

      -- Produce key sequence for robot pression robot remote key pad.

      Previous_Button : Remote_Buttons := 'A';
      Door_Code : Unbounded_String := Code_Element.My_Code;

   begin -- Robot_Sequence
      Code_Element.My_Code := Empty;
      for R in Positive range 1 .. Length (Door_Code) loop
         Code_Element.My_Code := @ &
           Remote_Map ((Previous_Button, Element (Door_Code, R)));
         Previous_Button :=Element (Door_Code, R);
      end loop; -- R in Positive range 1 .. Length (Door_Code)
   end Robot_Sequence;

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

   procedure Build_Expansion_Table (Remote_Map : in Button_Maps.Map;
                                    Expansion_Table : out Expansion_Tables.Map)
   is

      -- Builds a table of the codes reqired by robot N to produce a single
      -- movement between keys for robot N+2.

      Previous_Key : Remote_Buttons;

   begin -- Build_Expansion_Table
      Clear (Expansion_Table);
      for R in Iterate (Remote_Map) loop
         Insert (Expansion_Table, Element (R), Expanded_Codes.Empty_Set);
      end loop; -- R in Iterate (Remote_Map)
      for E in Iterate (Expansion_Table) loop
         Previous_Key := 'A';
         for C in Positive range 1 .. Length (Key (E)) loop
            include (Expansion_Table (E),
                     Remote_Map ((Previous_Key, Element (Key (E), C))));
            Previous_Key := Element (Key (E), C);
         end loop; -- C in Positive range 1 .. Length (Key (E))
      end loop; -- E in Iterate (Expansion_Table)
   end Build_Expansion_Table;

   procedure Do_Robot (N_Robots : in Positive;
                       Expasion_Table : in Expansion_Tables.Map;
                       Code_Element : in out Code_Elements) is

      Next_Robot : Frequency_Maps.Map := Frequency_Maps.Empty_Map;
      First, Last : Positive := 1;
      Previous_Key : Remote_Buttons := 'A';
      Command : Codes;

   begin -- Do_Robot
      Clear (Code_Element.Frequency_Map);
      while Last <= Length (Code_Element.My_Code) loop
         while Last <= Length (Code_Element.My_Code) and then
           Element (Code_Element.My_Code, Last) /= 'A' loop
            Last := @ + 1;
         end loop; -- Element (Code_Element.My_Code, Last) /= 'A'
         if Last <= Length (Code_Element.My_Code) then
            -- Command is a sequence ending in 'A' possibly just 'A'
            Command := Unbounded_Slice (Code_Element.My_Code, First, Last);
            if Contains (Code_Element.Frequency_Map, Command) then
               Code_Element.Frequency_Map (Command) :=
                 Code_Element.Frequency_Map (Command) + 1;
            else
               Insert (Code_Element.Frequency_Map, Command, 1);
            end if; -- Contains (Code_Element.Frequency_Map,
            Last := @ + 1;
            First := Last;
         end if; -- Last <= Length (Code_Element.My_Code)
      end loop; -- Last <= Length (Code_Element.My_Code)
      -- Code_Element.My_Code now containes the code for the robot operating
      -- the first directional key pad. the remaining directional keypads are
      -- managed below.
      for R in Positive range 2 .. N_Robots loop
         Clear (Next_Robot);
         for C in Iterate (Code_Element.Frequency_Map) loop
            for E in Iterate (Expasion_Table (Key (C))) loop
               if not Contains (Next_Robot, Element (E)) then
                  Insert (Next_Robot, Element (E), 0);
               end if; -- not Contains (Next_Robot, Element (E))
               Next_Robot (Element (E)) :=
                 Next_Robot (Element (E)) + Element (C);
            end loop; -- E in Iterate (Expasion_Table (Key (C)))
         end loop; -- C in Iterate (Code_Element.Frequency_Map)
         Code_Element.Frequency_Map := Copy (Next_Robot);
      end loop; -- R in Positive range 2 .. N_Robots
   end Do_Robot;

   function Complexity_2 (Code_Element : in Code_Elements) return Big_Natural is

      First : Positive;
      Last : Natural;
      Code_Number, Total_Length : Big_Natural := 0;

   begin -- Complexity_2
      Find_Token (Code_Element.Door_Code, Decimal_Digit_Set, Inside, First,
                  Last);
      Code_Number :=
        Big_Natural'Value (Slice (Code_Element.Door_Code, First, Last));
      for C in Iterate (Code_Element.Frequency_Map) loop
         Total_Length := @ + Big_Natural (Length(Key (C))) * Element (C);
      end loop; -- C in Iterate (Code_Element.Frequency_Map)
      return Code_Number * Total_Length;
   end Complexity_2;

   Code_List : Code_Lists.List;
   Door_Map, Remote_Map : Button_Maps.Map;
   Sum : Natural := 0;
   Expansion_Table : Expansion_Tables.Map;
   Sum_2 : Big_Natural := 0;

begin -- December_21
   Read_Input (Code_List);
   Build_Door_Map (Door_Map);
   Build_Remote_Map (Remote_Map);
   for C in Iterate (Code_List) loop
      Door_Sequence (Door_Map, Code_List (C));
      Robot_Sequence (Remote_Map, Code_List (C));
      Robot_Sequence (Remote_Map, Code_List (C));
      Sum := @ + Complexity (Element (C));
   end loop; -- C in Iterate (Code_List)
   Put_Line ("Part one:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Build_Expansion_Table (Remote_Map, Expansion_Table);
   for C in Iterate (Code_List) loop
      Code_List (C).My_Code := Empty;
      Door_Sequence (Door_Map, Code_List (C));
      Robot_Sequence (Remote_Map, Code_List (C));
      -- Code list contains code for first robot to robot sequences.
      Do_Robot (25, Expansion_Table, Code_List (C));
      Sum_2 := @ + Complexity_2 (Element (C));
   end loop; -- C in Iterate (Code_List)
   Put_Line ("Part two:" & Sum_2'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_21;
