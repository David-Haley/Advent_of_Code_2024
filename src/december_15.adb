with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with DJH.Execution_Time; use DJH.Execution_Time;
with DJH.Gcd_Lcm;

procedure December_15 is

   subtype Ordinates is Integer;
   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   Origin : constant Coordinates := (0, 0);

   function "<" (Left, Right : Coordinates) return Boolean is
     (Left.Y < Right.Y or (Left.Y = Right.Y and Left.X < Right.X));

   function "+" (Left, Right : Coordinates) return Coordinates is
     ((Left.X + Right.X, Left.Y + Right.Y));

   pragma Inline_Always ("<", "+");

   type Directions is (Up, Down, Left, Right);
   Increment : constant array (Directions) of Coordinates :=
     ((0, -1), (0, 1), (-1, 0), (1, 0));

   type Storage_Elements is (Wall, Box, Free);

   package Maps is new
     Ada.Containers.Ordered_Maps (Coordinates, Storage_Elements);
   use Maps;

   type Storage_Elements_2 is (Wall, Left_Box, Right_Box, Free);

   package Maps_2 is new
     Ada.Containers.Ordered_Maps (Coordinates, Storage_Elements_2);
   use Maps_2;

   package Command_Lists is new Ada.Containers.Doubly_Linked_Lists (Directions);
   use Command_Lists;

   procedure Read_Input (Map : out Maps.Map;
                         Command_List : out Command_Lists.List;
                         Initial_Robot_Position : out Coordinates) is

      Input_File : File_Type;
      Text : Unbounded_String;
      X : Ordinates;
      Y : Ordinates := Origin.Y;
      End_Map : Boolean := False;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_15.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Map);
      Clear (Command_List);
      while not End_Of_File (Input_File) and not End_Map loop
         Get_Line (Input_File, Text);
         X := Origin.X;
         if Length(Text) > 0 then
            for T in Positive range 1 .. Length (Text) loop
               case Element (Text, T) is
                  when '#' =>
                     Include (Map, (X, Y), Wall);
                  when 'O' =>
                     Include (Map, (X, Y), Box);
                  when '.' =>
                     Include (Map, (X, Y), Free);
                  when '@' =>
                     Initial_Robot_Position := (X, Y);
                     Include (Map, (X, Y), Free);
                  when others =>
                     raise Program_Error with "Invalid charactrer '" &
                       Element (Text, T) & "' found";
               end case; -- Element (Text, T)
               X := @ + 1;
            end loop; -- T in Positive range 1 .. Length (Text)
            Y := @ + 1;
         else
            End_Map := True;
         end if; -- Length(Text) > 0
      end loop; -- not End_Of_File (Input_File) and not End_Map
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         for T in Positive range 1 .. Length (Text) loop
            case Element (Text, T) is
            when '^' =>
               Append (Command_List, Up);
            when 'v' =>
               Append (Command_List, Down);
            when '<' =>
               Append (Command_List, Left);
            when '>' =>
               Append (Command_List, Right);
            when others =>
               raise Program_Error with "Invalid charactrer '" &
                 Element (Text, T) & "' found";
            end case; -- Element (Text, T)
         end loop; -- T in Positive range 1 .. Length (Text)
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   procedure Read_Input (Map : out Maps_2.Map;
                         Command_List : out Command_Lists.List;
                         Initial_Robot_Position : out Coordinates) is

      Input_File : File_Type;
      Text : Unbounded_String;
      X : Ordinates;
      Y : Ordinates := Origin.Y;
      End_Map : Boolean := False;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_15.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Map);
      Clear (Command_List);
      while not End_Of_File (Input_File) and not End_Map loop
         Get_Line (Input_File, Text);
         X := Origin.X;
         if Length(Text) > 0 then
            for T in Positive range 1 .. Length (Text) loop
               case Element (Text, T) is
                  when '#' =>
                     Include (Map, (X, Y), Wall);
                     X := @ + 1;
                     Include (Map, (X, Y), Wall);
                  when 'O' =>
                     Include (Map, (X, Y), Left_Box);
                     X := @ + 1;
                     Include (Map, (X, Y), Right_Box);
                  when '.' =>
                     Include (Map, (X, Y), Free);
                     X := @ + 1;
                     Include (Map, (X, Y), Free);
                  when '@' =>
                     Initial_Robot_Position := (X, Y);
                     Include (Map, (X, Y), Free);
                     X := @ + 1;
                     Include (Map, (X, Y), Free);
                  when others =>
                     raise Program_Error with "Invalid charactrer '" &
                       Element (Text, T) & "' found";
               end case; -- Element (Text, T)
               X := @ + 1;
            end loop; -- T in Positive range 1 .. Length (Text)
            Y := @ + 1;
         else
            End_Map := True;
         end if; -- Length(Text) > 0
      end loop; -- not End_Of_File (Input_File) and not End_Map
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         for T in Positive range 1 .. Length (Text) loop
            case Element (Text, T) is
            when '^' =>
               Append (Command_List, Up);
            when 'v' =>
               Append (Command_List, Down);
            when '<' =>
               Append (Command_List, Left);
            when '>' =>
               Append (Command_List, Right);
            when others =>
               raise Program_Error with "Invalid charactrer '" &
                 Element (Text, T) & "' found";
            end case; -- Element (Text, T)
         end loop; -- T in Positive range 1 .. Length (Text)
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   procedure Put ( Map : in out Maps.Map;
                   Robot_Position : in out Coordinates) is

      Max_X, Max_Y : Ordinates := 0;

   begin -- Put
      for M in Iterate (Map) loop
         if Key (M).X > Max_X then
            Max_X := Key (M).X;
         end if; -- Key (M).X > Max_X
         if Key (M).Y > Max_Y then
            Max_Y := Key (M).Y;
         end if; -- Key (M).Y > Max_Y
      end loop; -- M in Iterate (Map)
      for Y in Ordinates range Origin.Y .. Max_Y loop
         for X in Ordinates range Origin.X .. Max_X loop
            if (X, Y) = Robot_Position and Map ((X, Y)) = Free then
               Put ('@');
            else
               case Map ((X, Y)) is
                  when Wall =>
                     Put ('#');
                  when Box =>
                     Put ('O');
                  when Free =>
                     Put ('.');
               end case; -- Map ((X, Y))
            end if; -- (X, Y) = Robot_Position and Map ((X, Y)) = Free
         end loop; -- X in Ordinates range Origin.X .. Max_X
         New_Line;
      end loop; -- Y in Oordinates range Origin.Y .. Max_Y
      New_Line;
   end Put;

   procedure Put ( Map : in out Maps_2.Map;
                   Robot_Position : in out Coordinates) is

      Max_X, Max_Y : Ordinates := 0;

   begin -- Put
      for M in Iterate (Map) loop
         if Key (M).X > Max_X then
            Max_X := Key (M).X;
         end if; -- Key (M).X > Max_X
         if Key (M).Y > Max_Y then
            Max_Y := Key (M).Y;
         end if; -- Key (M).Y > Max_Y
      end loop; -- M in Iterate (Map)
      for Y in Ordinates range Origin.Y .. Max_Y loop
         for X in Ordinates range Origin.X .. Max_X loop
            if (X, Y) = Robot_Position and Map ((X, Y)) = Free then
               Put ('@');
            else
               case Map ((X, Y)) is
                  when Wall =>
                     Put ('#');
                  when Left_Box =>
                     Put ('[');
                  when Right_Box =>
                     Put (']');
                  when Free =>
                     Put ('.');
               end case; -- Map ((X, Y))
            end if; -- (X, Y) = Robot_Position and Map ((X, Y)) = Free
         end loop; -- X in Ordinates range Origin.X .. Max_X
         New_Line;
      end loop; -- Y in Oordinates range Origin.Y .. Max_Y
      New_line;
   end Put;

   procedure Follow_Plan (Map : in out Maps.Map;
                          Command_List : in Command_Lists.List;
                          Robot_Position : in out Coordinates) is

      function Move (Map : in out Maps.Map;
                     Current : in Coordinates;
                     Direction : in Directions) return Boolean is

         -- Has side effect of moving Boxes, where possible.
         -- Has "Defensive programing" checks after return from lower level
         -- calls to identify any logic errors close to where they occur.

         Can_Move : Boolean;
         Next : constant Coordinates := Current + Increment (Direction);

      begin -- Move
         case Map (Current) is
            when Free =>
               Can_Move := True;
            when Box =>
               Can_Move := Move (Map, Next, Direction);
               if Can_Move then
                  If Map (Next) = Free then
                     Map (Next) := Box;
                  else
                     raise Program_Error with "Expected free space at" &
                       Next'Img;
                  end if; -- Map (Next) = Free
                  if Map (Current) = Box then
                     Map (Current) := Free;
                  else
                     raise Program_Error with "Expected Box at" &
                       Current'Img;
                  end if; -- Map (Current) = Box
               end if; -- Can_Move
            when Wall =>
               Can_Move := False;
         end case; -- Map (Current)
         return Can_Move;
      end Move;

   begin -- Follow_Plan
      for C in Iterate (Command_List) loop
         if Move (Map, Robot_Position + Increment (Element (C)),
                  Element (C)) then
            if Map (Robot_Position + Increment (Element (C))) = Free then
               Robot_Position := @ + Increment (Element (C));
            else
               raise Program_Error with "Robot expected free space at" &
                 Coordinates'Image (Robot_Position + Increment (Element (C)));
            end if; -- Map (Robot_Position + Increment (Element (C))) = Free
         end if; -- Move (Map, Robot_Position + Increment (Element (C)), ...
      end loop; -- C in Iterate (Command_List)
   end Follow_Plan;

   procedure Follow_Plan (Map : in out Maps_2.Map;
                          Command_List : in Command_Lists.List;
                          Robot_Position : in out Coordinates) is

      function Move (Map : in out Maps_2.Map;
                     Current : in Coordinates;
                     Direction : in Directions) return Boolean is

         -- Has side effect of moving Boxes, where possible.
         -- Has "Defensive programing" checks after return from lower level
         -- calls to identify any logic errors close to where they occur.

         Can_Move : Boolean;
         Next, Next_Other, Current_Other : Coordinates;

      begin -- Move
         case Map (Current) is
            when Free =>
               Can_Move := True;
            when Left_Box =>
               Next := Current + Increment (Direction);
               Current_Other := Current + Increment (Right);
               Next_Other := Next + Increment (Right);
               case Direction is
                  when Up | Down =>
                     Can_Move := Move (Map, Next, Direction) and
                       Move (Map, Next_Other, Direction);
                  when Right =>
                     Can_Move := Move (Map, Next_Other, Direction);
                  when Left =>
                     raise Program_Error with "Left_Box, Left direction";
               end case; -- Direction
               if Can_Move then
                  If Map (Next_Other) = Free then
                     Map (Next_Other) := Right_Box;
                  else
                     raise Program_Error with "Expected free space at" &
                       Next_Other'Img & " (Left_Box Other)";
                  end if; -- Map (Next_Other) = Free
                  if Map (Current_Other) = Right_Box then
                     Map (Current_Other) := Free;
                  else
                     raise Program_Error with "Expected Right_Box at" &
                       Current'Img & "(Left_Box other)";
                  end if; -- Map (Current) = Left_Box
                  If Map (Next) = Free then
                     Map (Next) := Left_Box;
                  else
                     raise Program_Error with "Expected free space at" &
                       Next'Img & " (Left_Box)";
                  end if; -- Map (Next) = Free
                  if Map (Current) = Left_Box then
                     Map (Current) := Free;
                  else
                     raise Program_Error with "Expected Left_Box at" &
                       Current'Img & " (Left_Box)";
                  end if; -- Map (Current) = Left_Box
               end if; -- Can_Move
            when Right_Box =>
               Next := Current + Increment (Direction);
               Current_Other := Current + Increment (Left);
               Next_Other := Next + Increment (Left);
               case Direction is
                  when Up | Down =>
                     Can_Move := Move (Map, Next, Direction) and
                       Move (Map, Next_Other, Direction);
                  when Right =>
                     raise Program_Error with "Right_Box, Right direction";
                  when Left =>
                     Can_Move := Move (Map, Next_Other, Direction);
               end case; -- Direction
               if Can_Move then
                  If Map (Next_Other) = Free then
                     Map (Next_Other) := Left_Box;
                  else
                     raise Program_Error with "Expected free space at" &
                       Next_Other'Img & " (Right_Box Other)";
                  end if; -- Map (Next_Other) = Free
                  if Map (Current_Other) = Left_Box then
                     Map (Current_Other) := Free;
                  else
                     raise Program_Error with "Expected Left_Box at" &
                       Current'Img & "(Right_Box other)";
                  end if; -- Map (Current) = Left_Box
                  If Map (Next) = Free then
                     Map (Next) := Right_Box;
                  else
                     raise Program_Error with "Expected free space at" &
                       Next'Img & " (Right_Box)";
                  end if; -- Map (Next) = Free
                  if Map (Current) = Right_Box then
                     Map (Current) := Free;
                  else
                     raise Program_Error with "Expected Right_Box at" &
                       Current'Img & " (Right_Box)";
                  end if; -- Map (Current) = Left_Box
               end if; -- Can_Move
            when Wall =>
               Can_Move := False;
         end case; -- Map (Current)
         return Can_Move;
      end Move;

      Move_Count : Natural := 0;

   begin -- Follow_Plan
      for C in Iterate (Command_List) loop
         if Move (Map, Robot_Position + Increment (Element (C)),
                  Element (C)) then
            if Map (Robot_Position + Increment (Element (C))) = Free then
               Robot_Position := @ + Increment (Element (C));
            else
               raise Program_Error with "Robot expected free space at" &
                 Coordinates'Image (Robot_Position + Increment (Element (C)));
            end if; -- Map (Robot_Position + Increment (Element (C))) = Free
         end if; -- Move (Map, Robot_Position + Increment (Element (C)), ...
         --  Put ("Move" & Move_Count'Img & ": ");
         --  case Element (C) is
         --     when Up =>
         --        Put_Line ("^");
         --     when Down =>
         --        Put_Line ("v");
         --     when Left =>
         --        Put_line ("<");
         --     when Right =>
         --        Put_Line (">");
         --  end case;
         --  Put (Map, Robot_Position);
         Move_Count := @ + 1;
      end loop; -- C in Iterate (Command_List)
   end Follow_Plan;


   Map : Maps.Map;
   Map_2 : Maps_2.Map;
   Command_List : Command_Lists.List;
   Robot_Position, Robot_Position_2 : Coordinates;
   Sum : Natural := 0;

begin -- December_15
   Read_Input (Map, Command_List, Robot_Position);
   Follow_Plan (Map, Command_List, Robot_Position);
   for M in Iterate (Map) loop
      if Element (M) = Box then
         Sum := @ + Key (M).Y * 100 + Key (M).X;
      end if; -- Element (M) = Box
   end loop; -- M in Iterate (Map)
   Put_Line ("Part one:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Read_Input (Map_2, Command_List, Robot_Position_2);
   Put_Line ("Part two commands" & Length (Command_List)'Img);
   Follow_Plan (Map_2, Command_List, Robot_Position_2);
   Sum := 0;
   for M in Iterate (Map_2) loop
      if Element (M) = Left_Box then
         Sum := @ + Key (M).Y * 100 + Key (M).X;
      end if; -- Element (M) = Left_Box
   end loop; -- M in Iterate (Map_2)
   Put_Line ("Part two:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part One");
   Put (Map, Robot_Position);
   Put_Line ("Part_Two");
   Put (Map_2, Robot_Position_2);
end December_15;
