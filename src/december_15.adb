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
     Ada.Containers.Ordered_Maps (Coordinates, Storage_Elements);
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
   end Put;

   procedure Follow_Plan (Map : in out Maps.Map;
                          Command_List : in Command_Lists.List;
                          Robot_Position : in out Coordinates) is

      function Move (Map : in out Maps.Map;
                     Current : in Coordinates;
                     Direction : in Directions) return Boolean is

         -- Has side effect of moving Boxes, where possible.

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
                  end if; -- Map (Current + Increment (Direction)) = Free
                  if Map (Current) = Box then
                     Map (Current) := Free;
                  else
                     raise Program_Error with "Expected Box at" &
                       Current'Img;
                  end if; -- Map (Current + Increment (Direction)) = Box
               end if; -- Can_Move
            when Wall =>
               Can_Move := False;
         end case; -- Map (Current)
         return Can_Move;
      end Move;

   begin -- Follow_Plan
      for C in Iterate (Command_List) loop
         if Move (Map, Robot_Position + Increment (Element (C)), Element (C)) then
            if Map (Robot_Position + Increment (Element (C))) = Free then
               Robot_Position := @ + Increment (Element (C));
            else
               raise Program_Error with "Robot expected free space at" &
                 Coordinates'Image (Robot_Position + Increment (Element (C)));
            end if; -- Map (Robot_Position + Increment (Element (C))) = Free
         end if; -- Move (Map, Robot_Position, Element (C))
      end loop; -- C in Iterate (Command_List)
   end Follow_Plan;

   Map : Maps.Map;
   Command_List : Command_Lists.List;
   Robot_Position : Coordinates;
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
   Put_Line ("Part two:");
   DJH.Execution_Time.Put_CPU_Time;
end December_15;
