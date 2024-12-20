with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_20 is

   subtype Ordinates is Integer;
   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   Origin : constant Coordinates := (1, 1);

   function "<" (Left, Right : Coordinates) return Boolean is
     (Left.Y < Right.Y or (Left.Y = Right.Y and Left.X < Right.X));

   function "+" (Left, Right : Coordinates) return Coordinates is
     ((Left.X + Right.X, Left.Y + Right.Y));

   pragma Inline_Always ("<", "+");

   type Directions is (North, South, East, West);
   Increment : constant array (Directions) of Coordinates :=
     ((0, -1), (0, 1), (-1, 0), (1, 0));

   subtype Times is Natural;
   subtype Cheats is Times range 1 .. Times'Last;

   type Paths is record
      Not_Visited :Boolean := True;
      Time : Times;
   end record;

   package Mazes is new
     Ada.Containers.Ordered_Maps (Coordinates, Paths);
   use Mazes; -- Only storing the path

   procedure Read_Input (Maze : out Mazes.Map;
                         Start, Finish : out Coordinates;
                         Minimum_Cheat : out Positive) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Y : Ordinates := Origin.Y;

   begin -- Read_Input
      Minimum_Cheat := 100;
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_20.txt");
      else
         Open (Input_File, In_File, Argument (1));
         if Argument_Count = 2 then
            Minimum_Cheat := Cheats'Value (Argument (2));
         end if; -- Argument_Count = 2
      end if; -- Argument_Count = 0
      Clear (Maze);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         for X in Ordinates range Origin.X .. Length (Text) loop
            case Element (Text, X) is
               when '.' =>
                  insert (Maze, (X, Y), (True, Times'Last));
               when 'S' =>
                  insert (Maze, (X, Y), (False, 0)); -- no return to start
                  Start := (X, Y);
               when 'E' =>
                  insert (Maze, (X, Y),  (True, Times'Last));
                  Finish := (X, Y);
               when '#' =>
                  Null;
                  -- Do nothing, since only the path is in the map no test for
                  -- wall is needed;
               when others =>
                  raise Program_Error with "unexpected chatacter '" &
                    Element (Text, X) & " at " & Coordinates'Image ((X, Y));
            end case; -- Element (Text, X)
         end loop; -- X in Ordinates range Origin.X .. Length (Text)
         Y := @ + 1;
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   procedure Mark_Path (Maze : in out Mazes.Map;
                        Start, Finish : in Coordinates) is

      -- Only one path ia assumed to exist (problem definition)

      type Search_Element is record
         Position : Coordinates;
         Time : Times;
      end record; -- Search_Element

      package Queue_Interface is new
        Ada.Containers.Synchronized_Queue_Interfaces (Search_Element);
      use Queue_Interface;

      package Queues is new
        Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interface);
      use Queues;

      Current, Next_Step : Search_Element;
      Queue : Queues.Queue;

   begin -- Mark_Path
      Current := (Start, 0);
      Queue.Enqueue (Current);
      while Queue.Current_Use > 0 loop
         Queue.Dequeue (Current);
         if Current.Position /= Finish then
            for D in Directions loop
               Next_Step.Position := Current.Position + Increment (D);
               Next_Step.Time := Current.Time + 1;
               if Contains (Maze, Next_Step.Position) and then
                 Maze (Next_Step.Position).Not_Visited then
                  -- Path exist and not visited in this
                  Queue.Enqueue (Next_Step);
                  Maze (Next_Step.Position) := (False, Next_Step.Time);
                  -- Store the Time required to get here;
               end if; -- Contains (Maze, Next_Step.Position) and then ..
            end loop; -- D in Directions
         end if; -- Current.Position /= Finish
   end loop; -- Queue.Current_Use > 0
   end Mark_Path;

   function Count_Cheats (Maze : in Mazes.Map;
                          Minimum_Cheat : in Cheats) return Natural is

      Test_Wall, Test_Path : Coordinates;
      Count : Natural := 0;

   begin -- Count_Cheats
      for M in Iterate (Maze) loop
         for D in Directions loop
            Test_Wall := Key (M) + Increment (D);
            Test_Path := Test_Wall + Increment (D);
            if (not Contains (Maze, Test_Wall) and Contains (Maze, Test_Path))
              and then (Maze (Test_Path).Time - (Element (M).Time + 2)) >=
                Minimum_Cheat then
               Count := @ + 1;
            end if; -- (not Contains (Maze, Test_Wall) and Contains (Maze, ...
         end loop; -- D in Directions
      end loop; -- M in Iterate (Maze)
      return Count;
   end Count_Cheats;

   Maze : Mazes.Map;
   Start, Finish : Coordinates;
   Minimum_Cheat : Cheats;

begin -- December_20
   Read_Input (Maze, Start, Finish, Minimum_Cheat);
   Mark_Path (Maze, Start, Finish);
   Put_Line ("Part one:" & Count_Cheats (Maze, Minimum_Cheat)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:");
   DJH.Execution_Time.Put_CPU_Time;
end December_20;
