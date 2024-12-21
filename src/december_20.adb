with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
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
   subtype Cheats is Times range 2 .. Times'Last;

   type Maze_Element is record
      Not_Visited, Is_Path : Boolean;
      Time : Times;
   end record;

   package Mazes is new
     Ada.Containers.Ordered_Maps (Coordinates, Maze_Element);
   use Mazes;

   procedure Read_Input (Maze : out Mazes.Map;
                         Start, Finish : out Coordinates;
                         Minimum_Cheat, Cheat_Duration : out Cheats) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Y : Ordinates := Origin.Y;

   begin -- Read_Input
      Minimum_Cheat := 100; --problem definition
      Cheat_Duration := 20; -- Part 2 only, problem definition
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_20.txt");
      else
         Open (Input_File, In_File, Argument (1));
         if Argument_Count = 3 then
            Minimum_Cheat := Cheats'Value (Argument (2));
            Cheat_Duration := Cheats'Value (Argument (3));
         end if; -- Argument_Count = 2
      end if; -- Argument_Count = 0
      Clear (Maze);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         for X in Ordinates range Origin.X .. Length (Text) loop
            case Element (Text, X) is
               when '.' =>
                  insert (Maze, (X, Y), (Not_Visited => True,
                                         Is_Path => True,
                                         Time => Times'Last));
               when 'S' =>
                  insert (Maze, (X, Y), (Not_Visited => False,
                                         Is_Path => True,
                                         Time => 0)); -- no return to start
                  Start := (X, Y);
               when 'E' =>
                  insert (Maze, (X, Y), (Not_Visited =>True,
                                         Is_Path => True,
                                         Time => Times'Last));
                  Finish := (X, Y);
               when '#' =>
                  Insert (Maze, (X, Y), (Not_Visited =>True,
                                         Is_Path => False,
                                         Time => Times'Last));
               when others =>
                  raise Program_Error with "unexpected chatacter '" &
                    Element (Text, X) & " at " & Coordinates'Image ((X, Y));
            end case; -- Element (Text, X)
         end loop; -- X in Ordinates range Origin.X .. Length (Text)
         Y := @ + 1;
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   procedure Reset_Maze (Start : in Coordinates;
                         Maze : in out Mazes.Map) is

   begin -- Reset_Maze
      for M in Iterate (Maze) loop
         Maze (M).Not_Visited := True;
      end loop; -- M in Iterate (Maze)
      Maze (Start).Not_Visited := False;
   end Reset_Maze;

   procedure Mark_Path (Maze : in out Mazes.Map;
                        Start, Finish : in Coordinates) is

      -- Only one path is assumed to exist (problem definition)

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
                 (Maze (Next_Step.Position).Is_Path and
                 Maze (Next_Step.Position).Not_Visited) then
                  -- Path exist and not visited in this
                  Queue.Enqueue (Next_Step);
                  Maze (Next_Step.Position).Not_Visited := False;
                  Maze (Next_Step.Position).Time := Next_Step.Time;
                  -- Store the Time required to get here;
               end if; -- Contains (Maze, Next_Step.Position) and then ..
            end loop; -- D in Directions
         end if; -- Current.Position /= Finish
      end loop; -- Queue.Current_Use > 0
      Reset_Maze (Start, Maze);
   end Mark_Path;

   function Count_Cheats (Maze : in Mazes.Map;
                          Minimum_Cheat : in Cheats) return Natural is

      Test_Wall, Test_Path : Coordinates;
      Count : Natural := 0;

   begin -- Count_Cheats
      for M in Iterate (Maze) loop
         if Element (M).Is_Path then
            for D in Directions loop
               Test_Wall := Key (M) + Increment (D);
               Test_Path := Test_Wall + Increment (D);
               if (Contains (Maze, Test_Wall) and then
                     not Maze (Test_Wall).Is_Path) and
                 (Contains (Maze, Test_Path) and then
                      (Maze (Test_Path).Is_Path
                       and (Maze (Test_Path).Time - (Element (M).Time + 2)) >=
                         Minimum_Cheat)) then
                  Count := @ + 1;
               end if; -- (Contains (Maze, Test_Wall) and then ...
            end loop; -- D in Directions
         end if; -- Element (M).Is_Path
      end loop; -- M in Iterate (Maze)
      return Count;
   end Count_Cheats;

   function Count_Cheats_2 (Maze : in out Mazes.Map;
                            Minimum_Cheat : in Cheats;
                            Cheat_Duration : in Cheats := Cheats'First)
                            return Natural is

      -- Has side effect of setting and unsetting Maze elements!

      type Search_Element is record
         Position : Coordinates;
         Time : Times;
      end record; -- Search_Element

      package Cheat_Lists is new
        Ada.Containers.Doubly_Linked_Lists (Search_Element);
      use Cheat_Lists;

      procedure List_Cheats (Maze : in out Mazes.Map;
                             Start : in Coordinates;
                             Cheat_Duration : in Cheats;
                             Cheat_List : out Cheat_Lists.List) is
         -- All cheats up to Cheat_Duration are found. any search that ends on
         -- the path is considered a potential cheat. The requirement for
         -- unique start and end pairs should be satisfied because any specific
         -- position will be enqueued as the shortest path and once only,
         -- because it will be marhed as visited.

         package Queue_Interface is new
           Ada.Containers.Synchronized_Queue_Interfaces (Search_Element);
         use Queue_Interface;

         package Queues is new
           Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interface);
         use Queues;

         Current, Next_Step : Search_Element;
         Queue : Queues.Queue;

      begin -- List_Cheats
         Reset_Maze (Start, Maze);
         Current.Position := Start;
         Current.Time := 0;
         Maze (Current.Position).Not_Visited := False;
         Next_Step.Time := Current.Time + 1;
         for D In Directions loop
            Next_Step.Position := Current.Position + Increment (D);
            if Contains (Maze, Next_Step.Position) then
               Maze (Next_Step.Position).Not_Visited := False;
               Queue.Enqueue (Next_Step);
            end if; -- Contains (Wall, Current.Position)
         end loop; -- D In Directions
         while Queue.Current_Use > 0 loop
            Queue.Dequeue (Current);
            if Current.Time >= 2 and Maze (Current.Position).Is_Path then
               Append (Cheat_List, Current);
            end if; -- Current.Time >= 2 and Maze (Current.Position).Is_Path
            if Current.Time < Cheat_Duration then
               Next_Step.Time := Current.Time + 1;
               for D in Directions loop
                  Next_Step.Position := Current.Position + Increment (D);
                  if Contains (Maze, Next_Step.Position) and then
                    Maze (Next_Step.Position).Not_Visited then
                     Maze (Next_Step.Position).Not_Visited := False;
                     Queue.Enqueue (Next_Step);
                  end if; -- Contains (Wall, Next_Step.Position) and then
               end loop; -- D in Directions
            end if; -- Current.Time < Cheat_Duration
         end loop; -- Queue.Current_Use > 0
      end List_Cheats;

      Cheat_List : Cheat_Lists.List;
      Count : Natural := 0;

   begin -- Count_Cheats_2
      for M in Iterate (Maze) loop
         if Element (M).Is_Path then
            Clear (Cheat_List);
            List_Cheats (Maze, Key (M), Cheat_Duration, Cheat_List);
            for C in Iterate (Cheat_List) loop
               if Maze (Element (C).Position).Time -
                 (Element (M).Time + Element (C).Time) >= Minimum_Cheat then
                  Count := @ + 1;
               end if; -- Maze (Element (C).Position).Time - ...
            end loop; -- C in Iterate (Cheat_List)
         end if; -- Element (M).Is_Path
      end loop; -- M in Iterate (Maze)
      return Count;
   end Count_Cheats_2;

   Maze : Mazes.Map;
   Start, Finish : Coordinates;
   Minimum_Cheat, Cheat_Duration : Cheats;

begin -- December_20
   Read_Input (Maze, Start, Finish, Minimum_Cheat, Cheat_Duration);
   Mark_Path (Maze, Start, Finish);
   Put_Line ("Part one:" & Count_Cheats (Maze, Minimum_Cheat)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:" &
               Count_Cheats_2 (Maze, Minimum_Cheat, Cheat_Duration)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_20;
