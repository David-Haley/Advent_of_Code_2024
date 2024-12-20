with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_16 is

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

   subtype Scores is Natural;
   Step : constant Scores := 1;
   Turn : constant Scores := 1000;

   type Paths is Array (Directions) of Scores;
   All_Visited : constant Paths := (others => 0);
   Not_Visited : constant Paths := (others => Scores'Last);

   package Mazes is new
     Ada.Containers.Ordered_Maps (Coordinates, Paths);
   use Mazes; -- Only storing the path

   procedure Read_Input (Maze : out Mazes.Map;
                         Start, Finish : out Coordinates) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Y : Ordinates := Origin.Y;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_16.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Maze);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         for X in Ordinates range Origin.X .. Length (Text) loop
            case Element (Text, X) is
               when '.' =>
                  insert (Maze, (X, Y), Not_Visited);
               when 'S' =>
                  insert (Maze, (X, Y), All_Visited); -- no return to start
                  Start := (X, Y);
               when 'E' =>
                  insert (Maze, (X, Y), Not_Visited);
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

   function Least_Score (Maze : in out Mazes.Map;
                         Start, Finish : in Coordinates) return Scores is

      -- The function has the side effect of setting the path element when
      -- visited.

      type Search_Element is record
         Position : Coordinates;
         Direction : Directions;
         Score : Scores;
      end record; -- Search_Element

      package Queue_Interface is new
        Ada.Containers.Synchronized_Queue_Interfaces (Search_Element);
      use Queue_Interface;

      package Queues is new
        Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interface);
      use Queues;

      Best : Scores := Scores'Last;
      Current, Next_Step : Search_Element;
      Queue : Queues.Queue;

   begin -- Least_Score
      Current := (Start, East, 0);
      Queue.Enqueue (Current);
      while Queue.Current_Use > 0 loop
         Queue.Dequeue (Current);
         if Current.Position = Finish then
            if Best > Current.Score then
               Best := Current.Score;
            end if; -- Best > Current.Score
         else
            for D in Directions loop
               Next_Step.Position := Current.Position + Increment (D);
               Next_Step.Direction := D;
               if D = Current.Direction then
                  Next_Step.Score := Current.Score + Step;
               else
                  Next_Step.Score := Current.Score + Step + Turn;
               end if; -- D = Current.Direction
               if Contains (Maze, Next_Step.Position) and then
                 Maze (Next_Step.Position) (Next_Step.Direction) >
                 Next_step .Score then
                  -- Path exist and not visited in this direction
                  Queue.Enqueue (Next_Step);
                  Maze (Next_Step.Position) (Next_Step.Direction) :=
                    Next_Step.Score;
                  -- Store the Score required to get here;
               end if; -- Contains (Maze, Next_Step.Position) and then ..
            end loop; -- D in Directions
         end if; -- Current.Position = Finish
      end loop; -- Queue.Current_Use > 0
      return Best;
   end Least_Score;

   function Count_Tiles (Maze : in out Mazes.Map;
                         Start, Finish : in Coordinates;
                         Best : in Scores) return Count_Type is

      -- The maze path has already been set to the minimum score to reach each
      -- tile by running part one. Thus the test for equality with these values
      -- will only enque mimimum score elements. There is no need to mark
      -- visited tiles because they are already marked with the lowest score.

      package Tile_Sets is new Ada.Containers.Ordered_Sets (Coordinates);
      use Tile_Sets;

      type Search_Element is record
         Position : Coordinates;
         Direction : Directions;
         Score : Scores;
         Tile_Set : Tile_Sets.Set;
      end record; -- Search_Element

      package Queue_Interface is new
        Ada.Containers.Synchronized_Queue_Interfaces (Search_Element);
      use Queue_Interface;

      package Queues is new
        Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interface);
      use Queues;

      Current, Next_Step : Search_Element;
      Queue : Queues.Queue;
      Best_Path_Tiles : Tile_Sets.Set := Tile_Sets.Empty_Set;

   begin -- Count_Tiles
      -- The step below is necessary to eliminate any paths that exceed the best
         -- score. It potentially reduces the search by stopping some searches
      -- early.
      for M in Iterate (Maze) loop
         for D in Directions loop
            if Maze (M) (D) > Best then
               Maze (M) (D) := Best;
            end if; -- Maze (M) (D) > Best
         end loop; -- D in Directions
      end loop; -- M in Iterate (Maze)
      Current := (Start, East, 0, Tile_Sets.Empty_Set);
      Include (Current.Tile_Set, Start);
      Queue.Enqueue (Current);
      while Queue.Current_Use > 0 loop
         Queue.Dequeue (Current);
         if Current.Position = Finish then
            Union (Best_Path_Tiles, Current.Tile_Set);
         else
            for D in Directions loop
               Next_Step.Position := Current.Position + Increment (D);
               Next_Step.Direction := D;
               if D = Current.Direction then
                  Next_Step.Score := Current.Score + Step;
               else
                  Next_Step.Score := Current.Score + Step + Turn;
               end if; -- D = Current.Direction
               Next_Step.Tile_Set := Copy (Current.Tile_Set);
               Include (Next_Step.Tile_Set, Next_Step.Position);
               if Contains (Maze, Next_Step.Position) and then
                 Maze (Next_Step.Position) (Next_Step.Direction) =
                 Next_step .Score then
                  -- Path exist and not visited in this direction
                  Queue.Enqueue (Next_Step);
               end if; -- Contains (Maze, Next_Step.Position) and then ..
            end loop; -- D in Directions
         end if; -- Current.Position = Finish
      end loop; -- Queue.Current_Use > 0
      return Length (Best_Path_Tiles);
   end Count_Tiles;

   Maze : Mazes.Map;
   Start, Finish : Coordinates;
   Best : Scores;

begin -- December_16
   Read_Input (Maze, Start, Finish);
   Best :=  Least_Score (Maze, Start, Finish);
   Put_Line ("Part one:" & Best'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:" & Count_Tiles (Maze, Start, Finish, Best)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_16;
