with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_06 is

   subtype Ordinates is Integer;
   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   function "<" (Left, Right : Coordinates) return Boolean is
     (Left.Y < Right.Y or (Left.Y = Right.Y and Left.X < Right.X));

   pragma Inline_Always ("<");

   type Trafficable is new Boolean;
   Clear : constant Trafficable := True;
   Blocked : constant Trafficable := False;

   package Maps is new
     Ada.Containers.Ordered_Maps (Coordinates, Trafficable);
   use Maps;

   type Directions is (Up, Down, Left, Right);

   type Guard_States is record
      Position : Coordinates;
      Direction : Directions := Up;
   end record; -- Guard_States

   package Visits is new Ada.Containers.Ordered_Sets (Coordinates);
   use Visits;

   procedure Read_Input (Map : out Maps.Map;
                         Guard_State : out Guard_States) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Y : Ordinates := 1;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_06.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Maps.Clear (Map);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         for X in Ordinates range 1 .. Length (Text) loop
            case Element (Text, X) is
               when '.' =>
                  Insert (Map, (X, Y), Clear);
               when '#' =>
                  Insert (Map, (X, Y), Blocked);
               when '^' =>
                  Insert (Map, (X, Y), Clear);
                  Guard_State.Position := (X, Y);
               when others =>
                  raise Program_Error with "Invalid character '" &
                    Element (Text, X) & "' at (" & X'Img & "," & Y'Img & ")";
            end case; -- Element (Text, X)
         end loop; -- X in Ordinates range 1 .. Length (Text)
         Y := @ + 1;
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   Procedure Next_State (Map : in Maps.Map;
                         Guard_State : in out Guard_States) is

   begin -- Next_State
      if Map (Guard_State.Position) = Clear then
         case Guard_State.Direction is
            when Up =>
               Guard_State.Position.Y := @ - 1;
            when Down =>
               Guard_State.Position.Y := @ + 1;
            when Left =>
               Guard_State.Position.X := @ - 1;
            when Right =>
               Guard_State.Position.X := @ + 1;
         end case; -- Guard_State.Direction
      else
         case Guard_State.Direction is
            when Up =>
               Guard_State.Position.Y := @ + 1; -- undo last step
               Guard_State.Direction := Right;
            when Down =>
               Guard_State.Position.Y := @ - 1; -- undo last step
               Guard_State.Direction := Left;
            when Left =>
               Guard_State.Position.X := @ + 1; -- undo last step
               Guard_State.Direction := Up;
            when Right =>
               Guard_State.Position.X := @ - 1; -- undo last step
               Guard_State.Direction := Down;
         end case; -- Guard_State.Direction
      end if; -- Map (Guard_State.Position) = Clear
   end Next_State;

   pragma Inline_Always (Next_State);

   Procedure Guard_Visits (Map : in Maps.Map;
                           Initial_Guard_State : in Guard_States;
                           Visited : out Visits.Set) is

      Guard_State : Guard_States := Initial_Guard_State;

   begin -- Guard_Visits
      Visits.Clear (Visited);
      Include (Visited, Guard_State.Position);
      while Contains (Map, Guard_State.Position) loop
         if Map (Guard_State.Position) = Clear then
            Include (Visited, Guard_State.Position);
         end if; -- Map (Guard_State.Position) = Clear
         Next_State (Map, Guard_State);
      end loop; -- Contains (Map, Guard_State.Position)
   end Guard_Visits;

   function Obstacles (Map_In : in Maps.Map;
                       Initial_Guard_State : in Guard_States;
                       Visited : in Visits.Set) return Natural is

      function "<" (Left, Right : Guard_States) return Boolean is
        (Left.Position.Y < Right.Position.Y or
           ((Left.Position.Y = Right.Position.Y and
                Left.Position.X < Right.Position.X) or
              (Left.Position = Right.Position and
                   Left.Direction < Right.Direction)));

      pragma Inline_Always ("<");

      package Previous_States is new Ada.Containers.Ordered_Sets (Guard_States);
      use Previous_States;

      Obstacle_Count : Natural := 0;
      Map : Maps.Map := Copy (Map_In);
      Guard_State : Guard_States;
      Previous_State : Previous_States.Set;

   begin -- Obstacles
      for P in iterate (Visited) loop
         if Element (P) /= Initial_Guard_State.Position then
            Map (Element (P)) := Blocked;
            Previous_States.Clear (Previous_State);
            Guard_State := Initial_Guard_State;
            while Contains (Map, Guard_State.Position) and
              not Contains (Previous_State, Guard_State) loop
               include (Previous_State, Guard_State);
               -- Origionally only included positions that could be occupied,
               -- This worked for the example and 193 locations in the input but
               -- failed, ininfinite loop.
               Next_State (Map, Guard_State);
            end loop; -- Contains (Map, Guard_State.Position)
            if Contains (Map, Guard_State.Position) then
               Obstacle_Count := @ + 1;
            end if; -- Contains (Map, Guard_State.Position)
            Map (Element (P)) := Clear;
         end if; -- Element (P) /= Initial_Guard_State.Position
      end loop; -- P in iterate (Visited)
      return Obstacle_Count;
   end Obstacles;

   Map : Maps.Map;
   Guard_State : Guard_States;
   Visited : Visits.Set;

begin -- December_06
   Read_Input (Map, Guard_State);
   Guard_Visits (Map, Guard_State, Visited);
   Put_Line ("Part one:" & Length (Visited)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:" & Obstacles (Map, Guard_State, Visited)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_06;
