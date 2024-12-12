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

procedure December_12 is

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

   type Directions is (Up, Down, Left, Right);
   Increment : constant array (Directions) of Coordinates :=
     ((0, -1), (0, 1), (-1, 0), (1, 0));

   subtype Plants is Character;
   type Fences is array (Directions) of Boolean;
   subtype Fence_Counts is Natural range 0 .. 4;
   subtype Vertex_Counts is Natural range 0 .. 4;
   subtype Regions is Natural;

   type Gardens is record
      Plant : Plants;
      Fence : Fences := (others => False);
      Visited : Boolean := False;
      Region : Regions;
   end record; -- Gardens

   package Maps is new
     Ada.Containers.Ordered_Maps (Coordinates, Gardens);
   use Maps;

   procedure Read_Input (Map : out Maps.Map) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Y : Ordinates := Origin.Y;
      Garden : Gardens;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_12.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Map);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         for X in Ordinates range Origin.X .. Length (Text) loop
            Garden.Plant := Element (Text, X);
            insert (Map, (X, Y), Garden);
         end loop; -- X in Ordinates range Origin.X .. Length (Text)
         Y := @ + 1;
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   function Fence_Cost (Map : in out Maps.Map) return Natural is

      -- The function has the side effects of setting Fence and Region, to
      -- allow gardens in distinct regions, having a boundary to be identified,

      function Count_Fences (Garden : in Gardens) return Fence_Counts is

         Result : Fence_Counts := 0;

      begin -- Count_Fences
         if Garden.Fence (Up) then
            Result := @ + 1;
         end if; -- Garden.Fence (Up)
         if Garden.Fence (Right) then
            Result := @ + 1;
         end if; -- Garden.Fence (Right)
         if Garden.Fence (Left) then
            Result := @ + 1;
         end if; -- Garden.Fence (Left)
         if Garden.Fence (Down) then
            Result := @ + 1;
         end if; -- Garden.Fence (Down)
         return Result;
      end Count_Fences;

      package Queue_Interface is new
        Ada.Containers.Synchronized_Queue_Interfaces (Coordinates);
      use Queue_Interface;

      package Queues is new
        Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interface);
      use Queues;

      Total_Cost : Natural := 0;
      Area, Perimeter : Natural;
      Current, Next_Garden : Coordinates;
      Queue : Queues.Queue;
      Region : Regions := 0;

   begin -- Fence_Cost
      for Mc in Iterate (Map) loop
         if not Element (MC).Visited then
            -- Unvisited garden, new region
            Area := 0;
            Perimeter := 0;
            Current := Key (Mc);
            Map (Current).Visited := True;
            Map (Current).Region := Region;
            Queue.Enqueue (Current);
            while Queue.Current_Use > 0 loop
               Area := @ + 1;
               Queue.Dequeue (Current);
               for D in Directions loop
                  Next_Garden := Current + Increment (D);
                  if Contains (Map, Next_Garden) then
                     if Map (Next_Garden).Plant = Map (Current). Plant then
                        -- this is part of the same region no and fence required
                        if not Map (Next_Garden).Visited then
                           Map (Next_Garden).Visited := True;
                           Map (Next_Garden).Region := Region;
                           Queue.Enqueue (Next_Garden);
                        end if; -- not Map (Next_Garden).Visited
                     else
                        -- boundary to another region
                        Map (Current).Fence (D) := True;
                     end if; -- Map (Next_Garden).Plant = Map (Current). Plant
                  else
                     -- is on boundary of map, fence required
                     Map (Current).Fence (D) := True;
                  end if; -- Contains (Map, Next_Garden)
               end loop; -- D in Directions
               Perimeter := @ + Count_Fences (Map (Current));
            end loop; -- Queue.Current_Use > 0
            Total_Cost := @ + Area * Perimeter;
            Region := @ + 1;
         end if; -- not Element (MC).Visited
      end loop; -- Mc in Iterate (Map)
      return Total_Cost;
   end Fence_Cost;

   function Discounted_Cost (Map : in Maps.Map) return Natural is

      package Garden_Lists is new
        Ada.Containers.Doubly_Linked_Lists (Coordinates);
      use Garden_Lists;

      package Region_Stores is new
        Ada.Containers.Ordered_Maps (Regions, Garden_Lists.List);
      use Region_Stores;

      function Count_Vertices (Map : in Maps.Map;
                               Current : in Coordinates) return Vertex_Counts is

         Result : Vertex_Counts := 0;
         Diagonal, Above, Below, To_Left, To_Right : Coordinates;

      begin -- Count_Vertices
         -- Count external vertices for example consider A
         -- BB
         -- BA
         if Map (Current).Fence (Up) and Map (Current).Fence (Left) then
            Result := @ + 1;
         end if; -- Map (Current).Fence (Up) and Map (Current).Fence (Left)
         if Map (Current).Fence (Up) and Map (Current).Fence (Right) then
            Result := @ + 1;
         end if; -- Map (Current).Fence (Up) and Map (Current).Fence (Right)
         if Map (Current).Fence (Down) and Map (Current).Fence (Left) then
            Result := @ + 1;
         end if; -- Map (Current).Fence (Down) and Map (Current).Fence (Left)
         if Map (Current).Fence (Down) and Map (Current).Fence (Right) then
            Result := @ + 1;
         end if; -- Map (Current).Fence (Down) and Map (Current).Fence (Right)
         -- Count internal vertices for example consider A
         -- AA | AA | BA | AB
         -- AB | AB | AA | AA
         Above := Current + Increment (Up);
         Below := Current + Increment (Down);
         To_Left := Current + Increment (Left);
         To_Right := Current + Increment (Right);
         Diagonal := Current + Increment (Up) + Increment (Left);
         If Contains (Map, Diagonal) and then
           (Map (Diagonal).Region /= Map (Current).Region and
                Map (Above).Region = Map (Current).Region and
                Map (To_Left).Region = Map (Current).Region) then
            Result := @ + 1;
         end if; -- Contains (Map, Diagonal) and then ...
         Diagonal := Current + Increment (Up) + Increment (Right);
         If Contains (Map, Diagonal) and then
           (Map (Diagonal).Region /= Map (Current).Region and
                Map (Above).Region = Map (Current).Region and
                Map (To_Right).Region = Map (Current).Region) then
            Result := @ + 1;
         end if; -- Contains (Map, Diagonal) and then ...
         Diagonal := Current + Increment (Down) + Increment (Left);
         If Contains (Map, Diagonal) and then
           (Map (Diagonal).Region /= Map (Current).Region and
                Map (Below).Region = Map (Current).Region and
                Map (To_Left).Region = Map (Current).Region) then
            Result := @ + 1;
         end if; -- Contains (Map, Diagonal) and then ...
         Diagonal := Current + Increment (Down) + Increment (Right);
         If Contains (Map, Diagonal) and then
           (Map (Diagonal).Region /= Map (Current).Region and
                Map (Below).Region = Map (Current).Region and
                Map (To_Right).Region = Map (Current).Region) then
            Result := @ + 1;
         end if; -- Contains (Map, Diagonal) and then ...
         return Result;
      end Count_Vertices;

      Region_Store : Region_Stores.Map := Region_Stores.Empty_Map;
      Vertices : Natural;
      -- Assumptiom number of vertices is the same as the number of straight
      -- edges.
      Total_Cost :Natural := 0;

   begin -- Discounted_Cost
      for Mc in Iterate (Map) loop
         if not Contains (Region_Store, Element (Mc).Region) then
            Insert (Region_Store, Element (Mc).Region, Garden_Lists.Empty_List);
         end if; -- not Contains (Region_Store, Element (Mc).Region)
         Append (Region_Store (Element (Mc).Region), Key (Mc));
      end loop; -- Mc in Iterate (Map)
      for R in Iterate (Region_Store) loop
         Vertices := 0;
         for G in Iterate (Element (R)) loop
            Vertices := @ + Count_Vertices (Map, Element (G));
         end loop; -- Iterate (Element (R))
         Total_Cost := @ + Vertices * Natural (Length (Element (R)));
      end loop; -- R in Iterate (Region_Store)
      return Total_Cost;
   end Discounted_Cost;

   Map : Maps.Map;

begin -- December_12
   Read_Input (Map);
   Put_Line ("Part one:" & Fence_Cost (Map)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:" & Discounted_Cost (Map)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_12;
