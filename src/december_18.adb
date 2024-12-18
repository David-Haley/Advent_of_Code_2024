with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_18 is

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

   package Byte_Lists is new Ada.Containers.Vectors (Positive, Coordinates);
   use Byte_Lists;

   package Maps is new Ada.Containers.Ordered_Maps (Coordinates, Boolean);
   use Maps;

   procedure Read_Input (Byte_List : out Byte_Lists.Vector;
                         Size, Dropped : out Positive) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Coordinate : Coordinates;
      Start_At, First : Positive;
      Last : Natural;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_18.txt");
         Size := 70;
         Dropped := 1024;
      else
         Open (Input_File, In_File, Argument(1));
         if Argument_Count = 3 then
            Size := Positive'Value (Argument (2));
            Dropped := Positive'Value (Argument (3));
         end if; -- Argument_Count = 3
      end if; -- Argument_Count = 0
      Clear (Byte_List);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Coordinate.X := Ordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Coordinate.Y := Ordinates'Value (Slice (Text, First, Last));
         Append (Byte_List, Coordinate);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   procedure Build_Map (Byte_List : in Byte_Lists.Vector;
                        Size, Dropped : in Positive;
                        Map : out Maps.Map) is

      Bc : Byte_Lists.Cursor := First (Byte_List);
      Byte_Count : Natural := 0;

   begin -- Build_Map
      Clear (Map);
      for X in Ordinates range Origin.X .. Size loop
         for Y in Ordinates range Origin.Y .. Size loop
            insert (Map, (X, Y), True);
         end loop; -- Y in Ordinates range Origin.Y .. Size
      end loop; -- X in Ordinates range Origin.X .. Size
      while Bc /= Byte_Lists.No_Element and Byte_Count < Dropped loop
         Exclude (Map, Element (Bc)); -- no longer available as a path
         Next (Bc);
         Byte_Count := @ + 1;
      end loop; -- Bc /= Byte_Lists.No_Element and Byte_Count < Dropped
   end Build_Map;

   function Steps (Map : in out Maps.Map;
                   Size : in Positive) return Natural is

      -- Has side effect of setting every Map element visited to False;

      type Search_Elements is record
         Position : Coordinates;
         Step : Natural;
      end record; -- Search_Elements

      package Queue_Interface is new
        Ada.Containers.Synchronized_Queue_Interfaces (Search_Elements);
      use Queue_Interface;

      package Queues is new
        Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interface);
      use Queues;

      Current : Search_Elements := (Origin, 0);
      Finish : constant Coordinates := (Size, Size);
      Next : Search_elements;
      Queue : Queues.Queue;
      Found : Boolean := False;

   begin -- Steps
      Queue.Enqueue (Current);
      Map (Current.Position) := False;
      while Queue.Current_Use > 0 and not Found loop
         Queue.Dequeue (Current);
         Found := Current.Position = Finish;
         if not Found then
            Next.Step := Current.Step + 1;
            for D in Directions loop
               Next.Position := Current.Position + Increment (D);
               if Contains (Map, Next.Position) and then
                 Map (Next.Position) then
                  Map (Next.Position) := False;
                  Queue.Enqueue (Next);
               end if; -- Contains (Map, Next.Position) and then ...
            end loop; -- D in Directions
         end if; -- not Found
      end loop; -- Queue.Current_Use > 0
      if Found then
         return Current.Step;
      else
         return Natural'Last;
      end if; -- Found
   end Steps;

   Function Blocker (Map : in out Maps.Map;
                     Byte_List : in Byte_Lists.Vector;
                     Size, Dropped : in Positive) return Coordinates is

      -- Has side effect of removing Map elements until search fails.
      -- A binary search for the Byte_List index at which the search fails would
      -- be faster as it would require 13 searches but would require a full
      -- rebuild of the map for each search.
      -- The heuristic of removing Map elements that were not part of the
      -- previous search reduced the execution time by about 10%, so not a great
      -- improvement.

      Next_Byte : Positive := Dropped + 1;
      Remove_More : Boolean;

   begin -- Blocker
      loop -- until search fails
         Remove_More := Map (Byte_List (Next_Byte));
         -- This location was not visited in the last search
         Exclude (Map, Byte_List (Next_Byte));
         while Remove_More and then Map (Byte_List (Next_Byte + 1)) loop
            -- Remove more Map elements that were not visited in the previous
            -- search.
            Next_Byte := @ + 1;
            Exclude (Map, Byte_List (Next_Byte));
         end loop; -- Remove_More and then Map (Byte_List (Next_Byte + 1))
         for M in Iterate (Map) loop
            Map (M) := True;
         end loop; -- M in Iterate (Map)
         -- Map visits must be reset after each search
         exit when Steps (Map, Size) = Natural'Last;
         -- will terminate with an exception if no blocking byte found
         Next_Byte := @ + 1;
      end loop; -- until search fails
      return Byte_List (Next_Byte);
   end Blocker;

   Byte_List : Byte_Lists.Vector;
   Size, Dropped : Positive;
   Map : Maps.Map;
   Last_Byte : Coordinates;

begin -- December_18
   Read_Input (Byte_List, Size, Dropped);
   Build_Map (Byte_List, Size, Dropped, Map);
   Put_Line ("Part one:" & Steps (Map, Size)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Last_Byte := Blocker (Map, Byte_List, Size, Dropped);
   Put_Line ("Part two:" &
               Last_Byte.X'Img & "," & Trim (Last_Byte.Y'Img, Left));
   DJH.Execution_Time.Put_CPU_Time;
end December_18;
