with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_14 is

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

   type Robots is record
      Position : Coordinates;
      Velocity : Coordinates; -- required for overloaded "+" operator
   end record; -- Robot

   package Robot_Lists is new Ada.Containers.Doubly_Linked_Lists (Robots);
   use Robot_Lists;

   procedure Read_Input (Robot_List : out Robot_Lists.List;
                        Columns, Rows : in out Ordinates) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Robot : Robots;
      Delimiters : constant Character_Set := To_Set ("pv=, ");
      Start_At, First : Positive;
      Last : Natural;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_14.txt");
      else
         Open (Input_File, In_File, Argument(1));
         if Argument_Count = 3 then
            Columns := Ordinates'Value (Argument (2));
            Rows := Ordinates'Value (Argument (3));
         end if; -- Argument_Count = 3
      end if; -- Argument_Count = 0
      Clear (Robot_List);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Delimiters, Start_At, Outside, First, Last);
         Robot.Position.X := Ordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Delimiters, Start_At, Outside, First, Last);
         Robot.Position.Y := Ordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Delimiters, Start_At, Outside, First, Last);
         Robot.Velocity.X := Ordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Delimiters, Start_At, Outside, First, Last);
         Robot.Velocity.Y := Ordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Append (Robot_List, Robot);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   procedure Update (Robot : in out Robots;
                     X_Limit, Y_Limit : in Ordinates) is

   begin -- Update
      Robot.Position := @ + Robot.Velocity;
      -- Teleport!
      Robot.Position.X := Robot.Position.X mod (X_Limit + 1);
      Robot.Position.Y := Robot.Position.Y mod (Y_limit + 1);
   end Update;

   pragma Inline_Always (Update);

   function Safety_Factor (Robot_List_In : in Robot_Lists.List;
                           X_Limit, Y_Limit : in Ordinates) return Natural is

      subtype Quadrents is Natural range 0 .. 3;

      type Limit_Ranges is record
         X_Low, X_High, Y_Low, Y_High : Ordinates;
      end record; -- Limits

      type Limits is array (Quadrents) of Limit_Ranges;

      function Count_Robots (Robot_List : in Robot_Lists. List;
                             Quadrent : Quadrents;
                             Limit : in Limits) return Natural is

         Count : Natural := 0;

      begin -- Count_Robots
         for R in Iterate (Robot_List) loop
            if Limit (Quadrent).X_Low <= Element (R).Position.X and
              Element (R).Position.X <= Limit (Quadrent).X_High and
              Limit (Quadrent).Y_Low <= Element (R).Position.Y and
              Element (R).Position.Y <= Limit (Quadrent).Y_High then
               Count := @ + 1;
            end if; -- Limit (Quadrent).X_Low <= Element (R).Position.X and ...
         end loop; -- R in Iterate (Robot_List)
         return Count;
      end Count_Robots;

      pragma Inline_Always (Count_Robots);

      Robot_List : Robot_Lists.List := Copy (Robot_List_In);
      Result : Natural := 1;
      Mid_X : constant Ordinates := (Origin.X + X_Limit) / 2;
      Mid_Y : constant Ordinates := (Origin.Y + Y_Limit) / 2;
      Limit : constant Limits := (0 => (Origin.X, Mid_X - 1,
                                        Origin.Y, Mid_Y - 1),
                                  1 => (Mid_X + 1, X_Limit,
                                        Origin.Y, Mid_Y - 1),
                                  2 => (Mid_X + 1, X_Limit,
                                        Mid_Y + 1, Y_Limit),
                                  3 => (Origin.X, Mid_X - 1,
                                        Mid_Y + 1, Y_Limit));

   begin -- Safety_Factor
      for T in Positive range 1 .. 100 loop
         for R in Iterate (Robot_List) loop
            Update (Robot_List (R), X_Limit, Y_Limit);
         end loop; -- R in Iterate (Robot_List)
      end Loop;
      for Q in Quadrents loop
         Result := @ * Count_Robots (Robot_List, Q, Limit);
      end loop; -- Q in Quadrents
      return Result;
   end Safety_Factor;

   procedure Display (Robot_List : in out Robot_Lists.List;
                      X_Limit, Y_Limit : in Ordinates) is

      subtype X_Coordinates is Ordinates range Origin.X .. X_Limit;
      subtype Y_Coordinates is Ordinates range Origin.Y .. Y_Limit;

      Screen_Buffer : array (X_Coordinates, Y_Coordinates) of Character :=
        (others => (others => ' '));

   begin -- Display
      for R in Iterate (Robot_List) loop
         Screen_Buffer (Element (R).Position.X, Element (R).Position.Y) := '#';
      end loop; -- R in Iterate (Robot_List)
      New_Line;
      for Y in Y_Coordinates loop
         for X in X_Coordinates loop
            Put (Screen_Buffer (X, Y));
         end loop; -- X in X_Coordinates
         New_Line;
      end loop; -- Y in Y_Coordinates
   end Display;

   procedure Tree (Robot_List : in out Robot_Lists.List;
                   X_Limit, Y_Limit : in Ordinates;
                   Ticks : out Positive) is

      -- Found by saving every image for 101 * 103 Ticks to a text file and
      -- then searching with a test editor for ##########. This needs to be
      -- revisited for a general solution. Looking for a maximal clump at the
      -- centre of the screen would work for my input.

      Tree_Top : constant Coordinates := ((Origin.X + X_Limit) /2, Origin.Y);

   begin -- Tree
      Ticks := 1;
      loop -- one tick
         for R in Iterate (Robot_List) loop
            Update (Robot_List (R), X_Limit, Y_Limit);
         end loop; -- R in Iterate (Robot_List)
         exit when Ticks = 7569;
         Ticks := @ + 1;
      end loop; -- one tick
   end Tree;

   Columns : Ordinates := 101;
   Rows : Ordinates := 103;
   X_Limit, Y_limit : Ordinates;
   Robot_List : Robot_Lists.List;
   Ticks : Positive;

begin -- December_14
   Read_Input (Robot_List, Columns, Rows);
   X_Limit := Columns - 1;
   Y_limit := Rows - 1;
   Put_Line ("Part one:" & Safety_Factor(Robot_List, X_Limit, Y_limit)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Tree (Robot_List, X_Limit, Y_limit, Ticks);
   Put_Line ("Part two:" & Ticks'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Display (Robot_List, X_Limit, Y_limit);
end December_14;
