with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_02 is

   subtype Report_Indices is Positive;
   subtype Level_Indices is Positive;
   subtype Levels is Positive;

   package Reports is new Ada.Containers.Vectors (Level_indices, Levels);
   use Reports;

   package Report_Stores is new
     Ada.Containers.Vectors (Report_indices, Reports.Vector);
   use Report_Stores;


   procedure Read_Input (Report_Store : out Report_Stores.Vector) is

      Package Level_IO is new Ada.Text_IO.Integer_IO (Levels);
      use Level_IO;

      Input_File : File_Type;
      Level : Levels;

   begin -- Read_Input
      Clear (Report_Store);
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_02.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      while not End_Of_File (Input_File) loop
         declare -- Read line
            Report : Reports.Vector := Reports.Empty_Vector;
         begin
            while not End_Of_Line (Input_File) loop
               Get (Input_File, Level);
               Append (Report, Level);
            end loop; --while not End_Of_Line (Input_File)
            Append (Report_Store, Report);
         end; -- Read line
         if not End_Of_File (Input_File) then
            Skip_Line (Input_File);
         end if; -- not End_Of_File (Input_File)
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   function Valid_Report (Report : in Reports.Vector) return Boolean is

      I : Level_Indices := First_Index (Report);
      J : Report_Indices := I + 1;
      Increasing : Boolean := Report (I) < Report (J);
      Result : Boolean := True;

   begin -- Valid_Report
      loop -- Check one pair of levels
         if Increasing then
            Result := @ and Report (I) < Report (J);
         else
            Result := @ and Report (I) > Report (J);
         end if;
         Result := @ and abs (Report (I) - Report (J)) >= 1 and
         abs (Report (I) - Report (J)) <= 3;
         exit when J >= Last_Index (Report);
         I := J;
         J := I + 1;
      end loop; -- Check one pair of levels
      return Result;
   end Valid_Report;

   function Valid_Dampen (Report_In : in Reports.Vector) return Boolean is

      Report : Reports.Vector;
      D : Level_Indices := First_Index (Report_In);
      Result : Boolean := False;

   begin -- Valid_Dampen
      loop -- One deletion
         Report := Copy (Report_In);
         Delete (Report, D);
         Result := Valid_Report (Report);
         exit when Result or D >= Last_Index (Report_In);
         D := @ + 1;
      end loop; -- One deletion
      return Result;
   end Valid_Dampen;

   Report_Store : Report_Stores.Vector;
   Count : Natural := 0;

begin -- December_02
   Read_Input (Report_Store);
   For R in Iterate (Report_Store) loop
      if Valid_Report (Element (R)) then
         Count := @ + 1;
      end if; -- Valid_Report (Element (R))
   end loop; -- R in Iterate (Report_Store)
   Put_Line ("Part one:" & Count'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Count := 0;
   For R in Iterate (Report_Store) loop
      if Valid_Report (Element (R)) then
         Count := @ + 1;
      elsif Valid_Dampen (Element (R)) then
         Count := @ + 1;
      end if; -- Valid_Report (Element (R))
   end loop; -- R in Iterate (Report_Store)
   Put_Line ("Part two:" & Count'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_02;
