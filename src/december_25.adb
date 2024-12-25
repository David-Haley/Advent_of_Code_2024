with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_25 is

   subtype Pin_Indices is Positive range 1 ..5;
   subtype Pin_Heights is Natural range 0 .. 5;
   type Pins is array (Pin_Indices) of Pin_Heights;

   package Pin_Lists is new Ada.Containers.Doubly_Linked_Lists (Pins);
   use Pin_Lists;

   procedure Read_Input (Key_List, Lock_List : out Pin_Lists.List) is

      subtype Five_Pins is String (Pin_Indices);
      Input_File : File_Type;
      Pin : Pins;
      Five_Pin : Five_Pins;
      Lock_First, Key_Last : constant Five_Pins := "#####";
      Key_First, Lock_Last : constant Five_Pins := ".....";
      Is_Key, Is_Lock : Boolean;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_25.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Key_List);
      Clear (Lock_List);
      while not End_Of_File (Input_File) loop
         Get (Input_File, Five_Pin);
         Skip_Line (Input_File);
         Is_Lock := Five_Pin = Lock_First;
         Is_Key := Five_Pin = Key_First;
         if not Is_Lock and not Is_Key then
            raise Program_Error with  "Not valid key or lock expected """ &
              Key_First & """ or """ & Lock_First & """ found """ &
              Five_Pin;
         end if; -- Five_Pin /= Lock_First and Five_Pin /= Key_First
         Pin := (others => 0);
         for H in Pin_Heights range 1 .. Pin_Heights'Last loop
            Get (Input_File, Five_Pin);
            Skip_Line (Input_File);
            for P in Pin_Indices loop
               if Five_Pin (P) = '#' then
                  Pin (P) := @ + 1;
               end if; -- Five_Pin (P) = '#'
            end loop; -- P in Pin_Indices
         end loop; -- H in Pin_Heights range 1 .. Pin_Heights'Last
         Get (Input_File, Five_Pin);
         Skip_Line (Input_File);
         if Five_Pin = Lock_Last and Is_Lock then
            Append (Lock_List, Pin);
         elsif Five_Pin = Key_Last and Is_Key then
            Append (Key_List, Pin);
         else
            raise Program_Error with "Not valid eod of key or lock expected """
              & Key_Last & """ or """ & Lock_Last & """ found """ & Five_Pin;
         end if; -- Five_Pin = Lock_First
         if not End_Of_File (Input_File) then
            Skip_Line (Input_File);
         end if; -- not End_Of_File (Input_File)
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   function Count_Fits (Key_List, Lock_List : in Pin_Lists.List)
                        return Natural is

      Result : Natural := 0;
      All_Fit : Boolean;

   begin -- Count_Fits
      for K in Iterate (Key_List) loop
         for L in Iterate (Lock_List) loop
            All_Fit := True;
            for P in Pin_Indices loop
               All_Fit := All_Fit and Element (K) (P) + Element (L) (P) <=
                 Pin_Heights'Last;
            end loop; -- P in Pin_Indices
            if All_Fit then
               Result := @ + 1;
            end if; -- All_Fit
         end loop; -- L in Iterate (Lock_List)
      end loop; -- K in Iterate (Key_List)
      return Result;
   end Count_Fits;

   Key_List, Lock_List : Pin_Lists.List;

begin -- December_25
   Read_Input (Key_List, Lock_List);
   Put_Line ("Part one:" & Count_Fits (Key_List, Lock_List)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_25;
