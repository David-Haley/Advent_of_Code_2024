with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_05 is

   subtype Page_Numbers is Positive;
   subtype Sequence_Numbers is Positive;

   type Rules is Record
      First, Second : Page_Numbers;
   end record; -- Rules

   package Rule_Lists is new Ada.Containers.Doubly_Linked_Lists (Rules);
   use Rule_Lists;

   package Sequences is new
     Ada.Containers.Ordered_Maps (Sequence_Numbers, Page_Numbers);
   use Sequences;

   package Page_Indices is new
     Ada.Containers.Ordered_Maps (Page_Numbers, Sequence_Numbers);
   use Page_Indices;

   type Updates is record
      Sequence : Sequences.Map := Sequences.Empty_Map;
      Page_Index : Page_Indices.Map := Page_Indices.Empty_Map;
   end record;  -- Updates

   package Update_Lists is new Ada.Containers.Doubly_Linked_Lists (Updates);
   use Update_Lists;

   procedure Read_Input (Rule_List : out Rule_Lists.List;
                         Update_List : out Update_Lists.List) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Rule : Rules;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_05.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Rule_List);
      Clear (Update_List);
      loop -- read one rule
         Get_Line (Input_File, Text);
         exit when Length (Text) = 0;
         Start_At := 1;
         Find_Token (Text, Decimal_Digit_Set, Start_at, Inside, First, Last);
         Rule.First := Page_Numbers'Value (Slice (Text, First, Last));
         Start_At := Last + 2; -- Skip '|'
         Find_Token (Text, Decimal_Digit_Set, Start_at, Inside, First, Last);
         Rule.Second := Page_Numbers'Value (Slice (Text, First, Last));
         Append (Rule_List, Rule);
      end loop; -- read one rule
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         declare -- Update declaration
            Update : Updates;
            Page_Number : Page_Numbers;
            Sequence_Number : Sequence_Numbers := Sequence_Numbers'First;
         begin
            Start_At := 1;
            while Start_At < Length (Text) loop
               Find_Token (Text, Decimal_Digit_Set, Start_at, Inside, First,
                           Last);
               Page_Number := Page_Numbers'Value (Slice (Text, First, Last));
               Insert (Update.Sequence, Sequence_Number, Page_Number);
               Start_At := Last + 2; -- skip ','
               Sequence_Number := @ + 1;
            end loop; -- Start_At < Length (Text)
            for S in Iterate (Update.Sequence) loop
               Insert (Update.Page_Index, Element (S), Key (S));
            end loop; -- S in Iterate (Update.Sequence)
            Append (Update_List, Update);
         end; -- Update declaration
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   function Valid_Single (Rule : in Rules;
                          Update : in Updates) return Boolean is

   begin -- Valid_Single
      if Contains (Update.Page_Index, Rule.First) and
        Contains (Update.Page_Index, Rule.Second) then
         return Update.Page_Index (Rule.First) <
           Update.Page_Index (Rule.Second);
      else
         return True;
      end if; -- Contains (Update.Page_Index, Rule.First) and ...
   end Valid_Single;

   pragma Inline_Always (Valid_Single);

   function Valid (Rule_List : in Rule_Lists.List;
                   Update : in Updates) return Boolean is

      Result : Boolean := True;

   begin -- Valid
      for R in Iterate (Rule_List) loop
         Result := @ and Valid_Single (Element (R), Update);
      end loop; -- R in Iterate (Rule_List)
      return Result;
   end Valid;

   procedure Remove_Printed (Rule_List : in Rule_Lists.List;
                             Update_List : in out Update_Lists.List) is

      U, Next_U : Update_Lists.Cursor := First (Update_List);

   begin -- Remove_Printed
      while U /= Update_Lists.No_element loop
         Next_U := Next (U);
         if Valid (Rule_List, Element (U)) then
            Delete (Update_List, U);
         end if; -- Valid (Rule_List, Element (U))
         U := Next_U;
      end loop; -- U /= Update_Lists.No_element
   end Remove_Printed;

   procedure Fix_Updates (Rule_List : in Rule_Lists.List;
                          Update_List : in out Update_Lists.List) is

      Temp : Page_Numbers;
      First_Index, Second_Index : Sequence_Numbers;

   begin -- Fix_Updates
      for U in Iterate (Update_List) loop
         loop -- Repeat until fixed
            for R in Iterate (Rule_List) loop
               if not Valid_Single (Element (R), Element (U)) then
                  -- Swap the pages that break the rule
                  First_Index := Element (U).Page_Index (Element (R).First);
                  Second_Index := Element (U).Page_Index (Element (R).Second);
                  Temp := Update_List (U).Sequence (Second_Index);
                  Update_List (U).Sequence (Second_Index) :=
                    Update_List (U).Sequence (First_Index);
                  Update_List (U).Sequence (First_Index) := Temp;
                  -- Update the index to reflect the swapped pages
                  Update_List (U).Page_Index (Element (R).First) :=
                    Second_Index;
                  Update_List (U).Page_Index (Element (R).Second) :=
                    First_Index;
               end if; -- not Valid_Single (Element (R), Element (U))
            end loop; -- R in Iterate (Rule_List)
            exit when Valid (Rule_List, Element (U));
         end loop; -- Repeat until fixed
      end loop; -- U in Iterate (Update_List)
   end Fix_Updates;

   pragma Inline_Always (Valid);

   Rule_List : Rule_Lists.List;
   Update_List : Update_Lists.List;
   Middle : Sequence_Numbers;
   Sum : Natural := 0;

begin -- December_05
   Read_Input (Rule_List, Update_List);
   for U in Iterate (Update_List) loop
      if Valid (Rule_List, Element (U)) then
         Middle := (First_Key (Element (U).Sequence) +
                      Last_Key (Element (U).Sequence)) / 2;
         Sum := @ + Element (U).Sequence (Middle);
      end if; -- Valid (Rule_List, Element (U))
   end loop; -- U in Iterate (Update_List)
   Put_Line ("Part one:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Remove_Printed (Rule_List, Update_List);
   Fix_Updates (Rule_List, Update_List);
   Sum := 0;
   -- can sum all updates now because they should all be valid
   for U in Iterate (Update_List) loop
      Middle := (First_Key (Element (U).Sequence) +
                   Last_Key (Element (U).Sequence)) / 2;
      Sum := @ + Element (U).Sequence (Middle);
   end loop; -- U in Iterate (Update_List)
   Put_Line ("Part two:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_05;
