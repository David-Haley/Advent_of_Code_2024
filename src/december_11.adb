with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with DJH.Execution_Time; use DJH.Execution_Time;
with DJH.Gcd_Lcm;

procedure December_11 is

   subtype Stones is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

   package Stone_Lines is new Ada.Containers.Doubly_Linked_Lists (Stones);
   use Stone_Lines;

   subtype Blinks is Positive range 1 ..25;

   procedure Read_Input (Stone_Line : out Stone_Lines.List) is

      package Stone_IO is new Ada.Text_IO.Integer_IO (Stones);
      use Stone_IO;

      Input_File : File_Type;
      Stone : Stones;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_11.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Stone_Line);
      while not End_Of_Line (Input_File) loop
         Get (Input_File, Stone);
         Append (Stone_Line, Stone);
      end loop; -- not End_Of_Line (Input_File)
      Close (Input_File);
   end Read_Input;

   procedure Blink (Stone_Line : in out Stone_Lines.List) is

      Sc : Stone_Lines.Cursor := First (Stone_Line);

   begin -- Blink
      while Sc /= Stone_Lines.No_Element loop
         if Element (Sc) = 0 then
            Stone_Line (Sc) := 1;
         elsif Trim (Element (Sc)'Img, Left)'Length mod 2 = 0 then
            declare -- Split stone
               Stone_String : String := Trim (Element (Sc)'Img, Left);
               Left, Right : Stones;
            begin
               Left :=
                 Stones'Value (Stone_String (Stone_String'First ..
                               (Stone_String'First + Stone_String'Last - 1) / 2
                              ));
               Right :=
                 Stones'Value (Stone_String ((Stone_String'First +
                                 Stone_String'Last + 1) / 2 .. Stone_String'Last
                              ));
               Stone_Line (Sc) := Right;
               Insert (Stone_Line, Sc, Left);
            end; -- Split stone
         else
            Stone_line (Sc) := Element (Sc) * 2024;
         end if; -- Element (Sc) = 0
         Next (Sc);
      end loop; -- Sc /= Stone_Lines.No_Element
   end Blink;

   Stone_Line :Stone_Lines.List;

begin -- December_11
   Read_Input (Stone_Line);
   for B in Blinks loop
      Blink (Stone_Line);
   end loop; -- B in Blinks
   Put_Line ("Part one:" & Length (Stone_Line)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:");
   DJH.Execution_Time.Put_CPU_Time;
end December_11;
