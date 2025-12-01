#================================================================================
# Class A Visitors
#================================================================================

def visit_CompilationUnit(self, node: ASTNode):
        """
        Processes all program units within a compilation unit.
        This is the entry point for the symbolic execution traversal.
        """
        # 1. Initialize self.path_constraints with a base path.
        # This sets up the main execution path which is unconditionally true.

        # 2. Iterate through each ProgramUnit child node.
        # A CompilationUnit is composed of one or more ProgramUnits.
        for child in self._get_children(node):
            # 3. For each child, call self.visit to process it under the base true path condition.
            # This kicks off the traversal for each program defined in the source file.
            self.visit(child)

        # 4. After visiting all children, construct the final set of path-dependent
        # implications and add them to the Z3 solver instance.
        self.finalize_and_add_to_solver()

def visit_ProgramUnit(self, node: ASTNode):
        """
        Processes a COBOL ProgramUnit by first handling all declarations in the
        DataDivision and then processing the logic in the ProcedureDivision.
        This ensures variables are defined before they are used in procedural code.
        """
        # Step 1: This method acts as a structural organizer.

        # Step 2: First Pass (Declarations)
        # Find and process the DataDivision to populate declarations first.
        data_division_child = self._find_child_node(node, "DataDivision")
        if data_division_child:
            # a. Find the DataDivision child node.
            # b. Set the declaration context flag.
            self._is_in_declaration_context = True
            # c. Visit the DataDivision to populate declarations, PIC info, etc.
            self.visit(data_division_child)
            # d. Unset the declaration context flag.
            self._is_in_declaration_context = False

        # Step 3: Second Pass (Procedure Logic)
        # Find and process the ProcedureDivision for program logic.
        procedure_division_child = self._find_child_node(node, "ProcedureDivision")
        if procedure_division_child:
            # a. Find the ProcedureDivision child node.
            # b. Visit the ProcedureDivision to build logical constraints.
            self.visit(procedure_division_child)

        # Step 4: Recursively visit any nested ProgramUnits.
        # The call to self.visit will re-apply this same two-pass logic for
        # each nested program.
        nested_programs = self._get_children(node, "ProgramUnit")
        for nested_program in nested_programs:
            self.visit(nested_program)

        # Step 5: Visit the EndProgramStatement.
        # This is typically for validation and completes the program scope.
        end_program_statement_child = self._find_child_node(node, "EndProgramStatement")
        if end_program_statement_child:
            self.visit(end_program_statement_child)

def visit_EndProgramStatement(self, node):
        """
        Handles the END PROGRAM statement. This is a terminal statement and
        does not generate any Z3 constraints. It simply marks the end of a
        program unit.

        Goal: Mark the end of a program unit. No Z3 logic is required.

        1. This is a terminal statement. The visitor method can be a no-op or
            log that the end of a program unit was reached. It does not
            generate any declarations or constraints.
        """
        # Per the plan, this is a terminal statement. No Z3 logic is required.
        # The method acts as a no-op, effectively stopping this branch of AST traversal.
        # Optionally, one could log the event for debugging purposes:
        # program_name_node = self._find_child_node(node, 'ProgramName')
        # if program_name_node:
        #     program_name = self._get_source_text(self._get_first_child(program_name_node))
        #     print(f"INFO: Reached END PROGRAM {program_name}.")
        pass

def visit_DataDivision(self, node: ASTNode):
        """
        Processes the DATA DIVISION by visiting each of its constituent sections.

        Goal: Process all sections within the DATA DIVISION.

        1.  This node acts as a container.
        2.  Iterate through each `DataDivisionSection` child (e.g., `WorkingStorageSection`, `LinkageSection`).
        3.  Call `self.visit(child)` for each section to process its contents.
        """
        # Step 1: This node acts as a container, so its primary role is to guide the
        # traversal to its children sections.

        # Step 2: Iterate through each child section of the DataDivision.
        children = self._get_children(node)
        for child in children:
            # Step 3: Call self.visit() for each section. The generic visit method
            # will dispatch to the correct specific visitor (e.g., visit_WorkingStorageSection)
            # while passing down the current path condition.
            self.visit(child)

def visit_DataDivisionSection(self, node: ASTNode):
        """
        Delegates processing to the specific section type within the Data Division.

        This method implements the plan:
        1.  Recognizes this node as a wrapper.
        2.  Identifies the specific child node (e.g., WorkingStorageSection).
        3.  Calls self.visit() on that child, passing the current path condition.
        """
        # Step 1 & 2: This node is a wrapper. We identify its specific child.
        # The grammar ensures DataDivisionSection has one child:
        # WorkingStorageSection, LinkageSection, or LocalStorageSection.
        # We use a helper to get the first (and only) child.
        child_section_node = self._get_first_child(node)

        # Step 3: Delegate processing by calling self.visit on the child.
        # This will invoke the correct visitor (e.g., visit_WorkingStorageSection)
        # for the specific section, passing along the path condition.
        if child_section_node:
            return self.visit(child_section_node)

        return None

def visit_WorkingStorageSection(self, node: ASTNode):
        """
        Processes all data description entries in the working storage section.

        Goal: Process all data description entries in working storage.

        1.  Iterate through each `DataDescriptionEntry` child node.
        2.  Call `self.visit(child)` for each entry to declare variables and add their constraints.
        """
        # Step 1: Iterate through each DataDescriptionEntry child node.
        data_description_entries = self._get_children(node, "DataDescriptionEntry")
        for child in data_description_entries:
            # Step 2: Call self.visit(child) for each entry.
            self.visit(child)

def visit_LinkageSection(self, node):
        """
        Processes all data description entries in the linkage section.
        The logic is identical to WorkingStorageSection for declaration purposes.
        """
        # 1. Iterate through each `DataDescriptionEntry` child node.
        data_description_entries = self._get_children(node, filter_type="DataDescriptionEntry")
        for child in data_description_entries:
            # 2. Call `self.visit(child)` for each entry. The logic is identical
            # to `WorkingStorageSection` for the purpose of declaration.
            self.visit(child)

def visit_LocalStorageSection(self, node: ASTNode):
        """
        Processes all data description entries in the LOCAL-STORAGE SECTION.

        This method follows the plan by iterating through each DataDescriptionEntry
        and dispatching the visit call to handle its declaration, similar to
        how WorkingStorageSection is processed.
        """
        # Goal: Process all data description entries in local storage.

        # 1. Iterate through each `DataDescriptionEntry` child node.
        data_description_entries = self._get_children(node, "DataDescriptionEntry")
        for child in data_description_entries:
            # 2. Call `self.visit(child)` for each entry.
            # The logic is identical to `WorkingStorageSection` for the purpose of declaration.
            self.visit(child)

def visit_DataDescriptionEntry(self, node: ASTNode):
        """
        Delegates processing to the specific format of the data description entry.
        This node acts as a wrapper for different formats.
        """
        # Step 1: This is a wrapper node.
        # The primary purpose of this node is to contain the actual format node.

        # Step 2: Identify the child node, which will be DataDescriptionEntryFormat1.
        # According to the grammar, DataDescriptionEntry directly contains DataDescriptionEntryFormat1.
        # We will find this specific child to process it.
        child = self._get_first_child(node, "DataDescriptionEntryFormat1")

        # Step 3: Call self.visit(child) on that child.
        # Delegate the processing to the visitor method for the specific format.
        # If for some reason the specific child isn't found, we can fall back to a generic visit
        # to ensure we don't miss any other potential children, though this is unlikely.
        if child:
            return self.visit(child)
        else:
            # Fallback in case of unexpected AST structure
            return self.generic_visit(node)

def visit_DataDescriptionEntryFormat1(self, node):
        """
        The core of variable declaration. Creates logical and physical Z3 variables,
        parses PIC/USAGE/VALUE clauses, and generates linking constraints.
        """
        import math
        from z3 import Int, Real, String, BitVec, IntVal, RealVal, StringVal
        from z3 import And, Or, If, Length, ULE, UGE
        from z3 import BV2Int, ToReal, Extract

        # Nested helper functions to implement the plan's logic in a self-contained way.
    
        def _calculate_byte_size(pic_info: Dict, usage: str) -> int:
            """
            Calculates the storage size in bytes for a variable based on its PIC and USAGE.
            Implements plan step 5.b.iii.
            """
            usage = usage.upper()
            total_digits = pic_info.get('total_digits', 0)
        
            if usage in ['BINARY', 'COMP', 'COMPUTATIONAL', 'COMP-4', 'COMP-5']:
                if 1 <= total_digits <= 4:
                    return 2
                elif 5 <= total_digits <= 9:
                    return 4
                elif 10 <= total_digits <= 18:
                    return 8
                else: # Should be an error, but default to 8 for safety
                    return 8
            elif usage in ['PACKED-DECIMAL', 'COMP-3', 'COMPUTATIONAL-3']:
                # ceil((total digits + sign) / 2 nibbles per byte)
                return math.ceil((total_digits + 1) / 2)
            elif usage == 'DISPLAY':
                # For DISPLAY, size is simply the character length of the picture string.
                return pic_info['char_length']
            else:
                # Default case, assumes 1 byte per character like DISPLAY
                return pic_info['char_length']

        def _generate_link_constraint(logical_var, physical_var, pic_info, usage):
            """
            Generates and adds the constraint linking the logical and physical variables.
            Implements plan step 5.b.v (Physical-Logical Link).
            """
            usage = usage.upper()
            is_signed = pic_info['is_signed']
            decimal_places = pic_info['decimal_places']
        
            # --- USAGE BINARY ---
            if usage in ['BINARY', 'COMP', 'COMPUTATIONAL', 'COMP-4', 'COMP-5']:
                physical_as_int = BV2Int(physical_var, is_signed)
                if decimal_places > 0:
                    linked_val = ToReal(physical_as_int) / (10**decimal_places)
                else:
                    linked_val = physical_as_int
                self._add_constraint(logical_var == linked_val)

            # --- USAGE PACKED-DECIMAL ---
            elif usage in ['PACKED-DECIMAL', 'COMP-3', 'COMPUTATIONAL-3']:
                total_digits = pic_info['total_digits']
                num_bytes = math.ceil((total_digits + 1) / 2)
            
                # Build the unsigned value from nibbles
                unscaled_value = IntVal(0)
                for i in range(total_digits):
                    byte_idx = i // 2
                    bit_pos_of_byte = (num_bytes * 8) - 1 - (byte_idx * 8)
                    if i % 2 == 0: # High nibble
                        nibble = Extract(bit_pos_of_byte, bit_pos_of_byte - 3, physical_var)
                    else: # Low nibble
                        nibble = Extract(bit_pos_of_byte - 4, bit_pos_of_byte - 7, physical_var)
                
                    self._add_constraint(ULE(nibble, 9)) # Each nibble must be a digit
                    unscaled_value = unscaled_value * 10 + BV2Int(nibble)

                # Get the sign nibble (last nibble)
                sign_byte_idx = (total_digits) // 2
                sign_bit_pos_of_byte = (num_bytes * 8) - 1 - (sign_byte_idx * 8)
                sign_nibble = Extract(sign_bit_pos_of_byte - 4, sign_bit_pos_of_byte - 7, physical_var)

                # Apply sign
                signed_value = If(And(is_signed, sign_nibble == 0xD), -unscaled_value, unscaled_value)
                if is_signed:
                    self._add_constraint(Or(sign_nibble == 0xC, sign_nibble == 0xD, sign_nibble == 0xF)) # C,F=+ D=-
                else: # Unsigned
                    self._add_constraint(sign_nibble == 0xF)
            
                # Scale for decimal and link
                if decimal_places > 0:
                    linked_val = ToReal(signed_value) / (10**decimal_places)
                else:
                    linked_val = signed_value
                self._add_constraint(logical_var == linked_val)
        
            # --- USAGE DISPLAY ---
            elif usage == 'DISPLAY':
                # For Alphanumeric/String types
                if pic_info['category'] in ['alphabetic', 'alphanumeric', 'edited']:
                    constraints = []
                    # Assuming EBCDIC for '0'-'9' is 0xF0-0xF9
                    # For simplicity, we model a basic string-to-byte array link
                    for i in range(pic_info['char_length']):
                        char_as_bv = Extract(i*8+7, i*8, physical_var) # Get i-th byte from physical var
                        # This link is symbolic and doesn't enforce character sets for now
                        # A full implementation would require a large ITE or a mapping function.
            
                # For Numeric types (Zoned Decimal)
                elif pic_info['category'] == 'numeric':
                    # This links the numeric value to its character representation.
                    # Example for S999: value 123 -> "123", value -123 -> "12L" (overpunch K)
                    # This is highly complex. For now, we establish a basic numeric link.
                    # The value is reconstructed from the ASCII/EBCDIC characters.
                    unscaled_value = IntVal(0)
                    for i in range(pic_info['char_length']):
                        char_byte = Extract((pic_info['char_length'] - 1 - i) * 8 + 7, (pic_info['char_length'] - 1 - i) * 8, physical_var)
                        # Assume last byte might hold an overpunched sign, so skip it for now.
                        if i < pic_info['char_length']-1 or not is_signed:
                            # Assuming EBCDIC '0' = 0xF0
                            digit = BV2Int(char_byte) - 0xF0 
                            unscaled_value = unscaled_value + (digit * (10**i))

                    # A proper implementation of sign, scaling etc. is needed.
                    # This is a placeholder for the complex Zoned Decimal decoding.
                    if decimal_places > 0:
                        linked_val = ToReal(unscaled_value) / (10**decimal_places)
                    else:
                        linked_val = unscaled_value
                    # This constraint is simplified and doesn't fully capture sign logic.
                    self._add_constraint(logical_var >= linked_val)


        def _python_to_z3_literal(py_val, z3_var):
            """ Converts a Python value to a Z3 literal matching the variable's type. """
            if isinstance(z3_var, z3.ArithRef): # Int or Real
                # Handles numeric literals like 123, -123.45, "+100"
                try:
                    num = float(py_val)
                    if z3.is_int(z3_var):
                        return IntVal(int(num))
                    else:
                        return RealVal(str(py_val)) # Use string to avoid float precision issues
                except (ValueError, TypeError):
                    # Handle figurative constants if they resolve to numbers
                    return None
            elif isinstance(z3_var, z3.SeqRef): # String
                return StringVal(py_val)
            return None


        # 1. Initialization
        level = None
        name = None
        pic_info = None
        usage = 'DISPLAY'  # COBOL default
        value_clause_py_val = None
    
        # 2. Extract Level and Name
        children = self._get_children(node)
        level_node = children[0]
        level = int(self._get_node_text(level_node))
    
        clause_start_index = 1
        next_node = self._get_nth_child(node, 1)

        if next_node and self._get_node_text(next_node) == 'FILLER':
            if not hasattr(self, '_filler_counter'): self._filler_counter = 0
            self._filler_counter += 1
            name = f"__FILLER_{self._filler_counter}"
            clause_start_index = 2
        elif next_node and next_node.get('Node') == 'DataName':
            name = self.visit(next_node)
            clause_start_index = 2
        else:
            if not hasattr(self, '_filler_counter'): self._filler_counter = 0
            self._filler_counter += 1
            name = f"__FILLER_{self._filler_counter}"
    
        # 3. Manage Hierarchy Context
        while self._current_group_context and self._current_group_context[-1]['level'] >= level:
            self._current_group_context.pop()
    
        context_names = [ctx['name'] for ctx in self._current_group_context]
        full_name = ".".join(context_names + [name])
        self._current_data_item_being_declared = full_name

        # 4. Parse Clauses
        for child in children[clause_start_index:]:
            node_type = child.get("Node")
            if node_type == 'DataPictureClause':
                pic_info = self.visit(child)
            elif node_type == 'DataUsageClause':
                usage = self.visit(child) # Should return a string like 'BINARY'
            elif node_type == 'DataValueClause':
                value_clause_py_val = self.visit(child) # Should return a Python value

        # 5. Post-Clause Processing (The Declaration Logic)
        if pic_info:  # Elementary Item
            # 5.b.i Update pic_info with context
            pic_info['name'] = full_name
            pic_info['level'] = level
            pic_info['usage'] = usage

            # 5.b.ii Create Logical Variable
            logical_var = None
            if pic_info['category'] == 'numeric' and not pic_info['has_decimal']:
                logical_var = Int(full_name)
            elif pic_info['category'] == 'numeric' and pic_info['has_decimal']:
                logical_var = Real(full_name)
            else: # Alphanumeric, Alphabetic, Edited
                logical_var = String(full_name)

            # 5.b.iii Create Physical Variable
            byte_size = _calculate_byte_size(pic_info, usage)
            physical_var = BitVec(f"{full_name}_physical", byte_size * 8)
        
            # 5.b.iv Store Declarations
            self._add_declaration(full_name, logical_var)
            self._add_declaration(f"{full_name}_physical", physical_var)
            self.pic_info[full_name] = pic_info

            # 5.b.v Generate Linking and Range Constraints
            if pic_info['category'] == 'numeric':
                total_digits, dec_places = pic_info['total_digits'], pic_info['decimal_places']
                max_unscaled = (10**total_digits) - 1
                min_unscaled = -max_unscaled if pic_info['is_signed'] else 0
                if pic_info['has_decimal']:
                    scale = 10**dec_places
                    self._add_constraint(And(logical_var >= RealVal(f"{min_unscaled}/{scale}"), 
                                                logical_var <= RealVal(f"{max_unscaled}/{scale}")))
                else:
                    self._add_constraint(And(logical_var >= min_unscaled, logical_var <= max_unscaled))
            else: # String-like
                self._add_constraint(Length(logical_var) == pic_info['char_length'])
        
            _generate_link_constraint(logical_var, physical_var, pic_info, usage)

        else:  # Group Item
            # 5.c.i Push group context for children
            self._current_group_context.append({'name': name, 'level': level})

        # 6. Handle VALUE Clause (Post-Declaration)
        if value_clause_py_val is not None and pic_info:
            logical_var = self.declarations.get(full_name)
            if str(logical_var):
                # z3_val = _python_to_z3_literal(value_clause_py_val, logical_var)
                # print(z3_val)
                if value_clause_py_val is not None:
                    constraint = (logical_var == value_clause_py_val)
                    self.update_assignment(full_name, 'VALUE', constraint)

        # 7. Cleanup
        self._current_data_item_being_declared = None
    
        return None

def visit_DataPictureClause(self, node: ASTNode):
        """
        Parses a COBOL PICTURE clause string and returns a structured dictionary
        of its detailed properties. This method does not create Z3 variables;
        it provides the necessary metadata for the calling visitor to do so.
        """
        # Step 1: Adhere to the goal of parsing the PictureString without creating Z3 variables.

        # Step 2: Find the PictureString child node.
        pic_string_node = self._find_child_node(node, "PictureString")
        if not pic_string_node:
            # This implies a malformed AST; return an empty dictionary.
            return {}
    
        raw_pic_str = self._get_source_text(pic_string_node)

        # Step 3: Define a detailed helper function `_parse_pic_string` within this visitor.
        def _parse_pic_string(pic_str: str) -> Dict[str, Any]:
            """
            Parses a raw PIC string, expands it, and analyzes its characters to
            produce a structured information dictionary.
            """
            # Step 4a: Initialize the pic_info dictionary with default values.
            pic_info = {
                "original_pic": pic_str,
                "expanded_pic": "",
                "category": "unknown",
                "is_signed": False,
                "has_decimal": False,
                "is_edited": False,
                "total_digits": 0,
                "integer_digits": 0,
                "decimal_places": 0,
                "leading_p_count": 0,
                "trailing_p_count": 0,
                "char_length": 0,
            }

            # Step 4b: `original_pic` is already stored during initialization.
        
            # Pre-process for case-insensitivity, which is standard for PIC strings.
            pic_str_upper = pic_str.upper()

            # Step 4c: Expand cardinality (e.g., 9(3) becomes 999).
            expanded_str = re.sub(r'([A-Z0-9])\((\d+)\)', 
                                    lambda m: m.group(1) * int(m.group(2)), 
                                    pic_str_upper)
        
            # Step 4d: Store the fully expanded PIC string.
            pic_info['expanded_pic'] = expanded_str

            # Step 4e: Analyze the characters of the expanded string.
        
            # Basic flags for sign and assumed decimal
            pic_info['is_signed'] = 'S' in expanded_str
            pic_info['has_decimal'] = 'V' in expanded_str
        
            # Check for any editing symbols to set the is_edited flag.
            editing_symbols = {'Z', '*', '.', ',', '$', 'B', '/', '0', '+', '-'}
            pic_info['is_edited'] = any(c in editing_symbols for c in expanded_str)

            # Calculate char_length (storage size).
            # S, V, and P are not counted in the size of the data item.
            non_storage_chars = {'S', 'V', 'P'}
            pic_info['char_length'] = sum(1 for c in expanded_str if c not in non_storage_chars)
        
            # Count Scaling Positions (P).
            temp_for_p = expanded_str[1:] if expanded_str.startswith('S') else expanded_str
        
            leading_p_match = re.match(r'^(P+)', temp_for_p)
            if leading_p_match:
                pic_info['leading_p_count'] = len(leading_p_match.group(1))
        
            trailing_p_match = re.search(r'(P+)$', temp_for_p)
            if trailing_p_match and (not leading_p_match or trailing_p_match.start() > leading_p_match.end() - 1):
                    pic_info['trailing_p_count'] = len(trailing_p_match.group(1))

            # Count integer and decimal digits.
            v_index = expanded_str.find('V')
            integer_part_str = expanded_str[:v_index] if v_index != -1 else expanded_str
            decimal_part_str = expanded_str[v_index + 1:] if v_index != -1 else ""
        
            numeric_placeholders = {'9', 'Z', '*'}
            pic_info['integer_digits'] = sum(1 for c in integer_part_str if c in numeric_placeholders)
            pic_info['decimal_places'] = sum(1 for c in decimal_part_str if c == '9')
        
            # Calculate total logical digits (numeric placeholders + scaling positions).
            pic_info['total_digits'] = (
                pic_info['integer_digits'] + 
                pic_info['decimal_places'] + 
                pic_info['leading_p_count'] + 
                pic_info['trailing_p_count']
            )
        
            # Step 4f: Determine Category based on the specified precedence.
            core_symbols = {c for c in expanded_str if c not in 'SVP'}
        
            if core_symbols and core_symbols.issubset({'A', 'B'}):
                pic_info['category'] = 'alphabetic'
            elif 'X' in expanded_str or 'G' in expanded_str or 'N' in expanded_str:
                pic_info['category'] = 'alphanumeric'
            elif pic_info['is_edited']:
                pic_info['category'] = 'edited'
            else:
                pic_info['category'] = 'numeric'
            
            # Step 4g: Return the populated dictionary.
            return pic_info

        # Step 5: Call the helper function on the PictureString's source text.
        pic_details = _parse_pic_string(raw_pic_str)
    
        # Step 6: Return the resulting dictionary to the calling DataDescriptionEntryFormat1 visitor.
        return pic_details

def visit_PictureString(self, node):
        """
        Goal: Provide its raw text to the parent DataPictureClause.

        1. This node is a container for the PIC components.
        2. Return the full source text of this node, `self._get_source_text(node)`.
            The parent visitor will handle the parsing.
        """
        # The PictureString node holds the complete raw PIC string (e.g., "S9(4)V99").
        # The parent DataPictureClause visitor will receive this raw string and
        # be responsible for parsing it into its constituent parts (sign, digits, etc.).
        return self._get_source_text(node)

def visit_PictureChars(self, node):
        """
        Handles the PictureChars node. This is a low-level grammar component.
        The full PictureString is processed by the parent DataPictureClause visitor.
        Therefore, this visitor performs no action.
        """
        # This rule is a low-level grammar component.
        # The visitor for `DataPictureClause` will process the full `PictureString` directly.
        # This visitor can be a no-op.
        return None

def visit_PictureCardinality(self, node):
        """
        Handles the PictureCardinality node, e.g., the '(5)' in 'PIC 9(5)'.

        This is a low-level grammar component. The parent `DataPictureClause`
        visitor is responsible for processing the entire `PictureString` source text,
        including expanding these cardinality expressions. Therefore, this method
        is a no-op as the logic is handled at a higher level.
        """
        # No action needed, as per the plan. The full picture string is processed by the parent.
        pass


def visit_DataSignClause(self, node: ASTNode):
        """
        Parses a SIGN clause and returns its properties for the parent node to use.

        Goal: Provide sign information to the parent `DataDescriptionEntry`.
        1.  Parse the clause to determine the sign's properties (`LEADING` or `TRAILING`, and if it is `SEPARATE`).
        2.  Return a structured object, e.g., `{'position': 'leading', 'is_separate': True}`.
        3.  The parent `DataDescriptionEntryFormat1` will use this information:
            a.  To adjust the `char_length` in `pic_info` if the sign is separate.
            b.  To generate the correct physical-to-logical linking constraints, modeling how the sign is encoded in the EBCDIC or packed-decimal representation.

        Args:
            node: The ASTNode for the DataSignClause.

        Returns:
            A dictionary containing the parsed sign information.
        """
        # Step 1: Parse the clause to determine the sign's properties
        # Initialize the dictionary to hold the properties. is_separate defaults to False.
        sign_properties = {
            'position': 'trailing',  # Default position if not specified
            'is_separate': False
        }

        # Check for LEADING or TRAILING keywords
        if self._find_child_node(node, "LEADING"):
            sign_properties['position'] = 'leading'
        elif self._find_child_node(node, "TRAILING"):
            sign_properties['position'] = 'trailing'

        # Check for the SEPARATE keyword
        if self._find_child_node(node, "SEPARATE"):
            sign_properties['is_separate'] = True

        # Step 2: Return the structured object for the parent node to process.
        return sign_properties

def visit_DataUsageClause(self, node):
        """
        Parses a DataUsageClause to determine the data item's storage format.
        This method normalizes various USAGE specifications (like COMP, COMP-3)
        into a standard set of types ('BINARY', 'PACKED-DECIMAL', etc.) and
        returns this normalized string to the parent DataDescriptionEntry visitor.
        """
        # Step 1: Extract the source text of the usage type.
        # The actual usage keyword (e.g., 'COMP', 'PACKED-DECIMAL') is the last
        # child of the DataUsageClause node, following optional 'USAGE IS'.
        type_node = self._get_nth_child(node, -1)
        if not type_node:
            # A valid DataUsageClause must have a type. If not, this is an
            # anomaly, but we'll default to 'DISPLAY' as it's the implicit default.
            return 'DISPLAY'

        # Get the raw text (e.g., "COMP-3", "COMPUTATIONAL") and make it uppercase for matching.
        raw_usage_text = self._get_source_text(type_node).upper()

        # Step 2: Normalize the text.
        # This map translates various COBOL USAGE keywords into a smaller,
        # canonical set of types that drive the Z3 variable representation.
        USAGE_MAP = {
            # Binary Types
            'BINARY': 'BINARY',
            'COMP': 'BINARY',
            'COMPUTATIONAL': 'BINARY',
            'COMP-4': 'BINARY',
            'COMPUTATIONAL-4': 'BINARY',

            # Special Truncation Binary Type
            'COMP-5': 'COMP-5',
            'COMPUTATIONAL-5': 'COMP-5',

            # Packed Decimal Types
            'PACKED-DECIMAL': 'PACKED-DECIMAL',
            'COMP-3': 'PACKED-DECIMAL',
            'COMPUTATIONAL-3': 'PACKED-DECIMAL',

            # Character Display Types
            'DISPLAY': 'DISPLAY',
            'DISPLAY-1': 'DISPLAY-1', # Double-Byte Character Set

            # Floating Point Types
            'COMP-1': 'COMP-1',
            'COMPUTATIONAL-1': 'COMP-1',
            'COMP-2': 'COMP-2',
            'COMPUTATIONAL-2': 'COMP-2',
        
            # Pointer Types
            'FUNCTION-POINTER': 'FUNCTION-POINTER'
        }

        # Look up the normalized type. Default to the raw text if not found,
        # although 'DISPLAY' is the safer implicit default in COBOL.
        normalized_usage = USAGE_MAP.get(raw_usage_text, 'DISPLAY')

        # Step 3: Return the normalized string.
        # The parent visitor (visit_DataDescriptionEntry) will use this string
        # to correctly define the Z3 variable's size and constraints.
        return normalized_usage

def visit_DataValueClause(self, node: ASTNode):
        """
        Goal: Provide the initial value for a variable.

        1.  Visit the `DataValueInterval` child to get the literal value.
        2.  The visitor for the literal will return a Python primitive or a Z3 literal.
        3.  Return this value to the parent `DataDescriptionEntryFormat1` visitor, which will create the assignment constraint *after* the variable has been fully declared.
        """
        # Step 1: Visit the `DataValueInterval` child to get the literal value.
        # The grammar allows for multiple DataValueIntervals, but for a simple VALUE clause,
        # we are interested in the first one.
        data_value_interval_node = self._find_child_node(node, "DataValueInterval")

        if data_value_interval_node:
            # The visit to the child node will return the actual value (Python primitive or Z3 literal).
            # This fulfills Step 2.
            value = self.visit(data_value_interval_node)

            # Step 3: Return this value to the parent visitor.
            return value
    
        # If no DataValueInterval is found, return None.
        return None

def visit_DataValueInterval(self, node: ASTNode):
        """
        Processes a DataValueInterval node, which can represent a single value
        or a range (e.g., in a condition-name VALUE clause).
        Goal: Extract the literal value.
        """
        # 1. Visit the `DataValueIntervalFrom` child node.
        # 2. The `THRU` part defines a range for `CONDITION` names, which is not the focus here.
        #    For `VALUE` clauses, we only care about the `From` part.
        from_node = self._find_child_node(node, "DataValueIntervalFrom")

        # 3. Return the result of visiting `DataValueIntervalFrom`.
        # The visit method will recursively call the appropriate visitor for the 'From' part
        # (e.g., visit_Literal) and return the extracted value.
        return self.visit(from_node)

def visit_DataValueIntervalFrom(self, node: ASTNode):
        """
        Processes a DataValueIntervalFrom node, which contains a literal value.
        Goal: Return a Z3 literal expression.

        1. The child is either a `literal` or `cobolWord`.
        2. Visit the child node.
        3. The `visit_literal` method will return a Z3 value (e.g., `z3.IntVal(123)`, `z3.StringVal("ABC")`).
            Similarly, `visit_cobolWord` in this context would likely return a literal value.
        4. Return this Z3 value.
        """
        # Step 1: The child is either a `literal` or `cobolWord`.
        # According to the grammar, there will be one child node.
        child_node = self._get_first_child(node)

        if child_node is None:
            raise ValueError("[ERROR] DataValueIntervalFrom node is missing its child literal/cobolWord node.")

        # Step 2 & 3: Visit the child node. The corresponding visitor
        # (e.g., visit_literal) is expected to parse the source text
        # and return the appropriate Z3 constant value.
        z3_literal = self.visit(child_node)

        # Step 4: Return this Z3 value.
        return z3_literal

def visit_ProcedureDivision(self, node):
        """
        Goal: Process the main body of the program's logic.
        """
        # 1. Find the `ProcedureDivisionBody` child node.
        procedure_body_node = self._find_child_node(node, "ProcedureDivisionBody")

        # 2. Call `self.visit(child)` to begin processing the program's statements.
        if procedure_body_node:
            return self.visit(procedure_body_node)

        return None

def visit_ProcedureDivisionBody(self, node: ASTNode):
        """
        Goal: Process all paragraphs and sections.
        """
        # 1. This node is a structural container.
        # It's role is to group its children, which will be processed in order.

        # 2. Iterate through all children (`Paragraphs`, `ProcedureSection`).
        # The children represent the executable parts of the procedure division body.
        for child in self._get_children(node):
            # 3. Call `self.visit(child)` for each child.
            # This continues the traversal down the AST, passing the current
            # execution path condition to each paragraph or section.
            self.visit(child)

def visit_ProcedureSection(self, node: ASTNode):
        """
        Processes paragraphs within a named section.
        This node is a structural container.
        """
        # Goal: Process paragraphs within a named section.

        # 1. This node is a structural container.
        # It serves to group paragraphs under a section header. The main logic
        # is within the paragraphs themselves.

        # 2. Visit the `Paragraphs` child node by calling `self.visit(child)`.
        # The paragraphs contain the actual executable statements.
        paragraphs_node = self._find_child_node(node, "Paragraphs")
        if paragraphs_node:
            self.visit(paragraphs_node)

def visit_Paragraphs(self, node: ASTNode):
        """
        Processes a 'Paragraphs' node which is a structural container for
        Sentence and Paragraph nodes.
        """
        # Step 1: This node is a structural container.
        # It groups a sequence of sentences and paragraphs without adding new logic.

        # Step 2 & 3: Iterate through all children (Sentence, Paragraph) and
        # call self.visit for each, passing the current path condition.
        for child in self._get_children(node):
            self.visit(child)

def visit_Paragraph(self, node):
        """
        Goal: Process all sentences within a named paragraph.

        1. Iterate through each `Sentence` child node.
        2. Call `self.visit(child)` for each sentence.
        """
        # Step 1: Iterate through each `Sentence` child node.
        sentence_nodes = self._get_children(node, filter_type="Sentence")
        for child in sentence_nodes:
            # Step 2: Call `self.visit(child)` for each sentence.
            self.visit(child)

def visit_Sentence(self, node):
        # Goal: Process all statements within a sentence.

        # 1.  Iterate through each `Statement` child node.
        # A Sentence node acts as a container for one or more statements.
        # We iterate through all its children to process each one. The `visit`
        # dispatcher will correctly handle each child type (e.g., Statement, DOT_FS).
        for child in self._get_children(node):
            # 2.  Call `self.visit(child)` for each statement.
            # This recursively calls the main visitor engine, which will dispatch
            # to the specific method for that child's node type (e.g.,
            # visit_MoveStatement, visit_IfStatement, etc.). The current
            # path condition is passed down to ensure any generated constraints
            # are correctly associated with the current execution path.
            self.visit(child)

def visit_Statement(self, node: ASTNode):
        """
        Goal: Delegate processing to the specific statement visitor.
        """
        # Step 1: Identify the specific child node (e.g., MoveStatement, IfStatement).
        # The 'Statement' node is a grammatical wrapper. The actual statement
        # (like IfStatement, MoveStatement, etc.) is its first child.
        specific_statement_node = self._get_first_child(node)

        # Step 2: Call self.visit(child) on that child node.
        # This will dispatch the call to the appropriate visitor method,
        # for example, self.visit_IfStatement(specific_statement_node).
        if specific_statement_node:
            return self.visit(specific_statement_node)

        return None


def visit_ComputeStore(self, node):
        """
        Visits a ComputeStore node to identify the target variable and rounding option.
        This method processes the part of a COMPUTE statement that specifies where
        the result is stored (e.g., 'MY-VAR ROUNDED').

        Args:
            node (ASTNode): The AST node representing the ComputeStore.

        Returns:
            Dict[str, Any]: A dictionary containing the target variable's resolved
                            name and a boolean indicating if ROUNDED is specified.
                            Example: {'name': 'MY-PROG.MY-VAR', 'rounded': True}
        """
        # 1. Visit the `Identifier` child node. This will return the fully resolved variable name.
        identifier_node = self._get_first_child(node, "Identifier")
        if not identifier_node:
            # This would indicate a malformed AST according to the grammar.
            raise ValueError("ComputeStore node is missing its Identifier child.")
    
        var_name = self.visit(identifier_node)

        # 2. Check for the optional `ROUNDED` keyword.
        rounded_node = self._find_child_node(node, "ROUNDED")
        is_rounded = rounded_node is not None

        # 3. Return the result to the parent `ComputeStatement` visitor.
        result = {'name': var_name, 'rounded': is_rounded}
    
        return result

def visit_DisplayStatement(self, node: ASTNode):
        """
        Models the DISPLAY statement. For now, this is a no-op as it relates to
        external output, not internal state changes.
        """
        # 1. This statement affects program output, not the state of Z3 variables.
        # 2. Log that a DISPLAY statement was encountered.
        # The .strip() and .replace() are for cleaner logging output.
        display_text = self._get_source_text(node).strip().replace('\n', ' ')
        print(f"[INFO] Encountered DISPLAY statement (no-op for state): {display_text}")

        # 3. Visit children (`DisplayOperand`) to ensure any identifiers are valid.
        # This will trigger visits to any Identifiers or Literals within the
        # operands, but will not generate any new constraints itself.
        self.generic_visit(node)

        # No constraints are generated, and no value is returned.
        return None

def visit_DisplayOperand(self, node):
        """
        Processes a DisplayOperand node.
        According to the plan, no action is needed here as the logic is handled
        by the parent `visit_DisplayStatement` method, which will iterate through
        its `DisplayOperand` children and process them directly. This visitor
        method exists for completeness of the traversal but performs no operations.
        It simply returns the result of visiting its child (Identifier or Literal).
        """
        # Goal: No action needed. Handled by parent `DisplayStatement`.
        # We still visit the child to get its value (e.g., a literal value or a variable's Z3 expression)
        # which will be returned up to the `visit_DisplayStatement` caller.
        return self.generic_visit(node)

def visit_DisplayAt(self, node):
        """
        Handles the AT clause of a DISPLAY statement.
        According to the plan, no action is needed here as the logic is
        managed by the parent visit_DisplayStatement method.
        """
        pass

def visit_DisplayUpon(self, node):
        """
        Handles the UPON clause of a DISPLAY statement.
        Goal: No action needed. Handled by parent `DisplayStatement`.
        The specific device (MnemonicName or EnvironmentName) is not relevant
        for the symbolic data state analysis.
        """
        # This clause specifies the output device, which is not modeled.
        # The parent DisplayStatement handles the values being displayed.
        pass

def visit_DisplayWith(self, node: ASTNode):
        """
        Handles the WITH NO ADVANCING clause of a DISPLAY statement.
        This clause modifies the behavior of the parent DISPLAY statement
        but does not generate any Z3 constraints itself. Its logic is
        handled by the `visit_DisplayStatement` method.
        """
        # Per the plan, no action is needed as this is handled by the parent DisplayStatement.
        pass

def visit_IfStatement(self, node: ASTNode):
        """
        Splits the execution path based on the IF condition.
        It creates two separate path conditions: one for the 'then' branch where the
        condition is true, and one for the 'else' branch where the condition is false.
        """
        # 1. Evaluate Condition
        # a. Visit the `Condition` child node to get the Z3 boolean expression.
        condition_node = self._find_child_node(node, "Condition")
        if not condition_node:
            raise ValueError("IF statement is missing a 'Condition' child node.")
        cond_expr = self.visit(condition_node)
        if not isinstance(cond_expr, z3.BoolRef):
            raise TypeError(f"Visitor for 'Condition' node did not return a Z3 boolean expression. Returned type: {type(cond_expr)}")
        # 2. Process 'Then' Branch
        # a. Create the path condition for the 'then' branch.
    
        # b. Visit the `IfThen` child node with the new path condition.
        if_then_node = self._find_child_node(node, "IfThen")
        # if not if_then_node:
        #     raise ValueError("IF statement is missing an 'IfThen' child node.")
        if if_then_node:
            self._add_constraint(cond_expr)

            self.visit(if_then_node)
    
        # 3. Process 'Else' Branch
        # a. Check for an `IfElse` child node.
        if_else_node = self._find_child_node(node, "IfElse")
        
        # b. If it exists, process it with the negated condition.
        if if_else_node:
            # i. Create the path condition for the 'else' branch.
            self._add_constraint(z3.Not(cond_expr))
            # ii. Visit the `IfElse` node with the new path.
            self.visit(if_else_node)
            # print(self.path_constraints)

def visit_IfThen(self, node: ASTNode):
        """
        Executes statements under the 'then' path condition.

        Args:
            node: The IfThen AST node.
        """
        # Goal: Execute statements under the 'then' path condition.

        # 1. Check if the child is `NEXT SENTENCE`.
        # We look for a "NEXT SENTENCE" node among the children. If it exists,
        # it signifies a control flow jump which we currently treat as a no-op
        # in the context of path condition analysis.
        next_sentence_node = self._find_child_node(node, "NEXT SENTENCE")
        if next_sentence_node:
            # If so, do nothing as this is a flow control statement beyond simple path conditions.
            return

        # 2. Otherwise, iterate through the `Statement*` children.
        # If it's not a `NEXT SENTENCE`, we process the sequence of statements.
        statement_nodes = self._get_children(node, "Statement")
        for statement in statement_nodes:
            # 3. For each `Statement` child, call `self.visit(child)`,
            # propagating the `then_path` received from the parent `IfStatement`.
            # This ensures that any constraints generated within these statements are
            # correctly associated with the condition that the IF was true.
            self.visit(statement)

def visit_IfElse(self, node):
        # Goal: Execute statements under the 'else' path condition.

        # Get all children that are not the 'ELSE' keyword token.
        # This will be either a single 'NEXT SENTENCE' node or a list of Statement nodes.
        statement_nodes = [
            child for child in self._get_children(node)
            if child.get("Node") != "ELSE"
        ]

        # 1. Check if the child is `NEXT SENTENCE`. If so, do nothing.
        if statement_nodes and statement_nodes[0].get("Node") == "NEXT SENTENCE":
            return

        # 2. Otherwise, iterate through the `Statement*` children.
        for child in statement_nodes:
            # 3. For each `Statement` child, call `self.visit(child)`,
            #    propagating the `else_path` received from the parent `IfStatement`.
            self.visit(child)

def visit_MoveStatement(self, node):
        """
        Delegates the handling of a MOVE statement to the appropriate visitor
        for either a simple MOVE TO or a MOVE CORRESPONDING.
        """
        # Goal: Delegate to the correct MOVE type visitor.

        # 1. Identify the child: `MoveToStatement` or `MoveCorrespondingToStatement`.
        move_to_child = self._find_child_node(node, "MoveToStatement")
        if move_to_child:
            # 2. Call `self.visit(child)` on that child.
            return self.visit(move_to_child)

        move_corr_child = self._find_child_node(node, "MoveCorrespondingToStatement")
        if move_corr_child:
            # 2. Call `self.visit(child)` on that child.
            return self.visit(move_corr_child)
    
        # According to the grammar, one of the above children should be present.
        # If not, we do nothing and let the generic visitor handle any other potential children.
        return self.generic_visit(node)

def visit_MoveToStatement(self, node: ASTNode):
        """
        Generates Z3 constraints for a COBOL MOVE statement, handling type conversions.
        """
        # Step 1: Get Source Value
        # 1.a. Visit the MoveToSendingArea child node.
        sending_area_node = self._find_child_node(node, 'MoveToSendingArea')
        if not sending_area_node:
            # This case should not happen with a valid AST
            return

        source_expr = self.visit(sending_area_node)

        # To handle type conversions, we need to know the source's PIC info if it's a variable.
        source_pic_info = None
        source_node_content = self._get_first_child(sending_area_node)
        if source_node_content and source_node_content.get('Node') == 'Identifier':
            # visit_Identifier returns a tuple: (resolved_name, z3_var)
            # print(source_node_content)
            source_name = self.visit(source_node_content)
            source_pic_info = self.pic_info.get(source_name)

        # Step 2: Assign to Targets
        # 2.a. Iterate through all Identifier nodes in the TO clause.
        target_identifier_nodes = self._get_children(node, 'Identifier')
        for target_node in target_identifier_nodes:
            # 2.b.i. Get target's name and Z3 variable.
            # print(self.path_constraints)
            # print(targ)
            target_z3_var = self.visit(target_node)
            target_var_name = str(target_z3_var)
            target_pic_info = self.pic_info.get(target_var_name)
            if not target_pic_info:
                raise Exception(f"PIC info not found for MOVE target '{target_var_name}'")

            # 2.b.ii. Type Conversion Logic
            final_source_expr = source_expr
            target_category = target_pic_info.get('category')

            # Determine source category from pic_info or Z3 type for literals
            source_category = None
            if source_pic_info:
                source_category = source_pic_info.get('category')
            elif z3.is_int(source_expr) or z3.is_real(source_expr):
                source_category = 'numeric'
            elif z3.is_string(source_expr):
                source_category = 'alphanumeric'

            # Numeric-to-Numeric
            if source_category in ['numeric', 'numeric-edited'] and target_category in ['numeric', 'numeric-edited']:
                # Per the plan, range constraints added at declaration handle truncation/padding.
                # De-editing of the source is assumed to be handled when visiting the source.
                final_source_expr = source_expr

            # String-to-String
            elif source_category in ['alphabetic', 'alphanumeric'] and target_category in ['alphabetic', 'alphanumeric']:
                target_len = target_pic_info.get('char_length')
                source_len = -1
                if source_pic_info:
                    source_len = source_pic_info.get('char_length')
                elif z3.is_string_value(source_expr):  # Handle string literals
                    source_len = len(source_expr.as_string())

                if source_len != -1 and target_len is not None:
                    if source_len > target_len:
                        final_source_expr = z3.SubString(source_expr, 0, target_len)
                    elif source_len < target_len:
                        padding_len = target_len - source_len
                        padding_str = " " * padding_len
                        final_source_expr = z3.Concat(source_expr, z3.StringVal(padding_str))

            # Numeric-to-String
            elif source_category in ['numeric', 'numeric-edited'] and target_category in ['alphabetic', 'alphanumeric']:
                # Model with an uninterpreted function as per the plan.
                # Note: Defining this function on each call is inefficient. It should ideally
                # be a class member initialized in __init__.
                src_sort = source_expr.sort()
                NumToString = z3.Function(f'NumToString_{src_sort}', src_sort, z3.StringSort())
                final_source_expr = NumToString(source_expr)

            # String-to-Numeric
            elif source_category in ['alphabetic', 'alphanumeric'] and target_category in ['numeric', 'numeric-edited']:
                # Model with an uninterpreted function as per the plan.
                target_sort = target_z3_var.sort()
                StringToNum = z3.Function(f'StringToNum_{target_sort}', z3.StringSort(), target_sort)
                final_source_expr = StringToNum(source_expr)

            # 2.b.iii. Create the final assignment constraint.
            constraint = (target_z3_var == final_source_expr)

            # 2.b.iv. Call update_assignment to store the constraint.
            self.update_assignment(target_var_name, 'MOVE', constraint)

def visit_MoveToSendingArea(self, node: ASTNode):
        """
        Visits the source of a MOVE statement (Identifier or literal) and returns its Z3 representation.
    
        This method implements the following plan:
        1. The child is either an `Identifier` or a `literal`.
        2. Visit the child node.
            a. If `Identifier`, the visit will return the Z3 logical variable from `self.declarations`.
            b. If `literal`, the visit will return a Z3 literal value (e.g., z3.IntVal, z3.StringVal).
        3. Return the resulting Z3 expression.
        """
        # The MoveToSendingArea node has exactly one child which represents the source.
        child_node = self._get_first_child(node)
    
        if not child_node:
            # This case should ideally not happen with a valid AST.
            raise ValueError("MoveToSendingArea node has no child to visit for the source value.")

        # Step 2 & 3: Visit the child node and return the result.
        # The generic `self.visit` will dispatch to the correct specific visitor
        # (e.g., visit_Identifier, visit_literal), which will return the
        # appropriate Z3 expression as per the plan.
        source_z3_expr = self.visit(child_node)
    
        return source_z3_expr

def visit_MoveCorrespondingToSendingArea(self, node: ASTNode):
        """
        Processes the sending area of a MOVE CORRESPONDING statement.
        Goal: Return the identifier of the sending group item.

        1.  Visit the `Identifier` child.
        2.  Return the resolved full name of the group item.
        """
        # Step 1: Visit the `Identifier` child.
        # The `MoveCorrespondingToSendingArea` node has a single `Identifier` child
        # which represents the source group item.
        identifier_node = self._get_first_child(node, 'Identifier')
        if not identifier_node:
            raise ValueError("MoveCorrespondingToSendingArea must have an Identifier child.")

        # Visiting the identifier node will resolve its fully qualified name.
        sending_group_identifier = self.visit(identifier_node)

        # Step 2: Return the resolved full name of the group item.
        # This name will be used by the parent `MoveStatement` visitor to orchestrate
        # the individual moves.
        return sending_group_identifier

def visit_ArithmeticExpression(self, node):
        """
        Builds a Z3 arithmetic expression by handling addition and subtraction.
        Grammar: ArithmeticExpression ::= MultDivs PlusMinus* ;
        """
        # 1. Visit the first `MultDivs` child to get the initial value (`result_expr`).
        mult_divs_children = self._get_children(node, "MultDivs")
        if not mult_divs_children:
            # According to the grammar, this should not happen.
            raise ValueError("ArithmeticExpression is missing its MultDivs child.")
    
        # The first child is always the initial part of the expression.
        # print(mult_divs_children)
        result_expr = self.visit(mult_divs_children[0])
        # print(result_expr)
        # 2. Iterate through the `PlusMinus` children.
        plus_minus_children = self._get_children(node, "PlusMinus")
    
        # 3. For each `PlusMinus` child:
        for plus_minus_node in plus_minus_children:
            # a. Visit it to get its operator (+ or -) and its operand (operand_expr).
            # We expect visit_PlusMinus to return a tuple: (operator_string, operand_z3_expr)
            operator, operand_expr = self.visit(plus_minus_node)
            # print(operand_expr)
            
            # b. Update result_expr.
            if operator == '+':
                result_expr = result_expr + operand_expr
            elif operator == '-':
                result_expr = result_expr - operand_expr
            else:
                # This case should ideally not be reached if the grammar is parsed correctly.
                raise ValueError(f"Unsupported operator '{operator}' in ArithmeticExpression.")

        # 4. Return the final result_expr.
        return result_expr

def visit_PlusMinus(self, node: ASTNode):
        """
        Visits a PlusMinus node, which represents an addition or subtraction operation.
        It returns the operator and the Z3 expression for the operand.
    
        Goal: Return an operator and a Z3 expression.

        1.  Identify the operator child (`PLUSCHAR` or `MINUSCHAR`).
        2.  Visit the `MultDivs` child to get its Z3 expression (`operand_expr`).
        3.  Return a tuple or dict, e.g., `{'op': '+', 'expr': operand_expr}`.
        """
        # Step 1: Identify the operator child (`PLUSCHAR` or `MINUSCHAR`).
        # The grammar ensures the first child is the operator.
        operator_node = self._get_first_child(node)
        if operator_node is None or operator_node.get("Node") not in ["PLUSCHAR", "MINUSCHAR"]:
            raise ValueError("Expected PLUSCHAR or MINUSCHAR as the first child of PlusMinus.")
    
        # Get the operator's source text, e.g., "+" or "-"
        op_symbol = self._get_source_text(operator_node).strip()

        # Step 2: Visit the `MultDivs` child to get its Z3 expression (`operand_expr`).
        mult_divs_node = self._find_child_node(node, "MultDivs")
        if mult_divs_node is None:
                raise ValueError("PlusMinus node is missing its 'MultDivs' operand child.")
    
        operand_expr = self.visit(mult_divs_node)


        return (op_symbol,operand_expr)

def visit_MultDivs(self, node: ASTNode):
        """
        Builds a Z3 expression with multiplication and division by visiting
        the Powers and MultDiv children nodes.
        Corresponds to the grammar rule: MultDivs ::= Powers MultDiv*
        """
        # 1. Visit the first `Powers` child to get the initial value (`result_expr`).
        # The grammar guarantees the first child is a `Powers` node.
        children = self._get_children(node)
        if not children:
            raise ValueError("MultDivs node has no children")

        powers_node = children[0]
        result_expr = self.visit(powers_node)
        # 2. Iterate through the `MultDiv` children.
        # These are the remaining children after the first `Powers` node.
        mult_div_children = self._get_children(node, filter_type="MultDiv")


        # 3. For each `MultDiv` child:
        for mult_div_child in mult_div_children:
            # a. Visit it to get its operator (`*` or `/`) and its operand (`operand_expr`).
            # We assume `visit_MultDiv` returns a tuple like ('*', operand_expr).
            # print(mult_div_child)
            visit_result = self.visit(mult_div_child)
        
            # The result from visiting a MultDiv node should be a tuple (operator, operand_expression)
            if not isinstance(visit_result, tuple) or len(visit_result) != 2:
                raise TypeError(f"Expected a (operator, expression) tuple from visiting MultDiv, but got {type(visit_result)}")
            operator, operand_expr = visit_result
            # print(operator)
            # b. Update `result_expr`: `result_expr = result_expr * operand_expr` or `result_expr = result_expr / operand_expr`.
            if operator == "*":
                result_expr = result_expr * operand_expr
            elif operator == "/":
                # Ensure we handle division by zero later by adding a constraint
                # in the calling context (e.g., COMPUTE statement) if necessary.
                result_expr = result_expr / operand_expr
            else:
                raise ValueError(f"Unsupported operator '{operator}' in MultDivs node")

        # 4. Return the final `result_expr`.
        return result_expr

def visit_MultDiv(self, node: ASTNode):
        """
        Processes a multiplication or division operation in an arithmetic expression.

        Goal: Return an operator and a Z3 expression.

        1.  Identify the operator child (`ASTERISKCHAR` or `SLASHCHAR`).
        2.  Visit the `Powers` child to get its Z3 expression (`operand_expr`).
        3.  Return a tuple or dict, e.g., `{'op': '*', 'expr': operand_expr}`.
        """
        # Step 1: Identify the operator child
        operator_node = self._get_first_child(node)
        if not operator_node:
            raise ValueError("MultDiv node is missing an operator child.")

        op_symbol = None
        if operator_node.get("Node") == "ASTERISKCHAR":
            op_symbol = "*"
        elif operator_node.get("Node") == "SLASHCHAR":
            op_symbol = "/"
        else:
            raise ValueError(f"Unexpected operator type in MultDiv: {operator_node.get('Node')}")

        # Step 2: Visit the Powers child to get its Z3 expression
        powers_node = self._find_child_node(node, "Powers")
        if not powers_node:
            raise ValueError("MultDiv node is missing a 'Powers' child.")

        operand_expr = self.visit(powers_node)

        # Step 3: Return a dictionary with the operator and its operand's expression
        return (op_symbol, operand_expr)

def visit_Powers(self, node: ASTNode):
        """
        Builds a Z3 expression for a base raised to one or more powers.
        Corresponds to the grammar rule: Powers ::= (PLUSCHAR | MINUSCHAR)? Basis Power* ;
        """
        # Plan Step 1: Check for an optional leading PLUSCHAR or MINUSCHAR.
        is_negative = False
        children = self._get_children(node)
        child_index = 0

        if children:
            first_child_node_type = children[0].get("Node")
            if first_child_node_type == "MINUSCHAR":
                is_negative = True
                child_index += 1
            elif first_child_node_type == "PLUSCHAR":
                child_index += 1

        # Plan Step 2: Visit the Basis child to get the base expression.
        # According to the grammar, a 'Basis' node must follow the optional sign.
        basis_node = children[child_index]
        base_expr = self.visit(basis_node)
        child_index += 1

        # Plan Step 3 & 4: Iterate through any Power children and update the expression.
        while child_index < len(children):
            power_node = children[child_index]
            if power_node.get("Node") != "Power":
                # If we encounter a node that is not a 'Power' node,
                # we stop processing as per the grammar `Power*`.
                break

            # Plan Step 4a: Visit it to get the exponent expression.
            # The `visit_Power` method is expected to return the exponent's value.
            exp_expr = self.visit(power_node)

            # Plan Step 4b: Update the result. Z3's ** operator handles exponentiation.
            base_expr = base_expr ** exp_expr

            child_index += 1

        # Plan Step 5: Apply the leading sign if present.
        if is_negative:
            base_expr = -base_expr

        # Plan Step 6: Return the final base_expr.
        return base_expr

def visit_Power(self, node):
        """
        Processes a Power node, which represents the exponent part of an expression.
        Goal: Return an exponent Z3 expression.
        Grammar: Power ::= DOUBLEASTERISKCHAR Basis ;
        """
        # 1. Visit the `Basis` child to get the exponent expression.
        # The Basis node represents the exponent in the power operation.
        basis_node = self._find_child_node(node, "Basis")
        exponent_expr = self.visit(basis_node)

        # 2. Return the expression.
        return exponent_expr

def visit_Basis(self, node):
        """
        Goal: Return a single operand for an arithmetic expression.
        """
        # Step 1: Identify the child type
        child = self._get_first_child(node)
        # print(node)
        if child is None:
            raise ValueError("Basis node has no children.")

        child_type = child.get("Node")

        # 1.a. LPARENCHAR: Visit the nested ArithmeticExpression and return its result.
        if child_type == "LPARENCHAR":
            # The grammar is LPARENCHAR ArithmeticExpression RPARENCHAR.
            # We find the ArithmeticExpression node among the children.
            arith_expr_node = self._find_child_node(node, "ArithmeticExpression")
            if arith_expr_node is None:
                raise ValueError("Expected ArithmeticExpression within parenthesized Basis.")
            result = self.visit(arith_expr_node)

        # 1.b. Identifier: Visit it to get the Z3 logical variable.
        elif child_type == "Identifier":
            result = self.visit(child)

        # 1.c. literal: Visit it to get a Z3 literal value.
        elif child_type == "Literal":
            result = self.visit(child)
    
        else:
            raise NotImplementedError(f"Unsupported child type in Basis: {child_type}")

        # 2. Return the resulting Z3 expression.
        return result

def visit_Condition(self, node: ASTNode):
        """
        Builds a Z3 boolean expression from a Condition node.
        A Condition consists of an initial CombinableCondition followed by zero
        or more AndOrConditions (e.g., "A > B AND C < D OR E = F").
        """
        # 1. Visit the first `CombinableCondition` child to get the initial boolean expression (`result_expr`).
        first_combinable_node = self._get_first_child(node, "CombinableCondition")
        if not first_combinable_node:
            # According to the grammar, a Condition must have at least one CombinableCondition.
            # Handle this gracefully in case of an incomplete AST.
            return z3.BoolVal(True)

        result_expr = self.visit(first_combinable_node)

        # 2. Iterate through the `AndOrCondition` children.
        and_or_nodes = self._get_children(node, "AndOrCondition")
        for and_or_node in and_or_nodes:
            # 3a. Visit it to get the operator (`AND` or `OR`) and the next boolean expression (`next_expr`).
            # The visitor for AndOrCondition should return a tuple: (operator_string, z3_expression).
            visit_result = self.visit(and_or_node)
            if not (isinstance(visit_result, tuple) and len(visit_result) == 2):
                raise TypeError(f"Expected (operator, expression) tuple from visiting AndOrCondition, but got {type(visit_result)}")

            operator, next_expr = visit_result

            # 3b. Update `result_expr` based on the operator.
            op_text = str(operator).upper()
            if op_text == "AND":
                result_expr = z3.And(result_expr, next_expr)
            elif op_text == "OR":
                result_expr = z3.Or(result_expr, next_expr)
            else:
                raise ValueError(f"Unrecognized logical operator '{op_text}' from AndOrCondition.")

        # 4. Return the final `result_expr`.
        return result_expr

def visit_AndOrCondition(self, node: ASTNode):
        """
        Processes an AND or OR operator in a condition, returning the operator
        and the Z3 expression for the subsequent condition.
        Goal: Return a logical operator and a boolean expression.
        """
        # 1. Identify the operator (`AND` or `OR`).
        # The operator is the first child node.
        children = self._get_children(node)
        operator_node = children[0]
        operator_type = operator_node.get("Node", "").upper()  # "AND" or "OR"

        # 2. Visit the `CombinableCondition` child to get its boolean expression (`bool_expr`).
        # The condition part is the second child.
        condition_node = children[1]
        bool_expr = self.visit(condition_node)

        # 3. Return a tuple or dict, e.g., `{'op': 'AND', 'expr': bool_expr}`.
        # This structure allows the parent `Condition` visitor to assemble the full expression.
        return (operator_type, bool_expr)

def visit_CombinableCondition(self, node: ASTNode):
        """
        Applies a NOT to a simple condition if present.
        Grammar: CombinableCondition ::= NOT? SimpleCondition ;
        """
        # 1. Visit the `SimpleCondition` child to get its boolean expression (`bool_expr`).
        simple_condition_node = self._find_child_node(node, "SimpleCondition")
        # Pass the current path condition down to the child visitor.
        bool_expr = self.visit(simple_condition_node)

        # 2. Check for the optional `NOT` keyword.
        not_node = self._find_child_node(node, "NOT")

        # 3. If `NOT` is present, return `z3.Not(bool_expr)`.
        if not_node:
            return z3.Not(bool_expr)
    
        # 4. Otherwise, return `bool_expr`.
        return bool_expr

def visit_SimpleCondition(self, node: ASTNode):
        """
        Handles a basic condition or a parenthesized group according to the grammar:
        SimpleCondition ::= LPARENCHAR Condition RPARENCHAR | RelationCondition ;
        """
        # Step 1: Identify the child type.
        # The plan requires distinguishing between a direct RelationCondition
        # and a parenthesized group starting with LPARENCHAR.

        # Step 1.a: Check for a parenthesized group `( Condition )`.
        # The presence of an `LPARENCHAR` child signals this case.
        if self._find_child_node(node, "LPARENCHAR"):
            # The actual expression is within the nested `Condition` node.
            condition_node = self._find_child_node(node, "Condition")
            if condition_node is None:
                raise ValueError("Invalid SimpleCondition: Found LPARENCHAR without a nested Condition node.")
        
            # Visit the nested condition and return the resulting boolean expression.
            return self.visit(condition_node)

        # Step 1.b: Check for a `RelationCondition`.
        # If it's not a parenthesized group, it must be a RelationCondition.
        relation_node = self._find_child_node(node, "RelationCondition")
        if relation_node:
            # Visit the RelationCondition node.
            result = self.visit(relation_node)
            # Step 2: Return the resulting boolean expression.
            return result
    
        # If neither of the expected child types are found, the AST is malformed.
        child_nodes = [child.get('Node', 'Unknown') for child in node.get("Children", [])]
        raise ValueError(f"Unexpected structure for SimpleCondition node. Expected 'RelationCondition' or 'LPARENCHAR' as a child, but found: {child_nodes}")

def visit_RelationCondition(self, node: ASTNode):
        """
        Delegates the processing of a relation condition to the specific type
        of condition (sign, arithmetic, etc.).

        Goal: Delegate to the specific relation condition type.

        1.  Identify the child (`RelationSignCondition`, `RelationArithmeticComparison`, etc.).
        2.  Visit the child and return its result.
        """
        # Step 1: Identify the child.
        # The RelationCondition node has exactly one child which represents the specific
        # type of the condition (e.g., RelationSignCondition, RelationArithmeticComparison).
        child_condition_node = self._get_first_child(node)

        if child_condition_node is None:
            # This case should ideally not happen based on a valid AST structure.
            # We can log a warning or raise an error. Returning a neutral value for now.
            print(f"[WARNING] RelationCondition node has no child to evaluate. Node: {node}")
            return z3.BoolVal(True)

        # Step 2: Visit the child and return its result.
        # The self.visit method will dispatch to the correct visitor for the child
        # (e.g., visit_RelationSignCondition), which will return the Z3 constraint.
        return self.visit(child_condition_node)

def visit_RelationSignCondition(self, node):
        """
        Generates constraints for a POSITIVE/NEGATIVE/ZERO check on an arithmetic expression.
        Grammar: RelationSignCondition ::= ArithmeticExpression IS? NOT? (POSITIVE | NEGATIVE | ZERO) ;
        """
        # Step 1: Visit the `ArithmeticExpression` to get the Z3 expression to test (`expr`).
        arith_expr_node = self._find_child_node(node, "ArithmeticExpression")
        if not arith_expr_node:
            raise ValueError("RelationSignCondition must contain an ArithmeticExpression node.")
    
        expr = self.visit(arith_expr_node)
        if expr is None:
            raise ValueError(f"Visiting ArithmeticExpression returned None for node: {arith_expr_node}")

        # Step 2 & 3: Identify the condition and generate the corresponding Z3 boolean expression.
        condition = None
        if self._find_child_node(node, "POSITIVE"):
            # a. POSITIVE: expr > 0
            condition = (expr > 0)
        elif self._find_child_node(node, "NEGATIVE"):
            # b. NEGATIVE: expr < 0
            condition = (expr < 0)
        elif self._find_child_node(node, "ZERO"):
            # c. ZERO: expr == 0
            condition = (expr == 0)
        else:
            raise ValueError("RelationSignCondition is missing a POSITIVE, NEGATIVE, or ZERO keyword.")

        # Step 4: Check for an optional `NOT` and wrap the result in `z3.Not()` if present.
        if self._find_child_node(node, "NOT"):
            condition = z3.Not(condition)
    
        # Step 5: Return the final boolean expression.
        return condition

def visit_RelationArithmeticComparison(self, node: ASTNode) -> z3.BoolRef:
        """
        Generates constraints for a comparison between two arithmetic expressions.
        e.g., A > B or X = Y + 1
        """
        # Step 1: Visit the first ArithmeticExpression to get lhs_expr.
        lhs_node = self._get_nth_child(node, 0)
        lhs_expr = self.visit(lhs_node)

        # Step 2: Visit the second ArithmeticExpression to get rhs_expr.
        rhs_node = self._get_nth_child(node, 2)
        rhs_expr = self.visit(rhs_node)

        # Step 3: Visit the RelationalOperator child. It will return a function `op_func`.
        op_node = self._get_nth_child(node, 1)
        # We visit the operator, which should return a callable Python function
        # (e.g., lambda a, b: a > b)
        op_func = self.visit(op_node)
    
        if not callable(op_func):
            raise TypeError(f"Expected visitor for '{op_node.get('Node')}' to return a callable function, but got {type(op_func)}")

        # Step 4: Apply the function: op_func(lhs_expr, rhs_expr).
        result_expr = op_func(lhs_expr, rhs_expr)
    
        # Step 5: Return the resulting boolean expression.
        return result_expr

def visit_RelationalOperator(self, node):
        """
        Parses a RelationalOperator node and returns a lambda function that
        performs the corresponding Z3 comparison.

        This method precisely follows the plan:
        1. Parse the source text of the node to identify the operator.
        2. Return a Python lambda function for the Z3 comparison.
        3. Handle the optional `NOT` by wrapping the lambda's result in `z3.Not()`.
        """
        # 1. Parse the source text of the node
        source_text = self._get_source_text(node).upper()
        # Handle compound operators first to avoid ambiguity
        if ">=" in source_text or "GREATER OR EQUAL" in source_text or "MORETHANOREQUAL" in source_text:
            return lambda a, b: a >= b
        elif "<=" in source_text or "LESS OR EQUAL" in source_text or "LESSTHANOREQUAL" in source_text:
            return lambda a, b: a <= b
        elif "<>" in source_text or "NOTEQUALCHAR" in source_text:
            return lambda a, b: a != b

        # 3. Check for negation
        is_not = "NOT" in source_text

        # 2. Determine the base comparison operator
        base_lambda = None
        if ">" in source_text or "GREATER" in source_text or "MORETHANCHAR" in source_text:
            base_lambda = lambda a, b: a > b
        elif "<" in source_text or "LESS" in source_text or "LESSTHANCHAR" in source_text:
            base_lambda = lambda a, b: a < b
        elif "=" in source_text or "EQUAL" in source_text or "EQUALCHAR" in source_text:
            base_lambda = lambda a, b: a == b
        else:
            # This case should ideally not be reached with a valid AST
            raise ValueError(f"Unrecognized relational operator in '{source_text}'")

        # 3. Return the final lambda, applying z3.Not() if negation was present
        if is_not:
            # Handles cases like "NOT GREATER THAN" or "IS NOT EQUAL TO"
            return lambda a, b: z3.Not(base_lambda(a, b))
        else:
            return base_lambda

def visit_Identifier(self, node: ASTNode):
        """
        Goal: The main entry point for resolving any type of identifier to its Z3 variable.
        This method acts as a dispatcher. It visits its child node (which could be a
        QualifiedDataName, TableCall, etc.) which performs the actual name resolution
        and returns a fully qualified name string. This string is then used to look up
        the corresponding Z3 variable in the symbol table.
        """
        # 1. The child can be `QualifiedDataName`, `TableCall`, etc.
        # There should be exactly one child under an Identifier node.
        child_node = self._get_first_child(node)
        if not child_node:
            raise ValueError("[ERROR] Identifier node has no child to resolve.")

        # 2. Visit the child node. The child visitor (e.g., `visit_QualifiedDataName`)
        #    should do the heavy lifting of resolving the name and return the
        #    fully qualified name string.
        # 3. Let the returned name be `full_name`.
        full_name = self.visit(child_node)
    
        if not isinstance(full_name, str) or not full_name:
            raise TypeError(f"[ERROR] Visitor for node type '{child_node.get('Node')}' did not return a valid name string.")

        # 4. Look up the Z3 logical variable in the symbol table.
        z3_var = self.declarations.get(full_name)
        if z3_var is None:
            # This indicates a logic error, as all variables should be declared
            # before this visitor is called in a usage context.
            raise NameError(f"[ERROR] Resolved identifier '{full_name}' has no corresponding Z3 variable in declarations.")

        # 5. Return the Z3 variable.
        return z3_var


def visit_CharacterPosition(self, node: ASTNode):
        """
        Processes the CharacterPosition node, which contains an ArithmeticExpression.
        The goal is to evaluate this expression and return the resulting Z3 expression
        representing the starting position for a substring operation. This result is
        then used by the parent node (e.g., ReferenceModifier).
        """
        # A CharacterPosition node contains an ArithmeticExpression as its child.
        # We visit this child to get the Z3 expression for the start position.
        expression_node = self._get_first_child(node)
        if expression_node:
            # Visit the child node (e.g., ArithmeticExpression) and return its value.
            # The parent node will handle the returned Z3 expression.
            return self.visit(expression_node)
    
        # This path is unexpected if the AST is correctly formed based on the grammar.
        # Returning None as a fallback.
        return None

def visit_Length(self, node: ASTNode):
        """
        Handles the LENGTH OF intrinsic function.
        Returns a Z3 integer literal representing the byte length of the identifier.
        The parent node is responsible for using this value in a larger expression
        or constraint, as per the plan.
        """
        # Per the plan, this method's goal is to return the Z3 expression for the length.
        # The creation of a constraint using this length is handled by the parent node.

        # The 'Length' node has one child: an ArithmeticExpression that resolves to
        # the identifier whose length is requested.
        child_expr_node = self._get_first_child(node)
        if not child_expr_node:
            raise ValueError(f"LENGTH OF function node is missing its identifier expression: '{self._get_source_text(node)}'")

        # Visit the child. The visitor for an identifier/expression should return
        # its fully qualified string name.
        var_name = self.visit(child_expr_node)

        if not isinstance(var_name, str):
            raise TypeError(f"Expected a variable name (str) from visiting the child of 'Length', but got {type(var_name)} for node: '{self._get_source_text(node)}'")

        # Look up the parsed declaration information for the variable.
        pic_data = self.pic_info.get(var_name)
        if not pic_data:
            raise NameError(f"Could not find declaration information for variable '{var_name}' used in LENGTH OF.")

        # Retrieve the 'char_length', which represents the total byte size of the data item.
        char_length = pic_data.get("char_length")
        if char_length is None:
            raise KeyError(f"'char_length' not found in PIC info for variable '{var_name}'.")

        # Return the length as a Z3 integer literal.
        return z3.IntVal(char_length)

def visit_Subscript(self, node):
        """
        Visits a Subscript node and returns the Z3 expression for the array index.

        According to the plan, this visitor's role is to produce the Z3
        expression representing the index. The actual array access logic is
        handled by the parent `TableCall` visitor. A Subscript node typically
        contains one child node that defines the index (e.g., a Literal,
        a QualifiedDataName, or an ArithmeticExpression).

        The `generic_visit` method is ideal here, as it will visit the child
        node, which in turn will be processed by its specific visitor
        (e.g., `visit_Literal`) to produce the required Z3 expression. This
        expression is then returned up to the `TableCall` visitor.
        """
        # The generic_visit method will traverse the child of the Subscript node
        # (which is the actual index expression) and return the corresponding
        # Z3 expression. This fulfills the method's goal.
        return self.generic_visit(node)

def visit_Argument(self, node: ASTNode):
        """
        Processes an argument node.
        An Argument is a wrapper around a literal, identifier, or expression.
        This method visits the actual content of the argument and returns its
        corresponding Z3 expression. The parent node (e.g., FunctionCall)
        is responsible for using this returned expression.
        """
        # The 'Argument' node is a wrapper. The actual value (literal, identifier, etc.)
        # is in its child node. We visit the child to get its Z3 representation.
        # self.generic_visit will traverse the children and return the result
        # from the first child that yields a value, which is the desired behavior here.
        return self.generic_visit(node)

def visit_QualifiedDataName(self, node: ASTNode):
        """
        Goal: Resolve a potentially qualified data name to its full unique name.
        Visits the specific format child of QualifiedDataName to handle resolution.
        """
        # 1. Visit the child, which will be one of the `QualifiedDataNameFormat` nodes.
        # The node has only one child, which represents the specific format of the qualified name.
        child_node = self._get_first_child(node)
        if child_node is None:
            # This case should ideally not happen if the AST is well-formed.
            raise ValueError("QualifiedDataName node has no children to visit.")

        # The visit call will dispatch to the appropriate visit_QualifiedDataNameFormatX method.
        # That method is responsible for parsing the name and its qualifiers and returning
        # the fully resolved unique name as a string.
        resolved_name = self.visit(child_node)

        # 2. Return the result from visiting that child.
        return resolved_name

def visit_QualifiedDataNameFormat1(self, node: ASTNode):
        """
        Resolves a data name with 'OF' or 'IN' qualifiers.
        For example, 'CHILD-ITEM OF PARENT-GROUP OF GRAND-PARENT'.
        """
        # Step 1: Visit the DataName or ConditionName child to get the base name.
        # The base name is always the first child of a QualifiedDataNameFormat1 node.
        base_name_node = self._get_first_child(node)
        base_name = self.visit(base_name_node)

        # Step 2: Create a list of qualifier names by visiting each QualifiedInData child.
        qualifier_nodes = self._get_children(node, "QualifiedInData")
        qualifiers = [self.visit(q_node) for q_node in qualifier_nodes]

        # Step 3: Reverse the order of qualifiers to match the hierarchy from top-level to lower-level.
        # COBOL: C OF B OF A => AST: C, [B, A] => Hierarchy: A.B.C
        # So we need to reverse [B, A] to [A, B].
        qualifiers.reverse()

        # Step 4: Call resolve_variable with the base name and the correctly ordered list of qualifiers.
        resolved_name = self.resolve_variable(base_name, qualifiers)

        # Step 5: Return the fully resolved name string.
        return resolved_name

def visit_QualifiedDataNameFormat2(self, node):
        """
        Handles the QualifiedDataNameFormat2 node.
        Goal: Not implemented. No action.
        This node type represents a qualified paragraph name (e.g., PARA-A IN SECTION-B),
        which is a control flow reference. The processing of the reference itself
        is handled by the statement using it (e.g., PERFORM). This visitor
        node does not introduce new variables or constraints on its own.
        """
        # Per the plan, this node type does not require any specific action.
        # The names it contains (ParagraphName, InSection) will be extracted
        # by its parent visitor method (e.g., visit_PerformStatement) as needed.
        pass

def visit_QualifiedDataNameFormat3(self, node):
        """
        Handles the QualifiedDataNameFormat3 node.
        Grammar: QualifiedDataNameFormat3 ::= TextName InLibrary ;
        According to the plan, this format is not implemented and requires no action.
        This is typically used for referencing elements from a copybook library,
        which is assumed to be resolved before this semantic analysis stage.
        """
        # Goal: Not implemented. No action.
        pass

def visit_QualifiedDataNameFormat4(self, node):
        """
        Handles the QualifiedDataNameFormat4 node.
        Grammar: QualifiedDataNameFormat4 ::= LINAGE_COUNTER InFile ;
        """
        # Goal: Not implemented. No action.
        # LINAGE-COUNTER is a special register related to file handling (REPORT WRITER).
        # Its symbolic representation is not currently handled.
        # We will simply do nothing and avoid visiting children.
        pass

def visit_QualifiedInData(self, node: ASTNode):
        """
        Delegates the visit to the child node (either InData or InTable)
        and returns its result, which is expected to be a qualifier name.
        """
        # Step 1: Visit the child and return its result (which will be a qualifier name).
        # The QualifiedInData node has one child (InData or InTable). We visit it
        # and propagate its result upwards. The generic_visit method handles this
        # perfectly by visiting children and returning the first non-None result.
        return self.generic_visit(node)

def visit_InData(self, node):
        # Goal: Return the qualifier name.

        # 1. Visit the `DataName` child.
        # Find the DataName node which holds the name of the qualifier.
        data_name_node = self._get_first_child(node, "DataName")
    
        # We visit the DataName child node. The `visit_DataName` method is expected
        # to process that node and return the name string.
        qualifier_name = self.visit(data_name_node)

        # 2. Return its source text.
        # The 'qualifier_name' variable now holds the source text returned
        # from visiting the DataName node.
        return qualifier_name

def visit_InFile(self, node):
        """
        Handles the IN/OF phrase for qualified data names.
        According to the plan, this is not implemented and takes no action.
        The logic for resolving qualified names is handled within the
        `visit_Identifier` method, which calls `self.resolve_variable`.
        This node itself doesn't generate constraints or return a value; it only
        provides context for its parent (e.g., a DataName).
        """
        # Goal: Not implemented. No action.
        # The FileName child will be extracted by the parent node (e.g., DataName)
        # to use as a qualifier.
        pass

def visit_InMnemonic(self, node):
        """
        Goal: Not implemented. No action.
        """
        # The logic for handling the mnemonic name is managed by the parent
        # statement (e.g., ACCEPT or DISPLAY). This node is a syntactic
        # wrapper and has no independent semantic action in Z3 translation.
        # We can visit its children to extract the name if needed, but the parent
        # node is responsible for that.
        mnemonic_name_node = self._find_child_node(node, "MnemonicName")
        if mnemonic_name_node:
            return self.visit(mnemonic_name_node)
        return None

def visit_InSection(self, node):
        # Goal: Not implemented. No action.
        # This clause is part of a larger identifier structure. The parent visitor
        # (e.g., visit_Identifier) is responsible for processing the qualification
        # by accessing this node's children directly. Therefore, this visitor
        # method performs no independent action.
        pass

def visit_InLibrary(self, node):
        """
        Handles the InLibrary clause.
        According to the plan, this is a no-op as it typically relates to
        compiler directives (like COPY) and has no runtime semantic effect
        on the program's symbolic state.
        """
        # Goal: Not implemented. No action.
        pass

def visit_InTable(self, node):
        """
        Handles the InTable clause (IN/OF).
        According to the plan, no action is taken for this node.
        The actual resolution of qualified names is handled by the parent node
        (e.g., an Identifier node) which will look for this child and extract
        the qualifier name from it. This node itself doesn't generate
        constraints or state changes.
        """
        # Goal: Not implemented. No action.
        pass

def visit_Literal(self, node: ASTNode):
        """
        Processes a Literal node by visiting its specific child type.
        Goal: Return a Z3 literal value.
        """
        # 1. Identify the child (NONNUMERICLITERAL, NumericLiteral, etc.).
        # A Literal node will have exactly one child representing the actual literal type.
        child_node = self._get_first_child(node)
        if not child_node:
            # This case is unexpected based on the grammar.
            return None

        # 2. Visit the child and return its result.
        # The visit method will dispatch to the correct handler (e.g., visit_NumericLiteral)
        # which will return the corresponding Z3 value (e.g., z3.IntVal, z3.StringVal).
        return self.visit(child_node)
def visit_NumericLiteral(self, node: ASTNode):
    """
    Returns a Z3 integer or real value depending on whether the literal has a decimal.
    """
    
    literal_str = node.get("Source Text").strip()
    try:
        if '.' in literal_str:
            return z3.RealVal(float(literal_str))
        else:
            return z3.IntVal(int(literal_str))
    except ValueError:
        print(f"Warning: Could not parse numeric literal: {literal_str}")
        return None


def visit_NONNUMERICLITERAL(self, node: ASTNode):
    """
    Returns a Z3 string value for non-numeric literals (usually quoted strings).
    """
    literal_str = node.get("Source Text").strip()


    # Remove enclosing quotes (COBOL often uses ' or ")
    if literal_str.startswith(("'", '"')) and literal_str.endswith(("'", '"')):
        literal_str = literal_str[1:-1]

    return z3.StringVal(literal_str)


def visit_figurativeConstant(self, node: ASTNode):
        """
        Goal: Return a Z3 literal for a figurative constant.
        """
        # 1. Get the source text of the constant (e.g., `SPACE`, `ZERO`).
        source_text = self._get_source_text(node).upper().strip()

        # 2. Return the corresponding Z3 literal.
        if source_text in ("SPACE", "SPACES"):
            # Represents a single space character.
            return z3.StringVal(' ')
        elif source_text in ("ZERO", "ZEROS", "ZEROES"):
            # Represents the numerical value 0. The context (e.g., MOVE statement)
            # is responsible for converting this to a string of '0's if needed.
            return z3.IntVal(0)
        elif source_text in ("HIGH-VALUE", "HIGH-VALUES"):
            # Represents the highest character value in the collating sequence.
            # Typically ASCII 255 for an 8-bit character.
            # We return a single-byte BitVec; the calling operation must handle filling.
            return z3.BitVecVal(0xFF, 8)
        elif source_text in ("LOW-VALUE", "LOW-VALUES"):
            # Represents the lowest character value, typically the null character (ASCII 0).
            return z3.BitVecVal(0x00, 8)
        elif source_text in ("QUOTE", "QUOTES"):
            # Represents the double-quote character.
            return z3.StringVal('"')
        # The 'ALL' keyword is handled by its parent node (e.g., `visit_literal`).
        # Other constants like NULL/NULLS are not covered by the current plan.
        else:
            raise NotImplementedError(f"Figurative constant '{source_text}' is not supported.")

def visit_BooleanLiteral(self, node):
        """
        Processes a BooleanLiteral node (TRUE or FALSE) and returns the corresponding Z3 boolean value.
        """
        # Goal: Return a Z3 boolean value.

        # 1. Get the source text.
        source_text = self._get_source_text(node).upper().strip()

        # 2. If `TRUE`, return `z3.BoolVal(True)`.
        if source_text == "TRUE":
            return z3.BoolVal(True)
    
        # 3. If `FALSE`, return `z3.BoolVal(False)`.
        elif source_text == "FALSE":
            return z3.BoolVal(False)
    
        # This case should not be reached if the AST is valid according to the grammar.
        # Adding an exception for robustness.
        else:
            raise ValueError(f"Unknown BooleanLiteral value: '{source_text}'")

def visit_ConditionName(self, node: ASTNode):
        """
        Handles an 88-level condition name.
        Goal: Not implemented. This would involve looking up the 88-level condition.
        """
        # 1. Log a warning that 88-level condition names are not yet supported.
        condition_name_text = self._get_source_text(node)
        print(f"[WARNING] 88-level condition name '{condition_name_text}' is not yet supported. The condition will be ignored.")
        # As this feature is not implemented, we return None, indicating no Z3 expression
        # is generated for this condition.
        return None

def visit_DataName(self, node):
        """
        Goal: Return the raw text of the data name.

        1. Use `_get_source_text(node)` to get the name.
        2. Return the string.
        """
        # Step 1: Use `_get_source_text(node)` to get the name.
        name = self._get_source_text(node)
        # print(name)
        # Step 2: Return the string.
        return name
    
