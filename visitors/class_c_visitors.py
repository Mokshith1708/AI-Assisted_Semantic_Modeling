#================================================================================
# Class C Visitors
#================================================================================

#================================================================================
# Class C Visitor 1: RelationCombinedComparison
#================================================================================

# Grammar Rule:
# RelationCombinedComparison : ArithmeticExpression RelationalOperator LPARENCHAR RelationCombinedCondition RPARENCHAR ;
# RelationCombinedCondition : ArithmeticExpression ((AND | OR) ArithmeticExpression)+ ;

# ------------------------------------------------------------------------------
# Original Generated Visitor (Class C)
# ------------------------------------------------------------------------------
# The LLM was unable to synthesize the logic for a combined relational
# comparison, like `A > B AND C`, which should be expanded to `A > B AND A > C`.
# Instead, it produced a warning stub, indicating that the feature is not supported.
def visit_RelationCombinedComparison_original(self, node: ASTNode):
    source_text = _get_node_text(node)
    print(f"[WARNING] The combined comparison syntax '{source_text}' is not yet supported. The behavior of this comparison will not be modeled.")
    return None

# ------------------------------------------------------------------------------
# Corrected Visitor
# ------------------------------------------------------------------------------
# The corrected visitor implements the expansion logic. It reuses the left-hand
# side of the comparison and combines it with each of the right-hand side
# expressions in the combined condition.
def visit_RelationCombinedComparison_corrected(self, node: ASTNode):
    children = _get_children(node)
    
    # The first child is the left-hand side expression (e.g., `A` in `A > B AND C`)
    lhs_expr = self.visit(children[0]) # Assumes self.visit correctly evaluates arithmetic expressions
    
    # The second child is the relational operator (e.g., `>`)
    op_func = self.visit(children[1]) # Assumes self.visit returns a callable operator function
    
    # The third child is the LPARENCHAR, and the fourth is the RelationCombinedCondition
    combined_condition_node = children[3]
    
    # The children of RelationCombinedCondition are the right-hand side expressions
    # (e.g., `B` and `C`) and the logical operators (e.g., `AND`)
    combined_children = _get_children(combined_condition_node)
    
    # The first right-hand side expression (e.g., `B`)
    rhs1_expr = self.visit(combined_children[0])
    
    # The first comparison (e.g., `A > B`)
    result_expr = op_func(lhs_expr, rhs1_expr)
    
    # Iterate over the remaining logical and arithmetic expressions
    i = 1
    while i < len(combined_children):
        logical_op_node = combined_children[i]
        logical_op = _get_node_text(logical_op_node).upper()
        
        rhs_expr = self.visit(combined_children[i+1])
        
        # The next comparison (e.g., `A > C`)
        next_comparison = op_func(lhs_expr, rhs_expr)
        
        if logical_op == "AND":
            result_expr = z3.And(result_expr, next_comparison)
        elif logical_op == "OR":
            result_expr = z3.Or(result_expr, next_comparison)
        
        i += 2
        
    return result_expr

#================================================================================
# Class C Visitor 2: DataRedefinesClause
#================================================================================

# Grammar Rule:
# DataRedefinesClause : REDEFINES DataName ;

# ------------------------------------------------------------------------------
# Original Generated Visitor (Class C)
# ------------------------------------------------------------------------------
# The LLM may fail to correctly implement REDEFINES because it requires
# non-local reasoning. It needs to find the variable being redefined and link
# the new variable to the same physical memory space. A failing implementation
# might return a simple stub or try to create a new variable.
def visit_DataRedefinesClause_original(self, node: ASTNode):
    data_name_node = _find_child_node(node, 'DataName')
    redefined_base_name = _get_node_text(data_name_node)
    print(f"[WARNING] REDEFINES for {redefined_base_name} is not fully implemented.")
    return None

# ------------------------------------------------------------------------------
# Corrected Visitor
# ------------------------------------------------------------------------------
# The corrected visitor resolves the name of the variable being redefined,
# finds its physical Z3 variable (the bit vector representing its memory),
# and returns this information to the parent `DataDescriptionEntryFormat1`
# visitor. The parent visitor will then use this existing physical variable
# for the new data item, effectively creating a redefinition.
def visit_DataRedefinesClause_corrected(self, node: ASTNode):
    data_name_node = _find_child_node(node, 'DataName')
    redefined_base_name = _get_node_text(data_name_node)

    # Resolve the full name of the variable being redefined
    redefined_full_name = self.resolve_variable(redefined_base_name, []) # Assumes resolve_variable is available
    
    # Retrieve the physical Z3 variable of the redefined item
    physical_var_key = f'{redefined_full_name}_physical'
    if physical_var_key not in self.declarations: # Assumes self.declarations is available
        raise NameError(f"Cannot redefine '{redefined_full_name}' because its physical variable was not found.")
        
    redefined_physical_var = self.declarations[physical_var_key]

    # Return information about the redefinition to the parent visitor
    # The parent (visit_DataDescriptionEntryFormat1) will use this to link
    # the new logical variable to the existing physical variable.
    return {
        'redefines': True,
        'target_physical_var': redefined_physical_var
    }

#================================================================================
# Class C Visitor 3: DataRenamesClause
#================================================================================

# Grammar Rule:
# DataRenamesClause : RENAMES QualifiedDataName ((THROUGH | THRU) QualifiedDataName)? ;

# ------------------------------------------------------------------------------
# Original Generated Visitor (Class C)
# ------------------------------------------------------------------------------
# The LLM explicitly marked this as "Not implemented in this phase".
# RENAMES creates a new logical view over a range of memory, potentially across
# multiple data items, making it complex to model symbolically without explicit
# memory layout knowledge.
def visit_DataRenamesClause_original(self, node: ASTNode):
    source_text = _get_node_text(node)
    print(f"[WARNING] The RENAMES clause is not yet supported and will be ignored. Clause: '{source_text}'")
    return None

# ------------------------------------------------------------------------------
# Corrected Visitor
# ------------------------------------------------------------------------------
# Correcting RENAMES would involve creating a new Z3 logical variable that
# represents a slice of an existing BitVec (physical memory) or a composite of
# multiple BitVecs, depending on the `THROUGH` clause. This requires detailed
# knowledge of the memory layout of the renamed items.
def visit_DataRenamesClause_corrected(self, node: ASTNode):
    # This is a complex feature that typically requires mapping a new logical
    # data item to a potentially non-contiguous or partial section of existing
    # physical memory, or even a range spanning multiple existing data items.
    # For a full correction, we would need:
    # 1. To resolve the start QualifiedDataName to its physical Z3 variable and bit offset.
    # 2. To resolve the optional end QualifiedDataName (if THRU is used) for the end bit offset.
    # 3. Create a new Z3 BitVec variable for the renamed item.
    # 4. Add constraints to link this new BitVec to the relevant slice(s) of the original BitVec(s).
    
    # Given the complexity, a simplified correction might involve:
    source_text = _get_node_text(node)
    print(f"[INFO] RENAMES clause '{source_text}' detected. Modeling as a direct alias for the starting item for now.")

    # Get the first QualifiedDataName
    qdn_nodes = _get_children(node, "QualifiedDataName")
    if not qdn_nodes:
        print(f"[ERROR] RENAMES clause '{source_text}' missing QualifiedDataName.")
        return None

    # Resolve the full name of the starting data item
    starting_data_name = self.visit(qdn_nodes[0]) # Assumes self.visit resolves QDN
    
    # A full implementation would create a new logical variable and link it
    # to the specified memory range. For this example, we'll return the starting
    # data item's logical variable, effectively treating RENAMES as a simple alias
    # for the first part of the renamed area.
    if starting_data_name in self.declarations:
        return self.declarations[starting_data_name]
    else:
        print(f"[ERROR] Could not resolve starting data item '{starting_data_name}' for RENAMES.")
        return None

#================================================================================
# Class C Visitor 4: DataValueIntervalTo
#================================================================================

# Grammar Rule:
# DataValueInterval : DataValueIntervalFrom DataValueIntervalTo? ;
# DataValueIntervalTo : (THROUGH | THRU) literal ;

# ------------------------------------------------------------------------------
# Original Generated Visitor (Class C)
# ------------------------------------------------------------------------------
# The LLM implemented DataValueIntervalTo as a no-op (`pass`), stating that
# "The parent node that contains the full 'VALUE <literal> THRU <literal>' clause
# is responsible for processing the entire range." This indicates a failure to
# generate the logic for handling ranges in 88-level condition names.
def visit_DataValueIntervalTo_original(self, node):
    # ISSUE: This is a no-op, which means the 'THRU' part of 88-level VALUE clauses
    # (used for ranges) is not processed.
    pass

# ------------------------------------------------------------------------------
# Corrected Visitor
# ------------------------------------------------------------------------------
# The corrected visitor would extract the literal value from the 'TO' part of
# a value range. This value would then be used by a parent visitor (e.g.,
# a ConditionNameDeclaration visitor, if implemented) to construct a Z3 range
# constraint (e.g., `start <= variable <= end`).
def visit_DataValueIntervalTo_corrected(self, node: ASTNode):
    # FIX: Extract the literal value for the 'TO' part of the range.
    # The actual Z3 constraint for the range (start <= variable <= end)
    # would be generated by a higher-level visitor (e.g., for ConditionName).
    
    # The children of DataValueIntervalTo are (THROUGH | THRU) and then a literal.
    literal_node = _get_nth_child(node, 1) # Assumes the literal is the second child (index 1)
    if not literal_node:
        raise ValueError("DataValueIntervalTo node is missing its literal child.")
    
    # Assumes self.visit(literal_node) returns the Z3 literal value
    z3_literal_to = self.visit(literal_node)
    
    return z3_literal_to

#================================================================================
# Class C Visitor 5: RelationCombinedCondition
#================================================================================

# Grammar Rule:
# RelationCombinedCondition : ArithmeticExpression ((AND | OR) ArithmeticExpression)+ ;

# ------------------------------------------------------------------------------
# Original Generated Visitor (Class C)
# ------------------------------------------------------------------------------
# This visitor is explicitly a no-op in the generated code, with a comment
# indicating that the logic is handled by the parent's visitor. This implies
# a deferral of the complex logic, similar to the `RelationCombinedComparison`.
def visit_RelationCombinedCondition_original(self, node):
    # Per the provided plan, the logic for this node is handled by the parent's visitor
    # (e.g., in an IF or PERFORM statement). The parent is expected to manually
    # iterate over this node's children to construct the combined boolean expression.
    # Therefore, this visitor method is intentionally a no-op.
    pass

# ------------------------------------------------------------------------------
# Corrected Visitor
# ------------------------------------------------------------------------------
# The corrected visitor for `RelationCombinedCondition` should construct a Z3
# boolean expression by iterating through its children (ArithmeticExpressions
# and logical operators) and combining them. This is a sub-component of
# `RelationCombinedComparison` and requires similar logic.
def visit_RelationCombinedCondition_corrected(self, node: ASTNode):
    children = _get_children(node)
    if not children:
        return z3.BoolVal(True) # Empty condition is always true

    # The first child is always an ArithmeticExpression
    current_expr = self.visit(children[0]) # Assumes self.visit evaluates arithmetic expressions

    # Iterate through the rest of the children (logical operator, ArithmeticExpression, ...)
    i = 1
    while i < len(children):
        logical_op_node = children[i]
        logical_op = _get_node_text(logical_op_node).upper()

        arith_expr_node = children[i+1]
        next_arith_expr = self.visit(arith_expr_node)

        if logical_op == "AND":
            current_expr = z3.And(current_expr, next_arith_expr)
        elif logical_op == "OR":
            current_expr = z3.Or(current_expr, next_arith_expr)
        else:
            raise ValueError(f"Unrecognized logical operator '{logical_op}' in RelationCombinedCondition.")
        
        i += 2
    
    return current_expr

#================================================================================
# Class C Visitor 6: Abbreviation
#================================================================================

# Grammar Rule:
# Abbreviation : NOT? RelationalOperator? ( ArithmeticExpression | LPARENCHAR ArithmeticExpression Abbreviation RPARENCHAR ) ;

# ------------------------------------------------------------------------------
# Original Generated Visitor (Class C)
# ------------------------------------------------------------------------------
# The LLM marked this as "Not implemented in this phase. Log a warning."
# Abbreviated conditions (e.g., `A > B AND C` meaning `A > B AND A > C`)
# require context from the preceding condition to correctly expand.
def visit_Abbreviation_original(self, node: ASTNode):
    source_text = _get_node_text(node).strip()
    print(f"[WARNING] Abbreviated condition '{source_text}' is not yet supported and will be ignored.")
    return None

# ------------------------------------------------------------------------------
# Corrected Visitor
# ------------------------------------------------------------------------------
# Correctly handling `Abbreviation` requires passing context (specifically,
# the left-hand side and relational operator of the *previous* comparison)
# down to this visitor. This is an example of non-local reasoning.
def visit_Abbreviation_corrected(self, node: ASTNode, previous_lhs_expr, previous_op_func):
    # This corrected visitor now expects 'previous_lhs_expr' and 'previous_op_func'
    # to be passed from its parent (e.g., visit_Condition).
    
    is_not = _find_child_node(node, "NOT") is not None
    
    # If a relational operator is explicitly provided in the abbreviation (e.g., `A > B OR < C`)
    # use it; otherwise, infer from the previous one.
    explicit_op_node = _find_child_node(node, "RelationalOperator")
    op_func = self.visit(explicit_op_node) if explicit_op_node else previous_op_func

    if not callable(op_func):
        raise TypeError(f"Could not determine a callable operator function for Abbreviation.")

    # The actual operand (ArithmeticExpression or nested Abbreviation)
    operand_node = _get_first_child(node, "ArithmeticExpression")
    if not operand_node: # Could be a nested Abbreviation as per grammar
        operand_node = _get_first_child(node, "Abbreviation")
        if operand_node:
            # Recursively handle nested abbreviation, passing current context
            operand_expr = self.visit(operand_node, previous_lhs_expr, op_func)
        else:
            raise ValueError("Abbreviation node missing expected operand.")
    else:
        operand_expr = self.visit(operand_node)

    result_expr = op_func(previous_lhs_expr, operand_expr)

    if is_not:
        result_expr = z3.Not(result_expr)
    
    return result_expr

#================================================================================
# Class C Visitor 7: generic_visit
#================================================================================

# This is not a grammar rule but a fallback mechanism. Its presence
# in Class C implies that the LLM failed to generate a specific visitor
# for some grammar rules, leading to the generic visitor being hit.

# ------------------------------------------------------------------------------
# Original Generated Visitor (Class C)
# ------------------------------------------------------------------------------
# The generic_visit method is meant as a fallback. However, if too many
# nodes fall into this, it indicates missing semantic understanding.
def generic_visit_original(self, node: ASTNode) -> Any:
    """Default visitor for structural/unhandled nodes. It just visits all children."""
    result = None
    for child in node.get("Children", []):
        child_result = self.visit(child)
        if child_result is not None and result is None:
            result = child_result
    return result

# ------------------------------------------------------------------------------
# Corrected Visitor
# ------------------------------------------------------------------------------
# A "corrected" generic_visit would imply that all specific grammar rules
# have dedicated visitors, and the generic_visit is truly only for AST structural
# nodes that don't have direct semantic implications.
# For a Class C context, the correction means reducing its usage by implementing
# specific visitors for previously unhandled nodes. Here, we'll modify it to
# explicitly warn if it's hit for a node that *should* have a semantic visitor.
def generic_visit_corrected(self, node: ASTNode) -> Any:
    """
    Corrected default visitor. Warns for unhandled semantic nodes.
    """
    node_type = node.get('Node', 'UNKNOWN')
    # FIX: Add a warning for semantic nodes that should have a specific visitor
    # but fell through to the generic one. This implies a gap in the LLM-generated visitors.
    # We might maintain a list of node types that are purely structural versus semantic.
    if node_type not in ["Statement", "Paragraphs", "ProgramUnit", "DataDivisionSection", "IfThen", "IfElse", "MultDivs", "Powers", "Basis", "Condition", "CombinableCondition", "SimpleCondition", "RelationCondition", "QualifiedDataName", "QualifiedInData", "Literal", "Argument"]:
        print(f"[WARNING] generic_visit_corrected: No specific visitor for semantic node type '{node_type}'. Processing children generically.")

    result = None
    for child in node.get("Children", []):
        child_result = self.visit(child)
        if child_result is not None and result is None:
            result = child_result
    return result