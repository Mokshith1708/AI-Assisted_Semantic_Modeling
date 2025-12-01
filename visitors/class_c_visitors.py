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
# Class C Visitor 2: DataValueIntervalTo
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
# Class C Visitor 3: RelationCombinedCondition
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
# Class C Visitor 4: Abbreviation
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
