# cobol_z3_visitor.py

import z3
import re
from typing import Dict, Any, List, Optional
from z3.z3util import get_vars


# A simple type alias for clarity
ASTNode = Dict[str, Any]

class CobolZ3Visitor:
    """
    Traverses a COBOL AST and generates Z3 constraints.
    Manages the symbolic state of the program execution.
    """
    def __init__(self):
        self.solver = z3.Solver()  # Z3 solver instance to hold and check all constraints.
        self.variables: Dict[str, z3.ExprRef] = {}  # Symbol table mapping variable names to Z3 expressions.
        self.pic_info: Dict[str, Dict] = {}  # Stores parsed PIC clause info for each variable.
        self._current_group_context: List[Dict] = []  # Stack to manage nested group-level declarations.
        self.declarations: Dict[str, z3.ExprRef] = {}  # Stores all declared Z3 variables with full names.
        self.constraints: List[z3.BoolRef] = []  # List of all Z3 constraints added during translation.
        self._current_data_item_being_declared: Optional[str] = None  # Tracks the current variable being processed.
        self._is_in_declaration_context: bool = False  # Flag to indicate if inside a declaration context.
        self.latest_assignments: Dict[str, z3.BoolRef] = {}  # Stores latest assignment constraint for each variable (used to override in MOVE)

    def resolve_variable(self, base_name: str, qualifiers: List[str] = []) -> str:
        """
        Resolves a variable like `I OF M` by finding full names ending with `M.I`, `X.M.I`, etc.
        """
        if qualifiers:
            suffix = ".".join(qualifiers + [base_name])
            matches = [full for full in self.pic_info if full.endswith(suffix)]
            if len(matches) == 1:
                return matches[0]
            elif not matches:
                raise Exception(f"[ERROR] Could not resolve '{base_name} OF {' OF '.join(qualifiers)}'")
            else:
                raise Exception(f"[ERROR] Ambiguous qualified reference '{suffix}', candidates: {matches}")
        else:
            # Lookup directly in pic_info for unqualified name
            matches = [key for key in self.pic_info if key.endswith(f".{base_name}") or key == base_name]
            if len(matches) == 1:
                return matches[0]
            elif not matches:
                raise Exception(f"[ERROR] Variable '{base_name}' not declared.")
            else:
                raise Exception(f"[ERROR] Ambiguous variable name '{base_name}', candidates: {matches}")

    def update_assignment(self, var_name: str, stmt_type: str, constraint: z3.BoolRef):
        """
        Updates assignment constraints for a variable.
        
        stmt_type: one of 'VALUE', 'MOVE', 'COMPUTE'
        self.constraints: list of active z3.BoolRef constraints
        self.latest_assignments: dict of {var_name: {type: constraint}}
        """

        def remove_old(var, type_):
            old = self.latest_assignments.get(var, {}).get(type_)
            if old and old in self.constraints:
                self.constraints.remove(old)

        if var_name not in self.latest_assignments:
            self.latest_assignments[var_name] = {}

        if stmt_type in {"MOVE", "VALUE"}:
            remove_old(var_name, "VALUE")
            self.latest_assignments[var_name]["VALUE"] = constraint
            self.constraints.append(constraint)

        elif stmt_type == "COMPUTE":
            lhs = constraint.arg(0)
            rhs = constraint.arg(1)

            # Remove old VALUE if present
            remove_old(var_name, "VALUE")
            self.latest_assignments[var_name].pop("VALUE", None)

            # Try substituting RHS vars using known VALUE constraints
            substitutions = []
            for other_var, type_map in self.latest_assignments.items():
                if other_var == var_name:
                    continue
                val_constr = type_map.get("VALUE")
                if val_constr and val_constr.decl().name() == "==":
                    lhs_sym = val_constr.arg(0)
                    rhs_val = val_constr.arg(1)
                    if lhs_sym in get_vars(rhs):
                        substitutions.append((lhs_sym, rhs_val))
                        remove_old(other_var, "VALUE")
                        self.latest_assignments[other_var].pop("VALUE", None)

            if substitutions:
                rhs = z3.substitute(rhs, substitutions)
                constraint = lhs == rhs

            self.latest_assignments[var_name]["COMPUTE"] = constraint
            self.constraints.append(constraint)

    # === AST Traversal Engine ===
    def visit(self, node: Optional[ASTNode]) -> Any:
        """Generic visit method to dispatch to the specific visitor function."""
        if not node:
            return None
        
        node_type = node.get('Node', '').replace('-', '_')
        method_name = f'visit_{node_type}'
        
        # Dispatch to specific visitor or the generic one
        visitor_func = getattr(self, method_name, self.generic_visit)
        
        return visitor_func(node)
    def generic_visit(self, node: ASTNode) -> Any:
        """Default visitor for structural/unhandled nodes. It just visits all children."""
        result = None
        for child in node.get("Children", []):
            child_result = self.visit(child)
            # For choice nodes, we return the value from the first successful path.
            if child_result is not None and result is None:
                result = child_result
        return result
    def _add_declaration(self, name: str, z3_var: z3.ExprRef):
        """Helper to add a declaration and prevent duplicates."""
        if name in self.declarations:
            print(f"[WARNING] Duplicate declaration attempted for '{name}'. Ignoring.")
            return
        self.declarations[name] = z3_var     
    def _add_constraint(self, constraint: z3.BoolRef):
        """Helper to add a constraint to the list."""
        self.constraints.append(constraint)
    def finalize_and_add_to_solver(self):
        """
        Final step to add all path-aware constraints to the solver using Implies.
        """
        if self.constraints:
            self.solver.add(self.constraints)

    # === HELPER METHODS (For use by generated visitors) ===
    def _find_child_node(self, parent_node: ASTNode, node_name: str) -> Optional[ASTNode]:
        """Finds the first child node with a specific 'Node' type."""
        for child in parent_node.get("Children", []):
            if child.get("Node") == node_name:
                return child
        return None

    def _get_node_text(self, node: Optional[ASTNode]) -> str:
        """Returns the 'Source Text' of the given AST node."""
        return node.get("Source Text", "") if node else ""

    def _get_source_text(self, node: Optional[ASTNode]) -> str:
        """Alias for `_get_node_text`, for naming consistency."""
        return self._get_node_text(node)

    def _get_child_text(self, parent_node: ASTNode, node_name: str) -> str:
        """Returns the 'Source Text' of the child node with the given type."""
        child = self._find_child_node(parent_node, node_name)
        if child:
            # Some terminals might have a nested ASTNodeToken
            inner_child = child.get("Children", [{}])[0]
            if inner_child:
                return self._get_node_text(inner_child)
            return self._get_node_text(child)
        return ""

    def _get_nth_child(self, node: ASTNode, index: int) -> Optional[ASTNode]:
        """Returns the nth child node of the given node, if exists."""
        children = node.get("Children", [])
        if 0 <= index < len(children):
            return children[index]
        return None

    def _get_children(self, node: ASTNode, filter_type: Optional[str] = None) -> List[ASTNode]:
        """
        Returns all children of the node. Optionally filter by 'Node' type.
        """
        children = node.get("Children", [])
        if filter_type:
            return [child for child in children if child.get("Node") == filter_type]
        return children

    def _get_first_child(self, node: ASTNode, filter_type: Optional[str] = None) -> Optional[ASTNode]:
        """Returns the first child node (optionally filtered by type)."""
        for child in node.get("Children", []):
            if not filter_type or child.get("Node") == filter_type:
                return child
        return None

    
    # === LLM-GENERATED SEMANTIC VISITORS WILL BE APPENDED HERE ===