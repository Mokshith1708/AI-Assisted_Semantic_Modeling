# AI-Assisted Semantic Modeling for COBOL Unit Test Generation

This repository contains the artifacts related to project on AI-Assisted Semantic Modeling of Languages for Symbolic Execution Driven Unit Test Generation.

## Repository Structure and File Descriptions

This section outlines the purpose of the main directories and key files within this repository.

### Root Directory (`/`)

`grammar_rules.txt`:
    *   **Description:** Defines the context-free grammar for the COBOL language dialect targeted by the semantic modeling process. This grammar is used to parse COBOL source code into an Abstract Syntax Tree (AST).

`visitor_class_template.py`:
    *   **Description:** A Python template file that outlines the structure of the AST visitor class. This template is augmented by LLM-generated methods to create a full semantic visitor for COBOL.

#### `visitors/`

This directory contains the categorized visitor methods, organized according to their generation quality and required manual correction, as discussed in the associated research paper.

*   `class_a_visitors.py`:
    *   **Description:** Contains visitor methods that were **Correctly Implemented (Class A)** by the LLM. These methods are structurally sound and semantically accurate, requiring no manual fixes.
*   `class_b_visitors.py`:
    *   **Description:** Contains visitor methods that were **Mostly Correct with Minor Issues (Class B)**. These methods have good core logic but required minor refinements for specific COBOL behaviors (e.g., rounding, specific type conversions). This file includes both the original LLM-generated version and the manually corrected version for each method.
*   `class_c_visitors.py`:
    *   **Description:** Contains visitor methods with **Significant Issues or Missing Semantics (Class C)**. The LLM struggled significantly with these, often due to complex non-local reasoning or structural transformations. This file includes both the original LLM-generated version (often a stub or partial implementation) and the manually corrected version for each method.


#### `test_case_generation/`

This directory contains the core logic for the test case generation, sample COBOL programs, and the outputs of the analysis.

*   `test_case_generation/cobol_z3_visitor.py`:
    *   **Description:** The primary Python script implementing the `CobolZ3Visitor` class. This class traverses the COBOL AST and translates program logic into Z3 constraints, which can then be solved to generate test inputs. This file originally contained all generated visitor methods before their categorization into separate files.

*  `test_case_generation/cobol_samples/`

    *   **Description:** Contains various sample COBOL programs (`.cbl` files) used for testing, demonstration, and evaluating the semantic modeling and test generation process. These samples range in complexity and demonstrate different COBOL features.

*  `test_case_generation/output/`

    *   **Description:** Sample output artifacts generated during the test case generation process.

