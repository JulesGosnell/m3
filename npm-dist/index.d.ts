/**
 * M3 JSON Schema Validator
 * The most complete JSON Schema validator. All drafts (3, 4, 6, 7, 2019-09, 2020-12, draft-next).
 */

export interface ValidationError {
  schemaPath: (string | number)[];
  documentPath: (string | number)[];
  message: string;
  document: any;
  schema: any;
  errors?: ValidationError[];
}

export interface ValidationResult {
  valid: boolean;
  errors: ValidationError[] | null;
}

export interface ValidateOptions {
  /** JSON Schema draft version */
  draft?: "draft3" | "draft4" | "draft6" | "draft7" | "draft2019-09" | "draft2020-12" | "draft-next";
  /** Treat format as assertion (default: annotation-only) */
  strictFormat?: boolean;
  /** Require actual integers (not 1.0 for integer type) */
  strictInteger?: boolean;
}

/**
 * Validate a document against a JSON Schema.
 * @param schema - JSON Schema object
 * @param document - Document to validate
 * @param options - Optional validation options
 * @returns Validation result with valid flag and errors
 */
export function validate(schema: object, document: any, options?: ValidateOptions): ValidationResult;

/**
 * Compile a schema and return a reusable validation function.
 * More efficient for validating many documents against one schema.
 * @param schema - JSON Schema object
 * @param options - Optional validation options
 * @returns Reusable validation function
 */
export function validator(schema: object, options?: ValidateOptions): (document: any) => ValidationResult;
