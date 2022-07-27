(formal_parameters
  [
    (identifier) @variable.parameter
    (array_pattern
      (identifier) @variable.parameter)
    (object_pattern
      [
        (pair_pattern value: (identifier) @variable.parameter)
        (shorthand_property_identifier_pattern) @variable.parameter
        (rest_pattern (identifier) @variable.parameter)
      ])
     (assignment_pattern
       (identifier) @variable.parameter)
  ]
)

(arrow_function . (identifier) @variable.parameter)
