// JSON Grammar
// ============
//
// Based on the grammar from RFC 7159 [1].
//


JSON_text
  = ws @value ws

begin_array     = ws "[" ws
begin_object    = ws "{" ws
end_array       = ws "]" ws
end_object      = ws "}" ws
name_separator  = ws ":" ws
value_separator = ws "," ws

ws "whitespace" = [ \t\n\r]*

// ----- 3. Values -----

value
  = false
  / null
  / true
  / object
  / array
  / number
  / string

false = "false" 
null  = "null"  
true  = "true"  

// ----- 4. Objects -----

object
  = begin_object
    members:(
      head:member
      tail:(value_separator @member)*
    )?
    end_object

member
  = name:string name_separator value:value 

// ----- 5. Arrays -----

array
  = begin_array
    values:(
      head:value
      tail:(value_separator @value)*
    )?
    end_array

// ----- 6. Numbers -----

number "number"
  = minus? int frac? exp? 

decimal_point
  = "."

digit1_9
  = [1-9]

e
  = [eE]

exp
  = e (minus / plus)? DIGIT+

frac
  = decimal_point DIGIT+

int
  = zero / (digit1_9 DIGIT*)

minus
  = "-"

plus
  = "+"

zero
  = "0"

// ----- 7. Strings -----

string "string"
  = quotation_mark chars:char* quotation_mark 

char
  = unescaped
  / escape
    sequence:(
        '"'
      / "\\"
      / "/"
      / "b" 
      / "f" 
      / "n" 
      / "r" 
      / "t" 
      / "u" digits:$(HEXDIG HEXDIG HEXDIG HEXDIG) 
    )


escape
  = "\\"

quotation_mark
  = '"'

unescaped
  = [^\0-\x1F\x22\x5C]

// ----- Core ABNF Rules -----

DIGIT  = [0-9]
HEXDIG = [0-9a-f]i
