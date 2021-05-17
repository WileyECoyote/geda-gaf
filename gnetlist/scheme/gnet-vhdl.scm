; -*-scheme-*-
;
;;; gEDA - GPL Electronic Design Automation
;;; gnetlist - gEDA Netlist
;
;;; Copyright (C) 1998-2010 Ales Hvezda
;;; Copyright (C) 1998-2015 gEDA Contributors (see ChangeLog for details)
;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;; 02110-1301 USA, <http://www.gnu.org/licenses/>.


;;; Various support functions shamelessly stolen from the verilog code and
;;; reshaped for vhdl. Doing this now saves labour when the implementations
;;; starts to divert further.

;;; Get port list of top-level Entity
;;; THHE changed this to the urefs of the I/O-PAD symbols rather than the
;;; net names. So the uref of the I/O port will become the port name in
;;; the VHDLport clause.

;;; THHE
;;;
;;; Since VHDL know about port directions, pins need a additional attribute.
;;; The code assumes the attribute "type" (IN, OUT, INOUT) on each pin of a symbol.
;;; In addition you can add the attribute "width" for a very simple definition of
;;; busses. (Not complete yet!)
;;;

(define vhdl:get-top-port-list
  (lambda ()
    ;; construct list
    (list (vhdl:get-matching-urefs "device" "IPAD"  packages)
          (vhdl:get-matching-urefs "device" "OPAD"  packages)
          (vhdl:get-matching-urefs "device" "IOPAD" packages))))

;;; Get matching urefs
(define vhdl:get-matching-urefs
  (lambda (attribute value package-list)
     (cond ((null? package-list) '())
          ((string=? (get-package-attribute (car package-list) attribute) value)
           (cons
            (cons (car package-list) (get-package-attribute (car package-list) "width"))
            (vhdl:get-matching-urefs attribute value (cdr package-list))))
          (else (vhdl:get-matching-urefs attribute value (cdr package-list))))

  )
)

;;; Port Clause
;;;
;;; According to IEEE 1076-1993 1.1.1:
;;;
;;; entity_header :=
;;;  [ formal_generic_clause ]
;;;  [ formal_port_clause ]
;;;
;;; generic_clause :=
;;;    GENERIC ( generic_list ) ;
;;;
;;; port_clause :=
;;;    PORT ( port_list ) ;
;;;
;;; According to IEEE 1076-1993 1.1.1.2:
;;;
;;; port_list := port_interface_list
;;;
;;; According to IEEE 1076-1993 4.3.2.1:
;;;
;;; interface_list := interface_element { ; interface_element }
;;;
;;; interface_element := interface_declaration
;;;
;;; According to IEEE 1076-1993 4.3.2:
;;;
;;; interface_declaration :=
;;;    interface_constant_declaration
;;;  | interface_signal_declaration
;;;  | interface_variable_declaration
;;;  | interface_file_declaration
;;;
;;; interface_signal_declaration :=
;;;  [ SIGNAL ] identifier_list : [ mode ] subtype_indication [ BUS ]
;;;  [ := static_expression ]
;;;
;;; mode := IN | OUT | INOUT | BUFFER | LINKAGE
;;;
;;; Implementation note:
;;;    Since the port list must contain signals will only the interface
;;;    signal declaration of the interface declaration be valid. Further,
;;;    we may safely assume that the SIGNAL symbol will not be needed.
;;;    The identifier list is reduced to a signle name entry, mode is set
;;;    to in, out or inout due to which part of the port list it comes from.
;;;    The mode types supported are in, out and inout where as buffer and
;;;    linkage mode is not supported. The subtype indication is currently
;;;    hardwired to standard logic, but should be controlled by attribute.
;;;    There is currently no support for busses and thus is the BUS symbol
;;;    no being applied. Also, there is currently no static expression
;;;    support, this too may be conveyed using attributes.
;;;

;;; This little routine writes a single pin on the port clause.
;;; It assumes a list containing (portname, mode, type) such as
;;; (CLK in Std_Logic width).
;;;
;;; THHE If you added a attribute width=n to a pin or to a I/O-PAD, you get
;;;      portname : IN Std_Logic_Vector(width-1 downto 0)
;;;
(define vhdl:write-port
  (lambda (port)
    (if (not (null? port))
        (begin
          (if (string=? (cadddr port) "unknown")
            (begin
              (display (car port))
              (display " : ")
              (display (cadr port))
              (display " ")
              (display (caddr port))
            )
          )
          (if (not (string=? (cadddr port) "unknown"))
            (begin
              (display (car port))
              (display " : ")
              (display (cadr port))
              (display " ")
              (display (caddr port))
              (display "_Vector(")
              (display (- (string->number(cadddr port)) 1))
              (display " downto 0)")
            )
          )
        )
    )
  )
)

;;; This little routine will actually write the full port clause given a list
;;; of pins, such as ((CLK in Std_Logic) (D in Std_Logic) (Q out Std_Logic))

(define vhdl:write-port-list
  (lambda (port-list)
    (if (not (null? port-list))
        (begin
          (display "    PORT (")
          (newline)
          (display "        ")
          (vhdl:write-port (car port-list))
          (for-each (lambda (pin)
                      (begin
                        (display ";")
                        (newline)
                        (display "        ")
                        (vhdl:write-port pin)
                      )
                    )
                    (cdr port-list))
          (display ");")
          (newline)
        )
    )
  )
)

;;; This is the real thing. It will take a port-list arrangement.
;;;
;;; The port-list is a list containing three list:
;;;  (in-port-list, out-port-list, inout-port-list)
;;;
;;; These lists will be transformed into a single list containing the full
;;; pin information. Currently is this done with hardwired to Std_Logic.

(define vhdl:write-port-clause
  (lambda (port-list)
    (let ((in (car port-list))
          (out (cadr port-list))
          (inout (caddr port-list)))
      (vhdl:write-port-list
        (append
          (map (lambda (pin)
                      (list (car pin) "in" "Std_Logic" (cdr pin))) in)
          (map (lambda (pin)
                      (list (car pin) "out" "Std_Logic" (cdr pin))) out)
          (map (lambda (pin)
                      (list (car pin) "inout" "Std_Logic" (cdr pin))) inout)
        )
      )
    )
  )
)

;;; Primary unit
;;;
;;; According to IEEE 1076-1993 11.1:
;;;
;;; primary_unit :=
;;;    entity_declaration
;;;  | configuration_declaration
;;;  | package_declaration
;;;
;;; Implementation note:
;;;    We assume that gEDA does not generate either a configuration or
;;;    package declaration. Thus, only a entity declaration will be generated.
;;;
;;; According to IEEE 1076-1993 1.1:
;;;
;;; entity_declaration :=
;;;    ENTITY identifier IS
;;;       entity_header
;;;       entity_declarative_part
;;;  [ BEGIN
;;;       entity_statement_part ]
;;;    END [ ENTITY ] [ entity_simple_name ] ;
;;;
;;; Implementation note:
;;;    We assume that no entity declarative part and no entity statement part
;;;    is to be produced. Further, it is good custom in VHDL-93 to append
;;;    both the entity keyword as well as the entity simple name to the
;;;    trailer, therefore this is done to keep VHDL compilers happy.
;;;
;;; According to IEEE 1076-1993 1.1.1:
;;;
;;; entity_header :=
;;;  [ formal_generic_clause ]
;;;  [ formal_port_clause ]
;;;
;;; Implementation note:
;;;    Initially we will assume that there is no generic clause but that there
;;;    is an port clause. We would very much like to have generic and the port
;;;    clause should be conditional (consider writting a test-bench).
;;;

(define vhdl:write-primary-unit
  (lambda (module-name port-list)
    (begin
      ; Entity header
      (display "-- Entity declaration")
      (newline)
      (newline)
      (display "ENTITY ")
      (display module-name)
      (display " IS")
      (newline)
      (vhdl:write-port-clause port-list)
      ; entity_declarative_part is assumed not to be used
      ; entity_statement_part is assumed not to be used
      ; Entity trailer
      (display "END ")
      (display module-name)
      (display ";")
      (newline)
      (newline)
    )
  )
)

;;
;; Secondary Unit Section
;;

;;; Component Declaration
;;;
;;; According to IEEE 1076-1993 4.5:
;;;
;;; component_declaration :=
;;;    COMPONENT identifier [ IS ]
;;;     [ local_generic_clause ]
;;;     [ local_port_clause ]
;;;    END COMPONENT [ component_simple_name ] ;
;;;
;;; Implementation note:
;;;    The component declaration should match the entity declaration of the
;;;    same name as the component identifier indicates. Since we do not yeat
;;;    support the generic clause in the entity declaration we shall not
;;;    support it here either. We will however support the port clause.
;;;
;;;    In the same fassion as before we will use the conditional IS symbol
;;;    as well as replicating the identifier as component simple name just to
;;;    be in line with good VHDL-93 practice and keep compilers happy.

(define vhdl:write-component-declarations
  (lambda (device-list)
    (begin
      (for-each
        (lambda (device)
          (begin
            ; Hmm... I just grabbed this if stuff... do I need it?
            (if (not (memv (string->symbol device) ; ignore specials
                           (map string->symbol (list "IOPAD" "IPAD" "OPAD" "HIGH" "LOW"))))
                (begin
                     (display "    COMPONENT ")
                     (display device)
                     (newline)
                     (vhdl:write-port-clause (vhdl:get-device-port-list
                                                (find-device packages device)))
                     (display "    END COMPONENT ")
                     (display ";")
                     (newline)
                     (newline)
                )
            )
          )
        ) device-list
      )
    )
  )
)

;;; THHE
;;; Build the port list from the symbols
;;;
;;; ... wouldn't it be better to feed get-pins, get-attribute-by-pinnumber and co.
;;;     with the device rather than the component? pin names and atributes are locked to
;;;     the symbol and not to the instance of the symbol in the sheet!

(define vhdl:get-device-port-list
  (lambda (device)
    ;; construct list
    (list (vhdl:get-device-matching-pins device (gnetlist:get-pins device) "IN")
          (vhdl:get-device-matching-pins device (gnetlist:get-pins device) "OUT")
          (vhdl:get-device-matching-pins device (gnetlist:get-pins device) "INOUT")
    )
  )
)

;;; THHE
;;; get a list of all pins of a given type
;;;

(define vhdl:get-device-matching-pins
  (lambda (device pin-list value)
    (cond ((null? pin-list) '())
          ((string=? (gnetlist:get-attribute-by-pinnumber device (car pin-list) "pintype" )
                     value)
           (cons
            (cons (car pin-list) (gnetlist:get-attribute-by-pinnumber device (car pin-list) "width"))
            (vhdl:get-device-matching-pins device (cdr pin-list) value))
           )
          (else (vhdl:get-device-matching-pins device (cdr pin-list) value))
    )
  )
)

;;; THHE
;;; build a list of all unique devices in in the list
;;;

(define vhdl:get-unique-devices
  (lambda (device-list)
      (cond ((null? device-list) '())
            ((not (contains? (cdr device-list) (car device-list)))
             (append (vhdl:get-unique-devices (cdr device-list))
                     (list (car device-list))))
            (else (vhdl:get-unique-devices (cdr device-list)))
      )
  )
)

;;; THHE
;;; build a list of  all unique devices in the schematic
;;;

(define unique-devices
  (lambda nil
    (vhdl:get-unique-devices (map get-device packages))
))


;;; Signal Declaration
;;;
;;; According to IEEE 1076-1993 4.3.1.2:
;;;
;;; signal_declaration :=
;;;    SIGNAL identifier_list : subtype_indication [ signal_kind ]
;;;    [ := expression ] ;
;;;
;;; signal_kind := REGISTER | BUS
;;;
;;; Implementation note:
;;;    Currently will the identifier list be reduced to a single entry.
;;;    There is no support for either register or bus type of signal kind.
;;;    Further, no default expression is being supported.
;;;    The subtype indication is hardwired to Std_Logic.

(define (vhdl:write-signal-declarations)
  (begin
    (for-each
     (lambda (signal)
       (begin
         (display "    SIGNAL ")
         (display signal)
         (display " : Std_Logic;")
         (newline)
       )
     )
     all-unique-nets)
  )
)

;;; Architecture Declarative Part
;;;
;;; According to IEEE 1076-1993 1.2.1:
;;;
;;; architecture_declarative_part :=
;;;  { block_declarative_item }
;;;
;;; block_declarative_item :=
;;;    subprogram_declaration
;;;  | subprogram_body
;;;  | type_declaration
;;;  | subtype_declaration
;;;  | constant_declaration
;;;  | signal_declaration
;;;  | shared_variable_declaration
;;;  | file_declaration
;;;  | alias_declaration
;;;  | component_declaration
;;;  | attribute_declaration
;;;  | attribute_specification
;;;  | configuration_specification
;;;  | disconnection_specification
;;;  | use_clause
;;;  | group_template_declaration
;;;  | group_declaration
;;;
;;; Implementation note:
;;;    There is currently no support for programs or procedural handling in
;;;    gEDA, thus will all declarations above involved in thus activites be
;;;    left unused. This applies to subprogram declaration, subprogram body,
;;;    shared variable declaration and file declaration.
;;;
;;;    Further, there is currently no support for type handling and therefore
;;;    will not the type declaration and subtype declaration be used.
;;;
;;;    The is currently no support for constants, aliases, configuration
;;;    and groups so the constant declaration, alias declaration, configuration
;;;    specification, group template declaration and group declaration will not
;;;    be used.
;;;
;;;    The attribute passing from a gEDA netlist into VHDL attributes must
;;;    wait, therefore will the attribute declaration and attribute
;;;    specification not be used.
;;;
;;;    The disconnection specification will not be used.
;;;
;;;    The use clause will not be used since we pass the responsibility to the
;;;    primary unit (where it is not yet supported).
;;;
;;;    The signal declation will be used to convey signals held within the
;;;    architecture.
;;;
;;;    The component declaration will be used to convey the declarations of
;;;    any external entity being used within the architecture.

(define (vhdl:write-architecture-declarative-part)
  (begin
    ; Due to my taste will the component declarations go first
    ; XXX - Broken until someday
    ; THHE fixed today ;-)
    (vhdl:write-component-declarations (unique-devices))
    ; Then comes the signal declatations
    (vhdl:write-signal-declarations)
  )
)

;;; Architecture Statement Part
;;;
;;; According to IEEE 1076-1993 1.2.2:
;;;
;;; architecture_statement_part :=
;;;  { concurrent_statement }
;;;
;;; According to IEEE 1076-1993 9:
;;;
;;; concurrent_statement :=
;;;    block_statement
;;;  | process_statement
;;;  | concurrent_procedure_call_statement
;;;  | concurrent_assertion_statement
;;;  | concurrent_signal_assignment_statement
;;;  | component_instantiation_statement
;;;  | generate_statement
;;;
;;; Implementation note:
;;;    We currently does not support block statements, process statements,
;;;    concurrent procedure call statements, concurrent assertion statements,
;;;    concurrent signal assignment statements or generarte statements.
;;;
;;;    Thus, we only support component instantiation statements.
;;;
;;; According to IEEE 1076-1993 9.6:
;;;
;;; component_instantiation_statement :=
;;;    instantiation_label : instantiation_unit
;;;  [ generic_map_aspect ] [ port_map_aspect ] ;
;;;
;;; instantiated_unit :=
;;;    [ COMPONENT ] component_name
;;;  | ENTITY entity_name [ ( architecture_identifier ) ]
;;;  | CONFIGURATION configuration_name
;;;
;;; Implementation note:
;;;    Since we are not supporting the generic parameters we will thus not
;;;    suppport the generic map aspect. We will support the port map aspect.
;;;
;;;    Since we do not yeat support the component form we will not yet use
;;;    the component symbol based instantiated unit.
;;;
;;;    Since we do not yeat support configurations we will not support the
;;;    we will not support the configuration symbol based form.
;;;
;;;    This leaves us with the entity form, which we will support initially
;;;    using only the entity name. The architecture identifier could possibly
;;;    be supported by attribute value.

(define vhdl:write-architecture-statement-part
  (lambda (packages)
    (begin
      (display "-- Architecture statement part")
      (newline)
      (vhdl:write-component-instantiation-statements packages)
      (display "-- Signal assignment part")
      (newline)
      (vhdl:write-signal-assignment-statements packages)
    )
  )
)
;;; THHE
;;; write component instantiation for each component in the sheet
;;;

(define vhdl:write-component-instantiation-statements
  (lambda (packages)
    (for-each (lambda (package)
      (begin
        (let ((device (get-device package)))
          (if (not (memv (string->symbol device)
                         (map string->symbol
                                (list "IOPAD" "IPAD" "OPAD"
                                 "HIGH" "LOW"))))
            (begin
              (display "    ")
              ; label
              (display package)
              (display " : ")
              ; entity name
              (display (get-device package))
              (newline)
              ; Generic map aspect should go in here
              ; Port map aspect
              (vhdl:write-port-map package)
              (display ";")
              (newline)
              (newline)
            )
          )
        )
      )
    )
    packages)
  )
)

;;; THHE
;;; Write the signal assignment for the top-level ports
;;; Since I like to have the urefs as port names in the top
;;; level entity, I have to assign them to the correspinding nets as well

(define vhdl:write-signal-assignment-statements
  (lambda (packages)
    (begin
      (for-each (lambda (port-ass) (vhdl:write-in-signal-assignment port-ass))
        (vhdl:get-top-level-ports packages "IPAD"))
      (for-each (lambda (port-ass) (vhdl:write-out-signal-assignment port-ass))
        (vhdl:get-top-level-ports packages "OPAD"))
      (for-each (lambda (port-ass) (vhdl:write-inout-signal-assignment port-ass))
        (vhdl:get-top-level-ports packages "IOPAD"))
    )
  )
)
;;; THHE
;;; get a list of the top-level ports (the urefs of the I/O-PADs)

(define vhdl:get-top-level-ports
  (lambda (package-list pad-type)
    (cond ((null? package-list) '())
          ((string=? (get-device (car package-list)) pad-type)
           (cons (cons (car package-list)
                       (cdar (get-pins-nets (car package-list))) )
                 (vhdl:get-top-level-ports (cdr package-list ) pad-type )))
           (else (vhdl:get-top-level-ports (cdr package-list ) pad-type ))

    )
  )
)

;;;THHE
(define vhdl:write-in-signal-assignment
  (lambda (port-assignment)
    (begin
      (display (cdr port-assignment))
      (display " <= ")
      (display (car port-assignment))
      (display ";")
      (newline)
    )
  )
)

;;;THHE
(define vhdl:write-out-signal-assignment
  (lambda (port-assignment)
    (begin
      (display (car port-assignment))
      (display " <= ")
      (display (cdr port-assignment))
      (display ";")
      (newline)
    )
  )
)


;;;THHE
(define vhdl:write-inout-signal-assignment
  (lambda (port-assignment)
    (begin
      (vhdl:write-in-signal-assignment port-assignment)
      (vhdl:write-out-signal-assignment port-assignment)
    )
  )
)

;;; Port map aspect
;;;
;;; According to IEEE 1076-1993 5.6.1.2:
;;;
;;; port_map_aspect := PORT MAP ( port_association_list )
;;;
;;; According to IEEE 1076-1993 4.3.2.2:
;;;
;;; association_list :=
;;;    association_element { , association_element }

(define vhdl:write-port-map
  (lambda (package)
    (begin
      (let ((pin-list (get-pins-nets package)))
        (if (not (null? pin-list))
            (begin
              (display "    PORT MAP (")
              (newline)
              (display "        ")
              (vhdl:write-association-element (car pin-list))
              (for-each (lambda (pin)
                          (display ",")
                          (newline)
                          (display "        ")
                          (vhdl:write-association-element pin))
                        (cdr pin-list))
              (display ")")
            )
        )
      )

    )
  )
)

;;; Association element
;;;
;;; According to IEEE 1076-1993 4.3.2.2:
;;;
;;; association_element :=
;;;  [ formal_part => ] actual_part
;;;
;;; formal_part :=
;;;    formal_designator
;;;  | function_name ( formal_designator )
;;;  | type_mark ( formal_designator )
;;;
;;; formal_designator :=
;;;    generic_name
;;;  | port_name
;;;  | parameter_name
;;;
;;; actual_part :=
;;;    actual_designator
;;;  | function_name ( actual_designator )
;;;  | type_mark ( actual_designator )
;;;
;;; actual_designator :=
;;;    expression
;;;  | signal_name
;;;  | variable_name
;;;  | file_name
;;;  | OPEN
;;;
;;; Implementation note:
;;;    In the association element one may have a formal part or relly on
;;;    positional association. The later is doomed out as bad VHDL practice
;;;    and thus will the formal part allways be present.
;;;
;;;    The formal part will not support either the function name or type mark
;;;    based forms, thus only the formal designator form is supported.
;;;
;;;    Of the formal designator forms will generic name and port name be used
;;;    as appropriate (this currently means that only port name will be used).
;;;
;;;    The actual part will not support either the function name or type mark
;;;    based forms, thus only the actual designator form is supported.

(define vhdl:write-association-element
  (lambda (pin)
    (begin
      (display (car pin))
      (display " => ")
      (if (strncmp? "unconnected_pin" (cdr pin) 15)
          (display "OPEN")
          (display (cdr pin))))))

;;; Secondary unit
;;;
;;; According to IEEE 1076-1993 11.1:
;;;
;;; secondary_unit :=
;;;    architecture_body
;;;  | package_body
;;;
;;; Implementation note:
;;;    Since we are not likely to create packages in gEDA in the near future
;;;    we will only support the architecture body.
;;;
;;; According to IEEE 1076-1993 1.2:
;;;
;;; architecture_body :=
;;;    ARCHITECTURE identifier OF entity_name IS
;;;       architecture_declarative_part
;;;    BEGIN
;;;       architecture_statement_part
;;;    END [ ARCHITECTURE ] [ architecture_simple_name ] ;
;;;
;;; Implementation note:
;;;    The identifier will identify one of many architectures for an entity.
;;;    Since we generate only an netlist architecture we will lock this to be
;;;    "netlist" for the time being. Just as with the entity declaration we
;;;    will use good VHDL-93 custom to add the architecture keyword as well
;;;    as the architecture simple name to the trailer to keep compilers happy.

(define vhdl:write-secondary-unit
  (lambda (module-name)
    (display "-- Secondary unit")
    (newline)
    (display "ARCHITECTURE netlist OF ")
    (display module-name)
    (display " IS")
    (newline)
    ; architecture_declarative_part
    (vhdl:write-architecture-declarative-part)
    (display "BEGIN")
    (newline)
    ; architecture_statement_part
    (vhdl:write-architecture-statement-part packages)
    (display "END netlist;")
    (newline)
  )
)

;;; Top level function
;;; Write structural VHDL representation of the schematic
;;;
(define (vhdl output-filename)
  (set-current-output-port (output-port output-filename))
  (let ((module-name (gnetlist:get-toplevel-attribute "module-name"))
        (port-list (vhdl:get-top-port-list)))
    (begin
      (display "-- Structural VHDL generated by gnetlist")
      (newline)
      (vhdl:write-context-clause)
      (vhdl:write-primary-unit module-name port-list)
      (newline)
      (vhdl:write-secondary-unit module-name)
    )
  )
  (close-output-port (current-output-port))
)

;;; Context clause
;;;
;;; According to IEEE 1076-1993 11.3:
;;;
;;; context_clause := { context_item }
;;; context_item := library_clause | use_clause
;;;
;;; Implementation note:
;;;    Both library and use clauses will be generated, eventually...
;;;    What is missing is the information from gEDA itself, i think.

(define (vhdl:write-context-clause)
  (display "-- Context clause")
  (newline)
  (display "library IEEE;")
  (newline)
  (display "use IEEE.Std_Logic_1164.all;")
  (newline)
)
