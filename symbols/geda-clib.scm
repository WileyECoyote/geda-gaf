; -*-Scheme-*-
;;;
;;; Add the default component libraries
;;;

(define geda-sym-path (build-path geda-data-path "sym"))

(define ComponentCatagories '("Standard" "Logic ""Optotronics" "Manufacturers" "Electro-Mechanical" "Misc" "Simulation"))

(map
     (lambda (dir)
         (if (list? dir)
            (component-library (build-path geda-sym-path (car dir)) (cadr dir))
            (component-library (build-path geda-sym-path dir))
         )
      )
      (reverse '(    
   ; Standard
                     ("passive" "Passive (standard)")
                     ("connector" "Connectors (standard)")
                     ("diode" "Diodes (standard)")
                     ("io" "Input/Output (standard)")
                     ("linear" "Linear (standard)")
                     ("logic" "Logic Symbols (standard)")
                     ("network" "Network (standard)")
                     ("opamp" "Operational Amplifiers")
                     ("serial" "Serial Devices")
                     ("source" "source (standard)")
                     ("rf" "Radio Elements (standard)")
                     ("thyristor" "Thyristors (standard)")
                     ("transistor" "Transistors (standard)")
                     ("IEC417" "IEC 60417")

   ; Logic
                     ("74" "74-series logic")
                     ("4000" "4000-series logic")
                     ("ecl" "ECL logic")

   ; Optotronics
                     ("opto" "Optocouplers (misc)")
                     ("display" "Displays")

   ; Manufacturers
                     ("allegro" "Allegro Microsystems")
                     ("altera" "Altera")
                     ("analogdevices" "Analog Devices")
                     ("amphenol" "Connectors (Amphenol)")
                     ("apex" "Apex Microtechnology")
                     ("atmel" "Atmel")
                     ("cirrus" "Cirrus Logic")
                     ("dallas" "Dallas Semiconductor")
                     ("dec" "DEC")
                     ("fairchild" "Fairchild Semiconductor")
                     ("idt" "IDT")
                     ("intel" "Intel")
                     ("irf" "International Rectifier")
                     ("lattice" "Lattice Semiconductor")
                     ("lineartech" "Linear Technology")
                     ("maxim" "Maxim/Dallas")
                     ("micro" "Microchip")
                     ("minicircuits" "Mini-Circuits")
                     ("national" "National Semiconductor")
                     ("panasonic" "Panasonic Electronics")
                     ("philips" "Philips Electronics")
                     ("st" "ST Microelectronics")
                     ("ti" "Texas Instruments")
                     ("vishay" "Vishay")
                     ("xilinx" "Xilinx")
 
   ; Electro-Mechanical
                     ("motor" "Motors")
                     ("meter" "Meters")
                     ("relay" "Relays (standard)")
                     ("switch" "Switches (standard)")

   ; Misc
                     ("bus" "PC104 bus")
                     ("memory" "Memory devices (misc)")
                     ("micro" "Microcontrollers (misc)")
                     ("tube" "Vacuum tubes (misc)")
                     ("rf" "RF elements (misc)")
                     ("pla" "Programmable Logic Arrays (misc)")
                     ("power" "Power Sysytems")
                     ("supervisor" "Microprocessor supervisors (misc)")
                     ("misc" "Misc. unsorted symbols")
                     ("misc_ic" "Misc. Integrated Circuits")
                     ("titleblock" "Titleblocks (standard)")
   ; Simulation
                     ("cascade" "Cascade simulation elements")
                     ("spice" "SPICE simulation elements")
                     ("switcap" "SWITCAP simulation elements")
                     ("verilog" "verilog")
                     ("vhdl" "vhdl")
           )
      )
)

(if (access? "/usr/local/share/gEDA/sym" R_OK)
  (begin
    (component-library-search "/usr/local/share/gEDA/sym" "Local shared")
    ;;(component-library "/usr/local/share/gEDA/sym" "local")
) ) 








