; -*-Scheme-*-
;;;
;;; Add the default component libraries
;;;

(define geda-sym-path (build-path geda-data-path "sym"))

(component-groups '("connector" "diode" "passive" "linear" "logic" "emechanic"))

(map
     (lambda (dir)
         (if (list? dir)
            (component-library (build-path geda-sym-path (car dir)) (cadr dir))
            (component-library (build-path geda-sym-path dir))
         )
      )
      (reverse '(
; Standard
                  ("passive" "Standard/Passive")
                     ("passive/capacitor"       "Standard/Capacitors")
                     ("passive/electrolytic"    "Standard/Electrolytic")
                     ("passive/resistor"        "Standard/Resistors")
                     ("passive/rf"              "Standard/Radio Elements")
                     ("passive/transformer"     "Standard/Transformers")
                     ("passive/crystal"         "Standard/Crystals")
                     ("passive/inductor"        "Standard/Inductors")
                     ("passive/lamp"            "Standard/Lamps")
                     ("passive/protection"      "Standard/Protection")
                     ("passive/audio"           "Standard/Audio")
                  ("connector" "Standard/Connectors")
                     ("connector/barrel"        "Standard/Cylindrical Barrel")
                     ("connector/coaxial"       "Standard/Coaxial")
                     ("connector/DB"            "Standard/D-subminiature")
                     ("connector/DIN"           "Standard/DIN")
                     ("connector/DIN-mini"      "Standard/Mini DIN")
                     ("connector/generic"       "Standard/Generic")
                     ("connector/header"        "Standard/Headers")
                     ("connector/jtag"          "Standard/JTAG")
                     ("connector/jumper"        "Standard/Jumper")
                     ("connector/phone"         "Standard/Phone")
                     ("connector/plug"          "Standard/Plugs")
                     ("connector/rj"            "Standard/RJ Registered Jack")
                     ("connector/rs"            "Standard/RS Radio Sector")
                     ("connector/socket"        "Standard/Sockets")
                     ("connector/terminal"      "Standard/Terminals")
                     ("connector/usb"           "Standard/Universal Serial Bus")
                  ("diode" "Standard/Diodes")
                     ("diode/segments"          "Standard/Segments")
                     ("diode/full-bridge"       "Standard/Full Bridge")
                     ("diode/half-bridge"       "Standard/Half Bridge")
                     ("diode/led"               "Standard/LED")
                     ("diode/zener"             "Standard/Zener-generic")
                     ("diode/zener2"            "Standard/Zener Diodes")
                  ("linear" "Standard/Linear")
                     ("linear/comparators"      "Standard/Comparators")
                     ("linear/opamp"            "Standard/Operational Amplifiers")
                     ("linear/amplifiers"       "Standard/Amplifiers")
                     ("linear/led_drivers"      "Standard/LED Drivers")
                     ("linear/mcontrol"         "Standard/Motor Controllers")
                     ("linear/regulators"       "Standard/Regulators")
                     ("linear/power"            "Standard/Power Control Systems")
                     ("linear/special"          "Standard/Special Purpose")
                     ("linear/temperature"      "Standard/Temperature")
                     ("linear/timers"           "Standard/Timers")

; Logic
                  ("logic" "Standard/Logic")
                     ("logic/74"                "74-series logic")
                     ("logic/4000"              "4000-series logic")
                     ("logic/ecl"               "ECL logic")
                     ("logic/logic-symbols"     "Logic Symbols")
                     ("logic/pla"               "Programmable Logic Arrays")
                  ("network"            "Standard/Network")
                  ("serial"             "Standard/Serial Devices")
                  ("source"             "Standard/Source")
                  ("io"                 "Standard/Input-Output")
                  ("thyristor"          "Standard/Thyristors")
                  ("transistor"         "Standard/Transistors")
                  ("IEC417"             "Standard/IEC 60417")

; Optotronics
                     ("opto"    "Optocouplers")
                     ("display" "Displays")

; Manufacturers
                     ("allegro"         "Manufacturers/Allegro Microsystems")
                     ("altera"          "Manufacturers/Altera")
                  ("analogdevices"   "Manufacturers/Analog Devices")
                     ("analogdevices/supervisor"       "Manufacturers/Microprocessor Supervisors")
                     ("amphenol"        "Manufacturers/Amphenol")
                     ("apex"            "Manufacturers/Apex Microtechnology")
                     ("atmel"           "Manufacturers/Atmel")
                     ("cirrus"          "Manufacturers/Cirrus Logic")
                     ("dallas"          "Manufacturers/Dallas Semiconductor")
                     ("dec"             "Manufacturers/DEC")
                     ("fairchild"       "Manufacturers/Fairchild Semiconductor")
                     ("idt"             "Manufacturers/IDT")
                     ("intel"           "Manufacturers/Intel")
                     ("irf"             "Manufacturers/International Rectifier")
                     ("lattice"         "Manufacturers/Lattice Semiconductor")
                     ("lineartech"      "Manufacturers/Linear Technology")
                     ("maxim"           "Manufacturers/Maxim")
                     ("micro"           "Manufacturers/Microchip")
                     ("minicircuits"    "Manufacturers/Mini-Circuits")
                     ("national"        "Manufacturers/National Semiconductor")
                  ("panasonic" "Manufacturers/Panasonic")
                     ("panasonic/fc-electrolytic"      "Manufacturers/FC Series Capacitors")
                     ("panasonic/hd-electrolytic"      "Manufacturers/HD Series Capacitors")
                     ("panasonic/resistors"            "Manufacturers/Resistors")
                     ("philips"         "Manufacturers/Philips Electronics")
                     ("st"              "Manufacturers/ST Microelectronics")
                     ("ti"              "Manufacturers/Texas Instruments")
                  ("vishay" "Manufacturers/Vishay")
                     ("vishay/nmosfet"                 "Manufacturers/N-Channel MOSFET")
                     ("vishay/pmosfet"                 "Manufacturers/P-Channel MOSFET")
                     ("xilinx"          "Manufacturers/Xilinx")

; Electro-Mechanical
                  ("emechanic" "Electro-Mechanical")
                     ("emechanic/motor"         "Standard/Motors")
                     ("emechanic/meter"         "Standard/Meters")
                     ("emechanic/relay"         "Standard/Relays")
                     ("emechanic/switch"        "Standard/Switches")

; Misc
                     ("memory"          "Memory devices")
                     ("micro"           "Microcontrollers")
                     ("tube"            "Vacuum tubes")
                     ("power"           "Power Systems")
                     ("misc"            "Misc. unsorted symbols")
                     ("titleblock"      "Titleblocks/titleblock")

; Simulation
                  ("simulation" "Simulation/Simulation")
                     ("simulation/cascade"      "Simulation/Cascade elements")
                     ("simulation/spice"        "Simulation/SPICE elements")
                     ("simulation/switcap"      "Simulation/SWITCAP elements")
                     ("simulation/verilog"      "Simulation/Verilog")
                     ("simulation/vhdl"         "Simulation/VHDL")
           )
      )
)

(if (access? "/usr/local/share/gEDA/sym" R_OK)
  (begin
    (component-library-search "/usr/local/share/gEDA/sym" "Local/Local")
    ;;(component-library "/usr/local/share/gEDA/sym" "local")
) )








