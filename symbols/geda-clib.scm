; -*-Scheme-*-
;;;
;;; Add default component libraries
;;;
;;; Abstract: This file controls which directories will be used as sources
;;; for symbols. Collectively the directories referenced here are known as
;;; "Standard Library". Sources, aka directories, and symbols can be loaded
;;; in virtually any SCM file, but these do not, or should not reference
;;; folders subordinate to the standard sym folders.
;;;
;;; This file implements methods to support organizing and filtering symbol
;;; libraries. These method can determine how or if symbols are displayed in
;;; gschem and are detailed in supporting documentation. Details are also
;;; explained in comments that follow.
;;;
;;; The "Standard" libraries are all subordinate to the location set by:
(define geda-sym-path (build-path geda-data-path "sym"))

;;; All folders in the list below will be added as sources. Each list entry,
;;; is also a list in the following format:
;;;
;;;                      ( string1 string2 [number] )
;;;
;;; where
;;;              string1 is the containing folder under "sym"
;;;              string2 is the [Category/]Text to Display
;;;              number is an optional style code
;;;
;;; When string1 is NOT directly under "sym" then string1 takes the form
;;;
;;;              "dir/sub-dir"
;;;
;;; The sub-dir itself is a source, and a member of the group named "dir".
;;; When string2 does not contain a Category, the source will be displayed
;;; with the "Standard" Category. Sources can be displayed directly under
;;; the categories "Standard", "Manufacturers" or "Simulation" or under
;;; the group name. Sub-groups that are a member of:
;;;
(component-groups '("connector" "diode" "passive" "linear" "logic" "emechanic"))

;;; can be toggled between the "Standard" category and the group folder.
;;; Sub-groups not containing a least one file, ie a file with the .sym
;;; extension, are not displayed.
;;;
;;; If the list entry has a number then that source will only be loaded if
;;; the number matches:
(define component-style 3)

;;; unless the number is 0, which are always loaded if
(define enable-style-zero 1)

;;; The dash zero sub-groups contain template symbols. This allows for symbol
;;; "themes". If a sub-groups contains generic symbols
(map
     (lambda (dir)
         (if (list? dir)
            (if (and (= (length dir) 3) (> component-style -1))
                (if (or (= (caddr dir) component-style)
                        (and (= (caddr dir) 0) enable-style-zero ) )
                    (component-library (build-path geda-sym-path (car dir)) (cadr dir))
                )
                (component-library (build-path geda-sym-path (car dir)) (cadr dir))
            )
            (component-library (build-path geda-sym-path dir))
         )
      )
      (reverse '(
; Standard
                  ("passive" "Standard/Passive")
                     ("passive/capacitor"       "Standard/Capacitors")
                     ("passive/electrolytic"    "Standard/Electrolytic")
                     ("passive/resistor"        "Standard/Resistors")
                     ("passive/inductor"        "Standard/Inductors")
                     ("passive/rf"              "Standard/Radio Elements")
                     ("passive/audio"           "Standard/Audio")
                     ("passive/transformer"     "Standard/Transformers")
                     ("passive/crystal"         "Standard/Crystals")
                     ("passive/resonator"       "Standard/Resonator")
                     ("passive/filter"          "Standard/Filters")
                     ("passive/lamp"            "Standard/Lamps")
                     ("passive/protection"      "Standard/Protection")
                     ("passive/thermistor"      "Standard/Thermistors")
                     ("passive/varistor"        "Standard/Varistor")
                     ("passive/magnetic"        "Standard/Magnetic")

                  ("connector" "Standard/Connectors")
                     ("connector/barrel"        "Standard/Cylindrical Barrel")
                     ("connector/coaxial"       "Standard/Coaxial")
                     ("connector/DB"            "Standard/D-subminiature")
                     ("connector/DIN"           "Standard/DIN")
                     ("connector/DIN-mini"      "Standard/Mini DIN")
                     ("connector/flex"          "Standard/Flex")
                     ("connector/generic"       "Standard/Generic")
                     ("connector/header"        "Standard/Headers")
                     ("connector/jtag"          "Standard/JTAG")
                     ("connector/jumper"        "Standard/Jumper")
                     ("connector/phone"         "Standard/Phone")
                     ("connector/plug"          "Standard/Plugs")
                     ("connector/rj"            "Standard/RJ Registered Jack")
                     ("connector/rs"            "Standard/RS Radio Sector")
                     ("connector/socket"        "Standard/Sockets")
                     ("connector/special"       "Standard/Special")
                     ("connector/terminal"      "Standard/Terminals")
                     ("connector/usb"           "Standard/Universal Serial Bus")

                  ("diode" "Standard/Diodes")
                     ("diode/schottky"          "Standard/Schottky")
                     ("diode/full-bridge"       "Standard/Full Bridge")
                     ("diode/half-bridge"       "Standard/Half Bridge")
                     ("diode/led"               "Standard/LED")
                     ("diode/transil"           "Standard/Transils")
                     ("diode/zener"             "Standard/Zener-generic")
                     ("diode/zener52"           "Standard/Zeners 52 Series")
                     ("diode/zener53"           "Standard/Zeners 53 Series")
                     ("diode/segments"          "Standard/Segments")

                  ("linear" "Standard/Linear")
                     ("linear/amplifiers"       "Standard/Amplifiers")
                     ("linear/audio"            "Standard/Audio")
                     ("linear/video"            "Standard/Video")
                     ("linear/comparators"      "Standard/Comparators")
                     ("linear/opamps"           "Standard/Operational Amplifiers")
                     ("linear/led_drivers"      "Standard/LED Drivers")
                     ("linear/mcontrol"         "Standard/Motor Control")
                     ("linear/mosdrive"         "Standard/MOS Gate Drivers")
                     ("linear/motion"           "Standard/Motion Sensors")
                     ("linear/reference"        "Standard/Linear Reference")
                     ("linear/regulator"        "Standard/Linear Regulators")
                     ("linear/switchreg"        "Standard/Switching Regulators")
                     ("linear/powmon"           "Standard/Power Monitors")
                     ("linear/special"          "Standard/Special Purpose")
                     ("linear/temperature"      "Standard/Temperature")
                     ("linear/timers"           "Standard/Timers")
                     ("linear/telephone"        "Standard/Telephonography")
; Logic
                  ("logic" "Standard/Logic")
                     ("logic/74"                "74-series logic")
                     ("logic/74A"               "74A-series logic")
                     ("logic/74H"               "74H-series logic")
                     ("logic/74F"               "74F-series logic")
                     ("logic/74L"               "74L-series logic")
                     ("logic/4000"              "4000-series logic")
                     ("logic/SN"                "SN-series Ti logic")
                     ("logic/ecl"               "Emitter Coupled logic")
                     ("logic/logic-symbols"     "Logic Symbols")
                     ("logic/pla"               "Programmable Logic Arrays")
                     ("logic/power"             "Power for Logic Symbols")
                  ("network"            "Standard/Network")
                  ("serial"             "Standard/Serial Devices")
                  ("source"             "Standard/Source")
                  ("io"                 "Standard/Input-Output")
                  ("thyristor"          "Standard/Thyristors")
                  ("transistor"         "Standard/Transistors")
                     ("transistor/basic"        "Standard/Basic transistors")
                     ("transistor/npn-smt"      "Standard/NPN Surface Mount")
                     ("transistor/pnp-smt"      "Standard/PNP Surface Mount")
                     ("transistor/npn-thru"     "Standard/NPN Thru-Hole")
                     ("transistor/pnp-thru"     "Standard/PNP Thru-Hole")
                     ("transistor/npn-power"    "Standard/NPN High Power")
                     ("transistor/pnp-power"    "Standard/PNP High Power")
                     ("transistor/npn-medium"   "Standard/NPN Medium Power")
                     ("transistor/pnp-medium"   "Standard/PNP Medium Power")
                     ("transistor/npn-bjt"      "Standard/NPN Bipolar Generic")
                     ("transistor/pnp-bjt"      "Standard/PNP Bipolar Generic")

                     ("transistor/nmosfet-0"    "Standard/Base N-Chan MOSFET" 0)
                     ("transistor/pmosfet-0"    "Standard/Base P-Chan MOSFET" 0)

                     ("transistor/nmosfet-e"    "Standard/N-Chan Enh MOSFET")
                     ("transistor/pmosfet-e"    "Standard/P-Chan Enh MOSFET")

                     ("transistor/nmosfet-d"    "Standard/N-Chan Dep MOSFET")
                     ("transistor/pmosfet-d"    "Standard/P-Chan Dep MOSFET")

                     ("transistor/igbt"          "Standard/Insulated Gate BiPolar")

                     ("transistor/njfet-0"      "Standard/N-Channel FET" 0)
                     ("transistor/njfet-thru"   "Standard/N-FET Thru-Hole")
                     ("transistor/njfet-diff"   "Standard/N-FET Differential Pair")
                     ("transistor/pjfet-0"      "Standard/P-Channel FET" 0)
                     ("transistor/pjfet-thru"   "Standard/P-FET Thru-Hole")

                     ("transistor/darl-bjt"     "Standard/Darlingtons")
                     ("transistor/darl-npn"     "Standard/NPN Darlington")
                     ("transistor/darl-pnp"     "Standard/PNP Darlington")
                     ("transistor/sziklai-bjt"  "Standard/Sziklai Pairs")
                     ("transistor/other"        "Standard/Other misc Transistors")

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
                  ("arduino"         "Manufacturers/Arduino")
                  ("atmel"           "Manufacturers/Atmel")
                  ("cirrus"          "Manufacturers/Cirrus Logic")
                  ("cypress"         "Manufacturers/Cypress")
                  ("dallas"          "Manufacturers/Dallas Semiconductor")
                  ("dec"             "Manufacturers/DEC")
                  ("epson"           "Manufacturers/Epson")
                     ("epson/crystal"                  "Manufacturers/Crystals")
                  ("exar"            "Manufacturers/Exar")
                  ("fairchild"       "Manufacturers/Fairchild Semiconductor")
                  ("idt"             "Manufacturers/IDT")
                  ("intel"           "Manufacturers/Intel")
                  ("irf"             "Manufacturers/International Rectifier")
                  ("ixys"            "Manufacturers/IXYS")
                  ("lattice"         "Manufacturers/Lattice Semiconductor")
                  ("lineartech"      "Manufacturers/Linear Technology")
                     ("lineartech/ldo"                 "Low-Dropout Regulators")
                     ("lineartech/opamps"              "Manufacturers/Operational Amplifiers")
                     ("lineartech/switchreg"           "Manufacturers/Switching Regulators")
                  ("maxim"           "Manufacturers/Maxim")
                  ("microchip"       "Manufacturers/Microchip")
                     ("microchip/battery"              "Manufacturers/Battery")
                     ("microchip/memory"               "Manufacturers/Memory devices")
                     ("microchip/motor"                "Manufacturers/Motor Control")
                     ("microchip/switchreg"            "Manufacturers/Switching Regulators")
                  ("microsemi"       "Manufacturers/Microsemi")
                  ("minicircuits"    "Manufacturers/Mini-Circuits")
                  ("mitsubishi"      "Manufacturers/Mitsubishi Electric")
                     ("mitsubishi/logic"               "Manufacturers/Logic")
                  ("moto"            "Manufacturers/Motorola")
                  ("murata"          "Manufacturers/Murata")
                     ("murata/capacitor"               "Manufacturers/Capacitors")
                     ("murata/filter"                  "Manufacturers/Filters")
                  ("national"        "Manufacturers/National Semiconductor")
                  ("onsemi"          "Manufacturers/ON Semiconductor")
                     ("onsemi/diode"                   "Manufacturers/Diodes")
                     ("onsemi/ldo"                     "Manufacturers/Low-Dropout Regulators")
                     ("onsemi/regulator"               "Manufacturers/Linear Regulators")
                     ("onsemi/schottky"                "Manufacturers/Schottky")
                  ("panasonic"       "Manufacturers/Panasonic")
                     ("panasonic/fc-electrolytic"      "Manufacturers/FC Series Capacitors")
                     ("panasonic/hd-electrolytic"      "Manufacturers/HD Series Capacitors")
                     ("panasonic/SEPC-electrolytic"    "Manufacturers/SEPC Series Capacitors")
                     ("panasonic/resistors"            "Manufacturers/Resistors")
                     ("panasonic/ERA2-resistors"       "Manufacturers/ERA2 Resistors")
                     ("panasonic/ERA3-resistors"       "Manufacturers/ERA3 Resistors")
                     ("panasonic/ERA6-resistors"       "Manufacturers/ERA6 Resistors")
                     ("panasonic/ERA8-resistors"       "Manufacturers/ERA8 Resistors")
                  ("philips"         "Manufacturers/Philips Electronics")
                  ("st"              "Manufacturers/ST Microelectronics")
                     ("st/ldo"                         "Manufacturers/Low-Dropout Regulators")
                  ("ti"              "Manufacturers/Texas Instruments")
                     ("ti/battery"                     "Manufacturers/Battery")
                     ("ti/isolate"                     "Manufacturers/Isolators")
                     ("ti/ldo"                         "Manufacturers/Low-Dropout Regulators")
                     ("ti/micro"                       "Manufacturers/Microcontrollers")
                     ("ti/motor"                       "Manufacturers/Motor Control")
                     ("ti/opamps"                      "Manufacturers/Operational Amplifiers")
                     ("ti/switchreg"                   "Manufacturers/Switching Regulators")
                  ("toshiba" "Manufacturers/Toshiba")
                     ("toshiba/converter"              "Manufacturers/DC-DC Converters")
                     ("toshiba/npn-power-1"            "Manufacturers/NPN Bipolar Power" 1)
                     ("toshiba/npn-power-2"            "Manufacturers/NPN Bipolar Power" 2)
                     ("toshiba/npn-power-3"            "Manufacturers/NPN Bipolar Power" 3)
                     ("toshiba/npn-power-4"            "Manufacturers/NPN Bipolar Power" 4)
                     ("toshiba/npn-small-1"            "Manufacturers/NPN Bipolar"       1)
                     ("toshiba/npn-small-2"            "Manufacturers/NPN Bipolar"       2)
                     ("toshiba/npn-small-3"            "Manufacturers/NPN Bipolar"       3)
                     ("toshiba/npn-small-4"            "Manufacturers/NPN Bipolar"       4)
                     ("toshiba/pnp-power-1"            "Manufacturers/PNP Bipolar Power" 1)
                     ("toshiba/pnp-power-2"            "Manufacturers/PNP Bipolar Power" 2)
                     ("toshiba/pnp-power-3"            "Manufacturers/PNP Bipolar Power" 3)
                     ("toshiba/pnp-power-4"            "Manufacturers/PNP Bipolar Power" 4)
                     ("toshiba/pnp-small-1"            "Manufacturers/PNP Bipolar"       1)
                     ("toshiba/pnp-small-2"            "Manufacturers/PNP Bipolar"       2)
                     ("toshiba/pnp-small-3"            "Manufacturers/PNP Bipolar"       3)
                     ("toshiba/pnp-small-4"            "Manufacturers/PNP Bipolar"       4)
                  ("vishay" "Manufacturers/Vishay")
                     ("vishay/diode"                   "Manufacturers/Diodes")
                     ("vishay/zener"                   "Manufacturers/Zener Diodes")
                     ("vishay/nmosfet"                 "Manufacturers/N-Channel MOSFET")
                     ("vishay/pmosfet"                 "Manufacturers/P-Channel MOSFET")
                     ("vishay/dual_pmos"               "Manufacturers/Dual P-MOSFET")
                     ("vishay/dual_nmos"               "Manufacturers/Dual N-MOSFET")
                     ("vishay/CCF02-resistors"         "Manufacturers/CCF02 Resistors")
                     ("vishay/CCF50-resistors"         "Manufacturers/CCF50 Resistors")
                     ("vishay/CCF55-resistors"         "Manufacturers/CCF55 Resistors")
                     ("vishay/CCF60-resistors"         "Manufacturers/CCF60 Resistors")
                     ("vishay/CPF-resistors"           "Manufacturers/CPF Resistors")

                  ("xilinx"                    "Manufacturers/Xilinx")

; Electro-Mechanical
                  ("emechanic" "Electro-Mechanical")
                     ("emechanic/breaker"       "Standard/Circuit Breaker")
                     ("emechanic/motor"         "Standard/Motors")
                     ("emechanic/meter"         "Standard/Meters")
                     ("emechanic/relay"         "Standard/Relays")
                     ("emechanic/switch"        "Standard/Switches")
                     ("emechanic/accelerometer" "Standard/Accelerometers")

; Misc
                  ("memory"          "Memory devices")
                  ("micro"           "Microcontrollers")
                     ("micro/arm"    "Standard/ARM Processors")
                     ("micro/MC68"   "Standard/68 Series")
                     ("micro/MSP430" "Standard/MSP430 Family")
                     ("micro/PIC"    "Standard/PIC Microcontrollers")
                  ("tube"            "Vacuum tubes")
                  ("graphic"         "Graphical symbols")
                     ("graphic/flow-chart" "Standard/Flow Chart")
                  ("asic"            "ASIC Components")
                  ("misc"            "Misc. unsorted symbols")
                  ("titleblock"      "Standard/titleblock")

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

;;; conceptually, one could also have Local themes
(if (access? "/usr/local/gEDA/sym" R_OK)
  (begin
    (component-library-search "/usr/local/gEDA/sym" "Local/Local")
    ;;(component-library "/usr/local/gEDA/sym" "local")
) )

;;; Why not auto-load-dot-sym if ./sym folder exist?
