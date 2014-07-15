/*
 *
 * Wiley E. Hill
 *
 */
#pragma once
#ifndef ASCII_H
#define ASCII_H

/* ASCII control codes */
#define ASCII_NUL       0x0      /* Null char */
#define ASCII_SOH       0x1      /* Start of Heading */
#define ASCII_STX       0x2      /* Start of Text */
#define ASCII_ETX       0x3      /* End of Text */
#define ASCII_EOT       0x4      /* End of Transmission */
#define ASCII_ENQ       0x5      /* Enquiry */
#define ASCII_ACK       0x6      /* Acknowledgment */
#define ASCII_BEL       0x7      /* Bell */
#define ASCII_BS        0x8      /* Back Space */
#define ASCII_HT        0x9      /* Horizontal Tab */
#define ASCII_LF        0x0A     /* Line Feed */
#define ASCII_VT        0x0B     /* Vertical Tab */
#define ASCII_NP        0x0C     /* Form Feed */
#define ASCII_CR        0x0D     /* Carriage Return */
#define ASCII_SO        0x0E     /* Shift Out / X_On */
#define ASCII_SI        0x0F     /* Shift In / X_Off */
#define ASCII_DLE       0x10     /* Data Line Escape */
#define ASCII_DC1       0x11     /* Device Control 1 (oft. XON) */
#define ASCII_DC2       0x12     /* Device Control 2 */
#define ASCII_DC3       0x13     /* Device Control 3 (oft. XOFF) */
#define ASCII_DC4       0x14     /* Device Control 4 */
#define ASCII_NAK       0x15     /* Negative Acknowledgement */
#define ASCII_SYN       0x16     /* Synchronous Idle */
#define ASCII_ETB       0x17     /* End of Transmit Block */
#define ASCII_CAN       0x18     /* Cancel */
#define ASCII_EM        0x19     /* End of Medium */
#define ASCII_SUB       0x1A     /* Substitute */
#define ASCII_ESC       0x1B     /* Escape*/
#define ASCII_FS        0x1C     /* File Separator */
#define ASCII_GS        0x1D     /* Group Separator */
#define ASCII_RS        0x1E     /* Record Separator*/
#define ASCII_US        0x1F     /* Unit Separator*/
#define ASCII_SP        0x20     /* Space */
#define ASCII_DEL       0x7F     /* Delete */

#define SPACE           0x20     /* Space */
#define ASCII_EXC       0x21     /* ! exclamation mark */
#define ASCII_QUO       0x22     /* " quotation mark  */
#define ASCII_PS        0x23     /* # number sign */
#define ASCII_DS        0x24     /* $ dollar sign */
#define ASCII_REC       0x25     /* % percent sign */
#define ASCII_AMP       0x26     /* & ampersand */
#define ASCII_APO       0x27     /* ' apostrophe_quote */
#define ASCII_OP        0x28     /* ( opening parenthesis */
#define ASCII_CP        0x29     /* ) closing parenthesis */
#define ASCII_AST       0x2A            /* * asterisk  */

#define ASCII_PLUS      0x2B     /* + Plus sign  */
#define ASCII_MINUS     0x2D     /* - Minus sign */
#define ASCII_PERIOD    0x2E     /* . Period  */
#define ASCII_FSLASH    0x2F     /* / Forward Slash  */

#define CARRIAGE_RETURN  ASCII_CR
#define SYNCHRONOUS_IDLE ASCII_SYN
#define SUBSTITUTE       ASCII_SUB
#define LINE_FEED        ASCII_LF
#define DATA_LINE_ESCAPE ASCII_DLE
#define DATA_SHIFT_IN    ASCII_SI

#define ASCII_SPACE                 SPACE
#define ASCII_EXCLAMATION_MARK      0x21
#define ASCII_QUOTATION_MARK        0x22
#define ASCII_NUMBER_SIGN           0x23
#define ASCII_DOLLAR_SIGN           0x24
#define ASCII_PERCENT_SIGN          0x25
#define ASCII_AMPERSAND             0x26
#define ASCII_APOSTROPHE            0x27
#define ASCII_LEFT_PARENTHESIS      0x28
#define ASCII_RIGHT_PARENTHESIS     0x29
#define ASCII_ASTERISK              0x2A
#define ASCII_PLUS_SIGN             0x2B
#define ASCII_COMMA                 0x2C
#define ASCII_HYPHEN_MINUS          0x2D
#define ASCII_FULL_STOP             0x2E
#define ASCII_SOLIDUS               0x2F
#define ASCII_DIGIT_ZERO            0x30
#define ASCII_DIGIT_ONE             0x31
#define ASCII_DIGIT_TWO             0x32
#define ASCII_DIGIT_THREE           0x33
#define ASCII_DIGIT_FOUR            0x34
#define ASCII_DIGIT_FIVE            0x35
#define ASCII_DIGIT_SIX             0x36
#define ASCII_DIGIT_SEVEN           0x37
#define ASCII_DIGIT_EIGHT           0x38
#define ASCII_DIGIT_NINE            0x39
#define ASCII_COLON                 0x3A
#define ASCII_SEMICOLON             0x3B
#define ASCII_LESS_THAN_SIGN        0x3C
#define ASCII_EQUAL_SIGN            0x3D
#define ASCII_GREATER_THAN_SIGN     0x3E
#define ASCII_QUESTION_MARK         0x3F
#define ASCII_COMMERCIAL_AT         0x40

#define ASCII_CAPITAL_LETTER_A              0x41
#define ASCII_CAPITAL_LETTER_B              0x42
#define ASCII_CAPITAL_LETTER_C              0x43
#define ASCII_CAPITAL_LETTER_D              0x44
#define ASCII_CAPITAL_LETTER_E              0x45
#define ASCII_CAPITAL_LETTER_F              0x46
#define ASCII_CAPITAL_LETTER_G              0x47
#define ASCII_CAPITAL_LETTER_H              0x48
#define ASCII_CAPITAL_LETTER_I              0x49
#define ASCII_CAPITAL_LETTER_J              0x4A
#define ASCII_CAPITAL_LETTER_K              0x4B
#define ASCII_CAPITAL_LETTER_L              0x4C
#define ASCII_CAPITAL_LETTER_M              0x4D
#define ASCII_CAPITAL_LETTER_N              0x4E
#define ASCII_CAPITAL_LETTER_O              0x4F
#define ASCII_CAPITAL_LETTER_P              0x50
#define ASCII_CAPITAL_LETTER_Q              0x51
#define ASCII_CAPITAL_LETTER_R              0x52
#define ASCII_CAPITAL_LETTER_S              0x53
#define ASCII_CAPITAL_LETTER_T              0x54
#define ASCII_CAPITAL_LETTER_U              0x55
#define ASCII_CAPITAL_LETTER_V              0x56
#define ASCII_CAPITAL_LETTER_W              0x57
#define ASCII_CAPITAL_LETTER_X              0x58
#define ASCII_CAPITAL_LETTER_Y              0x59
#define ASCII_CAPITAL_LETTER_Z              0x5A
#define ASCII_LEFT_SQUARE_BRACKET           0x5B
#define ASCII_REVERSE_SOLIDUS               0x5C
#define ASCII_RIGHT_SQUARE_BRACKET          0x5D
#define ASCII_CIRCUMFLEX_ACCENT             0x5E
#define ASCII_LOW_LINE                      0x5F
#define ASCII_GRAVE_ACCENT                  0x60
#define ASCII_SMALL_LETTER_A                0x61
#define ASCII_SMALL_LETTER_B                0x62
#define ASCII_SMALL_LETTER_C                0x63
#define ASCII_SMALL_LETTER_D                0x64
#define ASCII_SMALL_LETTER_E                0x65
#define ASCII_SMALL_LETTER_F                0x66
#define ASCII_SMALL_LETTER_G                0x67
#define ASCII_SMALL_LETTER_H                0x68
#define ASCII_SMALL_LETTER_I                0x69
#define ASCII_SMALL_LETTER_J                0x6A
#define ASCII_SMALL_LETTER_K                0x6B
#define ASCII_SMALL_LETTER_L                0x6C
#define ASCII_SMALL_LETTER_M                0x6D
#define ASCII_SMALL_LETTER_N                0x6E
#define ASCII_SMALL_LETTER_O                0x6F
#define ASCII_SMALL_LETTER_P                0x70
#define ASCII_SMALL_LETTER_Q                0x71
#define ASCII_SMALL_LETTER_R                0x72
#define ASCII_SMALL_LETTER_S                0x73
#define ASCII_SMALL_LETTER_T                0x74
#define ASCII_SMALL_LETTER_U                0x75
#define ASCII_SMALL_LETTER_V                0x76
#define ASCII_SMALL_LETTER_W                0x77
#define ASCII_SMALL_LETTER_X                0x78
#define ASCII_SMALL_LETTER_Y                0x79
#define ASCII_SMALL_LETTER_Z                0x7A
#define ASCII_LEFT_CURLY_BRACKET            0x7B
#define ASCII_VERTICAL_LINE                 0x7C
#define ASCII_RIGHT_CURLY_BRACKET           0x7D
#define ASCII_TILDE                         0x7E
#define ASCII_DELETE                        0x7F

#define FORWARD_SLASH        ASCII_SOLIDUS
#define BACKSLASH      ASCII_REVERSE_SOLIDUS
#endif


