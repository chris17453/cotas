# NOCMA NOFD NOZERO OFF

## Summary
fieldname - fn/v - Required - The name of the field to be formatted. May be either a numeric
or alpha field.
receiving_field - fn/v - Optional - If you want to put the formatted numeric field in another
field this is the name of the A type receiving field. This only applies if the format
field is of type N.
neg_how - Optional - LTPCA - L - Leading negative sign, T - Trailing negative sign, P Parentheses, C - Trailing CR, A - Angle brackets. This is applicable only when the
format field is of type N.
picture - f/c/e - Required - This is used to create what is called a ‘slider field’. Slider fields are
useful when you have a field you want to display on the screen, and that field is too
wide to fit within the screen or current window. You can specify how many characters
to display by preceding that value with an ‘S’. For example:
‘S30’
will display 30 characters of the field at any time. When the user enters that field it
will start at the ‘beginning.’ As characters are entered the data will appear to ‘slide’
across the entry box. The user may press the LEFT and RIGHT arrows to go back
and forth in the field. Also the HOME key goes to the beginning of the field, and the
END key to the current ending character in the field. This allows you to enter large
fields without having to use the TASEdit program or an outside editor. This only
applies when the format field is of type A.
NOCMA - Optional - If this option is set the program will NOT include comma characters in
the resulting formatted field. The default is to include comma characters. This only
applies when the format field is of type N.

