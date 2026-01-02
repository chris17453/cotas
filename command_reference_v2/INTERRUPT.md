# INTERRUPT

## Summary
We recommend you use the following register list just as we list it below:
define reg_al,reg_ah,reg_bl,reg_bh,reg_cl,reg_ch,reg_dl,reg_dh type b
define reg_si,reg_di,reg_ds,reg_es type i
define carry,zero type l
You can name the registers anything you want; however, they must be in the order listed above. Using
the REDEFINE command you can create 16 bit values that overlay two 8 bit regs. For example:
define reg_dx type i
redefine reg_dx loc reg_dl
The following program achieves the same result as the GETDOSV program shown in the documentation
for the LOAD command. However, this program doesn’t need an outside assembler program to get the
results. Compare the differences:
;GETDOSV in TAS using the INT command.
;
;Returns DOS version
;
;define the register list
;
define reg_al,reg_ah,reg_bl,reg_bh,reg_cl,reg_ch,reg_dl,reg_dh type b
define reg_si,reg_di,reg_ds,reg_es type i
define carry,zero type l
;
;now do the DOS call
;
reg_ah=48
int 33 regs reg_al
&& int 33 is 21h
;
;the version is returned in reg_al and reg_ah
;
clrscr
? ‘DOS VERSION: ‘,trim(str(reg_al),’l’),’.’,trim(str(reg_ah),’l’) wait
;
;that’s all there is to it!

## Signature
```
PROGRAM EDITOR
```

## Details
System -> Programming -> Interrupt
