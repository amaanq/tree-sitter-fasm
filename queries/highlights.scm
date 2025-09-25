;; Flat Assembler g Highlight Queries

;; Keywords

[
  "if"
  "else"
  "end"
] @keyword.conditional

[
  "repeat"
  "rept"
  "while"
  "iterate"
  "irp"
  "break"
] @keyword.repeat

[
  "match"
  "rawmatch"
  "rmatch"
] @keyword.conditional

[
  "org"
  "section"
  "virtual"
  "namespace"
  "include"
  "format"
  "display"
  "err"
  "assert"
  "eval"
  "restartout"
  "postpone"
  "forward"
  "reverse"
  "common"
] @keyword.directive

[
  "equ"
  "reequ"
  "define"
  "redefine"
  "label"
  "element"
  "restore"
] @keyword.directive.define

[
  "db" "dw" "dd" "dp" "dq" "dt" "ddq" "dqq" "ddqq"
  "rb" "rw" "rd" "rp" "rq" "rt" "rdq" "rqq" "rdqq"
  "emit"
  "file"
  "dup"
] @keyword.directive

[
  "macro"
  "struc"
  "local"
  "purge"
  "restruc"
] @keyword.function

[
  "calminstruction"
  "assemble"
  "arrange"
  "compute"
  "check"
  "publish"
  "transform"
  "stringify"
  "take"
  "taketext"
  "call"
  "jump"
  "jyes"
  "jno"
  "exit"
] @keyword.function

[
  "load"
  "store"
  "at"
  "from"
] @keyword.operator

;; Operators

[
  "+"
  "-"
  "*"
  "/"
  "mod"
  "and"
  "or"
  "xor"
  "not"
  "shl"
  "shr"
  "bsf"
  "bsr"
  "bswap"
] @operator

[
  "="
  ":="
  "=:"
  "<"
  ">"
  "<="
  ">="
  "<>"
  "relativeto"
  "eqtype"
  "eq"
] @operator

[
  "sizeof"
  "lengthof"
  "float"
  "trunc"
  "string"
  "defined"
  "definite"
  "used"
] @keyword.operator

[
  "&"
  "|"
  "~"
] @operator

; Literals
;---------

;; Variables

((simple_identifier) @variable
  (#set! priority 99))

((case_insensitive_identifier) @variable
  (#set! priority 99))

((forced_identifier) @variable
  (#set! priority 99))

(relative_identifier) @variable.member

(namespaced_identifier "." _ @variable.member)

(repeat_parameter) @variable.builtin

((simple_identifier) @variable.builtin
  (#any-of? @variable.builtin
    "al" "cl" "dl" "bl" "ah" "ch" "dh" "bh"
    "ax" "cx" "dx" "bx" "sp" "bp" "si" "di"
    "eax" "ecx" "edx" "ebx" "esp" "ebp" "esi" "edi"
    "rax" "rcx" "rdx" "rbx" "rsp" "rbp" "rsi" "rdi"
    "r8" "r9" "r10" "r11" "r12" "r13" "r14" "r15"
    "r8b" "r9b" "r10b" "r11b" "r12b" "r13b" "r14b" "r15b"
    "r8w" "r9w" "r10w" "r11w" "r12w" "r13w" "r14w" "r15w"
    "r8d" "r9d" "r10d" "r11d" "r12d" "r13d" "r14d" "r15d"
    "es" "cs" "ss" "ds" "fs" "gs"
    "cr0" "cr2" "cr3" "cr4" "cr8"
    "dr0" "dr1" "dr2" "dr3" "dr6" "dr7"
    "st0" "st1" "st2" "st3" "st4" "st5" "st6" "st7"
    "sil" "dil" "bpl" "spl"
    "mm0" "mm1" "mm2" "mm3" "mm4" "mm5" "mm6" "mm7"
    "xmm0" "xmm1" "xmm2" "xmm3" "xmm4" "xmm5" "xmm6" "xmm7"
    "xmm8" "xmm9" "xmm10" "xmm11" "xmm12" "xmm13" "xmm14" "xmm15"
    "xmm16" "xmm17" "xmm18" "xmm19" "xmm20" "xmm21" "xmm22" "xmm23"
    "xmm24" "xmm25" "xmm26" "xmm27" "xmm28" "xmm29" "xmm30" "xmm31"
    "ymm0" "ymm1" "ymm2" "ymm3" "ymm4" "ymm5" "ymm6" "ymm7"
    "ymm8" "ymm9" "ymm10" "ymm11" "ymm12" "ymm13" "ymm14" "ymm15"
    "ymm16" "ymm17" "ymm18" "ymm19" "ymm20" "ymm21" "ymm22" "ymm23"
    "ymm24" "ymm25" "ymm26" "ymm27" "ymm28" "ymm29" "ymm30" "ymm31"
    "zmm0" "zmm1" "zmm2" "zmm3" "zmm4" "zmm5" "zmm6" "zmm7"
    "zmm8" "zmm9" "zmm10" "zmm11" "zmm12" "zmm13" "zmm14" "zmm15"
    "zmm16" "zmm17" "zmm18" "zmm19" "zmm20" "zmm21" "zmm22" "zmm23"
    "zmm24" "zmm25" "zmm26" "zmm27" "zmm28" "zmm29" "zmm30" "zmm31"
    "k0" "k1" "k2" "k3" "k4" "k5" "k6" "k7"
    "bnd0" "bnd1" "bnd2" "bnd3"))

;; Modules

(namespace_block
  (simple_identifier) @module)

;; Labels

(label_definition
  label: _ @label)

(area_label_definition
  label: _ @label)

(calm_label
  label: _ @label)

(macro_concatenation . ".") @label

;; Constants

((simple_identifier) @constant
  (#lua-match? @constant "^[A-Z][A-Z0-9_]*$"))

(labeled_instruction
    (simple_identifier) @constant
    (data_directive))

(current_address) @constant.builtin

(base_address) @constant.builtin

(file_offset) @constant.builtin

((simple_identifier) @constant.builtin
  (#any-of? @constant.builtin
    "byte" "word" "dword" "fword" "pword" "qword"
    "tbyte" "tword" "dqword" "xword" "qqword" "yword"
    "dqqword" "zword"))

((simple_identifier) @constant.builtin
  (#any-of? @constant.builtin
    "__time__" "__file__" "__line__" "__source__" "%t"))

;; Types

((simple_identifier) @type
  (#lua-match? @type "^[A-Z][a-z]"))

(struc_definition name: _ @type)

;; Functions

(simple_instruction
  .
  (simple_identifier) @function)

(macro_definition
  name: (simple_identifier) @function)

(calm_definition
  (simple_identifier) @function)

((simple_instruction (simple_identifier) @function.builtin)
  (#any-of? @function.builtin
    "aaa" "aad" "aam" "aas" "adc" "add" "addpd" "addps" "addsd" "addss" "addsubpd" "addsubps" "aesdec" "aesdeclast" "aesenc" "aesenclast" "aesimc" "and" "andnpd" "andnps" "andpd" "andps" "arpl" "bound" "bsf" "bsr" "bswap" "bt" "btc" "btr" "bts" "call" "cbw" "cdq" "cdqe" "clc" "cld" "clflush" "cli" "clts" "cmc" "cmp"
    "cmova" "cmovae" "cmovb" "cmovbe" "cmovc" "cmove" "cmovg" "cmovge" "cmovl" "cmovle" "cmovna" "cmovnae" "cmovnb" "cmovnbe" "cmovnc" "cmovne" "cmovng" "cmovnge" "cmovnl" "cmovnle" "cmovno" "cmovnp" "cmovns" "cmovnz" "cmovo" "cmovp" "cmovpe" "cmovpo" "cmovs" "cmovz" "cmppd" "cmpps" "cmps" "cmpsb" "cmpsd" "cmpsw" "cmpss"
    "cmpxchg" "cmpxchg8b" "cmpxchg16b" "comiss" "cpuid" "cqo" "crc32" "cvtdq2pd" "cvtdq2ps" "cvtpd2dq" "cvtpd2pi" "cvtpd2ps" "cvtpi2pd" "cvtpi2ps" "cvtps2dq" "cvtps2pd" "cvtps2pi" "cvtsd2si" "cvtsd2ss" "cvtsi2sd" "cvtsi2ss" "cvtss2sd" "cvtss2si" "cvttpd2dq" "cvttpd2pi" "cvttps2dq" "cvttps2pi" "cvttsd2si" "cvttss2si" "cwd"
    "cwde" "daa" "das" "dec" "div" "divpd" "divps" "divsd" "divss" "dppd" "dpps" "emms" "enter" "extractps" "extrq" "f2xm1" "fabs" "fadd" "faddp" "fbld" "fbstp" "fchs" "fclex" "fcmovb" "fcmovbe" "fcmove" "fcmovnb" "fcmovnbe" "fcmovne" "fcmovnu" "fcmovu" "fcom" "fcomi" "fcomip" "fcomp" "fcompp" "fcos" "fdecstp" "fdiv"
    "fdivp" "fdivr" "fdivrp" "femms" "ffree" "fiadd" "ficom" "ficomp" "fidiv" "fidivr" "fild" "fimul" "fincstp" "finit" "fist" "fistp" "fisttp" "fisub" "fisubr" "fld" "fld1" "fldcw" "fldenv" "fldl2e" "fldl2t" "fldlg2" "fldln2" "fldpi" "fldz" "fmul" "fmulp" "fnclex" "fninit" "fnop" "fnsave" "fnstcw" "fnstenv" "fnstsw"
    "fpatan" "fprem" "fprem1" "fptan" "frndint" "frstor" "fsave" "fscale" "fsin" "fsincos" "fsqrt" "fst" "fstcw" "fstenv" "fstp" "fstsw" "fsub" "fsubp" "fsubr" "fsubrp" "ftst" "fucom" "fucomp" "fucompp" "fwait" "fxam" "fxch" "fxrstor" "fxrstor64" "fxsave" "fxsave64" "fxtract" "fyl2x" "fyl2xp1" "haddpd" "haddps" "hlt"
    "hsubpd" "hsubps" "idiv" "imul" "in" "inc" "ins" "insb" "insd" "insertps" "insertq" "insw" "int" "int3" "into" "invlpg" "iret" "iretd" "iretw" "ja" "jae" "jb" "jbe" "jc" "jcxz" "je" "jecxz" "jg" "jge" "jl" "jle" "jmp" "jna" "jnae" "jnb" "jnbe" "jnc" "jne" "jng" "jnge" "jnl" "jnle" "jno" "jnp" "jns" "jnz" "jo" "jp"
    "jpe" "jpo" "js" "jz" "lahf" "lar" "lddqu" "ldmxcsr" "lds" "lea" "leave" "les" "lfence" "lfs" "lgdt" "lgs" "lidt" "lldt" "lmsw" "lock" "lods" "lodsb" "lodsd" "lodsq" "lodsw" "loop" "loope" "looped" "loopew" "loopne" "loopned" "loopnew" "loopnz" "loopnzd" "loopnzw" "loopw" "loopz" "loopzd" "loopzw" "lsl" "lss" "ltr"
    "lzcnt" "maskmovdqu" "maskmovq" "maxpd" "maxps" "maxsd" "maxss" "mfence" "minpd" "minps" "minsd" "minss" "monitor" "mov" "movapd" "movaps" "movd" "movddup" "movdq2q" "movdqa" "movdqu" "movhlps" "movhpd" "movhps" "movlhps" "movlpd" "movlps" "movmskpd" "movmskps" "movntdq" "movntdqa" "movnti" "movntpd" "movntps" "movntq"
    "movntsd" "movntss" "movq" "movq2dq" "movs" "movsb" "movsd" "movshdup" "movsldup" "movsq" "movss" "movsw" "movsx" "movsxd" "movupd" "movups" "movzx" "mpsadbw" "mul" "mulpd" "mulps" "mulsd" "mulss" "mwait" "neg" "nop" "not" "or" "orpd" "orps" "out" "outs" "outsb" "outsd" "outsw" "pabsb" "pabsd" "pabsw" "packssdw"
    "packsswb" "packusdw" "packuswb" "paddb" "paddd" "paddq" "paddsb" "paddsw" "paddusb" "paddusw" "paddw" "palignr" "pand" "pandn" "pause" "pavgb" "pavgusb" "pavgw" "pblendvb" "pblendw" "pcmpeqb" "pcmpeqd" "pcmpeqq" "pcmpeqw" "pcmpestri" "pcmpestrm" "pcmpgtb" "pcmpgtd" "pcmpgtq" "pcmpgtw" "pcmpistri" "pcmpistrm" "pextrb"
    "pextrd" "pextrq" "pextrw" "pf2id" "pf2iw" "pfacc" "pfadd" "pfcmpeq" "pfcmpge" "pfcmpgt" "pfmax" "pfmin" "pfmul" "pfnacc" "pfpnacc" "pfrcp" "pfrcpit1" "pfrcpit2" "pfrsqit1" "pfrsqrt" "pfsub" "pfsubr" "phaddw" "phaddd" "phaddsw" "phminposuw" "phsubw" "phsubd" "phsubsw" "pi2fd" "pi2fw" "pinsrb" "pinsrd" "pinsrq" "pinsrw"
    "pmaddubsw" "pmaddwd" "pmaxsb" "pmaxsd" "pmaxsw" "pmaxub" "pmaxud" "pmaxuw" "pminsb" "pminsw" "pminub" "pminud" "pminuw" "pmovmskb" "pmovsxbd" "pmovsxbq" "pmovsxbw" "pmovsxdq" "pmovsxwd" "pmovsxwq" "pmovzxbd" "pmovzxbq" "pmovzxbw" "pmovzxdq" "pmovzxwd" "pmovzxwq" "pmuldq" "pmulhrsw" "pmulhrw" "pmulhuw" "pmulhw"
    "pmulld" "pmullw" "pmuludq" "pop" "popa" "popad" "popaw" "popcnt" "popf" "popfd" "popfw" "por" "prefetch" "prefetchnta" "prefetcht0" "prefetcht1" "prefetcht2" "prefetchw" "psadbw" "pshufb" "pshufd" "pshufhw" "pshuflw" "pshufw" "psignb" "psignd" "psignw" "pslld" "pslldq" "psllq" "psllw" "psrad" "psraw" "psrld" "psrldq"
    "psrlq" "psrlw" "psubb" "psubd" "psubq" "psubsb" "psubsw" "psubusb" "psubusw" "psubw" "pswapd" "ptest" "punpckhbw" "punpckhdq" "punpckhqdq" "punpckhwd" "punpcklbw" "punpckldq" "punpcklqdq" "punpcklwd" "push" "pusha" "pushad" "pushaw" "pushf" "pushfd" "pushfw" "pxor" "rcl" "rcpps" "rcpss" "rcr" "rdmsr" "rdmsrq" "rdpmc"
    "rdtsc" "rep" "repe" "repne" "repnz" "repz" "ret" "retd" "retf" "retfd" "retfw" "retn" "retnd" "retnw" "retw" "rol" "ror" "roundpd" "roundps" "roundsd" "roundss" "rsm" "rsqrtps" "rsqrtss" "sahf" "sal" "salc" "sar" "sbb" "scas" "scasb" "scasd" "scasq" "scasw" "seta" "setae" "setb" "setbe" "setc" "sete" "setg" "setge"
    "setl" "setle" "setna" "setnae" "setnb" "setnbe" "setnc" "setne" "setng" "setnge" "setnl" "setnle" "setno" "setnp" "setns" "setnz" "seto" "setp" "setpe" "setpo" "sets" "setz" "sfence" "sgdt" "shl" "shld" "shr" "shrd" "shufpd" "shufps" "sidt" "sldt" "smsw" "sqrtpd" "sqrtps" "sqrtsd" "sqrtss" "stc" "std" "sti" "stmxcsr"
    "stos" "stosb" "stosd" "stosq" "stosw" "str" "sub" "subpd" "subps" "subsd" "subss" "swapgs" "syscall" "sysenter" "sysexit" "sysexitq" "sysret" "sysretq" "test" "ucomiss" "ucomisd" "ud2" "unpckhpd" "unpckhps" "unpcklpd" "unpcklps" "wait" "wbinvd" "wrmsr" "wrmsrq" "xadd" "xchg" "xlat" "xlatb" "xor" "xorpd" "xorps"))

((simple_instruction (simple_identifier) @function.builtin)
  (#any-of? @function.builtin
    "vaesdec" "vaesdeclast" "vaesenc" "vaesenclast" "vaesimc" "vaddpd" "vaddps" "vaddsd" "vaddss" "vaddsubpd" "vaddsubps" "vandnpd" "vandnps" "vandpd" "vandps" "vblendpd" "vblendps" "vblendvpd" "vblendvps" "vbroadcastf128" "vbroadcasti128" "vbroadcastsd" "vbroadcastss" "vcmppd" "vcmpps" "vcmpsd" "vcmpss" "vcomiss"
    "vcomisd" "vcvtdq2pd" "vcvtdq2ps" "vcvtpd2dq" "vcvtpd2ps" "vcvtps2dq" "vcvtps2pd" "vcvtsd2si" "vcvtsd2ss" "vcvtsi2sd" "vcvtsi2ss" "vcvtss2sd" "vcvtss2si" "vcvttpd2dq" "vcvttps2dq" "vcvttsd2si" "vcvttss2si" "vdivpd" "vdivps" "vdivsd" "vdivss" "vdppd" "vdpps" "vextractf128" "vextracti128" "vextractps" "vfmadd132pd"
    "vfmadd132ps" "vfmadd132sd" "vfmadd132ss" "vfmadd213pd" "vfmadd213ps" "vfmadd213sd" "vfmadd213ss" "vfmadd231pd" "vfmadd231ps" "vfmadd231sd" "vfmadd231ss" "vfmaddsub132pd" "vfmaddsub132ps" "vfmaddsub213pd" "vfmaddsub213ps" "vfmaddsub231pd" "vfmaddsub231ps" "vfmsub132pd" "vfmsub132ps" "vfmsub132sd" "vfmsub132ss"
    "vfmsub213pd" "vfmsub213ps" "vfmsub213sd" "vfmsub213ss" "vfmsub231pd" "vfmsub231ps" "vfmsub231sd" "vfmsub231ss" "vfmsubadd132pd" "vfmsubadd132ps" "vfmsubadd213pd" "vfmsubadd213ps" "vfmsubadd231pd" "vfmsubadd231ps" "vfnmadd132pd" "vfnmadd132ps" "vfnmadd132sd" "vfnmadd132ss" "vfnmadd213pd" "vfnmadd213ps" "vfnmadd213sd"
    "vfnmadd213ss" "vfnmadd231pd" "vfnmadd231ps" "vfnmadd231sd" "vfnmadd231ss" "vfnmsub132pd" "vfnmsub132ps" "vfnmsub132sd" "vfnmsub132ss" "vfnmsub213pd" "vfnmsub213ps" "vfnmsub213sd" "vfnmsub213ss" "vfnmsub231pd" "vfnmsub231ps" "vfnmsub231sd" "vfnmsub231ss" "vgatherdpd" "vgatherdps" "vgatherqpd" "vgatherqps" "vhaddpd"
    "vhaddps" "vhsubpd" "vhsubps" "vinsertf128" "vinserti128" "vinsertps" "vldmxcsr" "vlddqu" "vmaskmovdqu" "vmaskmovpd" "vmaskmovps" "vmaxpd" "vmaxps" "vmaxsd" "vmaxss" "vminpd" "vminps" "vminsd" "vminss" "vmovapd" "vmovaps" "vmovd" "vmovddup" "vmovdqa" "vmovdqu" "vmovhlps" "vmovhpd" "vmovhps" "vmovlhps" "vmovlpd"
    "vmovlps" "vmovmskpd" "vmovmskps" "vmovntdq" "vmovntdqa" "vmovntpd" "vmovntps" "vmovq" "vmovsd" "vmovshdup" "vmovsldup" "vmovss" "vmovupd" "vmovups" "vmpsadbw" "vmulpd" "vmulps" "vmulsd" "vmulss" "vorpd" "vorps" "vpabsb" "vpabsd" "vpabsw" "vpackssdw" "vpacksswb" "vpackusdw" "vpackuswb" "vpaddb" "vpaddd" "vpaddq"
    "vpaddsb" "vpaddsw" "vpaddusb" "vpaddusw" "vpaddw" "vpalignr" "vpand" "vpandn" "vpavgb" "vpavgw" "vpblendvb" "vpblendw" "vpbroadcastb" "vpbroadcastd" "vpbroadcastq" "vpbroadcastw" "vpcmpeqb" "vpcmpeqd" "vpcmpeqq" "vpcmpeqw" "vpcmpestri" "vpcmpestrm" "vpcmpgtb" "vpcmpgtd" "vpcmpgtq" "vpcmpgtw" "vpcmpistri" "vpcmpistrm"
    "vperm2f128" "vperm2i128" "vpermd" "vpermilpd" "vpermilps" "vpermpd" "vpermps" "vpermq" "vpextrb" "vpextrd" "vpextrq" "vpextrw" "vpgatherdd" "vpgatherdq" "vpgatherqd" "vpgatherqq" "vphaddw" "vphaddd" "vphaddsw" "vphminposuw" "vphsubw" "vphsubd" "vphsubsw" "vpinsrb" "vpinsrd" "vpinsrq" "vpinsrw" "vpmaddubsw" "vpmaddwd"
    "vpmaskmovd" "vpmaskmovq" "vpmaxsb" "vpmaxsd" "vpmaxsw" "vpmaxub" "vpmaxud" "vpmaxuw" "vpminsb" "vpminsd" "vpminsw" "vpminub" "vpminud" "vpminuw" "vpmovmskb" "vpmovsxbd" "vpmovsxbq" "vpmovsxbw" "vpmovsxdq" "vpmovsxwd" "vpmovsxwq" "vpmovzxbd" "vpmovzxbq" "vpmovzxbw" "vpmovzxdq" "vpmovzxwd" "vpmovzxwq" "vpmuldq"
    "vpmulhrsw" "vpmulhuw" "vpmulhw" "vpmulld" "vpmullw" "vpmuludq" "vpor" "vpsadbw" "vpshufb" "vpshufd" "vpshufhw" "vpshuflw" "vpsignb" "vpsignd" "vpsignw" "vpslld" "vpsllw" "vpsllq" "vpslldq" "vpsllvd" "vpsllvq" "vpsrad" "vpsraw" "vpsravd" "vpsrld" "vpsrlw" "vpsrlq" "vpsrldq" "vpsrlvd" "vpsrlvq" "vpsubb" "vpsubd"
    "vpsubq" "vpsubsb" "vpsubsw" "vpsubusb" "vpsubusw" "vpsubw" "vptest" "vpunpckhbw" "vpunpckhdq" "vpunpckhqdq" "vpunpckhwd" "vpunpcklbw" "vpunpckldq" "vpunpcklqdq" "vpunpcklwd" "vpxor" "vrcpps" "vrcpss" "vroundpd" "vroundps" "vroundsd" "vroundss" "vrsqrtps" "vrsqrtss" "vshufpd" "vshufps" "vsqrtpd" "vsqrtps" "vsqrtsd"
    "vsqrtss" "vstmxcsr" "vsubpd" "vsubps" "vsubsd" "vsubss" "vtestpd" "vtestps" "vucomiss" "vucomisd" "vunpckhpd" "vunpckhps" "vunpcklpd" "vunpcklps" "vxorpd" "vxorps" "vzeroall" "vzeroupper"))

(calm_command "call" name: _ @function.call)

;; Parameters

(macro_parameters
  parameter: _ @variable.parameter)

;; Literals

(number) @number

(string) @string

(literal_pattern "=" _ @string.regexp)

;; Punctuation

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

[
  ","
  "."
  ":"
  "::"
  "?"
  "!"
] @punctuation.delimiter

[
  "#"
] @punctuation.special

(special_char) @punctuation.special

(case_insensitive_pattern
  "?" @operator)

;; Comments

(comment) @comment @spell

;; Errors

(ERROR) @error
