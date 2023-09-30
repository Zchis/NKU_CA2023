#include <stdio.h>

#include "shell.h"

//从指令中提取不同的字段
uint32_t extract_op(uint32_t inst) { return inst >> 26; }//右移26位，得到op操作码，6位，也就是[31:26]

uint32_t extract_rs(uint32_t inst) { return (inst >> 21) & 0x1f; }//右移21位,取与，得到rs源寄存器，5位，也就是[25:21]

uint32_t extract_rt(uint32_t inst) { return (inst >> 16) & 0x1f; }//右移16位,取与，得到rt源寄存器，5位，也就是[20:16]

uint32_t extract_rd(uint32_t inst) { return (inst >> 11) & 0x1f; }//右移11位,取与，得到rd源寄存器，5位，也就是[15:11]

uint32_t extract_target(uint32_t inst) { return inst & 0x3ffffff; }//j型指令，得到target address目标地址，26位，[25:0]

uint32_t extract_imm(uint32_t inst) { return inst & 0xffff; }//i型指令，得到immediate立即数，16位，[15:0]

uint32_t extract_shamt(uint32_t inst) { return (inst >> 6) & 0x1f; }//右移5位,取与，得到shamt移位位数，5位，[10:6]

uint32_t extract_funct(uint32_t inst) { return inst & 0x3f; }//取与，得到funct功能码，6位，[5:0]


//三个符号拓展
/// Sign extend the 16 bits immediate
uint32_t sign_ext(uint32_t imm) {
    int32_t signed_imm = *((int16_t*)&imm);
    uint32_t extended_imm = *((uint32_t*)&signed_imm);
    return extended_imm;
}

/// Sign extend a byte to 32 bits
uint32_t sign_ext_byte(uint8_t imm) {
    int32_t signed_imm = *((int8_t*)&imm);
    uint32_t extended_imm = *((uint32_t*)&signed_imm);
    return extended_imm;
}

uint32_t sign_ext_half(uint16_t imm) {
    int32_t signed_imm = *((int16_t*)&imm);
    uint32_t extended_imm = *((uint32_t*)&signed_imm);
    return extended_imm;
}

//零拓展
uint32_t zero_ext(uint32_t imm) { return imm; }

uint32_t zero_ext_byte(uint8_t imm) { return imm; }

uint32_t zero_ext_half(uint16_t imm) { return imm; }

void process_instruction() {
    /* execute one instruction here. You should use CURRENT_STATE and modify
     * values in NEXT_STATE. You can call mem_read_32() and mem_write_32() to
     * access memory. */
    uint32_t inst = mem_read_32(CURRENT_STATE.PC);//从内存中读取指令

    printf("Instruction: 0x%08x\n", inst);

    uint32_t op = extract_op(inst);//得到各个部分的值
    uint32_t rs = extract_rs(inst);
    uint32_t rt = extract_rt(inst);
    uint32_t imm = extract_imm(inst);
    uint32_t rd = extract_rd(inst);
    uint32_t shamt = extract_shamt(inst);
    uint32_t funct = extract_funct(inst);

    switch (op) {//通过switch语句找到最终的指令
        case 0x0: {//op=0，通常表示R-Type指令，用于寄存器之间的操作。
            switch (funct) {
                case 0x0: {//funct=0x0，SLL指令，逻辑左移。
                    // SLL
                    NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] << shamt;
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x2: {//funct=0x2，SRL指令，逻辑右移。
                    // SRL
                    NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] >> shamt;
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x3: {//funct=0x3，SRA指令，算术右移。
                    // SRA
                    int32_t val = *((int32_t*)&CURRENT_STATE.REGS[rt]);
                    val = val >> shamt;
                    NEXT_STATE.REGS[rd] = val;
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x4: {//funct=0x4，SLLV指令。SLLV用于将一个寄存器中的值左移由另一个寄存器中的值指定的位数，并将结果存储在目标寄存器中。
                    // SLLV
                    uint32_t shamt = CURRENT_STATE.REGS[rs] & 0x1f;
                    NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] << shamt;
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x6: {//funct=0x6，SRLV指令。SRLV用于将一个寄存器中的值右移由另一个寄存器中的值指定的位数，并将结果存储在目标寄存器中。
                    // SRLV
                    uint32_t shamt = CURRENT_STATE.REGS[rs] & 0x1f;
                    NEXT_STATE.REGS[rd] = CURRENT_STATE.REGS[rt] >> shamt;
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x7: {//funct=0x7，SRAV指令。SRAV用于将一个寄存器中的值右移由另一个寄存器中的值指定的位数，并将结果存储在目标寄存器中。它执行算术右移操作，保持符号位不变。
                    // SRAV
                    int32_t val = *((int32_t*)&CURRENT_STATE.REGS[rt]);
                    uint32_t shamt = CURRENT_STATE.REGS[rs] & 0x1f;
                    val = val >> shamt;
                    NEXT_STATE.REGS[rd] = val;
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x8: {//funct=0x8，JR指令。无条件跳转到寄存器中存储的地址，通常用于函数返回。
                    // JR
                    NEXT_STATE.PC = CURRENT_STATE.REGS[rs];
                    break;
                }
                case 0x9: {//funct=0x9，JALR指令。JALR用于跳转到寄存器中存储的地址，并将返回地址存储到另一个寄存器中，通常用于函数调用。
                    // JALR
                    NEXT_STATE.REGS[rd] = CURRENT_STATE.PC + 4;
                    NEXT_STATE.PC = CURRENT_STATE.REGS[rs];
                    break;
                }
                case 0xc: {//funct=0xc，SYSCALL指令。用于执行系统调用，通常用于向操作系统请求服务。
                    // SYSCALL
                    if (CURRENT_STATE.REGS[2] == 0x0a) {
                        RUN_BIT = FALSE;
                    } else {
                        NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    }
                    break;
                }
                case 0x10: {//funct=0x10，MFHI指令。将HI寄存器中的值移动到目标寄存器中，通常用于乘法运算结果的高32位。
                    // MFHI
                    NEXT_STATE.REGS[rd] = CURRENT_STATE.HI;
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x11: {//funct=0x11，MTHI指令。将一个寄存器中的值移动到HI寄存器中，通常用于将乘法运算结果的高32位存储在HI寄存器中。
                    // MTHI
                    NEXT_STATE.HI = CURRENT_STATE.REGS[rs];
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x12: {//funct=0x12，MFLO指令。将LO寄存器中的值移动到目标寄存器中，通常用于将乘法运算结果的低32位存储在目标寄存器中。
                    // MFLO
                    NEXT_STATE.REGS[rd] = CURRENT_STATE.LO;
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x13: {//funct=0x13，MTLO指令。将一个寄存器中的值移动到LO寄存器中，通常用于将数据准备好以便进行乘法运算。
                    // MTLO
                    NEXT_STATE.LO = CURRENT_STATE.REGS[rs];
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x18: {//funct=0x18，MULT指令。执行有符号整数乘法，将两个寄存器中的值相乘，结果存储在HI和LO寄存器中。
                    // MULT
                    int64_t lhs = *((int32_t*)&CURRENT_STATE.REGS[rs]);
                    int64_t rhs = *((int32_t*)&CURRENT_STATE.REGS[rt]);
                    int64_t product = lhs * rhs;
                    uint64_t uint_product = *((uint32_t*)&product);
                    NEXT_STATE.HI =
                        (uint32_t)((uint_product >> 32) & 0xffffffff);
                    NEXT_STATE.LO = (uint32_t)(uint_product & 0xffffffff);
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x19: {//funct=0x19，MULTU指令。执行无符号整数乘法，将两个寄存器中的值相乘，结果存储在HI和LO寄存器中。
                    // MULTH
                    uint64_t lhs = CURRENT_STATE.REGS[rs];
                    uint64_t rhs = CURRENT_STATE.REGS[rt];
                    uint64_t product = lhs * rhs;

                    NEXT_STATE.HI = (uint32_t)((product >> 32) & 0xffffffff);
                    NEXT_STATE.LO = (uint32_t)(product & 0xffffffff);
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x1a: {//funct=0x1a，DIV指令。执行有符号整数除法，将一个寄存器中的值除以另一个寄存器中的值，商存储在LO寄存器中，余数存储在HI寄存器中。
                    // DIV
                    int32_t lhs = *((int32_t*)&CURRENT_STATE.REGS[rs]);
                    int32_t rhs = *((int32_t*)&CURRENT_STATE.REGS[rt]);
                    NEXT_STATE.LO = lhs / rhs;
                    NEXT_STATE.HI = lhs % rhs;
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x1b: {//funct=0x1b，DIVU指令。执行无符号整数除法，将一个寄存器中的值除以另一个寄存器中的值，商存储在LO寄存器中，余数存储在HI寄存器中。
                    // DIVU
                    uint32_t lhs = CURRENT_STATE.REGS[rs];
                    uint32_t rhs = CURRENT_STATE.REGS[rt];
                    NEXT_STATE.LO = lhs / rhs;
                    NEXT_STATE.HI = lhs % rhs;
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x20: {//funct=0x20，ADD指令。将两个寄存器中的值相加，结果存储在目标寄存器中。
                    // ADD
                    NEXT_STATE.REGS[rd] =
                        CURRENT_STATE.REGS[rs] + CURRENT_STATE.REGS[rt];
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x21: {//funct=0x21，ADDU指令。ADDU用于将两个寄存器中的值无符号相加，结果存储在目标寄存器中。
                    // ADDU
                    NEXT_STATE.REGS[rd] =
                        CURRENT_STATE.REGS[rs] + CURRENT_STATE.REGS[rt];
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x22: {//funct=0x22，SUB指令。将一个寄存器中的值减去另一个寄存器中的值，结果存储在目标寄存器中。
                    // SUB
                    NEXT_STATE.REGS[rd] =
                        CURRENT_STATE.REGS[rs] - CURRENT_STATE.REGS[rt];
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x23: {//funct=0x23，SUBU指令。将一个寄存器中的值无符号相减，结果存储在目标寄存器中。
                    // SUBU
                    NEXT_STATE.REGS[rd] =
                        CURRENT_STATE.REGS[rs] - CURRENT_STATE.REGS[rt];
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x24: {//funct=0x24，AND指令。执行位逻辑与操作，将两个寄存器中的值进行按位与操作，结果存储在目标寄存器中。
                    // AND
                    NEXT_STATE.REGS[rd] =
                        CURRENT_STATE.REGS[rs] & CURRENT_STATE.REGS[rt];
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x25: {//funct=0x25，OR指令。执行位逻辑或操作，将两个寄存器中的值进行按位或操作，结果存储在目标寄存器中。
                    // OR
                    NEXT_STATE.REGS[rd] =
                        CURRENT_STATE.REGS[rs] | CURRENT_STATE.REGS[rt];
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x26: {//funct=0x26，XOR指令。执行位逻辑异或操作，将两个寄存器中的值进行按位异或操作，结果存储在目标寄存器中。
                    // XOR
                    NEXT_STATE.REGS[rd] =
                        CURRENT_STATE.REGS[rs] ^ CURRENT_STATE.REGS[rt];
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x27: {//funct=0x27，NOR指令。执行位逻辑或非操作，将两个寄存器中的值进行按位或非操作，结果存储在目标寄存器中。
                    // NOR
                    NEXT_STATE.REGS[rd] =
                        ~(CURRENT_STATE.REGS[rs] | CURRENT_STATE.REGS[rt]);
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x2a: {//funct=0x2a，SLT指令。将一个寄存器中的值与另一个寄存器中的值进行有符号比较，如果前者小于后者，则将目标寄存器置为1，否则置为0。
                    // SLT
                    int32_t lhs = *((int32_t*)&CURRENT_STATE.REGS[rs]);
                    int32_t rhs = *((int32_t*)&CURRENT_STATE.REGS[rt]);
                    NEXT_STATE.REGS[rd] = (lhs < rhs) ? 1 : 0;
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                case 0x2b: {//funct=0x2b，SLTU指令。将一个寄存器中的值与另一个寄存器中的值进行无符号比较，如果前者小于后者，则将目标寄存器置为1，否则置为0。
                    // SLTU
                    NEXT_STATE.REGS[rd] =
                        CURRENT_STATE.REGS[rs] < CURRENT_STATE.REGS[rt] ? 1 : 0;
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    break;
                }
                default: {
                    printf("Unknown instruction: 0x%x\n", inst);
                    break;
                }
            }
            break;
        }
        case 0x8: {//op=8，addi指令，用于将一个寄存器与一个立即数相加。
            // ADDI
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] + sign_ext(imm);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        case 0x9: {//op=9，addiu指令，无符号加法
            // ADDIU
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] + sign_ext(imm);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        case 0xc: {//op=0xc，ANDI指令，与立即数取与
            // ANDI
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] & zero_ext(imm);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        case 0xd: {//op=0xd，ORI指令，与立即数取或
            // ORI
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] | zero_ext(imm);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        case 0xe: {//op=0xe，XORI指令，与立即数取异或
            // XORI
            NEXT_STATE.REGS[rt] = CURRENT_STATE.REGS[rs] ^ zero_ext(imm);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        case 0x4: {//beq指令，用于比较寄存器值是否相等，如果相等则分支。
            // BEQ

            uint32_t offset = sign_ext(imm) << 2;

            if (CURRENT_STATE.REGS[rs] == CURRENT_STATE.REGS[rt]) {
                NEXT_STATE.PC = CURRENT_STATE.PC + offset;
            } else {
                NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            }
            break;
        }
        case 0x1: {//op=0x1，条件分支指令
            uint32_t offset = sign_ext(imm) << 2;

            switch (rt) {
                case 0x0: {//rt=0x0，BLTZ指令。根据一个寄存器中的值是否小于零来进行条件分支。如果寄存器值小于零，将执行跳转。
                    // BLTZ
                    if ((CURRENT_STATE.REGS[rs] & 0x80000000) != 0) {
                        NEXT_STATE.PC = CURRENT_STATE.PC + offset;
                    } else {
                        NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    }
                    break;
                }
                case 0x10: {//rt=0x10，BLTZAL指令。BLTZAL与BLTZ类似，但它会在跳转之前将下一条指令的地址存储在寄存器31（$ra）中，以实现函数调用的返回地址保存。
                    // BLTZAL
                    NEXT_STATE.REGS[31] = CURRENT_STATE.PC + 4;
                    if ((CURRENT_STATE.REGS[rs] & 0x80000000) != 0) {
                        NEXT_STATE.PC = CURRENT_STATE.PC + offset;
                    } else {
                        NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    }
                    break;
                }
                case 0x1: {//rt=0x1，BGEZ指令。用于根据一个寄存器中的值是否大于或等于零来进行条件分支。如果寄存器值大于或等于零，将执行跳转。
                    // BGEZ
                    if ((CURRENT_STATE.REGS[rs] & 0x80000000) == 0) {
                        NEXT_STATE.PC = CURRENT_STATE.PC + offset;
                    } else {
                        NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    }
                    break;
                }
                case 0x11: {//rt=0x11，BGEZAL指令。BGEZAL与BGEZ类似，但它会在跳转之前将下一条指令的地址存储在寄存器31（$ra）中，以实现函数调用的返回地址保存。
                    // BGEZAL
                    NEXT_STATE.REGS[31] = CURRENT_STATE.PC + 4;
                    if ((CURRENT_STATE.REGS[rs] & 0x80000000) == 0) {
                        NEXT_STATE.PC = CURRENT_STATE.PC + offset;
                    } else {
                        NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    }
                    break;
                }
            }
            break;
        }
        case 0x5: {//op=0x5，条件分支指令，BNE，如果它们不相等，则执行跳转到指定的目标地址。
            // BNE

            uint32_t offset = sign_ext(imm) << 2;

            printf("BNE: offset: %d, rs: %d, rt: %d\n", offset, rs, rt);

            printf("rs: 0x%08x\n", CURRENT_STATE.REGS[rs]);
            printf("rt: 0x%08x\n", CURRENT_STATE.REGS[rt]);

            if (CURRENT_STATE.REGS[rs] != CURRENT_STATE.REGS[rt]) {
                NEXT_STATE.PC = CURRENT_STATE.PC + offset;
            } else {
                NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            }
            break;
        }
        case 0x6: {//op=0x6，条件分支指令，BLEZ，比较一个寄存器中的值是否小于等于零，如果满足条件，则执行跳转到指定的目标地址。
            // BLEZ

            uint32_t offset = sign_ext(imm) << 2;

            if (rt == 0) {
                if ((CURRENT_STATE.REGS[rs] & 0x80000000) != 0 ||
                    CURRENT_STATE.REGS[rs] == 0) {
                    NEXT_STATE.PC = CURRENT_STATE.PC + offset;
                } else {
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                }
            } else {
                // Illegal instruction
                printf("Illegal rt in BLEZ.\n");
            }
            break;
        }
        case 0x7: {//op=0x7，条件分支指令，BGTZ，大于0则跳转
            // BGTZ
            uint32_t offset = sign_ext(imm) << 2;

            printf("BGTZ: offset: 0x%08x, rs: %d, rt: %d, pc: 0x%08x\n", offset,
                   rs, rt, CURRENT_STATE.PC);

            if (rt == 0) {
                if ((CURRENT_STATE.REGS[rs] & 0x80000000) == 0 &&
                    CURRENT_STATE.REGS[rs] != 0) {
                    NEXT_STATE.PC = CURRENT_STATE.PC + offset;
                    printf("PC: 0x%08x\n", NEXT_STATE.PC);
                } else {
                    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                }
            } else {
                // Illegal instruction
                printf("Illegal rt in BGTZ.\n");
            }
            break;
        }
        case 0x2: {//op=0x2，j指令，用于无条件跳转到一个32位地址。
            // J
            uint32_t target = extract_target(inst);
            NEXT_STATE.PC = (CURRENT_STATE.PC & 0xf0000000) | (target << 2);
            break;
        }
        case 0x3: {//op=0x3，jal指令，用于跳转并链接，将返回地址存储在寄存器中。
            // JAL
            uint32_t target = extract_target(inst);
            NEXT_STATE.REGS[31] = CURRENT_STATE.PC + 4;
            NEXT_STATE.PC = (CURRENT_STATE.PC & 0xf0000000) | (target << 2);
            break;
        }
        case 0xf: {//op=0xf，lui指令，低位加载
            // LUI

            if (rs == 0) {
                NEXT_STATE.REGS[rt] = imm << 16;
                NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            } else {
                // Illegal instruction
            }
            break;
        }
        case 0x20: {//op=0x20，lb指令，从内存加载一个字节到目标寄存器中
            // LB

            uint32_t addr = sign_ext(imm) + CURRENT_STATE.REGS[rs];

            uint8_t byte = mem_read_32(addr) & 0xff;

            NEXT_STATE.REGS[rt] = sign_ext_byte(byte);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        case 0x24: {//op=0x24，lbu指令，用于从内存中加载一个字节的数据，并将其零扩展后存储到目标寄存器中。
            // LBU

            uint32_t addr = sign_ext(imm) + CURRENT_STATE.REGS[rs];

            uint8_t byte = mem_read_32(addr) & 0xff;

            NEXT_STATE.REGS[rt] = zero_ext_byte(byte);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        case 0x21: {//op=0x21，LH指令。从内存中加载一个半字（16位）的数据，并将其符号扩展后存储到目标寄存器中。
            // LH

            uint32_t addr = sign_ext(imm) + CURRENT_STATE.REGS[rs];

            uint16_t half = mem_read_32(addr) & 0xffff;

            NEXT_STATE.REGS[rt] = sign_ext_half(half);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        case 0x25: {//op=0x25，这代表了LHU指令。从内存中加载一个半字（16位）的数据，并将其零扩展后存储到目标寄存器中。
            // LHU

            uint32_t addr = sign_ext(imm) + CURRENT_STATE.REGS[rs];

            uint16_t half = mem_read_32(addr) & 0xffff;

            NEXT_STATE.REGS[rt] = zero_ext_half(half);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        case 0x23: {//op=0x23，这代表了LW指令。从内存中加载一个字（32位）的数据，并将其存储到目标寄存器中。
            // LW

            uint32_t addr = sign_ext(imm) + CURRENT_STATE.REGS[rs];

            NEXT_STATE.REGS[rt] = mem_read_32(addr);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        case 0x28: {//op=0x28，SB指令。将一个字节的数据从寄存器写入内存。
            // SB

            uint32_t addr = sign_ext(imm) + CURRENT_STATE.REGS[rs];

            uint32_t val = (mem_read_32(addr) & 0xffffff00) |
                           (CURRENT_STATE.REGS[rt] & 0xff);

            mem_write_32(addr, val);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        case 0x29: {//op=0x29，SH（指令。将一个半字（16位）的数据从寄存器写入内存。
            // SH

            uint32_t addr = sign_ext(imm) + CURRENT_STATE.REGS[rs];

            uint32_t val = (mem_read_32(addr) & 0xffff0000) |
                           (CURRENT_STATE.REGS[rt] & 0xffff);
            mem_write_32(addr, val);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        case 0x2b: {//op=0x2b，这代表了SW指令。将一个字（32位）的数据从寄存器写入内存。
            // SW
            uint32_t addr = sign_ext(imm) + CURRENT_STATE.REGS[rs];

            mem_write_32(addr, CURRENT_STATE.REGS[rt]);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
        default: {
            printf("unimplemented instruction: 0x%08x\n", inst);
            break;
        }
    }
}
