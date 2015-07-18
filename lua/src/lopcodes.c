/*
** $Id: lopcodes.c,v 1.37 2005/11/08 19:45:36 roberto Exp $
** See Copyright Notice in lua.h
*/


#define lopcodes_c
#define LUA_CORE


#include "lopcodes.h"


/* ORDER OP */

const char *const luaP_opnames[NUM_OPCODES+1] = {
  "GETFIELD",
  "TEST",
  "CALL_I",
  "CALL_C",
  "EQ",
  "EQ_BK",
  "GETGLOBAL",
  "MOVE",
  "SELF",
  "RETURN",
  "GETTABLE_S",
  "GETTABLE_N",
  "GETTABLE",
  "LOADBOOL",
  "TFORLOOP",
  "SETFIELD",
  "SETTABLE_S",
  "SETTABLE_S_BK",
  "SETTABLE_N",
  "SETTABLE_N_BK",
  "SETTABLE",
  "SETTABLE_BK",
  "TAILCALL_I",
  "TAILCALL_C",
  "TAILCALL_M",
  "LOADK",
  "LOADNIL",
  "SETGLOBAL",
  "JMP",
  "CALL_M",
  "CALL",
  "INTRINSIC_INDEX",
  "INTRINSIC_NEWINDEX",
  "INTRINSIC_SELF",
  "INTRINSIC_INDEX_LITERAL",
  "INTRINSIC_NEWINDEX_LITERAL",
  "INTRINSIC_SELF_LITERAL",
  "TAILCALL",
  "GETUPVAL",
  "SETUPVAL",
  "ADD",
  "ADD_BK",
  "SUB",
  "SUB_BK",
  "MUL",
  "MUL_BK",
  "DIV",
  "DIV_BK",
  "MOD",
  "MOD_BK",
  "POW",
  "POW_BK",
  "NEWTABLE",
  "UNM",
  "NOT",
  "LEN",
  "LT",
  "LT_BK",
  "LE",
  "LE_BK",
  "CONCAT",
  "TESTSET",
  "FORPREP",
  "FORLOOP",
  "SETLIST",
  "CLOSE",
  "CLOSURE",
  "VARARG",
  "TAILCALL_I_R1",
  "CALL_I_R1",
  "SETUPVAL_R1",
  "TEST_R1",
  "NOT_R1",
  "GETFIELD_R1",
  "SETFIELD_R1",
  "NEWSTRUCT",
  "DATA",
  "SETSLOTN",
  "SETSLOTI",
  "SETSLOT",
  "SETSLOTS",
  "SETSLOTMT",
  "CHECKTYPE",
  "CHECKTYPES",
  "GETSLOT",
  "GETSLOTMT",
  "SELFSLOT",
  "SELFSLOTMT",
  "GETFIELD_MM",
  "CHECKTYPE_D",
  "GETSLOT_D",
  "GETGLOBAL_MEM",
  NULL
};


#define opmode(t,a,b,c,m) (((t)<<7) | ((a)<<6) | ((b)<<4) | ((c)<<2) | (m))

const lu_byte luaP_opmodes[NUM_OPCODES] = {
/*       T  A    B       C     mode		   opcode	*/
  opmode(0, OpArgU, OpArgR, OpArgK, iABC) 		/* OP_MOVE */
 ,opmode(1, OpArgU, OpArgN, OpArgU, iABC)		/* OP_LOADK */
 ,opmode(0, OpArgU, OpArgU, OpArgU, iABC)		/* OP_LOADBOOL */
 ,opmode(0, OpArgU, OpArgU, OpArgU, iABC)		/* OP_LOADNIL */
 ,opmode(0, OpArgN, OpArgK, OpArgR, iABC)		/* OP_GETUPVAL */
 ,opmode(0, OpArgN, OpArgK, OpArgR, iABC)		/* OP_GETGLOBAL */
 ,opmode(0, OpArgU, OpArgK, OpArgU, iABx)		/* OP_GETTABLE */
 ,opmode(0, OpArgU, OpArgR, OpArgN, iABC)		/* OP_SETGLOBAL */
 ,opmode(0, OpArgU, OpArgR, OpArgR, iABC)		/* OP_SETUPVAL */
 ,opmode(0, OpArgU, OpArgU, OpArgN, iABC)		/* OP_SETTABLE */
 ,opmode(0, OpArgU, OpArgR, OpArgR, iABC)		/* OP_NEWTABLE */ // 0xa
 ,opmode(0, OpArgU, OpArgR, OpArgR, iABC)		/* OP_SELF */
 ,opmode(0, OpArgU, OpArgR, OpArgR, iABC)		/* OP_ADD */
 ,opmode(0, OpArgU, OpArgU, OpArgU, iABC)		/* OP_SUB */
 ,opmode(0, OpArgU, OpArgN, OpArgU, iABC)		/* OP_MUL */
 ,opmode(0, OpArgU, OpArgK, OpArgR, iABC)		/* OP_DIV */
 ,opmode(0, OpArgU, OpArgK, OpArgR, iABC)		/* OP_MOD */   // 0x10
 ,opmode(0, OpArgU, OpArgK, OpArgR, iABC)		/* OP_POW */
 ,opmode(0, OpArgU, OpArgK, OpArgR, iABC)		/* OP_UNM */
 ,opmode(0, OpArgU, OpArgK, OpArgR, iABC)		/* OP_NOT */
 ,opmode(0, OpArgU, OpArgK, OpArgR, iABC)		/* OP_LEN */
 ,opmode(0, OpArgU, OpArgK, OpArgR, iABC)		/* OP_CONCAT */ // 0x15
 ,opmode(0, OpArgU, OpArgU, OpArgU, iABC)		/* OP_JMP */
 ,opmode(0, OpArgU, OpArgU, OpArgU, iABC)		/* OP_EQ */
 ,opmode(0, OpArgU, OpArgU, OpArgU, iABC)		/* OP_LT */  //0x18
 ,opmode(0, OpArgU, OpArgK, OpArgN, iABx)		/* OP_LE */
 ,opmode(0, OpArgU, OpArgR, OpArgN, iABC)		/* OP_TEST */   //0x1A
 ,opmode(0, OpArgU, OpArgK, OpArgN, iABx)		/* OP_TESTSET */
 ,opmode(0, OpArgN, OpArgR, OpArgN, iAsBx)		/* OP_CALL */
 ,opmode(0, OpArgU, OpArgU, OpArgU, iABC)		/* OP_TAILCALL */ //0x1d
 ,opmode(0, OpArgU, OpArgU, OpArgU, iABC)		/* OP_RETURN */
 ,opmode(0, OpArgU, OpArgU, OpArgU, iABC)		/* OP_FORLOOP */
 ,opmode(0, OpArgU, OpArgU, OpArgU, iABC)		/* OP_FORPREP */ 
 ,opmode(0, OpArgU, OpArgU, OpArgU, iABC)		/* OP_TFORLOOP */  //0x21
 ,opmode(0, OpArgU, OpArgU, OpArgU, iABC)		/* OP_SETLIST */
 ,opmode(0, OpArgU, OpArgU, OpArgU, iABC)		/* OP_CLOSE */
 ,opmode(0, OpArgU, OpArgU, OpArgU, iABC)		/* OP_CLOSURE */
 ,opmode(0, OpArgU, OpArgU, OpArgU, iABC)		/* OP_VARARG */  //0x25
 ,opmode(0, OpArgU, OpArgU, OpArgN, iABC)
 ,opmode(0, OpArgU, OpArgU, OpArgN, iABC) 
 ,opmode(0, OpArgU, OpArgK, OpArgR, iABC)   //0x28
 ,opmode(0, OpArgU, OpArgK, OpArgR, iABC)
 ,opmode(0, OpArgU, OpArgK, OpArgR, iABC)
 ,opmode(0, OpArgU, OpArgK, OpArgR, iABC)
 ,opmode(0, OpArgU, OpArgK, OpArgR, iABC)
 ,opmode(0, OpArgU, OpArgK, OpArgR, iABC)
 ,opmode(0, OpArgU, OpArgK, OpArgR, iABC)
 ,opmode(0, OpArgU, OpArgK, OpArgR, iABC)
 ,opmode(0, OpArgU, OpArgK, OpArgR, iABC)
 ,opmode(0, OpArgU, OpArgK, OpArgR, iABC)
 ,opmode(0, OpArgU, OpArgK, OpArgR, iABC)
 ,opmode(0, OpArgU, OpArgK, OpArgR, iABC)
 ,opmode(0, OpArgU, OpArgU, OpArgU, iABC) //0x34
 ,opmode(0, OpArgU, OpArgR, OpArgN, iABC)
 ,opmode(0, OpArgU, OpArgR, OpArgN, iABC)
 ,opmode(0, OpArgU, OpArgR, OpArgN, iABC)
 ,opmode(0, OpArgN, OpArgK, OpArgR, iABC) //0x38
 ,opmode(0, OpArgN, OpArgK, OpArgR, iABC)
 ,opmode(0, OpArgN, OpArgK, OpArgR, iABC)
 ,opmode(0, OpArgN, OpArgK, OpArgR, iABC)
 ,opmode(0, OpArgU, OpArgU, OpArgU, iABC) //0x3c
 ,opmode(0, OpArgU, OpArgR, OpArgU, iABC)
 ,opmode(0, OpArgU, OpArgR, OpArgN, iAsBx)
 ,opmode(0, OpArgU, OpArgR, OpArgN, iAsBx) //0x3f
 ,opmode(0, OpArgU, OpArgU, OpArgR, iABC)
 ,opmode(0, OpArgU, OpArgN, OpArgN, iABC) // 0x41
 ,opmode(0, OpArgU, OpArgU, OpArgN, iABC)
 ,opmode(0, OpArgU, OpArgU, OpArgN, iABC) //0x43
 ,opmode(0, OpArgN, OpArgU, OpArgU, iABC)
 ,opmode(0, OpArgN, OpArgU, OpArgU, iABC)
 ,opmode(0, OpArgU, OpArgU, OpArgN, iABC)
 ,opmode(0, OpArgU, OpArgN, OpArgU, iABC) // 0x47
 ,opmode(0, OpArgU, OpArgR, OpArgN, iABC)
 ,opmode(0, OpArgU, OpArgR, OpArgK, iABC)
 ,opmode(0, OpArgU, OpArgK, OpArgR, iABC)
 ,opmode(0, OpArgU, OpArgU, OpArgU, iABC)
 ,opmode(0, OpArgN, OpArgR, OpArgN, iABx)
 ,opmode(0, OpArgU, OpArgN, OpArgU, iABC) //0x4d
 ,opmode(0, OpArgU, OpArgU, OpArgR, iABC)
 ,opmode(0, OpArgU, OpArgU, OpArgR, iABC)
 ,opmode(0, OpArgU, OpArgU, OpArgR, iABC)
 ,opmode(0, OpArgU, OpArgU, OpArgR, iABC) //0x51
 ,opmode(0, OpArgU, OpArgU, OpArgN, iABx)
 ,opmode(0, OpArgU, OpArgU, OpArgN, iABx)
 ,opmode(0, OpArgU, OpArgR, OpArgU, iABC)
 ,opmode(0, OpArgU, OpArgR, OpArgU, iABC)
 ,opmode(0, OpArgU, OpArgR, OpArgU, iABC)
 ,opmode(0, OpArgU, OpArgR, OpArgU, iABC)
 ,opmode(0, OpArgU, OpArgR, OpArgK, iABC)
 ,opmode(0, OpArgU, OpArgU, OpArgN, iABx)
 ,opmode(0, OpArgU, OpArgR, OpArgU, iABC)
 ,opmode(0, OpArgU, OpArgK, OpArgU, iABx)


};

