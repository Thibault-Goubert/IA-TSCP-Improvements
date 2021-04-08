/*
 *	DATA.H
 *	Tom Kerrigan's Simple Chess Program (TSCP)
 *
 *	Copyright 1997 Tom Kerrigan
 */


/* this is basically a copy of data.c that's included by most
   of the source files so they can use the data.c variables */

extern int color[64];
extern int piece[64];
extern int side;
extern int xside;
extern int castle;
extern int ep;
extern int fifty;
extern int ply;
extern int hply;
extern gen_t gen_dat[GEN_STACK];
extern int first_move[MAX_PLY];
extern int history[64][64];
extern hist_t hist_dat[HIST_STACK];
extern int max_time;
extern int max_depth;
extern int start_time;
extern int stop_time;
extern int nodes;
extern move pv[MAX_PLY][MAX_PLY];
extern int pv_length[MAX_PLY];
extern BOOL follow_pv;
extern int64 hash;
extern int64 hash_piece[2][6][64];
extern int64 hash_side;
extern int64 hash_ep[64];
extern int mailbox[120];
extern int mailbox64[64];
extern BOOL slide[6];
extern int offsets[6];
extern int offset[6][8];
extern int castle_mask[64];
extern char piece_char[6];
extern int init_color[64];
extern int init_piece[64];

// Etape 1
extern int board[64];
extern int pospiece[33];

// Opti2
int canAttack[6][64][64];

//DM3
extern unsigned long x[55];
extern HtTyp HT[HT_SIZE];
extern HtLearning HTLearning[HT_SIZE];