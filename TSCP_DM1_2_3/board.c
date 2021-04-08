/*
 *	BOARD.C
 *	Tom Kerrigan's Simple Chess Program (TSCP)
 *
 *	Copyright 1997 Tom Kerrigan
 */

#include <stdlib.h>
#include <assert.h>
#include "defs.h"
#include "data.h"
#include "protos.h"

/* init_board() sets the board to the initial game state. */

void init_board()
{
	int i;
	for (i = 0; i < 64; ++i) {
		color[i] = init_color[i];
		piece[i] = init_piece[i];
	}

	syncBoard();
	checkBoard();

	side = LIGHT;
	xside = DARK;
	castle = 15;
	ep = -1;
	fifty = 0;
	ply = 0;
	hply = 0;

	//init_hash(); //Fait dans le main
	set_hash();  /* init_hash() must be called before this function */

	first_move[0] = 0;
}

void syncBoard()
{
	memset(board, EMPTY_BOARD, sizeof(board));
	for (int i = 0; i <= 32; i++) pospiece[i] = PIECE_DEAD;

	int minW = 2;
	int minB = 18;

	for (int i = 0; i < 64; ++i) {
		if (piece[i] != EMPTY) { // Si à la case N°i on trouve une piece		
			if (color[i] == LIGHT) { //Si c'est une piece blanche
				if (piece[i] == KING) { //Si c'est le roi blanc
					board[i] = 1;
					pospiece[1] = i;
				}
				else {
					board[i] = minW;
					pospiece[minW] = i;
					minW++;
				}
			}
			else {	//Sinon si c'est une piece noir
				if (piece[i] == KING) {	//Si c'est le roi noir
					board[i] = 17;
					pospiece[17] = i;
				}
				else {
					board[i] = minB;
					pospiece[minB] = i;
					minB++;
				}
			}
		}
	}
}

int checkBoard()
{ // Cette fonction peut être plus ou moins longue, dépendamment de vos besoins en matière de debug	
	for (int i = 0; i < 64; ++i) // Pour chaque case du plateau
	{
		//Si la piece N°i n'est pas vide alors que le plateau est vide
		if (piece[i] != EMPTY && board[i] == 0) { //piece[11]=0(PAWN) && board[11]=0(empty)
			
			return 0;
		}

		//Si la piece N°i est vide alors que le plateau ne l'est pas
		if (piece[i] == EMPTY && board[i] != EMPTY_BOARD) // Piece[16]=6 & board[16]=0 || piece[0]=6 & board[0]=18
		{
			return 0;
		}

		//Si la couleur de la piece à la case i est blanche alors que la plateau indique une piece noir ou une case vide
		if (color[i] == LIGHT && (board[i] > 16 || board[i] == 0))
		{ 
			return 0;
		}

		//Si la couleur de la piece à la case i est noir alors que que le plateau indique une piece blanche ou une case vide
		if (color[i] == DARK && board[i] < 17)
		{
			return 0;
		}

		//Si la piece à la case i est différente de la case i
		if (board[i] != EMPTY_BOARD && pospiece[board[i]] != i) 
		{ 
			return 0;
		}
	}
	for (int i = 1; i < 33; ++i) //Pour chaque piece
	{
		//Si la piece N°posPiece n'est pas morte et que la piece à la position de la piece N°posPiece n'est pas la piece N°posPiece 
		if (pospiece[i] != PIECE_DEAD && board[pospiece[i]] != i) //pospiece[6]=27 != piece_dead && board[27]=21
		{
			return 0;
		}

		//Si la piece N°posPiece n'est pas morte et que la piece à la position de la piece N°posPiece est vide
		if (pospiece[i] != PIECE_DEAD && piece[pospiece[i]] == EMPTY)
		{
			return 0;
		}

		//Si la piece N°posPiece n'est pas morte et que la couleur de la piece à la position de la piece N°posPiece est différente de la couleur de la piece N°posPiece
		if (pospiece[i] != PIECE_DEAD && (color[pospiece[i]] == LIGHT && i > 16 || color[pospiece[i]] == DARK && i < 17))
		{
			return 0;
		}
	}
	//Si aucune des condition n'est vrai, alors c'est que tout va bien :)
	return 1;
}

/* hash64 */
unsigned long y[55];
int j, k;
int initAl = 1;
static unsigned long Aleatoire(void)
{
	unsigned long ul;
	if (initAl)
	{
		int i;
		initAl = 0;
		for (i = 0; i < 55; i++) y[i] = x[i];
		j = 24 - 1;
		k = 55 - 1;
	}
	ul = (y[k] += y[j]);
	if (--j < 0) j = 55 - 1;
	if (--k < 0) k = 55 - 1;
	return(ul);
}
static int64 Aleatoire64() {
	int64 rand = Aleatoire();
	rand << 32;
	rand |= (int64)Aleatoire();
	return rand;
}
	
/* init_hash() initializes the random numbers used by set_hash(). */

void init_hash()
{
	int i, j, k;

	srand(0);
	for (i = 0; i < 2; ++i)
		for (j = 0; j < 6; ++j)
			for (k = 0; k < 64; ++k)
				hash_piece[i][j][k] = Aleatoire64();

	hash_side = Aleatoire64();

	for (i = 0; i < 64; ++i)
		hash_ep[i] = Aleatoire64();
}

/* hash_rand() XORs some shifted random numbers together to make sure
   we have good coverage of all 32 bits. (rand() returns 16-bit numbers
   on some systems.) */

unsigned int hash_rand()
{
	unsigned int i;
	unsigned int r = 0;

	for (i = 0; i < 32; ++i)
		r ^= (unsigned int)(rand() << i);

	return r;
}

/* set_hash() uses the Zobrist method of generating a unique number (hash)
   for the current chess position. Of course, there are many more chess
   positions than there are 32 bit numbers, so the numbers generated are
   not really unique, but they're unique enough for our purposes (to detect
   repetitions of the position). 
   The way it works is to XOR random numbers that correspond to features of
   the position, e.g., if there's a black knight on B8, hash is XORed with
   hash_piece[BLACK][KNIGHT][B8]. All of the pieces are XORed together,
   hash_side is XORed if it's black's move, and the en passant square is
   XORed if there is one. (A chess technicality is that one position can't
   be a repetition of another if the en passant state is different.) */

void set_hash()
{
	int i;

	hash = 0;	
	for (i = 0; i < 64; ++i)
		if (color[i] != EMPTY)
			hash ^= hash_piece[color[i]][piece[i]][i];

	if (side == DARK)
		hash ^= hash_side;
	if (ep != -1)
		hash ^= hash_ep[ep];
}

/* in_check() returns TRUE if side s is in check and FALSE
   otherwise. It just scans the board to find side s's king
   and calls attack() to see if it's being attacked. */

BOOL in_check(int s)
{
	if(s==LIGHT) return attack(pospiece[1], DARK); //Return si le roi blanc est en echec
	else return attack(pospiece[17], LIGHT); //Return si le roi noir est en echec
}

/* attack() returns TRUE if square sq is being attacked by side
   s and FALSE otherwise. */

BOOL attack(int sq, int s)
{
	int i, j, n;

	//Déclaration de l'index à partir duquel commencer
	int colorStart = 1; //Si ce sont les blanc on commence à 1
	if (s == DARK) colorStart = 17; //Si ce sont les noirs on commence à 17
	int colorEnd = colorStart + 16; //Dans les deux cas, on fini à < +16

	int index; //Index de la piece dans pospiece
	for (index = colorStart; index < colorEnd; ++index) {
		i = pospiece[index]; //La piece index est à la case i
		if (i == PIECE_DEAD) continue; // il se passe quoi si pospiece[index] = case 0 ? | la tour noir par exmple		
		if (piece[i] == PAWN) {
			if (s == LIGHT) {
				if (COL(i) != 0 && i - 9 == sq) return TRUE;
				if (COL(i) != 7 && i - 7 == sq) return TRUE;
			}
			else {
				if (COL(i) != 0 && i + 7 == sq) return TRUE;
				if (COL(i) != 7 && i + 9 == sq) return TRUE;
			}
		}
		else {
			if (canAttack[piece[i]][i][sq]){
				for (j = 0; j < offsets[piece[i]]; ++j) {
					for (n = i;;) {
						n = mailbox[mailbox64[n] + offset[piece[i]][j]];
						if (n == -1) break;
						if (n == sq) return TRUE;
						if (color[n] != EMPTY) break;
						if (!slide[piece[i]]) break;
					}
				}
			}
		}
	}
	return FALSE;
}

/* gen() generates pseudo-legal moves for the current position.
   It scans the board to find friendly pieces and then determines
   what squares they attack. When it finds a piece/square
   combination, it calls gen_push to put the move on the "move
   stack." */

void gen()
{
	int i, j, n;

	/* so far, we have no moves for the current ply */
	first_move[ply + 1] = first_move[ply];

	//Déclaration de l'index à partir duquel commencer
	int colorStart = 1; //Si ce sont les blanc on commence à 1
	if (side == DARK) colorStart = 17; //Si ce sont les noirs on commence à 17
	int colorEnd = colorStart + 16; //Dans les deux cas, on fini à < +16

	int index; //Index de la piece dans pospiece
	for (index = colorStart; index < colorEnd; ++index){
		i = pospiece[index]; //La piece index est à la case i
		if (i == PIECE_DEAD) continue; // il se passe quoi si pospiece[index] = case 0 ? | la tour noir par exmple
		if (piece[i] == PAWN) {
			if (side == LIGHT) {
				if (COL(i) != 0 && color[i - 9] == DARK) gen_push(i, i - 9, 17);
				if (COL(i) != 7 && color[i - 7] == DARK) gen_push(i, i - 7, 17);
				if (color[i - 8] == EMPTY) {
					gen_push(i, i - 8, 16);
					if (i >= 48 && color[i - 16] == EMPTY) gen_push(i, i - 16, 24);
				}
			}
			else {
				if (COL(i) != 0 && color[i + 7] == LIGHT) gen_push(i, i + 7, 17);
				if (COL(i) != 7 && color[i + 9] == LIGHT) gen_push(i, i + 9, 17);
				if (color[i + 8] == EMPTY) {
					gen_push(i, i + 8, 16);
					if (i <= 15 && color[i + 16] == EMPTY) gen_push(i, i + 16, 24);
				}
			}
		}
		else {
			for (j = 0; j < offsets[piece[i]]; ++j) {
				for (n = i;;) {
					n = mailbox[mailbox64[n] + offset[piece[i]][j]];
					if (n == -1) break;
					if (color[n] != EMPTY) {
						if (color[n] == xside)
							gen_push(i, n, 1);
						break;
					}
					gen_push(i, n, 0);
					if (!slide[piece[i]]) break;
				}
			}
		}
	}

	/* generate castle moves */
	if (side == LIGHT) {
		if (castle & 1)
			gen_push(E1, G1, 2);
		if (castle & 2)
			gen_push(E1, C1, 2);
	}
	else {
		if (castle & 4)
			gen_push(E8, G8, 2);
		if (castle & 8)
			gen_push(E8, C8, 2);
	}

	/* generate en passant moves */
	if (ep != -1) {
		if (side == LIGHT) {
			if (COL(ep) != 0 && color[ep + 7] == LIGHT && piece[ep + 7] == PAWN)
				gen_push(ep + 7, ep, 21);
			if (COL(ep) != 7 && color[ep + 9] == LIGHT && piece[ep + 9] == PAWN)
				gen_push(ep + 9, ep, 21);
		}
		else {
			if (COL(ep) != 0 && color[ep - 9] == DARK && piece[ep - 9] == PAWN)
				gen_push(ep - 9, ep, 21);
			if (COL(ep) != 7 && color[ep - 7] == DARK && piece[ep - 7] == PAWN)
				gen_push(ep - 7, ep, 21);
		}
	}
}

/* gen_caps() is basically a copy of gen() that's modified to
   only generate capture and promote moves. It's used by the
   quiescence search. */

void gen_caps()
{
	int i, j, n;

	first_move[ply + 1] = first_move[ply];

	//Déclaration de l'index à partir duquel commencer
	int colorStart = 1; //Si ce sont les blanc on commence à 1
	if (side == DARK) colorStart = 17; //Si ce sont les noirs on commence à 17
	int colorEnd = colorStart + 16; //Dans les deux cas, on fini à < +16

	int index; //Index de la piece dans pospiece
	for (index = colorStart; index < colorEnd; ++index) {
		i = pospiece[index]; //La piece index est à la case i
		if (i == PIECE_DEAD) continue; // il se passe quoi si pospiece[index] = case 0 ? | la tour noir par exmple
		if (piece[i] == PAWN) {
			if (side == LIGHT) {
				if (COL(i) != 0 && color[i - 9] == DARK)
					gen_push(i, i - 9, 17);
				if (COL(i) != 7 && color[i - 7] == DARK)
					gen_push(i, i - 7, 17);
				if (i <= 15 && color[i - 8] == EMPTY)
					gen_push(i, i - 8, 16);
			}
			if (side == DARK) {
				if (COL(i) != 0 && color[i + 7] == LIGHT)
					gen_push(i, i + 7, 17);
				if (COL(i) != 7 && color[i + 9] == LIGHT)
					gen_push(i, i + 9, 17);
				if (i >= 48 && color[i + 8] == EMPTY)
					gen_push(i, i + 8, 16);
			}
		}
		else {
			for (j = 0; j < offsets[piece[i]]; ++j) {
				for (n = i;;) {
					n = mailbox[mailbox64[n] + offset[piece[i]][j]];
					if (n == -1) break;
					if (color[n] != EMPTY) {
						if (color[n] == xside) gen_push(i, n, 1);
						break;
					}
					if (!slide[piece[i]]) break;
				}
			}
		}
	}
	if (ep != -1) {
		if (side == LIGHT) {
			if (COL(ep) != 0 && color[ep + 7] == LIGHT && piece[ep + 7] == PAWN)
				gen_push(ep + 7, ep, 21);
			if (COL(ep) != 7 && color[ep + 9] == LIGHT && piece[ep + 9] == PAWN)
				gen_push(ep + 9, ep, 21);
		}
		else {
			if (COL(ep) != 0 && color[ep - 9] == DARK && piece[ep - 9] == PAWN)
				gen_push(ep - 9, ep, 21);
			if (COL(ep) != 7 && color[ep - 7] == DARK && piece[ep - 7] == PAWN)
				gen_push(ep - 7, ep, 21);
		}
	}
}

/* gen_push() puts a move on the move stack, unless it's a
   pawn promotion that needs to be handled by gen_promote().
   It also assigns a score to the move for alpha-beta move
   ordering. If the move is a capture, it uses MVV/LVA
   (Most Valuable Victim/Least Valuable Attacker). Otherwise,
   it uses the move's history heuristic value. Note that
   1,000,000 is added to a capture move's score, so it
   always gets ordered above a "normal" move. */

void gen_push(int from, int to, int bits)
{
	gen_t *g;
	
	if (bits & 16) {
		if (side == LIGHT) {
			if (to <= H8) {
				gen_promote(from, to, bits);
				return;
			}
		}
		else {
			if (to >= A1) {
				gen_promote(from, to, bits);
				return;
			}
		}
	}
	g = &gen_dat[first_move[ply + 1]++];
	g->m.b.from = (char)from;
	g->m.b.to = (char)to;
	g->m.b.promote = 0;
	g->m.b.bits = (char)bits;
	if (color[to] != EMPTY)
		g->score = 1000000 + (piece[to] * 10) - piece[from];
	else
		g->score = history[from][to];
}


/* gen_promote() is just like gen_push(), only it puts 4 moves
   on the move stack, one for each possible promotion piece */

void gen_promote(int from, int to, int bits)
{
	int i;
	gen_t *g;
	
	for (i = KNIGHT; i <= QUEEN; ++i) {
		g = &gen_dat[first_move[ply + 1]++];
		g->m.b.from = (char)from;
		g->m.b.to = (char)to;
		g->m.b.promote = (char)i;
		g->m.b.bits = (char)(bits | 32);
		g->score = 1000000 + (i * 10);
	}
}

/* makemove() makes a move. If the move is illegal, it
   undoes whatever it did and returns FALSE. Otherwise, it
   returns TRUE. */

BOOL makemove(move_bytes m)
{
	int64 hash_temp = 0;

	/* test to see if a castle move is legal and move the rook
	   (the king is moved with the usual move code later) */
	if (m.bits & 2) { // cas roque
		int from, to;
		if (in_check(side))
			return FALSE;
		switch (m.to) {
			case 62:
				if (color[F1] != EMPTY || color[G1] != EMPTY ||
						attack(F1, xside) || attack(G1, xside))
					return FALSE;
				from = H1;
				to = F1;
				break;
			case 58:
				if (color[B1] != EMPTY || color[C1] != EMPTY || color[D1] != EMPTY ||
						attack(C1, xside) || attack(D1, xside))
					return FALSE;
				from = A1;
				to = D1;
				break;
			case 6:
				if (color[F8] != EMPTY || color[G8] != EMPTY ||
						attack(F8, xside) || attack(G8, xside))
					return FALSE;
				from = H8;
				to = F8;
				break;
			case 2:
				if (color[B8] != EMPTY || color[C8] != EMPTY || color[D8] != EMPTY ||
						attack(C8, xside) || attack(D8, xside))
					return FALSE;
				from = A8;
				to = D8;
				break;
			default:  /* shouldn't get here */
				from = -1;
				to = -1;
				break;
		}
		//On retire le tour de sa case de départ et on ajoute la tour à la case d'arrivée
		hash_temp = hash_piece[side][ROOK][from] ^ hash_piece[side][ROOK][to];

		board[to] = board[from];
		board[from] = EMPTY_BOARD;
		pospiece[board[to]] = to;

		color[to] = color[from];
		piece[to] = piece[from];
		color[from] = EMPTY;
		piece[from] = EMPTY;
	}

//#ifdef DEBUG
//	assert(checkBoard());
//#endif // DEBUG

	/* back up information so we can take the move back later. */
	hist_dat[hply].m.b = m;
	hist_dat[hply].capture = piece[(int)m.to]; //Piece capturé
	hist_dat[hply].captureBoard = board[(int)m.to]; //Index de la piece capturé
	hist_dat[hply].castle = castle;
	hist_dat[hply].ep = ep;
	hist_dat[hply].fifty = fifty;
	hist_dat[hply].hash = hash; // On sauvegarde le hash
	++ply;
	++hply;

	hash ^= hash_temp; //Si rook, ajoute le hash

	/* update the castle, en passant, and
	   fifty-move-draw variables */
	castle &= castle_mask[(int)m.from] & castle_mask[(int)m.to];
	if (m.bits & 8) {
		if (side == LIGHT)
			ep = m.to + 8;
		else
			ep = m.to - 8;
	}
	else
		ep = -1;
	if (m.bits & 17)
		fifty = 0;
	else
		++fifty;

	/* move the piece */

	//On retire la piece de la où elle est
	hash ^= hash_piece[side][piece[(int)m.from]][(int)m.from];

	color[(int)m.to] = side;
	if (board[(int)m.to] != EMPTY_BOARD) { //Si la case d'arrivée n'est pas vide, on capture
		pospiece[board[(int)m.to]] = PIECE_DEAD; 		

		//On retire la piece capturée
		hash ^= hash_piece[xside][piece[(int)m.to]][(int)m.to];
	}
	if (m.bits & 32) { //Si c'est une promotion
		piece[(int)m.to] = m.promote;
	}
	else { //Sinon c'est un déplacement
		piece[(int)m.to] = piece[(int)m.from];
	}

	//On ajoute la piece déplacée
	hash ^= hash_piece[side][piece[(int)m.to]][(int)m.to];

	color[(int)m.from] = EMPTY;
	piece[(int)m.from] = EMPTY;
	
	board[(int)m.to] = board[(int)m.from]; //La case d'arrivée prend la valeur de la case de départ
	board[(int)m.from] = EMPTY_BOARD; //La case de départ est maintenant vide
	pospiece[board[(int)m.to]] = (int)m.to; //La position de la piece à la case d'arrivée est la case d'arrivée

//#ifdef DEBUG
//	assert(checkBoard());
//#endif // DEBUG

	/* erase the pawn if this is an en passant move */
	if (m.bits & 4) {
		if (side == LIGHT) {
			//On retire le pion si c'est un en passant move
			hash ^= hash_piece[xside][PAWN][(int)m.to + 8];

			//hist_dat[hply].capture = piece[(int)m.to + 8]; Inutile car on sait d'avance que c'est un pion
			hist_dat[hply-1].captureBoard = board[(int)m.to + 8]; // On remplace la valeur précédemment sauvegardé

			color[m.to + 8] = EMPTY;
			piece[m.to + 8] = EMPTY;

			pospiece[board[(int)m.to + 8]] = PIECE_DEAD;
			board[(int)m.to + 8] = EMPTY_BOARD;
		}
		else {
			//On retire le pion si c'est un en passant move
			hash ^= hash_piece[xside][PAWN][(int)m.to - 8];

			//hist_dat[hply].capture = piece[(int)m.to - 8]; Inutile car on sait d'avance que c'est un pion
			hist_dat[hply-1].captureBoard = board[(int)m.to - 8];// On remplace la valeur précédemment sauvegardé

			color[m.to - 8] = EMPTY;
			piece[m.to - 8] = EMPTY;

			pospiece[board[(int)m.to - 8]] = PIECE_DEAD;
			board[(int)m.to - 8] = EMPTY_BOARD;
		}
	}

//#ifdef DEBUG
//	assert(checkBoard());
//#endif // DEBUG

	/* switch sides and test  for legality (if we can capture
	   the other guy's king, it's an illegal position and
	   we need to take the move back) */
	side ^= 1;
	xside ^= 1;

	if (in_check(xside)) {
		takeback();
		return FALSE;
	}

	hash ^= hash_side; // A faire tout le temps pour ajouter sur un side et retirer sur l'autre
		
	if (ep != -1) {//Si ep != -1
		hash ^= hash_ep[ep]; // On ajoute le hash
	}
	if (hist_dat[hply - 1].ep != -1) {//Si l'ep précédent est différent de -1
		hash ^= hash_ep[hist_dat[hply - 1].ep]; //On retire le hash
	}

//#ifdef DEBUG
//	assert(hash == get_hash(m));
//#endif	

	return TRUE;
}

int64 get_hash(move_bytes m) {
	int i; //Initialisation de la variable pour les cases du plateaux
	int64 h = 0; //Initialisation d'un hash à 0
	int64 diff_h = 0;
	for (i = 0; i < 64; ++i) { //Pour chaque case du tableau
		if (color[i] != EMPTY) { //Si la case n'est pas vide
			//Execution de l'opération XOR entre le hash 0 et le hash_piece[color][piece][case ]/* indexed by piece [color][type][square] */
			h ^= hash_piece[color[i]][piece[i]][i]; //Notre hash prend la valeur du hash créé pour cette couleur, avec ce type de piece, sur cette case
		}
	}

	if (side == DARK) h ^= hash_side;
	if (ep != -1) h ^= hash_ep[ep];

	hist_t hyst = hist_dat[hply - 1];
	diff_h = hash ^ h;
	if (diff_h != 0) {
		for (int k = 0; k < 2; k++) {
			for (int kk = 0; kk < 6; kk++) {
				for (int kkk = 0; kkk < 64; kkk++) {
					if (diff_h == hash_piece[k][kk][kkk]) {
						return diff_h;
					}
				}
			}
		}
		for (i = 0; i < 64; ++i) {
			if (diff_h == hash_ep[i]) {
				return diff_h;
			}
		}
	}
	return h;
}

/* takeback() is very similar to makemove(), only backwards :)  */

void takeback()
{
	move_bytes m;

	side ^= 1;
	xside ^= 1;
	--ply;
	--hply;
	m = hist_dat[hply].m.b;
	castle = hist_dat[hply].castle;
	ep = hist_dat[hply].ep;
	fifty = hist_dat[hply].fifty;
	hash = hist_dat[hply].hash;

#ifdef DEBUG
	assert(checkBoard());
#endif // DEBUG

	color[(int)m.from] = side;

	if (m.bits & 32) //Si le move est une promotion
		piece[(int)m.from] = PAWN;
	else
		piece[(int)m.from] = piece[(int)m.to];

	if (hist_dat[hply].capture == EMPTY) { //Si le move n'est pas une capture
		color[(int)m.to] = EMPTY;
		piece[(int)m.to] = EMPTY;

		board[(int)m.from] = board[(int)m.to];
		board[(int)m.to] = EMPTY_BOARD;
		pospiece[board[(int)m.from]] = (int)m.from;
	}
	else {
		//Piece déplacé
		//Color & Piece déjà fait au dessus
		board[(int)m.from] = board[(int)m.to];
		pospiece[board[(int)m.from]] = (int)m.from;

		//Piece capturé
		if (!(m.bits & 4)) { //Si pas en passant
			color[(int)m.to] = xside;
			piece[(int)m.to] = hist_dat[hply].capture;
			board[(int)m.to] = hist_dat[hply].captureBoard;
			pospiece[board[(int)m.to]] = (int)m.to;
		}
	}

#ifdef DEBUG
	assert(checkBoard());
#endif // DEBUG

	if (m.bits & 2) { //Si le move est un roque
		int from, to;

		switch(m.to) {
			case 62:
				from = F1;
				to = H1;
				break;
			case 58:
				from = D1;
				to = A1;
				break;
			case 6:
				from = F8;
				to = H8;
				break;
			case 2:
				from = D8;
				to = A8;
				break;
			default:  /* shouldn't get here */
				from = -1;
				to = -1;
				break;
		}
		color[to] = side;
		piece[to] = ROOK;
		color[from] = EMPTY;
		piece[from] = EMPTY;

		board[to] = board[from];
		board[from] = EMPTY_BOARD;
		pospiece[board[to]] = to;
	}
#ifdef DEBUG
	assert(checkBoard());
#endif // DEBUG

	if (m.bits & 4) { //Si le move est un en passant
		if (side == LIGHT) {
			color[m.to + 8] = xside;
			piece[m.to + 8] = PAWN;

			board[(int)m.to + 8] = hist_dat[hply].captureBoard;
			pospiece[board[(int)m.to + 8]] = (int)m.to + 8;
		}
		else {
			color[m.to - 8] = xside;
			piece[m.to - 8] = PAWN;

			board[(int)m.to - 8] = hist_dat[hply].captureBoard;
			pospiece[board[(int)m.to - 8]] = (int)m.to - 8;
		}
	}

#ifdef DEBUG
	assert(checkBoard());
#endif // DEBUG
}


HtTyp* getTT()
{
	HtTyp* pTransp;
	pTransp = &HT[hash % (HT_SIZE - 1)];

	if (pTransp->hash == hash)
		return pTransp;
	return NULL;
}

void putTT(int depth, int eval, move m, int alpha, int beta)
{
	HtTyp* pTransp;
	pTransp = &HT[hash % (HT_SIZE - 1)];

	if (pTransp->depth <= depth)
	{
		pTransp->hash = hash;
		pTransp->depth = depth;
		pTransp->move = m;

		if (eval <= alpha)
		{
			pTransp->flag = FLAG_U_BOUND;
			SCALE_MATE_VALUE(eval);
			pTransp->score = eval;
		}
		else
		{
			if (eval >= beta)
			{
				pTransp->flag = FLAG_L_BOUND;
				SCALE_MATE_VALUE(eval);
				pTransp->score = eval;
			}
			else
			{
				SCALE_MATE_VALUE(eval);
				pTransp->score = eval;
				pTransp->flag = FLAG_VALID;
			}
		}
	}
}

void initHT()
{
	memset(HT, 0, sizeof(HT));
}

HtLearning* getLearn(int64 h) {
	HtLearning* htLearning;
	htLearning = &HTLearning[h % (HT_SIZE - 1)];
	
	if (htLearning->hash == h)
		return htLearning;
	return NULL;
}

void learn(int64 h, int depth, int eval) {
	HtLearning* htLearning;
	htLearning = &HTLearning[h % (HT_SIZE - 1)];

	if (htLearning->depth <= depth)
	{
		htLearning->hash = h;
		htLearning->depth = depth;
		htLearning->score = eval;
	}
}

void initHtLearning()
{
	memset(HTLearning, 0, sizeof(HTLearning));
}