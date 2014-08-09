#include <vector>
#include <iostream>
#include <set>
#include <cstdio>
#include <queue>
#include <cstdio>
#include <cstdlib>
#include <cassert>
#include <ctime>
#include <algorithm>
#include <unistd.h>
#include <tuple>
#include <algorithm>
#include <limits>
#include <map>
#include <unordered_map>
#include <random>

using namespace std;
typedef unsigned int ui;
typedef unsigned long long ull;

random_device seed_gen;
mt19937 eng(seed_gen());

inline int getm(ull board, int x, int y) {
    int k = x*16+y*4;
    return (board>>k) & 0xf;
}
inline ull setm(ull board, int x, int y, ull d) {
    int k = x*16+y*4;
    return (board & ~(0xfULL<<k)) | (d<<k);
}

inline ui getr(ull board, int x) {
    return (board>>(x*16))&0xffff;
}
inline ull setr(ull board, int x, ui d) {
    int k = x*16;
    return (board & ~(0xffffULL<<k)) | ((ull)(d)<<k);
}

inline ui getc(ull board, int y) {
    ui r = 0;
    ui mask = 0xf;
    board >>= 4*y;
    for (int i = 0; i < 4; i++) {
        r |= board & mask;
        mask <<= 4;
        board >>= 12;
    }
    return r;
}
inline ull setc(ull board, int y, ull d) {
    ull mask = 0xfULL<<(y*4);
    d <<= 4*y;
    for (int i = 0; i < 4; i++) {
        board = (board & ~(mask)) | (d&mask);
        d <<= 12;
        mask <<= 16;
    }
    return board;
}

uint exihash[1<<16], slihash[1<<16], slibhash[1<<16], canhash[1<<16];
void init() {
    for (ull i = 0; i < (1<<16); i++) {
        for (int j = 0; j < 4; j++) {
            if (getm(i, 0, j)) exihash[i]++;
        }
        int r = 0;
        for (int j = 0; j < 3; j++) {
            int u1 = getm(i, 0, j), u2 = getm(i, 0, j+1);
            if (u1 && u1==u2) r++;
        }
        canhash[i] = r;
        int c = 0;
        ull ii = i;
        for (int j = 1; j < 4; j++) {
            int d = getm(ii, 0, j);
            if (!d) continue;
            for (int k = j-1; k >= c; k--) {
                int u = getm(ii, 0, k);
                if (!u) {
                    ii = setm(ii, 0, k+1, 0);
                    ii = setm(ii, 0, k, d);
                    continue;
                }
                if (d != u) break;
                ii = setm(ii, 0, k+1, 0);
                ii = setm(ii, 0, k, d+1);
                c = k+1;
            }
        }
        slihash[i] = ii;
        c = 3;
        ii = i;
        for (int j = 2; j >= 0; j--) {
            int d = getm(ii, 0, j);
            if (!d) continue;
            for (int k = j+1; k <= c; k++) {
                int u = getm(ii, 0, k);
                if (!u) {
                    ii = setm(ii, 0, k-1, 0);
                    ii = setm(ii, 0, k, d);
                    continue;
                }
                if (d != u) break;
                ii = setm(ii, 0, k-1, 0);
                ii = setm(ii, 0, k, d+1);
                c = k-1;
            }
        }
        slibhash[i] = ii;
    }
}

inline ull slided(ull board, int dir) {
    switch (dir) {
    case 0:
        for (int i = 0; i < 4; i++) {
            board=setr(board, i, slihash[getr(board,i)]);
        }
        break;
    case 1:
        for (int j = 0; j < 4; j++) {
            board=setc(board, j, slihash[getc(board,j)]);
        }
        break;
    case 2:
        for (int i = 0; i < 4; i++) {
            board=setr(board, i, slibhash[getr(board,i)]);
        }
        break;
    case 3:
        for (int j = 0; j < 4; j++) {
            board=setc(board, j, slibhash[getc(board,j)]);
        }
        break;
    }  
    return board;
}

bool is_fill(ull board) {
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            if (!getm(board, i, j)) return false;
        }
    }
    return true;
}


inline int num_exist(ull board) {
    int r = 0;
    for (int i = 0; i < 4; i++) {
        r += exihash[getr(board, i)];
    }
    return r;
}

inline int can_slide(ull board) {
    int r = 0;
    for (int i = 0; i < 4; i++) {
        r += canhash[getr(board, i)];
        r += canhash[getc(board, i)];
    }
    return r;
}

inline double calp(ull board) {
    double u = (16-num_exist(board));
    double u2 = can_slide(board);
    return u+u2;
}

double calc(ull board, int dps) {
    double p = 0.0;
    int u = 0;
    int ne;
    const static int k[] = {0, 10, 1};
    ull b, b2;
    double p2, p3;
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            if (getm(board, i, j)) continue;
            for (int d = 1; d <= 2; d++) {
                u++;
                b = setm(board, i, j, d);
                if (!dps) {
                    p += calp(b)*k[d];
                } else {
                    p2 = -100000;
                    for (int k = 0; k < 4; k++) {
                        b2 = slided(b, k);
                        if (b == b2) continue;
                        ne = 1+num_exist(b2)/6;
                        p3 = calc(b2, min(ne, dps-1));
                        p2 = max(p2, p3);
                    }
                    p += p2*k[d];
                }
            }
        }
    }
    return p/u;
}

int solve(ull board) {
    ull b;
    int r = 0;
    double p = -10000000;
    for (int i = 0; i < 4; i++) {
        b = slided(board, i);
        if (b == board) continue;
        double np = calc(b, 1+num_exist(b)/6);
        if (p < np) {
            p = np;
            r = i;
        }
    }
    return r;
}

int pc[] = {
    223, 1,
    222, 1,
    215, 0,
    209, 0,
    203, 0,
    202, 0,
    221, 1,
    220, 1,
    228, 1,
    227, 1,
    226, 1,
    232, 0,
};
void boardp(ull board) {
    for (int x = 0; x < 4; x++) {
        for (int y = 0; y < 4; y++) {
            int u = getm(board, x, y);
            int d = 1<<u;
            if (d == 1) d = 0;
            if (u >= 1 && u <= 12) {
                printf("\x1b[48;5;%dm", pc[(u-1)*2]);
                if (pc[(u-1)*2+1]) {
                    printf("\x1b[38;5;243m");
                }
            }
            printf("%4d", d);
            printf("\x1b[39m");
            printf("\x1b[49m");
            printf(" ");
        }
        printf("\n");
    }
    printf("\n");
}

bool is_end(ull board) {
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            if (!getm(board, i, j)) return false;
        }
    }
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 3; j++) {
            if (getm(board, i, j) == getm(board, i, j+1)) return false;
        }
    }
    for (int j = 0; j < 4; j++) {
        for (int i = 0; i < 3; i++) {
            if (getm(board, i, j) == getm(board, i+1, j)) return false;
        }
    }
    return true;
}

ull addrp(ull board, int d) {
    uniform_int_distribution<int> rd(0,3);
    while (true) {
        int x = rd(eng), y = rd(eng);
        if (!getm(board, x, y)) {
            board = setm(board, x, y, d);
            break;
        }
    }
    return board;
}

ull addr(ull board) {
    if (is_fill(board)) return board;
    uniform_int_distribution<int> rd(0, 9);
    return addrp(board, rd(eng) == 0 ? 2 : 1);
}

int main() {
    init();
    ull board = 0;
    board = addrp(board, 1);
    board = addrp(board, 1);
    while (true) {
        boardp(board);
        if (is_end(board)) {
            break;
        }
        int n = solve(board);
        ull b = slided(board, n);
        if (b == board) assert(false);
        board = b;
        board = addr(board);
        //sleep(1);
    }
    printf("End\n");
    return 0;
}
