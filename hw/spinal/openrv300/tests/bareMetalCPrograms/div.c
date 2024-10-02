int main() {
    int a = 12;
    int b = 4;
    int c = 3;
    int f = -11;
    int d = f % c; /* -2 */
    int e = (-f) % (-c); /* 2 */


	int a1 = (a / c) * b / (-d) / e * 100, b1 = 24;
	while(b1 != 0){
		int r = a1 % b1;
		a1 = b1;
		b1 = r;
	}

    int neg = -1;
	int overflow = (1 << 31);
	overflow = - overflow;
	overflow /= neg;
	if((unsigned int) overflow != 0x80000000) {
	    return 0;
	}
	overflow = (1 << 31);
	overflow = - overflow;
	overflow %= neg;
	if(overflow != 0) {
	    return 2;
	}

	int div_by_zero = 3 / 0;
	if(div_by_zero != -1) {
	    return 1;
	}
	div_by_zero = 3 % 0;
	if(div_by_zero != 3) {
	    return 3;
	}

    return a1; /* 8 */
}
