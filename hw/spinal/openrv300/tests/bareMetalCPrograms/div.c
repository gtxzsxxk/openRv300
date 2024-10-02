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

    return a1; /* 8 */
}
