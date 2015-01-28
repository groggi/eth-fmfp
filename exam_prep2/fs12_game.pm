#define N 100
bit turn = 0;
byte pileA = N;
byte pileB = N;

byte lastA = 0;
bit lastPile = 0;

inline choose(a, n) {
	a = 2;
	do
		:: (a < n-2) -> a++;
		:: break;
	od
}

proctype playerA() {
  do
  :: turn == 0;
  	if
  	:: pileA == 0 && pileB == 0 -> break;
  	fi;

	if
	:: choose(lastA, pileA);
		  pileA = pileA - lastA;
		  lastPile = 0;
	:: choose(lastA, pileB);
		  pileB = pileB - lastA;
		  lastPile = 1;
	fi;

	turn = 1;
  od
}

proctype playerB() {
  do
  :: turn == 1;
  	if
  	:: pileA == 0 && pileB == 0 -> break;
  	fi;

	if
		:: lastPile == 0 -> pileA = pileA - lastA
		:: lastPile == 1 -> pileB = pileB - lastA
	fi;

	turn = 0;
  od
}

proctype watchdog() {
	assert((pileA > 0 || pileB > 0) || (pileA == 0 && pileB == 0 && turn == 1))
}

init {
	atomic {
		run playerA();
		run playerB();
		run watchdog();
	}
}