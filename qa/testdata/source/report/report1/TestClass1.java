/*     1 */	package org.codecover.tests;
/*     2 */	import java.util.List;
/*     3 */	
/*     4 */	public class TestClass1 { 
/*     5 */		
/*     6 */		private boolean x;
/*     7 */		private int k;
/*     8 */		
/*     9 */		private static String A_MESSAGE;
/*    10 */		
/*    11 */		static {
/*    12 */			A_MESSAGE = "I am a message"; // S1
/*    13 */		}
/*    14 */		
/*    15 */		{
/*    16 */			k = 0; // S2
/*    17 */		}
/*    18 */		
/*    19 */		public TestClass1() {
/*    20 */			x = true; // S3
/*    21 */		}
/*    22 */		
/*    23 */		public void canGoWrong() throws Exception {
/*    24 */			if (x) { // S4 C1 B1 B2
/*    25 */				throw new Exception();
/*    26 */			}
/*    27 */		}
/*    28 */		
/*    29 */		public void helloWorld() {
/*    30 */			System.out.println("Hello world!"); // S5
/*    31 */		}
/*    32 */		
/*    33 */		public void doSomething() {
/*    34 */			canGoWrong();	// S6	
/*    35 */			if (x) { // S7 C2 B3 B4
/*    36 */				for (int i = 0; i < 10; i++) { // S8  C3 L1
/*    37 */					k += i; // S9
/*    38 */				}
/*    39 */				x = !x; // S10
/*    40 */			}
/*    41 */			while (k > 0) { // S11 C4 L2
/*    42 */				k = k - 6; // S12
/*    43 */			}
/*    44 */			
/*    45 */			do {  // S13
/*    46 */				k = k * 3 + 1; //S14
/*    47 */			} while (k % 2 != 0); // C5 L3
/*    48 */			
/*    49 */			if (k < 0 && x) { // S15 C6 B5
/*    50 */				k = -k; // S16
/*    51 */			} else { // B6
/*    52 */				k = k / 2; // S17
/*    53 */				x = !x; // S18
/*    54 */			}
/*    55 */				
/*    56 */			canGoWrong(); // S19
/*    57 */		}
/*    58 */		
/*    59 */		public static void main() {
/*    60 */			helloWorld(); // S20
/*    61 */			doSomething(); // S21
/*    62 */			doSomething(); // S22 
/*    63 */		}
/*    64 */		
/*    65 */	/*
/*    66 */		public void assignment() { 
/*    67 */			int i, j;
/*    68 */			i = 0;
/*    69 */		}
/*    70 */		
/*    71 */		public void initializingAssignment() { 
/*    72 */			int i = 0;
/*    73 */		}
/*    74 */		
/*    75 */		public void forLoop() {
/*    76 */			int k = 0;
/*    77 */			for (int i = 0; i < 10; i++) {
/*    78 */				k++;
/*    79 */			}
/*    80 */		}
/*    81 */	
/*    82 */		public void emptyForLoop() {
/*    83 */			int k = 0;
/*    84 */			for (int i = 0; i < 10; i++) {
/*    85 */			}
/*    86 */		}
/*    87 */		
/*    88 */		public void whileLoop() {
/*    89 */			int k = 0;
/*    90 */			int i = 0;
/*    91 */			while (i < 10) {
/*    92 */				k++;
/*    93 */				i++;
/*    94 */			}
/*    95 */		}
/*    96 */		
/*    97 */		public void emptyWhileLoop() {
/*    98 */			int k = 0;
/*    99 */			int i = 0;
/*   100 */			while (i < 10) {
/*   101 */				k++;
/*   102 */				i++;
/*   103 */			}
/*   104 */		}
/*   105 */		
/*   106 */		public void foreachLoop()  {
/*   107 */			int[] a = new int[] {0, 1, 2, 3, 4};
/*   108 */			int k = 0;
/*   109 */			for (int i : a) {
/*   110 */				k++;
/*   111 */			}
/*   112 */		}
/*   113 */		
/*   114 */		public void emptyForeachLoop()  {
/*   115 */			int[] a = new int[] {0, 1, 2, 3, 4};
/*   116 */			int k = 0;
/*   117 */			for (int i : a) {
/*   118 */				k++;
/*   119 */			}
/*   120 */		}
/*   121 */		
/*   122 */		public void ifThen() {
/*   123 */			boolean x = true;
/*   124 */			int i = 0;
/*   125 */			if (x) {
/*   126 */				i++;
/*   127 */			}
/*   128 */		}
/*   129 */		
/*   130 */		public void ifThenElse() {
/*   131 */			boolean x = true;
/*   132 */			int i = 0;
/*   133 */			if (x) {
/*   134 */				i++;
/*   135 */			} else {
/*   136 */				i--;
/*   137 */			}
/*   138 */		}
/*   139 */		
/*   140 */		public void ifEmptyThen() {
/*   141 */			boolean x = true;
/*   142 */			int i = 0;
/*   143 */			if (x) {
/*   144 */			}
/*   145 */		}
/*   146 */		
/*   147 */		public void ifThenEmptyElse() {
/*   148 */			boolean x = true;
/*   149 */			int i = 0;
/*   150 */			if (x) {
/*   151 */				i++;
/*   152 */			} else {			
/*   153 */			}
/*   154 */		}
/*   155 */			
/*   156 */		public void ifThenEmptyThen() {
/*   157 */			boolean x = true;
/*   158 */			int i = 0;
/*   159 */			if (x) {
/*   160 */			} else {			
/*   161 */				i++;
/*   162 */			}
/*   163 */		}
/*   164 */	
/*   165 */		public void ifEmptyThenEmptyElse() {
/*   166 */			boolean x = true;
/*   167 */			int i = 0;
/*   168 */			if (x) {
/*   169 */			} else {			
/*   170 */			}		
/*   171 */		}
/*   172 */		
/*   173 */		public static void main(String[] args) {
/*   174 */			TestClass tc = new TestClass();
/*   175 */			tc.assignment();
/*   176 */			tc.initializingAssignment();
/*   177 */			tc.foreachLoop();
/*   178 */			tc.emptyForeachLoop();
/*   179 */			tc.forLoop();
/*   180 */			tc.emptyForLoop();
/*   181 */			tc.whileLoop();
/*   182 */			tc.emptyWhileLoop();
/*   183 */			tc.ifThen();
/*   184 */		}
/*   185 */	*/
/*   186 */	}