/*     1 */	package org.codecover.tests;
/*     2 */	import java.io.File;
/*     3 */	
/*     4 */	public class TestClass2 { 
/*     5 */		
/*     6 */		private int someInt;
/*     7 */		Worker worker;
/*     8 */		
/*     9 */		public TestClass2(int someInt) {
/*    10 */			this.someInt = someInt; //S1
/*    11 */		}
/*    12 */		
/*    13 */		public void makeWorker() {
/*    14 */			worker = new DefaultWorkerImpl(); //S2
/*    15 */		}
/*    16 */		
/*    17 */		public void botherWorker(int i) {
/*    18 */			do { // S3
/*    19 */				try { // S4 B1
/*    20 */					worker.doWork(i); // S5
/*    21 */				} catch (Exception e) { //B2
/*    22 */					System.exit(255); // S6
/*    23 */				}
/*    24 */			} while (!worker.isWorkReallyDone()); // C1 L1
/*    25 */		}
/*    26 */		
/*    27 */		protected static interface Worker {
/*    28 */			public void doWork(int x) throws Exception;
/*    29 */			public boolean isWorkReallyDone();
/*    30 */		}
/*    31 */		
/*    32 */		protected static class DefaultWorkerImpl implements Worker {
/*    33 */			
/*    34 */			private int internalX = 0;  // S7
/*    35 */			private boolean workIsDone = false; //S8
/*    36 */			
/*    37 */			public void doWork(int x) {
/*    38 */				if (workIsDone || x == 0) // S9 C2 B3 B4
/*    39 */					internalX = x; // S10
/*    40 */				internalX--; //S11
/*    41 */				workIsDone = internalX <= 0; //S12
/*    42 */			}
/*    43 */			
/*    44 */			public boolean isWorkReallyDone() {
/*    45 */				return workIsDone;
/*    46 */			}
/*    47 */		}
/*    48 */	}