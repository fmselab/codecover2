package org.codecover.eclipse.utils.recommendationgenerator;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Hashtable;

public class JustusTestCaseList {

	private Hashtable<String, BlackboxTestCase> testcaseTable;

	public JustusTestCaseList(File file) throws IOException {
		testcaseTable = new Hashtable<String, BlackboxTestCase>();

		BufferedReader in = new BufferedReader(new FileReader(file));

		String zeile;

		while ((zeile = in.readLine()) != null) {

			String[] values = zeile.split(";");

			BlackboxTestCase tc = new BlackboxTestCase(values[1], values[2].charAt(0));

			testcaseTable.put(tc.getName(), tc);

			int trenner = tc.getName().indexOf(" / ");
			if (trenner != -1) {
				String kurzName = tc.getName().substring(trenner + 3);
				testcaseTable.put(kurzName, tc);
			}

		}
	}

	public BlackboxTestCase get(String name) {

		if (name.endsWith(" (1)") || name.endsWith(" (2)")) {
			name = name.substring(0, name.length() - 4);
		}

		BlackboxTestCase tcj = testcaseTable.get(name);

		if (tcj != null) {
			return tcj;
		}

		int trenner = name.indexOf(" / ");
		if (trenner != -1) {
			String suchName = name.substring(trenner + 3);
			return testcaseTable.get(suchName);
		}

		return null;
	}

}
