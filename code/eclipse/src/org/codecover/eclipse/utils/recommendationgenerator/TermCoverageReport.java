package org.codecover.eclipse.utils.recommendationgenerator;

import java.util.Vector;

import org.codecover.model.TestCase;

public class TermCoverageReport {
	public int m_coverage;
	public boolean missingValue;
	public String termText;
	public Vector<TestCase> testCaseSet;

	public TermCoverageReport() {
		testCaseSet = new Vector<TestCase>();
	}
}
