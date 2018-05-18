package org.codecover.eclipse.utils.recommendationgenerator.datacollectors;

import org.codecover.eclipse.utils.recommendationgenerator.DataCollector;
import org.codecover.eclipse.utils.recommendationgenerator.UncoveredBranch;
import org.eclipse.jdt.core.JavaModelException;

public class PassingTestCaseCountDataCollector extends DataCollector {

	@Override
	public void invoke() throws JavaModelException {
		for (UncoveredBranch ub : getErrorIndicator().getBranches()) {
			final int passingTestcases = ub.m_testCaseList.size();
			if (passingTestcases == 1) {
				getErrorIndicator().addProbabilityInformation(ub, 15.0);
				continue;
			}
			if (passingTestcases <= 2 ) {
				getErrorIndicator().addProbabilityInformation(ub, 12.0);
				continue;
			}
			if (passingTestcases <= 3 ) {
				getErrorIndicator().addProbabilityInformation(ub, 9.0);
				continue;
			}
			if (passingTestcases <= 5 ) {
				getErrorIndicator().addProbabilityInformation(ub, 5.0);
				continue;
			}
			if (passingTestcases <= 10 ) {
				getErrorIndicator().addProbabilityInformation(ub, 2.0);
				continue;
			}
		}
	}

}
