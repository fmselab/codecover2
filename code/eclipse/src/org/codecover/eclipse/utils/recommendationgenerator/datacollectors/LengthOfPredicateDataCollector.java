package org.codecover.eclipse.utils.recommendationgenerator.datacollectors;

import org.codecover.eclipse.utils.recommendationgenerator.DataCollector;
import org.codecover.eclipse.utils.recommendationgenerator.UncoveredBranch;
import org.eclipse.jdt.core.JavaModelException;

public class LengthOfPredicateDataCollector extends DataCollector {

	@Override
	public void invoke() throws JavaModelException {
		for (UncoveredBranch ub : getErrorIndicator().getBranches()) {
			int length = ub.m_branchInfo.m_statementText.length();
			double val = 0.0;
			if (length > 200) {
				val = 15;
			} else if (length > 150) {
				val = 12;
			} else if (length > 100) {
				val = 7;
			} else if (length > 50) {
				val = 4;
			}
			getErrorIndicator().addProbabilityInformation(ub, val);
		}
	}

}
