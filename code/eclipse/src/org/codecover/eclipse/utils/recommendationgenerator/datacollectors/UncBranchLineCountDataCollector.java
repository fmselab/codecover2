package org.codecover.eclipse.utils.recommendationgenerator.datacollectors;

import org.codecover.eclipse.utils.recommendationgenerator.DataCollector;
import org.codecover.eclipse.utils.recommendationgenerator.UncoveredBranch;
import org.eclipse.jdt.core.JavaModelException;

public class UncBranchLineCountDataCollector extends DataCollector {

	@Override
	public void invoke() throws JavaModelException {
		for (UncoveredBranch ub : getErrorIndicator().getBranches()) {
			if (ub.lineFrom != null && ub.lineTo != null && ub.lineFrom > 0 && ub.lineTo > 0 && ub.lineTo >= ub.lineFrom) {
				int linesInBlock = ub.lineTo-ub.lineFrom;
				double result = 0;
				if (linesInBlock > 50) {
					result = 10;
				} else if (linesInBlock > 20) {
					result = 6;
				} else if (linesInBlock > 10) {
					result = 4;
				}
				getErrorIndicator().addProbabilityInformation(ub, result);
			}
		}
	}

}
