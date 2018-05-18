package org.codecover.eclipse.utils.recommendationgenerator.datacollectors;

import org.codecover.eclipse.utils.recommendationgenerator.DataCollector;
import org.codecover.eclipse.utils.recommendationgenerator.UncoveredBranch;
import org.eclipse.jdt.core.JavaModelException;

public class CodeBlockTypeDataCollector extends DataCollector {

	@Override
	public void invoke() throws JavaModelException {
		for (UncoveredBranch ub : getErrorIndicator().getBranches()) {
			double val = 0.0;
			switch (ub.m_branchInfo.m_type) {
			case ifBranch:
				val = 5.0;
				break;
			case whileE:
				val = 5.0;
				break;
			case while1:
				val = 3.0;
				break;
			case while0:
				val = 3.0;
				break;
			case whileN:
				val = 3.0;
				break;
			case catchBranch:
				val = 2.0;
				break;
			case switchBranch:
				val = 4.0;
				break;
			case term:
				val = 2.0;
				break;
			}
			getErrorIndicator().addProbabilityInformation(ub, val);
		}
	}

}
