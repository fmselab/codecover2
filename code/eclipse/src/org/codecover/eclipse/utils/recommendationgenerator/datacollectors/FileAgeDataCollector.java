package org.codecover.eclipse.utils.recommendationgenerator.datacollectors;

import java.util.Date;

import org.codecover.eclipse.utils.recommendationgenerator.DataCollector;
import org.eclipse.core.resources.IResource;
import org.eclipse.jdt.core.JavaModelException;

public class FileAgeDataCollector extends DataCollector {

	public FileAgeDataCollector() {
	}

	@Override
	public void invoke() throws JavaModelException {
		// < 1 Woche: 10
		// < 2 Wochen: 9
		// < 3 Wochen: 8
		// < 4 Wochen: 7
		// < 5 Wochen: 6
		// < 6 Wochen: 5
		
		for (IResource resource : getRelevantFiles()) {
			Date firstRevisionDate = SVNInfoCache.getFirstRevisionDate(resource);
			if (firstRevisionDate == null) {
				continue;
			}
			Date today = new Date();
			int ageInWeeks = ((int)(today.getTime()-firstRevisionDate.getTime())) / (1000*60*60*24*7);
			double score = 0.0d;
			score = 11.0 - ageInWeeks;
			if (score > 0) {
				this.getErrorIndicator().addProbabilityInformation(resource, score);
			}
			
		}
	}

}
