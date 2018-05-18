package org.codecover.eclipse.utils.recommendationgenerator.datacollectors;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import org.codecover.eclipse.utils.recommendationgenerator.DataCollector;
import org.codecover.eclipse.utils.recommendationgenerator.Parameter;
import org.codecover.eclipse.utils.recommendationgenerator.UncoveredBranch;
import org.codecover.model.TestCase;
import org.eclipse.jdt.core.JavaModelException;

public class CCPriorityDataCollector extends DataCollector {

	private HashMap<String, Character> testCasePriorityInfo = new HashMap<String, Character>();
	
	@Override
	public void invoke() throws JavaModelException {
		testCasePriorityInfo = new HashMap<String, Character>();
		Parameter parameter = this.getErrorIndicator().getParameter("priorityFile"); //$NON-NLS-1$
		if (parameter != null) {
			Object value = parameter.getValue();
			if (value != null && value instanceof File) {
				try {
					BufferedReader reader = new BufferedReader(new FileReader((File)value));
					String line = null;
					while ((line = reader.readLine()) != null) {
						String[] split = line.split(";"); //$NON-NLS-1$
						String testCaseName = split[0];
						String priority = split[1];
						testCasePriorityInfo.put(testCaseName.trim(), priority.charAt(0));
					}
				} catch (FileNotFoundException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (IndexOutOfBoundsException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		
		for (UncoveredBranch branch : getErrorIndicator().getBranches()) {
			for (TestCase testCase : branch.m_testCaseList) {
				if (testCasePriorityInfo.containsKey(testCase.getName())) {
					Character character = testCasePriorityInfo.get(testCase.getName());
					double val = 2.0 * (5.0-character.charValue()+65.0);
					this.getErrorIndicator().addProbabilityInformation(branch, val);
				}
			}
		}
	}

}
