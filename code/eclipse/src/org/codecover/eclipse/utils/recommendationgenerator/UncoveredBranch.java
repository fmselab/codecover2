package org.codecover.eclipse.utils.recommendationgenerator;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.codecover.model.TestCase;
import org.eclipse.core.resources.IResource;

public class UncoveredBranch {
	public BranchInfo m_branchInfo;
	public String m_methode;
	public List<TestCase> m_testCaseList;
	
	public Double score = 0.0d;
	public IResource resource;
	public Integer lineFrom;
	public Integer lineTo;
	public Map<ErrorDataSource, Double> sourceScore = new HashMap<ErrorDataSource, Double>();
	public String info = "";
	
	public UncoveredBranch(String methode, BranchInfo branchInfo,
			List<TestCase> testCaseList) {
		m_methode = methode;
		m_branchInfo = branchInfo;
		m_testCaseList = testCaseList;
	}
}
