package org.codecover.eclipse.utils.recommendationgenerator;

import java.util.Arrays;
import java.util.List;

public enum BranchType {
	ifBranch ("If"), 
	whileE  ("Schleifenkörper nicht"), 
	while0  ("Schleifenkörper nicht nie"), 
	switchBranch ("Switch"), 
	catchBranch ("Catch"), 
	term  ("Term"), 
	while1  ("Schleifenkörper nicht genau einmal"), 
	whileN  ("Schleifenkörper nicht n Mal");
	
	private String niceName;
	
	BranchType(String niceName) {
		this.niceName = niceName;
	}
	
	
	public static List<BranchType> getAllAsList() {
		return Arrays.asList(BranchType.values());
	}


	public String getNiceName() {
		return niceName;
	}
	
}
