package org.codecover.eclipse.utils.recommendationgenerator;

import java.util.Set;

import org.eclipse.core.resources.IResource;
import org.eclipse.jdt.core.JavaModelException;

public abstract class DataCollector {

	private ErrorIndicator errorIndicator;
	private Set<IResource> relevantFiles;
	
	
	public DataCollector() {
	}
	
	protected ErrorIndicator getErrorIndicator() {
		return this.errorIndicator;
	}
	
	public void setRelevantFiles(Set<IResource> files) {
		this.relevantFiles = files;
	}
	
	public Set<IResource> getRelevantFiles() {
		return this.relevantFiles;
	}
	
	public abstract void invoke() throws JavaModelException;

	public void setParentErrorIndicator(ErrorIndicator errorIndicator2) {
		this.errorIndicator = errorIndicator2;
	}

	
	
}
