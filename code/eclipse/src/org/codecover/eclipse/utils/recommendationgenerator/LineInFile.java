package org.codecover.eclipse.utils.recommendationgenerator;

import org.eclipse.core.resources.IResource;

public class LineInFile {
	/**
	 * 
	 */
	private IResource resource;
	private Integer line;
	public LineInFile(IResource file, int line) {
		this.line = line;
		this.resource = file;
	}
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((line == null) ? 0 : line.hashCode());
		result = prime * result + ((resource == null) ? 0 : resource.hashCode());
		return result;
	}
	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (!(obj instanceof LineInFile)) {
			return false;
		}
		LineInFile other = (LineInFile) obj;
		if (line == null) {
			if (other.line != null) {
				return false;
			}
		} else if (!line.equals(other.line)) {
			return false;
		}
		if (resource == null) {
			if (other.resource != null) {
				return false;
			}
		} else if (!resource.equals(other.resource)) {
			return false;
		}
		return true;
	}

}