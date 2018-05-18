package org.codecover.eclipse.utils.recommendationgenerator;

public class BlackboxTestCase {
	private String name;
	private char priority;
	
	
	public BlackboxTestCase(String name, char priority) {
		this.name = name;
		this.priority = priority;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public char getPriority() {
		return priority;
	}

	public void setPriority(char priority) {
		this.priority = priority;
	}
}
