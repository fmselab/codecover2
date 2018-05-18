package org.codecover.eclipse.utils.recommendationgenerator;


public class Parameter {
	private String name;
	private String description;
	private Object type;
	private Object value;
	private boolean usedIfAuto;
	private boolean usedIfXml;

	public Parameter(String name, String description, Object type, boolean usedIfAuto, boolean usedIfXml) {
		super();
		this.name = name;
		this.description = description;
		this.type = type;
		this.usedIfAuto = usedIfAuto;
		this.usedIfXml = usedIfXml;
	}
	
	public Parameter(String name, String description, Object type, Object value, boolean usedIfAuto, boolean usedIfXml) {
		super();
		this.name = name;
		this.description = description;
		this.type = type;
		this.value = value;
		this.usedIfAuto = usedIfAuto;
		this.usedIfXml = usedIfXml;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Object getType() {
		return type;
	}

	public void setType(Object type) {
		this.type = type;
	}

	public Object getValue() {
		return value;
	}

	public void setValue(Object value) {
		this.value = value;
	}


	public boolean isUsedIfAuto() {
		return usedIfAuto;
	}

	public boolean isUsedIfXml() {
		return usedIfXml;
	}

}
