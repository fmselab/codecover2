package org.codecover.eclipse.utils.recommendationgenerator;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.eclipse.core.resources.IResource;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

public final class ErrorIndicator {

	private Map<IResource, Double> fileErrorProbabilityMap = new HashMap<IResource, Double>();
	
	private Map<UncoveredBranch, Double> branchErrorProbabilityMap = new HashMap<UncoveredBranch, Double>();

	private Map<LineInFile, Double> fileLineErrorProbabilityMap = new HashMap<LineInFile, Double>();

	private List<Parameter> parameters = new ArrayList<Parameter>();

	private String description;

	private String name;

	private File xmlErrorDataInformation;

	private DataCollector dataCollector;
	
	private Set<IResource> relevantFiles;
	
	private boolean enabled = true;
	
	private ErrorIndicatorMode mode = ErrorIndicatorMode.MANUAL;

	private ArrayList<UncoveredBranch> branches;

	private boolean autoOnly;

	

	public boolean isAutoOnly() {
		return autoOnly;
	}

	public ErrorIndicator(String name, String description) {
		this.name = name;
		this.description = description;
	}

	public ErrorIndicator(String name, String description, DataCollector collector) {
		this(name, description, collector, false);
	}

	public ErrorIndicator(String name, String description, DataCollector collector, boolean autoOnly) {
		this.autoOnly = autoOnly;
		this.name = name;
		this.description = description;
		this.dataCollector = collector;
		if (this.dataCollector != null) {
			this.dataCollector.setParentErrorIndicator(this);
			this.mode = ErrorIndicatorMode.AUTO;
		}
	}

	public void addProbabilityInformation(IResource resource, double value) {
		fileErrorProbabilityMap.put(resource, value);
	}

	public void addProbabilityInformation(IResource resource, int line, Double value) {
		fileLineErrorProbabilityMap.put(new LineInFile(resource, line), value);
	}
	
	public void addProbabilityInformation(UncoveredBranch branch, Double value) {
		branchErrorProbabilityMap.put(branch, value);
	}
	
	public boolean isReady() {
		if (this.dataCollector == null && this.xmlErrorDataInformation == null) {
			return false;
		}
		return true;
	}
	
	public boolean canDetermineAutomatically() {
		return this.dataCollector != null;
	}
	
	public ErrorIndicatorMode getMode() {
		return mode;
	}

	public void setMode(ErrorIndicatorMode mode) {
		this.mode = mode;
	}

	/**
	 * Returns the error-probability
	 */
	public Double getValueFor(IResource path) {
		if (this.mode == ErrorIndicatorMode.OFF) {
			return 0.0;
		}
		if (path == null) {
			return 0.0d;
		}
		Double d = fileErrorProbabilityMap.get(path);
		if (d == null) {
			return 0.0d;
		}
		return d;
	}

	public Double getValueFor(IResource resource, int line) {
		Double d = fileLineErrorProbabilityMap.get(new LineInFile(resource, line));
		return d==null?0.0:d;
	}
	
	public Double getValueFor(UncoveredBranch ub) {
		Double d = branchErrorProbabilityMap.get(ub);
		return d==null?0.0:d;
	}

	public boolean addParameter(Parameter arg0) {
		return parameters.add(arg0);
	}

	public List<Parameter> getParameters() {
		return parameters;
	}

	public String getName() {
		return name;
	}

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public String getDescription() {
		return description;
	}

	public DataCollector getDataCollector() {
		return dataCollector;
	}

	public File getXmlErrorDataInformation() {
		return xmlErrorDataInformation;
	}

	public void setXmlErrorDataInformation(File xmlErrorDataInformation) throws NumberFormatException, ParserConfigurationException, SAXException, IOException, MalformedErrorIndicatorFileException {
		this.xmlErrorDataInformation = xmlErrorDataInformation;
		
	}

	private void readErrorIndicator() throws ParserConfigurationException, SAXException, IOException,
			NumberFormatException, MalformedErrorIndicatorFileException {
		if (this.xmlErrorDataInformation == null || !this.xmlErrorDataInformation.canRead()) {
			return;
		}
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		DocumentBuilder db = dbf.newDocumentBuilder();
		Document doc = db.parse(this.xmlErrorDataInformation);
		doc.getDocumentElement().normalize();

		NodeList errorIndicators = doc.getElementsByTagName("errorIndicator");
		for (int eICnt = 0; eICnt < errorIndicators.getLength(); eICnt++) {
			Element errorIndicator = (Element) errorIndicators.item(eICnt);
			if (errorIndicator.getNodeType() == Node.ELEMENT_NODE) {

				NodeList fileInfos = errorIndicator.getElementsByTagName("file");
				for (int fiCnt = 0; fiCnt < fileInfos.getLength(); fiCnt++) {
					Element fileInfo = (Element) fileInfos.item(fiCnt);
					String path = fileInfo.getAttribute("path");
					String clazz = fileInfo.getAttribute("class");
					String value = fileInfo.getAttribute("value");
					IResource file = null;
					if (clazz != null && clazz.length() > 0) {
						file = ToResourceConverter.getResourceFromClass(clazz);
					} else {
						file = ToResourceConverter.getResourceFromPath(path);
					}
					addProbabilityInformation(file, Integer.parseInt(value));
				}

				NodeList lineInfos = errorIndicator.getElementsByTagName("line");
				for (int liCnt = 0; liCnt < lineInfos.getLength(); liCnt++) {
					Element lineInfo = (Element) lineInfos.item(liCnt);
					String path = lineInfo.getAttribute("path");
					String clazz = lineInfo.getAttribute("class");
					String severity = lineInfo.getAttribute("severity");
					String lineFrom = lineInfo.getAttribute("lineFrom");
					String lineTo = lineInfo.getAttribute("lineTo");
					for (int a = Integer.valueOf(lineFrom); a < Integer.valueOf(lineTo); a++) {
						IResource file = null;
						if (clazz != null && clazz.length() > 0) {
							file = ToResourceConverter.getResourceFromClass(clazz);
						} else {
							file = ToResourceConverter.getResourceFromPath(path);
						}
						if ("warning".equals(severity)) {
							addProbabilityInformation(file, a, 10.0);
						} else if ("error".equals(severity)) {
							addProbabilityInformation(file, a, 20.0);
						}
					}
				}
			}

		}
	}

	public void invoke() {
		if (this.mode == ErrorIndicatorMode.OFF) {
			return;
		}
		if (this.mode == ErrorIndicatorMode.AUTO && this.dataCollector != null) {
			try {
				this.dataCollector.setRelevantFiles(this.relevantFiles);
				this.dataCollector.invoke();
			} catch (JavaModelException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		} else if (this.mode == ErrorIndicatorMode.MANUAL) {
			try {
				for (Parameter p : this.parameters) {
					if (p.getType() instanceof File) {
						this.xmlErrorDataInformation = (File) p.getValue();
						break;
					}
				}
				readErrorIndicator();
			} catch (NumberFormatException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (ParserConfigurationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (SAXException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (MalformedErrorIndicatorFileException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		} else {
			MessageDialog.openInformation(new Shell(), 
					"Problem", 
					"Implementierungsfehler. ErrorIndicator "+this.name);
		}
	}

	public void setRelevantFiles(Set<IResource> relevantFiles) {
		this.relevantFiles = relevantFiles;
	}

	public Parameter getParameter(String name) {
		for (Parameter parameter : this.getParameters()) {
			if (parameter.getName().equals(name)) {
				return parameter;
			}
		}
		return null;
	}

	public void setBranches(ArrayList<UncoveredBranch> branches) {
		this.branches = branches;
	}

	public ArrayList<UncoveredBranch> getBranches() {
		return branches;
	}

	

}
