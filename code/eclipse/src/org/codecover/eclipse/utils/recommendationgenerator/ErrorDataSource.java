package org.codecover.eclipse.utils.recommendationgenerator;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IResource;

public class ErrorDataSource {

	private String name;
	
	private String description;

	private double weight = 1.0;
	
	private List<ErrorIndicator> errorIndicators = new ArrayList<ErrorIndicator>();

	private Set<IResource> relevantFiles;

	private ArrayList<UncoveredBranch> branches;
	
	

	public ErrorDataSource(String name, String description) {
		this.name = name;
		this.description = description;
	}
	
	public ErrorDataSource(String name) {
		this.name = name;
		this.description = "";
	}
	
	public void addErrorIndicator(ErrorIndicator ei) {
		this.errorIndicators.add(ei);
	}
	
	public double getValueFor(IResource resource) {
		double sum = 0;
		for (ErrorIndicator ei : errorIndicators) {
			sum += ei.getValueFor(resource);
		}
		return sum * this.weight;
	}
	
	public double getValueFor(IResource path, int line) {
		double sum = 0;
		for (ErrorIndicator ei : errorIndicators) {
			sum += ei.getValueFor(path, line);
		}
		return sum * weight;
	}
	
	public Double getValueFor(UncoveredBranch ub) {
		double sum = 0.0;
		for (ErrorIndicator ei : errorIndicators) {
			sum += ei.getValueFor(ub);
		}
		return sum * weight;
	}
	
	
	public boolean hasAutomaticallyDeterminedIndicators() {
		for (ErrorIndicator ei : this.errorIndicators) {
			if (ei.getDataCollector() != null) {
				return true;
			}
		}
		return false;
	}
	
	public void invoke() {
		long a = new Date().getTime();
		List<Thread> threads = new ArrayList<Thread>();
		for (final ErrorIndicator ei : this.errorIndicators) {
			Thread thread = new Thread() {
				@Override
				public void run() {
					ei.setRelevantFiles(relevantFiles);
					ei.setBranches(branches);
					ei.invoke();
				};
			};
			threads.add(thread);
			thread.start();
		}
		for (Thread t : threads) {
			try {
				t.join();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		System.out.println("Dauer von "+this.name+": "+(new Date().getTime()-a)/1000+" Sekunden");
	}
	
	
	public void setWeight(double weight) {
		this.weight = weight;
	}
	
	public double getWeight() {
		return weight;
	}

	public String getName() {
		return name;
	}

	public String getDescription() {
		return description;
	}

	public List<ErrorIndicator> getErrorIndicators() {
		return errorIndicators;
	}

	public void setRelevantFiles(Set<IResource> relevantFiles) {
		this.relevantFiles = relevantFiles;
	}

	public void setBranches(ArrayList<UncoveredBranch> uncoveredBranches) {
		this.branches = uncoveredBranches;
	}

	

	

	
	
}
