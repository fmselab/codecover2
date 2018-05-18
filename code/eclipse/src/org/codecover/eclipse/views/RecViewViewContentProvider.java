package org.codecover.eclipse.views;

import java.util.ArrayList;
import java.util.List;

import org.codecover.eclipse.utils.recommendationgenerator.RecommendationGenerator;
import org.codecover.eclipse.utils.recommendationgenerator.UncoveredBranch;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;

class RecViewViewContentProvider implements IStructuredContentProvider {
	/**
	 * 
	 */
	private RecommendationsView recommendationsView;

	
	private List<UncoveredBranch> viewCache = new ArrayList<UncoveredBranch>();


	private TableViewer viewer;


	private RecommendationGenerator recommendationGenerator;
	
	RecViewViewContentProvider(RecommendationsView recommendationsView, 
			TableViewer viewer, RecommendationGenerator generator) {
		this.recommendationsView = recommendationsView;
		this.viewer = viewer;
		this.recommendationGenerator = generator;
		
		init();
	}
	
	public void init() {
		viewCache = this.recommendationGenerator.getUnsortedList();
		viewer.refresh();
		viewer.setComparator(new ViewerComparator() {
			
		});
	}

	@Override
	public void inputChanged(Viewer v, Object oldInput, Object newInput) {
	}

	@Override
	public void dispose() {
	}

	@Override
	public Object[] getElements(Object parent) {
		if (viewCache == null) {
			viewCache = new ArrayList<UncoveredBranch>();
		}
		return viewCache.toArray();
	}

	public void sort() {
		this.recommendationGenerator.sort();
		this.viewCache = this.recommendationGenerator.getSortedList();
		if (this.viewCache == null) {
			this.viewCache = new ArrayList<UncoveredBranch>();
		}
		viewer.refresh();
	}

	public void refreshViewWithoutInvoking() {
		this.recommendationGenerator.weightChanged();
		this.viewCache = this.recommendationGenerator.getSortedList();
		if (this.viewCache == null) {
			this.viewCache = new ArrayList<UncoveredBranch>();
		}
		viewer.refresh();
	}
}