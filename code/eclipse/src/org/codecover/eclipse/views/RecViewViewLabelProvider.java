package org.codecover.eclipse.views;

import java.text.DecimalFormat;

import org.codecover.eclipse.utils.recommendationgenerator.UncoveredBranch;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

class RecViewViewLabelProvider extends LabelProvider implements ITableLabelProvider {
	/**
	 * 
	 */
	private final RecommendationsView recommendationsView;

	/**
	 * @param recommendationsView
	 */
	RecViewViewLabelProvider(RecommendationsView recommendationsView) {
		this.recommendationsView = recommendationsView;
	}

	@Override
	public String getColumnText(Object obj, int index) {
		DecimalFormat df = new DecimalFormat("0.#");
		UncoveredBranch branch = ((UncoveredBranch) obj);
		Double double1 = null;
		switch (index) {
		case 0:
			return branch.m_methode;
		case 1:
			return branch.m_branchInfo.m_statementText.replaceAll("\t", " ").replaceAll("\n", " ").replaceAll("\r", " ").replaceAll("  ", " ");
		case 2:
			return branch.m_branchInfo.m_type.getNiceName();
		case 3:
			double1 = branch.score;
			if (double1 == null) return "";
			return df.format(double1);
		case 4:
			double1 = branch.sourceScore.get(this.recommendationsView.recommendationGenerator.getCodeErrorDataSource());
			if (double1 == null) return "";
			return df.format(double1);
		case 5:
			double1 = branch.sourceScore.get(this.recommendationsView.recommendationGenerator.getVersionErrorDataSource());
			if (double1 == null) return "";
			return df.format(double1);
		case 6:
			double1 = branch.sourceScore.get(this.recommendationsView.recommendationGenerator.getCcErrorDataSource());
			if (double1 == null) return "";
			return df.format(double1);
		case 7:
			double1 = branch.sourceScore.get(this.recommendationsView.recommendationGenerator.getExpertErrorDataSource());
			if (double1 == null) return "";
			return df.format(double1);
		case 8:
			double1 = branch.sourceScore.get(this.recommendationsView.recommendationGenerator.getProcessDataSource());
			if (double1 == null) return "";
			return df.format(double1);
		case 9:
			double1 = branch.sourceScore.get(this.recommendationsView.recommendationGenerator.getQualityErrorDataSource());
			if (double1 == null) return "";
			return df.format(double1);
		case 10:
			return branch.m_branchInfo.m_amountLines + "";
		case 11:
			return branch.m_testCaseList.size() + "";
		
		default:
			return getText(obj);
			
			
		
		}
		
		
		

	}

	@Override
	public Image getColumnImage(Object obj, int index) {
		switch (index) {
		case 0:
			JavaUI.getSharedImages().getImageDescriptor(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_PUBLIC);
		}
		return null;
	}

	@Override
	public Image getImage(Object obj) {
		return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_ELEMENT);
	}
	
	
}