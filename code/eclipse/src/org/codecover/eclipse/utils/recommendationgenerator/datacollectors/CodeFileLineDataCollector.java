package org.codecover.eclipse.utils.recommendationgenerator.datacollectors;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import org.codecover.eclipse.utils.recommendationgenerator.DataCollector;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.JavaModelException;

public class CodeFileLineDataCollector extends DataCollector {

	@Override
	public void invoke() throws JavaModelException {
		for (IResource resource : getRelevantFiles()) {
			double lineCnt = 0.0;
			if (resource instanceof IFile) {
				IFile file = (IFile) resource;
				try {
					InputStream contents = file.getContents();
					BufferedReader reader = new BufferedReader(new InputStreamReader(contents));
					String strLine;
					while ((strLine = reader.readLine()) != null) {
						if (strLine.trim().length() > 0) {
							lineCnt++;
						}
					}
				} catch (CoreException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
			
			int max = ((Integer) this.getErrorIndicator().getParameter("Grenze").getValue());
			if (lineCnt > max) {
				lineCnt = max;
			}
			System.out.print("Datei "+resource.getName()+" hat "+lineCnt+" Zeilen. Das gibt ");
			lineCnt = lineCnt / 50;
			System.out.println(lineCnt+" Punkte.");
			this.getErrorIndicator().addProbabilityInformation(resource, lineCnt);
		}
	}

}
