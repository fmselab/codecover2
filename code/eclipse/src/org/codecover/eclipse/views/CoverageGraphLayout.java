/******************************************************************************
 * Copyright (c) 2009 Negar Koochakzadeh, Vahid Garousi			              *
 * All rights reserved. This program and the accompanying materials           *
 * are made available under the terms of the Eclipse Public License v1.0      *
 * which accompanies this distribution, and is available at                   *
 * http://www.eclipse.org/legal/epl-v10.html                                  *
 ******************************************************************************/

package org.codecover.eclipse.views;

import java.awt.Dimension;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import edu.uci.ics.jung.algorithms.layout.AbstractLayout;
import edu.uci.ics.jung.graph.Graph;

/**
 * This {@link CoverageGraphLayout} is a subclass of AbstractLayout from
 * jung visualization tool, this layout is used for drawing Coverage Graph 
 * 
 * Project supervisor: Vahid Garousi (http://www.ucalgary.ca/~vgarousi/)
 * Software Quality Engineering Research Group (SoftQual)
 * Department of Electrical and Computer Engineering
 * Schulich School of Engineering
 * University of Calgary, Alberta, Canada
 * http://www.softqual.ucalgary.ca
 * @author Negar Koochakzadeh
 * @version 1.0 
 */

public class CoverageGraphLayout<V,E> extends AbstractLayout<V,E> {

	org.eclipse.swt.graphics.Point size;
	public CoverageGraphLayout(Graph<V,E> g,org.eclipse.swt.graphics.Point size) {
		super(g);
		this.size = size;
		this.setSize(new Dimension(size.x,size.y));
	}
	public void orderVertices(V[] vertices) {
		List<V> list = Arrays.asList(vertices);
		
		final Comparator<V> myComparator = new Comparator<V>() {
			@Override
			public int compare(V n1, V n2){			
				return ((CoverageGraphNode)n1).getLable().compareTo(((CoverageGraphNode)n2).getLable());
			}
		};
	
		Collections.sort(list, myComparator);
	}
	
	@Override
	public void reset() {
		initialize();
	}

	@Override
	@SuppressWarnings("unchecked")
	public void initialize() {
		Graph<V,E> graph = getGraph();
		Dimension d = getSize();
		if(graph != null && d != null) {
			V[] vertices =
				(V[])graph.getVertices().toArray();
			orderVertices(vertices);

			double height = d.getHeight();
			double width = d.getWidth();
			
			double xTS = 500;
			double xSUT = 1000;
			
			double yTS = 200;
			double ySUT = 200;

			int NumOfSUTNodes = 0;
			int NumOfTestNodes = 0;
			int GraphHeight = 0;
			for (int i = 0; i < vertices.length; i++) {
				if( ((CoverageGraphNode)vertices[i]).type == "SUT" )
					NumOfSUTNodes ++;
				else
					NumOfTestNodes++;
			}
			if (NumOfSUTNodes > NumOfTestNodes)
				GraphHeight = NumOfSUTNodes * 30;
			else
				GraphHeight = NumOfTestNodes * 30;
			
			
			
			for (int i = 0; i < vertices.length; i++) {
				//Point2D coord = transform(vertices[i]);
				
				if( ((CoverageGraphNode)vertices[i]).type == "SUT" ){
					this.setLocation(vertices[i],xSUT,ySUT);
					ySUT += GraphHeight / NumOfSUTNodes;
				}
				else{
					this.setLocation(vertices[i],xTS,yTS);
					yTS += GraphHeight / NumOfTestNodes;
				}
			}
		}
	}
}
