/******************************************************************************
 * Copyright (c) 2009 Negar Koochakzadeh, Vahid Garousi			              *
 * All rights reserved. This program and the accompanying materials           *
 * are made available under the terms of the Eclipse Public License v1.0      *
 * which accompanies this distribution, and is available at                   *
 * http://www.eclipse.org/legal/epl-v10.html                                  *
 ******************************************************************************/

package org.codecover.eclipse.views;

import java.util.Vector;

import org.codecover.model.TestCase;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.Location;

/**
 * This {@link CoverageGraphNode} is the type of each node in Coverage Graph 
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

public class CoverageGraphNode {
	String type;
	public String level;
	String packageName;
	String className;
	public Location ClassLocation;
	String methodName;
	public Location MethodLocation;
	String itemID;
	public Location itemLocation;
	public String Content;
	Integer NumOfItems = 0;
	public HierarchyLevel HL;
	public Vector<TestCase> Testcases;
	Boolean CompletName;
	public CoverageGraphNode(){}
	//Constructor for test nodes:
	public CoverageGraphNode(String type,String level,
			  String packageName,
			  String className, 
			  String methodName,
			  Boolean CompletName){
		this.CompletName = CompletName;
		Testcases = new Vector<TestCase>();
		this.level = level;
		this.type = type;
		this.NumOfItems = 1;
		if(this.level.compareTo("Package")==0){
			this.packageName = packageName;
			this.className = "";
			this.methodName = "";
			this.itemID = "";
		}
		else if(this.level.compareTo("Class")==0){
			this.packageName = packageName;
			this.className = className;
			this.methodName = "";
			this.itemID = "";
		}
		else if(this.level.compareTo("Method")==0){
			this.packageName = packageName;
			this.className = className;
			this.methodName = methodName;
			this.itemID = "";
		}
	}
	//Constructor for SUT nodes:
	public CoverageGraphNode(String type,String level,
				  String packageName,
				  String className, Location classLoc,
				  String methodName,Location methodLoc,
				  String itemID,Location itemLoc, String itemContent,
				  HierarchyLevel hl,Boolean CompletName) {	
		this.CompletName = CompletName;
		this.level = level;
		this.type = type;
		this.NumOfItems = 1;
		this.HL = hl;
		if(this.level.compareTo("Package")==0){
			this.packageName = packageName;
			this.className = "";
			this.methodName = "";
			this.itemID = "";
		}
		else if(this.level.compareTo("Class")==0){
			this.packageName = packageName;
			this.className = className;
			this.methodName = "";
			this.itemID = "";
			this.ClassLocation = classLoc;
		}
		else if(this.level.compareTo("Method")==0){
			this.packageName = packageName;
			this.className = className;
			this.methodName = methodName;
			this.itemID = "";
			this.MethodLocation = methodLoc;
		}
		else if(this.level.compareTo("Item")==0){
			this.packageName = packageName;
			this.className = className;
			this.methodName = methodName;
			this.itemID = itemID;
			this.itemLocation = itemLoc;
			this.Content = itemContent;
		}
	}
	public String getLable(){
		if(level.compareTo("Package") == 0)
			return packageName; 
		else if(level.compareTo("Class") == 0)
			return packageName+"."+className;
		else if(level.compareTo("Method") == 0)
			return packageName+"."+className+"."+methodName;
		else if(level.compareTo("Item") == 0)
			return packageName+"."+className+"."+methodName+"."+itemID;
		return "";
	}
	public String getShortLable(){
		if(level.compareTo("Package") == 0)
			return packageName; 
		else if(level.compareTo("Class") == 0)
			return className;
		else if(level.compareTo("Method") == 0)
			return methodName;
		else if(level.compareTo("Item") == 0)
			return itemID;
		return "";
	}
	
}
