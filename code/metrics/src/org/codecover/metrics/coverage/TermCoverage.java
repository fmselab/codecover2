/******************************************************************************
 * Copyright (c) 2010 RS                                                      *
 * All rights reserved. This program and the accompanying materials           *
 * are made available under the terms of the Eclipse Public License v1.0      *
 * which accompanies this distribution, and is available at                   *
 * http://www.eclipse.org/legal/epl-v10.html                                  *
 ******************************************************************************/

package org.codecover.metrics.coverage;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.codecover.model.TestCase;
import org.codecover.model.mast.BasicBooleanTerm;
import org.codecover.model.mast.BooleanAssignment;
import org.codecover.model.mast.BooleanResult;
import org.codecover.model.mast.BooleanTerm;
import org.codecover.model.mast.Branch;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.OperatorTerm;
import org.codecover.model.mast.RootTerm;
import org.codecover.model.mast.Statement;
import org.codecover.model.mast.StatementSequence;
import org.codecover.model.utils.criteria.ConditionCoverage;
import org.codecover.model.utils.criteria.Criterion;

/**
 * This class would need to override getCoverage(testCases, term).
 *
 * @author RS
 * @version 1.0 ($Id: TermCoverage.java 69 2010-01-27 19:31:18Z schmidberger $)
 */
public class TermCoverage extends AbstractCoverageMetric {
    private static final String CACHING_KEY = "TermCoverage";

    private static final String NAME = "Term Coverage";

    private static final String DESCRIPTION = "";

    private static TermCoverage instance = new TermCoverage();

    private TermCoverage() {
        super(CACHING_KEY);
    }

    /**
     * This method returns an instance of TermCoverage.
     *
     * @return instance of TermCoverage.
     */
    public static TermCoverage getInstance() {
        return instance;
    }

    /**
     * (non-Javadoc)
     *
     * @see org.codecover.metrics.Metric#getDescription()
     */
    public String getDescription() {
        return DESCRIPTION;
    }

    /**
     * (non-Javadoc)
     *
     * @see org.codecover.metrics.Metric#getName()
     */
    public String getName() {
        return NAME;
    }

    /**
     * (non-Javadoc)
     *
     * @see org.codecover.metrics.Metric#getRequiredCriteria()
     */
    public Set<Criterion> getRequiredCriteria() {
      return Collections.<Criterion>singleton(
          org.codecover.model.utils.criteria.ConditionCoverage.getInstance());
    }

    /**
     * Calculates the {@link ConditionCoverage}
     *
     * @see org.codecover.metrics.coverage.AbstractCoverageMetric#getCoverage(java.util.List,
     *      org.codecover.model.mast.RootTerm)
     */
    public CoverageResult getCoverageLocal(Collection<TestCase> testCases,
            RootTerm term) {
        // The real work is done in the accept(Collection<TestCase> testCases, final
        // RootTerm term, final Visitor post) method
        return CoverageResult.NULL;
    }


    private static final CoverageResult zeroTwoResult = new CoverageResult(0, 2);
    private static final CoverageResult oneTwoResult = new CoverageResult(1, 2);
    private static final CoverageResult twoTwoResult = new CoverageResult(2, 2);

    @Override
    public void accept(Collection<TestCase> testCases, final RootTerm term, final PostMetricVisitor post) {

        // merge assignments of all the test cases
    	Map<BooleanAssignment, Boolean> assignmentsMap = new HashMap<BooleanAssignment, Boolean>();
        for (TestCase testCase : testCases) {
            assignmentsMap.putAll(testCase.getAssignments(term));
        }
        
        Map<BooleanTerm, BooleanResult> wirksamMapT = new HashMap<BooleanTerm, BooleanResult>();
        Map<BooleanTerm, BooleanResult> wirksamMapF = new HashMap<BooleanTerm, BooleanResult>();

        
        Set<BooleanAssignment> assignmentsSet = assignmentsMap.keySet();
        for (BooleanAssignment assignment : assignmentsSet) {
                                                           
            Map<BooleanTerm, BooleanResult> termResults = new HashMap<BooleanTerm, BooleanResult>();
        	this.evaluateTermWirksamkeit(term, term.getTerm(), assignment, termResults, wirksamMapT, wirksamMapF);
        }        
        
        // finally visit terms
        for (int i = 0; i < term.getTerm().getBasicBooleanTerms(); i++) {
            final BasicBooleanTerm bbTerm = term.getTermAtPosition(i);
            
            if (wirksamMapF.get(bbTerm) != null && wirksamMapT.get(bbTerm) != null) { // both T and F wirksam
                post.visit(bbTerm, term, twoTwoResult, noHints);
            } else {
                if (wirksamMapF.get(bbTerm) != null) { // only F wirksam
                	post.visit(bbTerm, term, oneTwoResult, noHints);
                } else {
	                if (wirksamMapT.get(bbTerm) != null) { // only T wirksam
	                	post.visit(bbTerm, term, oneTwoResult, noHints);
	                } else {
	                	post.visit(bbTerm, term, zeroTwoResult, noHints); // neither T nor F wirksam
	                }
                }
            }
        }

        super.accept(testCases, term, post);
    }
    
    /**
     * Recursive invoke and fills termResults with term/termResult
     * @param rootTerm head
     * @param booleanTerm the concerned term
     * @param assignment
     * @param termResults the return value, which is set in for each term
     * @return
     */
   private final BooleanResult evaluateTermResults(RootTerm rootTerm,
            BooleanTerm booleanTerm, BooleanAssignment assignment,  
            Map<BooleanTerm, BooleanResult> termResults) {

        if (booleanTerm instanceof BasicBooleanTerm) {
            int position = rootTerm.getPositionOfTerm((BasicBooleanTerm) booleanTerm);

            BooleanResult booleanResult = assignment.getResults().get(position);
            termResults.put(booleanTerm, booleanResult);

            return booleanResult;
            
        } else if (booleanTerm instanceof OperatorTerm) {
        	
        	OperatorTerm operatorTerm = (OperatorTerm)booleanTerm;

            List<BooleanResult> operatorResults = new LinkedList<BooleanResult>();
            
                                
            // Evaluate all the operands of this OperatorTerm and store the
            // results in a list.            
            for (BooleanTerm subTerms : operatorTerm.getOperands()) {
            	   
            	BooleanResult result = evaluateTermResults(rootTerm, subTerms, assignment, termResults);
	            operatorResults.add(result);
            }

            // Retrieve the Boolean result produced by the assignment of the
            // operands.
            BooleanAssignment operatorAssignment = new BooleanAssignment(operatorResults);
            Boolean result = operatorTerm.getOperator()
                    .getPossibleAssignments().get(operatorAssignment);            

            // Convert into BooleanResult.
            BooleanResult booleanResult;
            if (result == null) {
                booleanResult = BooleanResult.NOT_EVALUATED;
            } else {
                booleanResult = result ? BooleanResult.TRUE : BooleanResult.FALSE;
            }
            
            termResults.put(operatorTerm, booleanResult);

            return booleanResult;
        }

        return BooleanResult.NOT_EVALUATED;
    }

    /*
     * Wirksamkeit der Einzelterme für die Belegung assignment feststellen
     * Wirksamkeit steht in wirksamMapT und wirksamMapF
     */
    public final void evaluateTermWirksamkeit(RootTerm rootTerm,
            BooleanTerm booleanTerm, BooleanAssignment assignment, 
            Map<BooleanTerm, BooleanResult> termResults, 
            Map<BooleanTerm, BooleanResult> wirksamMapT, Map<BooleanTerm, BooleanResult> wirksamMapF) {
    	
    		evaluateTermResults(rootTerm, booleanTerm, assignment,termResults );    		
    	    evaluateTermWirksamkeit(rootTerm, booleanTerm, termResults, wirksamMapT, wirksamMapF);
    }
    

    private final void evaluateTermWirksamkeit(RootTerm rootTerm,
            BooleanTerm booleanTerm, 
            Map<BooleanTerm, BooleanResult> termResults, 
            Map<BooleanTerm, BooleanResult> wirksamMapT, Map<BooleanTerm, BooleanResult> wirksamMapF) {
    	            
    	// der Term ist nur ein BasicBooleanTerm 
        if (booleanTerm instanceof BasicBooleanTerm) {
 
        	BooleanResult result = termResults.get(booleanTerm);
        	
   	 		if(result == BooleanResult.TRUE) {
 				wirksamMapT.put(booleanTerm, BooleanResult.TRUE);                   	 		           	 				           	 			
   	 		}
   	 		if(result == BooleanResult.FALSE) {
 				wirksamMapF.put(booleanTerm, BooleanResult.FALSE);                   	 		           	 				           	 			
   	 		}        	        	
        }    	
    	
        // der Term ist irgendwie zusammengesetzt und wird jetzt traversiert
        if (booleanTerm instanceof OperatorTerm) {
        	
        	OperatorTerm operatorTerm = (OperatorTerm)booleanTerm;

            List<BooleanResult> operatorResults = new LinkedList<BooleanResult>();
            
            // Operation && oder || ermitteln
            final int OPERATION_UNDEF = 0;
            final int OPERATION_AND = 1;
            final int OPERATION_OR = 2;
            int operation = OPERATION_UNDEF;
            
            String operationString = ((OperatorTerm)booleanTerm).getShortNameOfOperator();
            
            if("AND".equals(operationString)) {
            	 operation = OPERATION_AND;
            }
            if("OR".equals(operationString)) {
            	 operation = OPERATION_OR;
            }

            // the ! operator
            if(operatorTerm.getOperands().size() ==1) {
            	BooleanTerm rhs = operatorTerm.getOperands().get(0);
            	BooleanResult resultR = termResults.get(rhs);
            	
   	 			if(rhs instanceof BasicBooleanTerm) {
           	 		if(resultR == BooleanResult.TRUE) {
      	 				wirksamMapT.put(rhs, BooleanResult.TRUE);                   	 		           	 				           	 			
           	 		}
           	 		if(resultR == BooleanResult.FALSE) {
      	 				wirksamMapF.put(rhs, BooleanResult.FALSE);                   	 		           	 				           	 			
           	 		}
  	 			} else {
  	 				// the !(...) construction
           	 		evaluateTermWirksamkeit(rootTerm, rhs, termResults, wirksamMapT, wirksamMapF);  	 				
  	 			}
           
            }
            
            
           if(operatorTerm.getOperands().size() == 2) {
            	BooleanTerm lhs = operatorTerm.getOperands().get(0);
            	BooleanTerm rhs = operatorTerm.getOperands().get(1);
            	
            	BooleanResult resultL = termResults.get(lhs);
            	BooleanResult resultR = termResults.get(rhs);
	            
	            // Retrieve the Boolean result produced by the assignment of the
	            // operands.
	            BooleanAssignment operatorAssignment = new BooleanAssignment(operatorResults);
	            Boolean operationResult = ((OperatorTerm) booleanTerm).getOperator().getPossibleAssignments().get(operatorAssignment);
	            
	            boolean wirksamL = false;
	            boolean wirksamR = false;
	            
           	 	if(operation == OPERATION_AND) {
           	 		if(resultL == BooleanResult.FALSE) {
       	 				wirksamL = true;
           	 			if(lhs instanceof BasicBooleanTerm) {
          	 				wirksamMapF.put(lhs, BooleanResult.FALSE);                   	 		           	 				
          	 			}           	 			
           	 		}
           	 		if(resultL == BooleanResult.TRUE && resultR == BooleanResult.FALSE) {
       	 				wirksamR = true;
           	 			if(rhs instanceof BasicBooleanTerm) {
           	 				wirksamMapF.put(rhs, BooleanResult.FALSE);                   	 		           	 				
           	 			}           	 			
           	 		}
           	 		if(resultL == BooleanResult.TRUE && resultR == BooleanResult.TRUE) {
       	 				wirksamL = true;
       	 				wirksamR = true;
           	 			if(rhs instanceof BasicBooleanTerm) {
           	 				wirksamMapT.put(rhs, BooleanResult.TRUE);              	 		           	 				
           	 			}           	 			
           	 			if(lhs instanceof BasicBooleanTerm) {
           	 				wirksamMapT.put(lhs, BooleanResult.TRUE);                   	 		           	 				
           	 			}           	 			
           	 		}
           	 	
           	 	}

           	 	if(operation == OPERATION_OR) {
           	 		if(resultL == BooleanResult.TRUE) {
       	 				wirksamL = true;
           	 			if(lhs instanceof BasicBooleanTerm) {
          	 				wirksamMapT.put(lhs, BooleanResult.TRUE);              	 		           	 				
           	 			}           	 			
           	 		}
           	 		if(resultL == BooleanResult.FALSE && resultR == BooleanResult.TRUE) {
       	 				wirksamR = true;
           	 			if(rhs instanceof BasicBooleanTerm) {
          	 				wirksamMapT.put(rhs, BooleanResult.TRUE);              	 		           	 				
           	 			}           	 			
           	 		}
           	 		if(resultL == BooleanResult.FALSE && resultR == BooleanResult.FALSE) {
       	 				wirksamL = true;
       	 				wirksamR = true;
          	 			if(rhs instanceof BasicBooleanTerm) {
          	 				wirksamMapF.put(rhs, BooleanResult.FALSE);              	 		           	 				
           	 			}           	 			
           	 			if(lhs instanceof BasicBooleanTerm) {
          	 				wirksamMapF.put(lhs, BooleanResult.FALSE);              	 		           	 				
           	 			}           	 			
           	 		}           	 		
           	 	}
            
           	 	if(wirksamL)
           	 		evaluateTermWirksamkeit(rootTerm, lhs, termResults, wirksamMapT, wirksamMapF);
           	 	
           	 	if(wirksamR)
           	 		evaluateTermWirksamkeit(rootTerm, rhs, termResults, wirksamMapT, wirksamMapF);

           } else {            
            	// vermutlich der unäre !-Operator
	            for (BooleanTerm subTerms : operatorTerm.getOperands()) {	            	   
	            	evaluateTermWirksamkeit(rootTerm, subTerms, termResults, wirksamMapT, wirksamMapF);
	            }
            }
        }
    }
}
