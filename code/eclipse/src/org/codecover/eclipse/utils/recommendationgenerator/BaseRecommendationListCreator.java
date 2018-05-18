package org.codecover.eclipse.utils.recommendationgenerator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import org.codecover.metrics.coverage.TermCoverage;
import org.codecover.model.TestCase;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.mast.BasicBooleanTerm;
import org.codecover.model.mast.BasicStatement;
import org.codecover.model.mast.BooleanAssignment;
import org.codecover.model.mast.BooleanResult;
import org.codecover.model.mast.BooleanTerm;
import org.codecover.model.mast.Branch;
import org.codecover.model.mast.ConditionalStatement;
import org.codecover.model.mast.CoverableItem;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.Location;
import org.codecover.model.mast.LoopingStatement;
import org.codecover.model.mast.RootTerm;
import org.codecover.model.mast.Statement;
import org.codecover.model.mast.StatementSequence;

public class BaseRecommendationListCreator {

	
	private String currentMethodName;
	private HashMap<Branch, BranchInfo> branchInfoMap;
	private Vector<TestCase> testCases;
	private ArrayList<UncoveredBranch> uncoveredBranches = new ArrayList<UncoveredBranch>();
	private TestSessionContainer testSessionContainer;
	
	
	public ArrayList<UncoveredBranch> getUncoveredBranches() {
		return uncoveredBranches;
	}
	
	public BaseRecommendationListCreator(TestSessionContainer testSessionContainer) {
		this.testSessionContainer = testSessionContainer;
	}

	
	@SuppressWarnings("nls")
	public void createRecommendationList() {
		branchInfoMap = new HashMap<Branch, BranchInfo>();
		uncoveredBranches = new ArrayList<UncoveredBranch>();

		// Logger logger = new SimpleLogger();
		// final MASTBuilder builder = new MASTBuilder(logger);
		// testSessionContainer = TestSessionContainer.load(
		// org.codecover.model.extensions.PluginManager.create(), logger,
		// builder, dbPath);

		final HierarchyLevel hl = testSessionContainer.getCode();

		analyse1(hl, "");
		
		List<UncoveredBranch> rem = new ArrayList<UncoveredBranch>();
		for (UncoveredBranch ub : uncoveredBranches) {
			if (ub.m_testCaseList.size() == 0) {
				rem.add(ub);
			}
		}
		uncoveredBranches.removeAll(rem);
	}
	
	
	
	/**
	 * Geht rekursiv d
	 * 
	 * @param hl
	 * @param qualifierParent
	 */
	@SuppressWarnings("nls")
	public void analyse1(HierarchyLevel hl, String qualifierParent) {

		if (!"".equals(qualifierParent)) { //$NON-NLS-1$
			qualifierParent += "."; //$NON-NLS-1$
		}

		if (qualifierParent.startsWith("default package.org.apache.jsp")) { //$NON-NLS-1$
			return;
		}

		if ("method".equals(hl.getType().getInternalName())) { //$NON-NLS-1$

			int amountStatements = 0;

			final int lengthX = "default package".length() + 1;
			currentMethodName = qualifierParent + hl.getName();
			currentMethodName = currentMethodName.substring(lengthX);
			final List<StatementSequence> statements = hl.getSequences();

			if (statements.size() > 1) {
				System.out.println("statements.size = " + statements.size());
			}
			for (final StatementSequence ss : statements) {
				amountStatements += analyse2(ss, false, qualifierParent, hl);
			}
		}

		final List<HierarchyLevel> kinder = hl.getChildren();
		for (final HierarchyLevel h : kinder) {

			analyse1(h, qualifierParent + hl.getName());
		}
	}
	
	
	

	/**
	 * Funktion, die eigentlich rekursiv alles durch geht.
	 * 
	 * @param statementSequence
	 *            Liste von Statements
	 * @param parent
	 *            übergeordnetes Stamtement
	 * @param l
	 * @param onlyCountStatements
	 * @param qualifierParent
	 * @param hl
	 * @return
	 */
	@SuppressWarnings("nls")
	private int analyse2(StatementSequence statementSequence, boolean onlyCountStatements, String qualifierParent,
			HierarchyLevel hl) {
		int amountStatements = 0;

		final List<Statement> statementList = statementSequence.getStatements();
		for (final Statement statement : statementList) {

			String statementText = "";

			// IF, Switch, Try
			if (statement instanceof ConditionalStatement) {
				final ConditionalStatement cs = (ConditionalStatement) statement;
				statementText = getDecisionText(cs);

				final String keyword = cs.getKeyword().getContent();
				final boolean isIf = keyword.equals("if");
				final boolean isSwitch = keyword.equals("switch");
				final boolean isCatch = keyword.equals("try");

				final List<Branch> branches = cs.getBranches();

				final Vector<TestCase> testcasesIf = new Vector<TestCase>();

				int branchNr = 0;

				final List<BranchInfo> unexecutedBraches = new Vector<BranchInfo>();

				boolean ifFullyCovered = true;
				for (final Branch branch : branches) {
					int amountStatementsPerBranch = 0;
					final StatementSequence innerSequence = branch.getSequence();

					final Vector<TestCase> testcasesBranch = getCoverage(branch.getCoverableItem());

					if (testcasesBranch.size() == 0) {
						// Aha: in diesem Branch haben wir keinen Testfall

						final BranchInfo branchInfo = new BranchInfo();
						branchInfo.m_hierarchyLevel = hl;
						branchInfoMap.put(branch, branchInfo);

						if (isIf) {
							branchInfo.m_type = BranchType.ifBranch;
							ifFullyCovered = false;
						}

						if (isSwitch) {
							branchInfo.m_type = BranchType.switchBranch;
						}

						if (isCatch) {
							branchInfo.m_type = BranchType.catchBranch;
						}

						branchInfo.m_branchNr = branchNr;
						branchInfo.m_statementText = statementText;

						if (isSwitch) {
							if (branch.getDecision().getLocations().iterator().hasNext()) {
								final Location location = branch.getDecision().getLocations().iterator().next();

								final int startOffSet = location.getStartOffset();
								final int endOffSet = location.getEndOffset();

								final String content = location.getFile().getContent().substring(startOffSet, endOffSet);

								branchInfo.m_statementText += " : " + content;
							}
						}

						if (isCatch) {
							if (branch.getDecision().getLocations().iterator().hasNext()) {
								final Location location = branch.getDecision().getLocations().iterator().next();

								final int startOffSet = location.getStartOffset();
								final int endOffSet = location.getEndOffset();

								final String content = location.getFile().getContent().substring(startOffSet, endOffSet);

								branchInfo.m_statementText = content;
							}
						}

						unexecutedBraches.add(branchInfo);
						amountStatementsPerBranch = analyse2(innerSequence, true, qualifierParent, hl);

						branchInfo.m_amountLines = amountStatementsPerBranch;

					} else {
						amountStatementsPerBranch = analyse2(innerSequence, onlyCountStatements, qualifierParent, hl);
					}

					amountStatements += amountStatementsPerBranch;
					testcasesIf.addAll(testcasesBranch);

					branchNr++;
				}

				if (ifFullyCovered && isIf) {
					// wenn nun beide Zweige ausgeführt sind, die Terme
					// betrachten
					handleTerms(cs, statementText, qualifierParent, currentMethodName, hl);
				}

				if (!onlyCountStatements) {
					for (final BranchInfo b : unexecutedBraches) {
						this.uncoveredBranches.add(new UncoveredBranch(currentMethodName, b, testcasesIf));
					}
				}
			}

			if (statement instanceof LoopingStatement) {
				final LoopingStatement ls = (LoopingStatement) statement;

				if (ls.getKeyword().getContent().equals("do")) {
					statementText = getLoopTextDo(ls);
				} else {
					statementText = getLoopTextForWhile(ls);
				}

				final int amountStatementsLoop = analyse2(ls.getBody(), true, qualifierParent, hl);
				amountStatements += amountStatementsLoop;

				getCoverage(ls.getCoverableItem());

				final CoverableItem me = ls.getMultipleExecutedItem();
				final CoverableItem ne = ls.getNeverExecutedItem();
				final CoverableItem oe = ls.getOnceExecutedItem();

				final Vector<TestCase> testcasesME = getCoverage(me);
				final Vector<TestCase> testcasesNE = getCoverage(ne);
				final Vector<TestCase> testcasesOE = getCoverage(oe);

				final Vector<TestCase> testcasesAll = new Vector<TestCase>();
				testcasesAll.addAll(testcasesME);
				testcasesAll.addAll(testcasesNE);
				testcasesAll.addAll(testcasesOE);

				if (testcasesME.size() == 0) {
					// Aha: diese Schleife wurde nicht mehrfach ausgeführt
					final BranchInfo branchInfo = new BranchInfo();
					branchInfo.m_hierarchyLevel = hl;
					branchInfo.m_type = BranchType.whileN;
					branchInfo.m_statementText = statementText;
					uncoveredBranches.add(new UncoveredBranch(currentMethodName, branchInfo, testcasesAll));
				}

				if (testcasesNE.size() == 0) {
					// Aha: in diesem Branch haben wir keinen Testfall
					final BranchInfo branchInfo = new BranchInfo();
					branchInfo.m_amountLines = amountStatementsLoop;
					branchInfo.m_hierarchyLevel = hl;
					branchInfo.m_type = BranchType.while0;
					branchInfo.m_statementText = statementText;
					uncoveredBranches.add(new UncoveredBranch(currentMethodName, branchInfo, testcasesAll));
				}

				if (testcasesOE.size() == 0) {
					// Aha: in diesem Branch haben wir keinen Testfall
					final BranchInfo branchInfo = new BranchInfo();
					branchInfo.m_hierarchyLevel = hl;
					branchInfo.m_type = BranchType.while1;
					branchInfo.m_statementText = statementText;
					uncoveredBranches.add(new UncoveredBranch(currentMethodName, branchInfo, testcasesAll));

				}

				if (testcasesOE.size() > 0 || testcasesME.size() > 0) {
					analyse2(ls.getBody(), onlyCountStatements, qualifierParent, hl);
				} else {
					// Schleife gar nicht ausgeführt
					final BranchInfo branchInfo = new BranchInfo();
					branchInfo.m_amountLines = amountStatementsLoop;
					branchInfo.m_hierarchyLevel = hl;
					branchInfo.m_type = BranchType.whileE;
					branchInfo.m_statementText = statementText;
					uncoveredBranches.add(new UncoveredBranch(currentMethodName, branchInfo, testcasesAll));
				}
			}

			if (statement instanceof BasicStatement) {
				statementText = getText(statement);
			}

			amountStatements++;
		}
		return amountStatements;
	}

	@SuppressWarnings("nls")
	private String getDecisionText(ConditionalStatement cs) {

		if (cs.getKeyword().getContent().equals("try")) {
			return "";
		}

		String decisionText = "";

		final Location location = cs.getLocation().getLocations().iterator().next();

		final int startOffSet = location.getStartOffset();
		final int endOffSet = location.getEndOffset();

		final String content = location.getFile().getContent().substring(startOffSet, endOffSet);

		int startErsterZweig = -1;
		final List<Branch> branches = cs.getBranches();
		for (final Branch b : branches) {
			if (b.getLocation().getLocations().size() > 0) {
				final int tmp = b.getLocation().getLocations().get(0).getStartOffset();
				if (startErsterZweig == -1) {
					startErsterZweig = tmp;
				} else {
					startErsterZweig = tmp < startErsterZweig ? tmp : startErsterZweig; // Minimuim
				}
			}
		}

		String tmp1;

		final int praedikatBeginn = content.indexOf('(');
		try {

			tmp1 = location.getFile().getContent().substring(praedikatBeginn + startOffSet + 1, startErsterZweig);

			final int praedikatEnde = tmp1.lastIndexOf(')');
			decisionText = tmp1.substring(0, praedikatEnde);

		} catch (final Exception e) {
			e.printStackTrace();
		}

		return decisionText;
	}

	private Vector<TestCase> getCoverage(CoverableItem item) {

		final Vector<TestCase> coveredBy = new Vector<TestCase>();
		final List<TestSession> testSessions = testSessionContainer.getTestSessions();

		for (final TestSession ts : testSessions) {
			final List<TestCase> testcases = ts.getTestCases();
			for (final TestCase tc : testcases) {
				if (tc.getCoverageCount(item) > 0) {
					coveredBy.add(tc);
				}
			}
		}

		return coveredBy;
	}

	@SuppressWarnings("nls")
	private void handleTerms(ConditionalStatement cs, String statementText, String qualifierParent, String mActualMethodName,
			HierarchyLevel hl) {

		// ist wohl immer nur 1 Term
		for (final RootTerm rt : cs.getTerms()) {

			final Vector<TermCoverageReport> termCoverageReportList = analyseRootTerm(getAlltestCases(), rt);

			for (final TermCoverageReport tcr : termCoverageReportList) {

				final BranchInfo branchInfo = new BranchInfo();
				branchInfo.m_type = BranchType.term;
				branchInfo.m_hierarchyLevel = hl;
				branchInfo.m_statementText = statementText;
				branchInfo.m_amountLines = rt.getAmountBasicBooleanTerms();
				branchInfo.m_termText = tcr.termText + "-->" + (tcr.missingValue ? "True" : "False");
				branchInfo.m_coverage = tcr.m_coverage;

				this.uncoveredBranches.add(new UncoveredBranch(currentMethodName, branchInfo, tcr.testCaseSet));
			}
		}

	}

	@SuppressWarnings("nls")
	private String getLoopTextDo(LoopingStatement ls) {
		String praedikatText = "";

		final Location location = ls.getLocation().getLocations().iterator().next();

		final int startOffSet = location.getStartOffset();
		final int endOffSet = location.getEndOffset();

		location.getFile().getContent().substring(startOffSet, endOffSet);

		final int endeKoerper = ls.getBody().getLocation().getLocations().get(0).getEndOffset();

		try {
			final String tmp1 = location.getFile().getContent().substring(endeKoerper, endOffSet);

			final int praedikatBeginn = tmp1.indexOf('(');
			final int praedikatEnde = tmp1.lastIndexOf(')');

			praedikatText = tmp1.substring(praedikatBeginn + 1, praedikatEnde);

		} catch (final Exception e) {
			e.printStackTrace();
		}

		return praedikatText;
	}

	@SuppressWarnings("nls")
	private String getLoopTextForWhile(LoopingStatement ls) {
		String praedikatText = "";

		final Location location = ls.getLocation().getLocations().iterator().next();

		final int startOffSet = location.getStartOffset();
		final int endOffSet = location.getEndOffset();

		final String content = location.getFile().getContent().substring(startOffSet, endOffSet);

		try {
			if (ls.getBody().getLocation().getLocations().size() == 0) {
				return ""; // body ist nur ein return ...
			}

			final int startKoerper = ls.getBody().getLocation().getLocations().get(0).getStartOffset();

			final int praedikatBeginn = content.indexOf('(');

			final String tmp1 = location.getFile().getContent().substring(praedikatBeginn + startOffSet + 1, startKoerper);

			final int praedikatEnde = tmp1.lastIndexOf(')');
			praedikatText = tmp1.substring(0, praedikatEnde);

		} catch (final Exception e) {
			e.printStackTrace();
		}

		return praedikatText;
	}

	private String getText(Statement s) {
		if (s.getLocation().getLocations().iterator().hasNext()) {
			return s.getLocation().getLocations().iterator().next().getContent();
		} else {
			return ""; //$NON-NLS-1$
		}
	}

	private final List<TestCase> getAlltestCases() {
		if (testCases == null) {
			testCases = new Vector<TestCase>();

			for (final TestSession testSession : testSessionContainer.getTestSessions()) {
				testCases.addAll(testSession.getTestCases());
			}
		}
		return testCases;
	}

	private Vector<TermCoverageReport> analyseRootTerm(Collection<TestCase> testCases, final RootTerm rootTerm) {

		final Vector<TermCoverageReport> termCoverageReportList = new Vector<TermCoverageReport>();

		final TermCoverage termCoverage = TermCoverage.getInstance();

		// 1. Wirksamkeiten ermitteln

		// merge assignments of all the test cases
		final Map<BooleanAssignment, Boolean> assignmentsMap = new HashMap<BooleanAssignment, Boolean>();
		for (final TestCase testCase : testCases) {
			assignmentsMap.putAll(testCase.getAssignments(rootTerm));
		}

		final Map<BooleanTerm, BooleanResult> wirksamMapT = new HashMap<BooleanTerm, BooleanResult>();
		final Map<BooleanTerm, BooleanResult> wirksamMapF = new HashMap<BooleanTerm, BooleanResult>();

		final Set<BooleanAssignment> assignmentsSet = assignmentsMap.keySet();
		for (final BooleanAssignment assignment : assignmentsSet) {

			final Map<BooleanTerm, BooleanResult> termResults = new HashMap<BooleanTerm, BooleanResult>();
			termCoverage.evaluateTermWirksamkeit(rootTerm, rootTerm.getTerm(), assignment, termResults, wirksamMapT, wirksamMapF);
		}

		// nun welche Einzelterme sind nur teilweise wirksam
		for (int index = 0; index < rootTerm.getAmountBasicBooleanTerms(); index++) {

			final BasicBooleanTerm bbt = rootTerm.getTermAtPosition(index);

			final boolean TUnwirksam = !wirksamMapT.containsKey(bbt);
			final boolean FUnwirksam = !wirksamMapF.containsKey(bbt);

			if (TUnwirksam || FUnwirksam) {

				TermCoverageReport tcr = null;
				for (final TestCase testCase : testCases) {
					final Set<BooleanAssignment> assignmentSet = testCase.getAssignments(rootTerm).keySet();

					boolean testcaseFits = false;
					boolean missingValue = false;
					for (final BooleanAssignment assignment : assignmentSet) {

						final BooleanResult booleanResult = assignment.getResults().get(index);
						if (TUnwirksam && booleanResult == BooleanResult.FALSE) {
							testcaseFits = true;
							missingValue = true;
							break;
						}
						if (FUnwirksam && booleanResult == BooleanResult.TRUE) {
							testcaseFits = true;
							missingValue = false;
							break;
						}
					}

					if (testcaseFits) {
						if (tcr == null) {
							tcr = new TermCoverageReport();
							termCoverageReportList.add(tcr);
							tcr.missingValue = missingValue;
							tcr.m_coverage = (wirksamMapT.size() + wirksamMapF.size()) * 100
									/ (rootTerm.getAmountBasicBooleanTerms() * 2);

							for (final Location location : bbt.getLocation().getLocations()) {
								if (tcr.termText == null) {
									tcr.termText = location.getContent();
								} else {
									tcr.termText += location.getContent();
								}
							}
						}
						tcr.testCaseSet.add(testCase);
					}
				}
			}
		}
		return termCoverageReportList;
	}

	public void reset() {
		createRecommendationList();
	}
}




/*
 * // Report ausgeben out.println("<html><table border=\"1\">"); out.println(
 * "<tr><th>Klasse</th><th>Pr‰dikat</th><th>Term</th><th>Anzahl Zeilen</th><th>Cov %</th><th>Typ</th><th>Testf‰lle</th><th>Anzahl Testf‰lle</th><th>Anzahl Testsequenzen</th>"
 * + priorityHeader + "</tr>");
 * 
 * for (final CoverageReport coverageReportRow : coverageReport) {
 * 
 * if (coverageReportRow.m_testCaseList.size() == 0) { continue; }
 * 
 * out.println("<tr>"); out.println("<td valign=\"top\">");
 * out.println(coverageReportRow.m_methode); out.println("</td>");
 * 
 * out.println("<td valign=\"top\">");
 * out.println(makeValidHTML(coverageReportRow.m_branchInfo.m_statementText));
 * out.println("</td>");
 * 
 * if (coverageReportRow.m_branchInfo.m_type == BranchType.term) {
 * out.println("<td valign=\"top\">");
 * out.println(coverageReportRow.m_branchInfo.m_termText); out.println("</td>");
 * } else { out.println("<td valign=\"top\">"); out.println("&nbsp;");
 * out.println("</td>"); }
 * 
 * out.println("<td valign=\"top\">"); out.println(String
 * .valueOf(coverageReportRow.m_branchInfo.m_amountLines) + ",0");
 * out.println("</td>");
 * 
 * out.println("<td valign=\"top\">"); out.println(String
 * .valueOf(coverageReportRow.m_branchInfo.m_coverage)); out.println("</td>");
 * 
 * out.println("<td valign=\"top\">"); if (coverageReportRow.m_branchInfo.m_type
 * == BranchType.ifBranch) {
 * out.println(branchText[coverageReportRow.m_branchInfo.m_branchNr]); }
 * 
 * if (coverageReportRow.m_branchInfo.m_type == BranchType.switchBranch) {
 * out.println("switch"); }
 * 
 * if (coverageReportRow.m_branchInfo.m_type == BranchType.catchBranch) {
 * out.println("catch"); }
 * 
 * if (coverageReportRow.m_branchInfo.m_type == BranchType.while0) {
 * out.println("while 0mal"); } if (coverageReportRow.m_branchInfo.m_type ==
 * BranchType.while1) { out.println("while 1mal"); } if
 * (coverageReportRow.m_branchInfo.m_type == BranchType.whileN) {
 * out.println("while Nmal"); } if (coverageReportRow.m_branchInfo.m_type ==
 * BranchType.whileE) { out.println("while ausgeführt"); } if
 * (coverageReportRow.m_branchInfo.m_type == BranchType.term) {
 * out.println("term"); }
 * 
 * out.println("</td>");
 * 
 * out.println("<td valign=\"top\">"); int amountTestcases = 0; final int
 * amountTestcasesPriority[] = new int[5];
 * 
 * final Hashtable<String, String> countTestSequences = new Hashtable<String,
 * String>();
 * 
 * for (final TestCase tc : coverageReportRow.m_testCaseList) {
 * 
 * TestCaseJustus tcj = TestCaseJustus.get(tc.getName());
 * 
 * if (tcj == null) { tcj = new TestCaseJustus(); tcj.setName(tc.getName());
 * tcj.setPriority('E'); }
 * 
 * amountTestcasesPriority[tcj.getPriority() - 'A']++;
 * 
 * final int slashPos = tc.getName().indexOf('/'); if (slashPos != -1) { final
 * String testSequenceName = tc.getName().substring( 0, slashPos);
 * countTestSequences.put(testSequenceName, testSequenceName); } else {
 * countTestSequences.put(tc.getName(), tc.getName()); System.out
 * .println("Error 2 - / nicht im Name, keine Testsequenz " + tc.getName());
 * 
 * }
 * 
 * amountTestcases++; if (amountTestcases <= 5) {
 * out.println(makeValidHTML(tc.getName()) + "\n"); } } out.println("</td>");
 * out.println("<td valign=\"top\">"); out.println(amountTestcases);
 * out.println("</td>"); out.println("<td valign=\"top\">");
 * out.println(countTestSequences.size()); out.println("</td>");
 * 
 * for (int i = 0; i < 5; i++) { out.println("<td valign=\"top\">");
 * out.println(amountTestcasesPriority[i]); out.println("</td>"); }
 * out.println("</tr>");
 * 
 * }
 * 
 * out.println("</table></html>"); out.close();
 */

