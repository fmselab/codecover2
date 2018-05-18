package org.codecover.instrumentation.c.counter;

public class CounterManager {
    final private String fileName;
    final private String stmtVarName;
    final private String stmtPrefix;
    final private String branchVarName;
    final private String branchPrefix;
    private final String loopTmpName;
    final private String loopVarName;
    final private String loopPrefix;
    private final String condVarName;
    private final String condPrefix;
    private final String qmoVarName;
    private final String qmoPrefix;

    private int stmtCounter;
    private int branchCounter;
    private int loopCounter;
    private int condCounter;
    private int qmoCounter;

    /**
     * @param id must be a positive integer
     * @param fileName
     */
    public CounterManager(String id, String fileName) {
        this.fileName = fileName;
        stmtPrefix = "S";
        stmtVarName = "CodeCover_S" + id;
        branchPrefix = "B";
        branchVarName = "CodeCover_B" + id;
        loopPrefix = "L";
        loopVarName = "CodeCover_L" + id;
        loopTmpName = "CodeCover_LTMP" + id;
        condPrefix = "C";
        condVarName = "CodeCover_C" + id;
        qmoPrefix = "Q";
        qmoVarName = "CodeCover_Q" + id;
    }

    public String getFileName() {
        return fileName;
    }

    public String stmtVarName() {
        return stmtVarName;
    }

    public String stmtPrefix() {
        return stmtPrefix;
    }

    public String stmtID(int i) {
        return stmtPrefix + Integer.toString(i);
    }

    public int newStmtID() {
        return stmtCounter++;
    }

    public int getStmtCnt() {
        return stmtCounter;
    }

    public String branchVarName() {
        return branchVarName;
    }

    public String branchPrefix() {
        return branchPrefix;
    }

    public String branchID(int i) {
        return branchPrefix + Integer.toString(i);
    }

    public int newBranchID() {
        return branchCounter++;
    }

    public int getBranchCnt() {
        return branchCounter;
    }

    public String loopVarName() {
        return loopVarName;
    }

    public String loopPrefix() {
        return loopPrefix;
    }

    public String loopID(int i) {
        return loopPrefix + Integer.toString(i);
    }

    public String loopTmpName() {
        return loopTmpName;
    }

    public int getloopTmpCnt() {
        return loopCounter;
    }

    public int newloopID() {
        return loopCounter++*3;
    }

    public int getloopCnt() {
        return loopCounter*3;
    }

    public String condVarName() {
        return condVarName;
    }

    public String condPrefix() {
        return condPrefix;
    }

    public int newCondID() {
        return condCounter++;
    }

    public int getCondCnt() {
        return condCounter;
    }

    public String condID(int i) {
        return condPrefix + Integer.toString(i);
    }

    public String qmoVarName() {
        return qmoVarName;
    }

    public String qmoPrefix() {
        return qmoPrefix;
    }

    public String qmoID(int i) {
        return qmoPrefix + Integer.toString(i);
    }

    public int newQmoID() {
        return qmoCounter++;
    }

    public int getQmoCnt() {
        return qmoCounter;
    }
}
