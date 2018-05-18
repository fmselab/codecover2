package org.codecover.simplejavaapp;

import org.codecover.simplejavaapp.controller.AppController;

/**
 * @author Markus Wittlinger
 * @version 1.0 ($Id: SimpleJavaApp.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class SimpleJavaApp {
    final static boolean thisIsAMac = System.getProperty("mrj.version") != null;

    /**
     * @param args
     */
    public static void main(String[] args) {
        if (thisIsAMac) {
            System.setProperty("apple.laf.useScreenMenuBar", "true");
        }
        
        AppController.initialize();
    }
}
