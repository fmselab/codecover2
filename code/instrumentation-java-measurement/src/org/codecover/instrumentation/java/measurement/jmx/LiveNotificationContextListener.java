/******************************************************************************
 * Copyright (c) 2007 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieß, Johannes Langauf,                         *
 *                    Christoph Marian Müller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This program and the accompanying materials           *
 * are made available under the terms of the Eclipse Public License v1.0      *
 * which accompanies this distribution, and is available at                   *
 * http://www.eclipse.org/legal/epl-v10.html                                  *
 ******************************************************************************/

package org.codecover.instrumentation.java.measurement.jmx;

import java.lang.management.ManagementFactory;
import java.lang.reflect.InvocationTargetException;

import javax.management.InstanceAlreadyExistsException;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanRegistrationException;
import javax.management.MBeanServer;
import javax.management.MalformedObjectNameException;
import javax.management.NotCompliantMBeanException;
import javax.management.ObjectName;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

/**
 * TODO comment
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: LiveNotificationContextListener.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class LiveNotificationContextListener implements ServletContextListener {
    private static final String GET_INSTANCE_METHOD_NAME = "getInstance";

    private static final String LIVE_NOTIFICATION_CLASS_NAME = "org.codecover.instrumentation.java.measurement.jmx.LiveNotification";

    private static final String COVERAGE_RESULT_LOG_FILE_CLASS_NAME = "org.codecover.instrumentation.java.measurement.CoverageResultLogFile";

    private static final String ERROR_REGISTERING_MBEAN = "Error registering MBean";

    private static final String ERROR_UNREGISTERING_MBEAN = "Error unregistering MBean";

    private static final String ERROR_INITIALISING_OBJECTS = "Error initialising objects";

    private static final String MBEAN_OBJECT_NAME = "CodeCover:type=LiveNotificationMBean";

    public void contextInitialized(ServletContextEvent servletContextEvent) {
        MBeanServer server = ManagementFactory.getPlatformMBeanServer();
        Object liveNotification = null;

        try {
            liveNotification = Class.forName(LIVE_NOTIFICATION_CLASS_NAME)
                    .newInstance();
            Class.forName(COVERAGE_RESULT_LOG_FILE_CLASS_NAME).getMethod(
                    GET_INSTANCE_METHOD_NAME, null).invoke(null, null);
        } catch (InstantiationException e) {
            servletContextEvent.getServletContext().log(
                    ERROR_INITIALISING_OBJECTS, e);
            return;
        } catch (IllegalAccessException e) {
            servletContextEvent.getServletContext().log(
                    ERROR_INITIALISING_OBJECTS, e);
            return;
        } catch (ClassNotFoundException e) {
            servletContextEvent.getServletContext().log(
                    ERROR_INITIALISING_OBJECTS, e);
            return;
        } catch (IllegalArgumentException e) {
            servletContextEvent.getServletContext().log(
                    ERROR_INITIALISING_OBJECTS, e);
            return;
        } catch (SecurityException e) {
            servletContextEvent.getServletContext().log(
                    ERROR_INITIALISING_OBJECTS, e);
            return;
        } catch (InvocationTargetException e) {
            servletContextEvent.getServletContext().log(
                    ERROR_INITIALISING_OBJECTS, e);
            return;
        } catch (NoSuchMethodException e) {
            servletContextEvent.getServletContext().log(
                    ERROR_INITIALISING_OBJECTS, e);
            return;
        }

        try {
            ObjectName objectName = new ObjectName(MBEAN_OBJECT_NAME);
            if (!server.isRegistered(objectName)) {
                server.registerMBean(liveNotification, objectName);
            }
        } catch (MalformedObjectNameException e) {
            servletContextEvent.getServletContext().log(
                    ERROR_REGISTERING_MBEAN, e);
        } catch (NullPointerException e) {
            servletContextEvent.getServletContext().log(
                    ERROR_REGISTERING_MBEAN, e);
        } catch (InstanceAlreadyExistsException e) {
            servletContextEvent.getServletContext().log(
                    ERROR_REGISTERING_MBEAN, e);
        } catch (MBeanRegistrationException e) {
            servletContextEvent.getServletContext().log(
                    ERROR_REGISTERING_MBEAN, e);
        } catch (NotCompliantMBeanException e) {
            servletContextEvent.getServletContext().log(
                    ERROR_REGISTERING_MBEAN, e);
        }

    }

    public void contextDestroyed(ServletContextEvent servletContextEvent) {
        MBeanServer server = ManagementFactory.getPlatformMBeanServer();
        try {
            ObjectName objectName = new ObjectName(MBEAN_OBJECT_NAME);
            if (server.isRegistered(objectName)) {
                server.unregisterMBean(objectName);
            }
        } catch (MalformedObjectNameException e) {
            servletContextEvent.getServletContext().log(
                    ERROR_UNREGISTERING_MBEAN, e);
        } catch (NullPointerException e) {
            servletContextEvent.getServletContext().log(
                    ERROR_UNREGISTERING_MBEAN, e);
        } catch (MBeanRegistrationException e) {
            servletContextEvent.getServletContext().log(
                    ERROR_UNREGISTERING_MBEAN, e);
        } catch (InstanceNotFoundException e) {
            servletContextEvent.getServletContext().log(
                    ERROR_UNREGISTERING_MBEAN, e);
        }

    }
}
