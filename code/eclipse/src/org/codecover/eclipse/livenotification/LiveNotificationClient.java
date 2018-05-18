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

package org.codecover.eclipse.livenotification;

import java.io.IOException;
import java.io.Writer;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.util.Observable;
import java.util.Set;

import javax.management.AttributeNotFoundException;
import javax.management.InstanceNotFoundException;
import javax.management.ListenerNotFoundException;
import javax.management.MBeanException;
import javax.management.MBeanServerConnection;
import javax.management.MBeanServerNotification;
import javax.management.MalformedObjectNameException;
import javax.management.Notification;
import javax.management.NotificationListener;
import javax.management.ObjectName;
import javax.management.ReflectionException;
import javax.management.remote.rmi.RMIConnector;
import javax.management.remote.rmi.RMIServer;

import org.codecover.eclipse.Messages;

/**
 * This class handles all the live notification communication. It is intended to
 * be used by any user-interaction client.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: LiveNotificationClient.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class LiveNotificationClient extends Observable {
    private static final String ERROR_FINISHING_TEST_SESSION = Messages
            .getString("LiveNotificationClient.ERROR_FINISHING_TEST_SESSION"); //$NON-NLS-1$

    private static final String ERROR_GETTING_LOG_FILE_NAME = Messages
            .getString("LiveNotificationClient.ERROR_GETTING_LOG_FILE_NAME"); //$NON-NLS-1$

    private static final String ERROR_ENDING_THE_TEST_CASE = Messages
            .getString("LiveNotificationClient.ERROR_ENDING_THE_TEST_CASE"); //$NON-NLS-1$

    private static final String ERROR_STARTING_THE_TEST_CASE = Messages
            .getString("LiveNotificationClient.ERROR_STARTING_THE_TEST_CASE"); //$NON-NLS-1$

    private static final String ERROR_FETCHING_THE_FILE = Messages
            .getString("LiveNotificationClient.ERROR_FETCHING_THE_FILE"); //$NON-NLS-1$

    private static final String ERROR_WRITING_THE_FILE = Messages
            .getString("LiveNotificationClient.ERROR_WRITING_THE_FILE"); //$NON-NLS-1$

    private static final String ERROR_DOWNLOADING_COVERAGE_LOG = Messages
            .getString("LiveNotificationClient.ERROR_DOWNLOADING_COVERAGE_LOG"); //$NON-NLS-1$

    private static final String ERROR_WITH_THE_CONNECTION = Messages
            .getString("LiveNotificationClient.ERROR_WITH_THE_CONNECTION"); //$NON-NLS-1$

    private static final String ERROR_LOOKUP_REGISTRY = Messages
            .getString("LiveNotificationClient.ERROR_LOOKUP_REGISTRY"); //$NON-NLS-1$

    private static final String ERROR_ESTABLISHING_THE_CONNECTION = Messages
            .getString("LiveNotificationClient.ERROR_ESTABLISHING_THE_CONNECTION"); //$NON-NLS-1$

    /** {@value} */
    public static final String LOOKUP_JMXRMI = "jmxrmi"; //$NON-NLS-1$

    /** {@value} */
    public static final String SERVER_DELEGATE_NAME = "JMImplementation:type=MBeanServerDelegate"; //$NON-NLS-1$

    /** {@value} */
    public static final String MBEAN_OBJECT_NAME = "CodeCover:type=LiveNotificationMBean"; //$NON-NLS-1$

    private final ObjectName mbeanName;

    private RMIConnector connector;

    private MBeanServerConnection serverConnection;

    private NotificationListener listener;

    private ObjectName serverDelegateObjectName;

    private ConnectionState clientState = ConnectionState.NOT_CONNECTED;

    /**
     * Constructor, which initializes the client
     */
    public LiveNotificationClient() {
        ObjectName localName = null;
        try {
            localName = new ObjectName(MBEAN_OBJECT_NAME);
        } catch (MalformedObjectNameException e) {
            // Cannot occur, since the name is a constant and well-formed.
            throw new Error(e);
        }

        this.mbeanName = localName;
    }

    /**
     * Connects this {@link LiveNotificationClient} with the given server
     * through the given port
     * 
     * @param host
     *            the host to connect to
     * @param port
     *            the port to be used
     * @throws LiveNotificationException
     *             Wraps an {@link IOException} occurred while communicating
     *             with the server.
     */
    public void connect(String host, int port) throws LiveNotificationException {
        try {
            Registry registry = LocateRegistry.getRegistry(host, port);
            RMIServer server = (RMIServer) registry.lookup(LOOKUP_JMXRMI);

            this.connector = new RMIConnector(server, null);
            this.connector.connect();

            this.serverConnection = this.connector.getMBeanServerConnection();

            Set<?> nameSet = this.serverConnection.queryNames(this.mbeanName,
                    null);

            if (nameSet.isEmpty()) {
                this.clientState = ConnectionState.CONNECTED_NO_MBEAN;
            } else {
                this.clientState = ConnectionState.CONNECTED_WITH_MBEAN;
            }
            sendStateChangeEvent();

            Set<?> delegateSet = this.serverConnection.queryNames(
                    new ObjectName(SERVER_DELEGATE_NAME), null);

            for (Object serverDelegateName : delegateSet) {
                this.listener = new RegistrationListener();
                this.serverDelegateObjectName = (ObjectName) serverDelegateName;
                this.serverConnection.addNotificationListener(
                        this.serverDelegateObjectName, this.listener, null,
                        null);
            }
        } catch (RemoteException e) {
            throw new LiveNotificationRemoteException(ERROR_ESTABLISHING_THE_CONNECTION, e);
        } catch (NotBoundException e) {
            throw new LiveNotificationNotBoundException(ERROR_LOOKUP_REGISTRY, e);
        } catch (IOException e) {
            disconnect();
            throw new LiveNotificationIOException(ERROR_WITH_THE_CONNECTION, e);
        } catch (MalformedObjectNameException e) {
            // Cannot occur, since the name is a constant and well-formed.
            throw new Error(e);
        } catch (InstanceNotFoundException e) {
            // Cannot occur, since the server delegate is received from the
            // server, so it exists.
            throw new Error(e);
        }
    }

    /**
     * Disconnects the {@link LiveNotificationClient} if it was previously
     * connected.
     * 
     * @throws LiveNotificationException
     *             Wraps an {@link IOException} occurred while communicating
     *             with the server.
     */
    public void disconnect() throws LiveNotificationException {
        if (this.connector == null) {
            return;
        }
        try {
            try {
                if (this.listener != null
                        && this.serverDelegateObjectName != null) {
                    this.serverConnection.removeNotificationListener(
                            this.serverDelegateObjectName, this.listener);
                }
            } catch (IOException e) {
                throw new LiveNotificationIOException(
                        ERROR_WITH_THE_CONNECTION, e);
            } catch (InstanceNotFoundException e) {
                // Will never occur, since it's MBeansServerDelegate.
                throw new Error(e);
            } catch (ListenerNotFoundException e) {
                // Will never occur, since the listener is a local object and no
                // mbean.
                throw new Error(e);
            }

            try {
                this.connector.close();
            } catch (IOException e) {
                throw new LiveNotificationIOException(
                        ERROR_WITH_THE_CONNECTION, e);
            }
        } finally {
            this.connector = null;
            this.serverConnection = null;

            this.clientState = ConnectionState.NOT_CONNECTED;
            sendStateChangeEvent();
        }
    }

    /**
     * Downloads the coverage log file from the server and writes it to the
     * given writer.
     * 
     * @param writer
     *            the writer to be used in writing the coverage log
     * @throws LiveNotificationException
     *             Wraps an {@link IOException} occurred while communicating
     *             with the server or an {@link MBeanException} which in turn
     *             wraps any {@link Exception} thrown by an invoked method.
     */
    public void downloadCoverageLogFile(Writer writer)
            throws LiveNotificationException {
        if (!this.clientState.equals(ConnectionState.CONNECTED_WITH_MBEAN)) {
            return;
        }

        try {
            this.serverConnection.invoke(this.mbeanName, "resetDownload", null, //$NON-NLS-1$
                    null);
        } catch (InstanceNotFoundException e) {
            // Cannot occur, since the invoke method is only reached when the
            // client state is CONNECTED_WITH_MBEAN.
            throw new Error(e);
        } catch (MBeanException e) {
            // The MBeanException wraps any exception thrown by the invoked
            // method, so we wrap and throw it again.
            throw new LiveNotificationMBeanException(
                    ERROR_DOWNLOADING_COVERAGE_LOG, e);
        } catch (ReflectionException e) {
            // Cannot occur, since the methods, that are invoked here are
            // correct.
            throw new Error(e);
        } catch (IOException e) {
            disconnect();
            throw new LiveNotificationIOException(ERROR_WITH_THE_CONNECTION, e);
        }

        try {
            char[] chunk;
            while ((chunk = (char[]) this.serverConnection.invoke(
                    this.mbeanName, "fetchNextLogFileChunk", null, null)) != null) { //$NON-NLS-1$

                try {
                    writer.write(chunk);

                } catch (IOException e) {
                    throw new LiveNotificationIOException(
                            ERROR_WRITING_THE_FILE, e);
                }
            }

            try {
                writer.flush();
            } catch (IOException e) {
                throw new LiveNotificationIOException(ERROR_WRITING_THE_FILE, e);
            }
        } catch (InstanceNotFoundException e) {
            // Cannot occur, since the invoke method is only reached when the
            // client state is CONNECTED_WITH_MBEAN.
            throw new Error(e);
        } catch (MBeanException e) {
            // The MBeanException wraps any exception thrown by the invoked
            // method, so we wrap and throw it again.
            throw new LiveNotificationMBeanException(
                    ERROR_DOWNLOADING_COVERAGE_LOG, e);
        } catch (ReflectionException e) {
            // Cannot occur, since the methods, that are invoked here are
            // correct.
        } catch (IOException e) {
            throw new LiveNotificationIOException(ERROR_FETCHING_THE_FILE, e);
        }
    }

    /**
     * Starts a test case with the given name
     * 
     * @param name
     *            the name
     * @throws LiveNotificationException
     *             Wraps an {@link IOException} occurred while communicating
     *             with the server or an {@link MBeanException} which in turn
     *             wraps any {@link Exception} thrown by an invoked method.
     */
    public void startTestCase(String name) throws LiveNotificationException {
        if (!this.clientState.equals(ConnectionState.CONNECTED_WITH_MBEAN)) {
            return;
        }

        try {
            this.serverConnection.invoke(this.mbeanName, "startTestCase", //$NON-NLS-1$
                    new Object[] {name}, new String[] {"java.lang.String"}); //$NON-NLS-1$
        } catch (InstanceNotFoundException e) {
            // Cannot occur, since the invoke method is only reached when the
            // client state is CONNECTED_WITH_MBEAN.
            throw new Error(e);
        } catch (MBeanException e) {
            // The MBeanException wraps any exception thrown by the invoked
            // method, so we wrap and throw it again.
            throw new LiveNotificationMBeanException(
                    ERROR_STARTING_THE_TEST_CASE, e);
        } catch (ReflectionException e) {
            // Cannot occur, since the methods, that are invoked here are
            // correct.
            throw new Error(e);
        } catch (IOException e) {
            disconnect();
            throw new LiveNotificationIOException(ERROR_WITH_THE_CONNECTION, e);
        }
    }

    /**
     * End the currently running test case
     * 
     * @throws LiveNotificationException
     *             Wraps an {@link IOException} occurred while communicating
     *             with the server or an {@link MBeanException} which in turn
     *             wraps any {@link Exception} thrown by an invoked method.
     */
    public void endTestCase() throws LiveNotificationException {
        if (!this.clientState.equals(ConnectionState.CONNECTED_WITH_MBEAN)) {
            return;
        }

        try {
            this.serverConnection.invoke(this.mbeanName, "endTestCase", null, //$NON-NLS-1$
                    null);
        } catch (InstanceNotFoundException e) {
            // Cannot occur, since the invoke method is only reached when the
            // client state is CONNECTED_WITH_MBEAN.
            throw new Error(e);
        } catch (MBeanException e) {
            // The MBeanException wraps any exception thrown by the invoked
            // method, so we wrap and throw it again.
            throw new LiveNotificationMBeanException(
                    ERROR_ENDING_THE_TEST_CASE, e);
        } catch (ReflectionException e) {
            // Cannot occur, since the methods, that are invoked here are
            // correct.
            throw new Error(e);
        } catch (IOException e) {
            disconnect();
            throw new LiveNotificationIOException(ERROR_WITH_THE_CONNECTION, e);
        }
    }

    /**
     * Gets the String representing the filename of the coverage log file
     * 
     * @return the filename
     * @throws LiveNotificationException
     *             Wraps an {@link IOException} occurred while communicating
     *             with the server or an {@link MBeanException} which in turn
     *             wraps any {@link Exception} thrown by an invoked method.
     */
    public String getLogFileName() throws LiveNotificationException {
        if (!this.clientState.equals(ConnectionState.CONNECTED_WITH_MBEAN)) {
            return null;
        }

        try {
            return (String) this.serverConnection.getAttribute(this.mbeanName,
                    "LogFileName"); //$NON-NLS-1$
        } catch (InstanceNotFoundException e) {
            // Cannot occur, since the invoke method is only reached when the
            // client state is CONNECTED_WITH_MBEAN.
            throw new Error(e);
        } catch (MBeanException e) {
            // The MBeanException wraps any exception thrown by the invoked
            // method, so we wrap and throw it again.
            throw new LiveNotificationMBeanException(
                    ERROR_GETTING_LOG_FILE_NAME, e);
        } catch (ReflectionException e) {
            // Cannot occur, since the methods, that are invoked here are
            // correct.
            throw new Error(e);
        } catch (IOException e) {
            disconnect();
            throw new LiveNotificationIOException(ERROR_WITH_THE_CONNECTION, e);
        } catch (AttributeNotFoundException e) {
            // Cannot occur, since the getAttribute method is only reached when
            // the client state is CONNECTED_WITH_MBEAN.
            throw new Error(e);
        }
    }

    /**
     * Finishes the whole test session
     * 
     * @throws LiveNotificationException
     *             Wraps an {@link IOException} occurred while communicating
     *             with the server or an {@link MBeanException} which in turn
     *             wraps any {@link Exception} thrown by an invoked method.
     */
    public void finish() throws LiveNotificationException {
        if (!this.clientState.equals(ConnectionState.CONNECTED_WITH_MBEAN)) {
            return;
        }

        try {
            this.serverConnection.invoke(this.mbeanName, "finish", null, null); //$NON-NLS-1$
        } catch (InstanceNotFoundException e) {
            // Cannot occur, since the invoke method is only reached when the
            // client state is CONNECTED_WITH_MBEAN.
            throw new Error(e);
        } catch (MBeanException e) {
            // The MBeanException wraps any exception thrown by the invoked
            // method, so we wrap and throw it again.
            throw new LiveNotificationMBeanException(
                    ERROR_FINISHING_TEST_SESSION, e);
        } catch (ReflectionException e) {
            // Cannot occur, since the methods, that are invoked here are
            // correct.
            throw new Error(e);
        } catch (IOException e) {
            disconnect();
            throw new LiveNotificationIOException(ERROR_WITH_THE_CONNECTION, e);
        }
    }

    private void sendStateChangeEvent() {
        setChanged();
        notifyObservers(this.clientState);
        clearChanged();
    }

    private final class RegistrationListener implements NotificationListener {

        /**
         * (non-Javadoc)
         * 
         * @see javax.management.NotificationListener#handleNotification(javax.management.Notification,
         *      java.lang.Object)
         */
        @Override
		public void handleNotification(Notification notification,
                Object handback) {
            if (!(notification instanceof MBeanServerNotification)) {
                return;
            }
            MBeanServerNotification serverNotification = (MBeanServerNotification) notification;

            ObjectName receivedName = serverNotification.getMBeanName();
            if (!receivedName.equals(LiveNotificationClient.this.mbeanName)) {
                return;
            }

            String type = serverNotification.getType();
            if (type.equals(MBeanServerNotification.REGISTRATION_NOTIFICATION)) {
                LiveNotificationClient.this.clientState = ConnectionState.CONNECTED_WITH_MBEAN;
                sendStateChangeEvent();
            } else if (type
                    .equals(MBeanServerNotification.UNREGISTRATION_NOTIFICATION)) {
                LiveNotificationClient.this.clientState = ConnectionState.CONNECTED_NO_MBEAN;
                sendStateChangeEvent();
            }
        }
    }

    /**
     * A enum containing all the states that can be held by this
     * {@link LiveNotificationClient}
     * 
     * @author Markus Wittlinger
     * @version 1.0 ($Id: LiveNotificationClient.java 1 2007-12-12 17:37:26Z t-scheller $)
     */
    public enum ConnectionState {
        /**
         * The constant representing the state, when no connection was
         * established
         */
        NOT_CONNECTED,
        /**
         * The constant representing the state, when a connection was
         * established, but the mbean is not registered
         */
        CONNECTED_NO_MBEAN,
        /**
         * * The constant representing the state, when a connection was
         * established and the mbean is registered
         */
        CONNECTED_WITH_MBEAN
    }
}
