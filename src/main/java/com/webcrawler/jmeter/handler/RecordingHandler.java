package com.webcrawler.jmeter.handler;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.util.LinkedList;

import org.apache.jmeter.control.LoopController;
import org.apache.jmeter.gui.tree.JMeterTreeModel;
import org.apache.jmeter.gui.tree.JMeterTreeNode;
import org.apache.jmeter.protocol.http.control.RecordingController;
import org.apache.jmeter.protocol.http.proxy.ProxyControl;
import org.apache.jmeter.save.SaveService;
import org.apache.jmeter.testelement.TestElement;
import org.apache.jmeter.testelement.TestPlan;
import org.apache.jmeter.threads.ThreadGroup;
import org.apache.jmeter.util.JMeterUtils;
import org.apache.jorphan.collections.HashTree;
import org.apache.jorphan.collections.ListedHashTree;

import com.webcrawler.util.Constants;

/**
 * The class RecordingHandler is use to Handle all the JMeter recording
 * controller
 * 
 * @author Junaid
 */
public class RecordingHandler {

	private ProxyControl proxy = null;
	private JMeterTreeModel treeModel = null;

	/**
	 * The method init() is use to initialize all the JMeter non-gui parameter
	 * 
	 * @throws Exception
	 *             when unable to initialize JMeter
	 */
	@SuppressWarnings("deprecation")
	public void init(Integer port) throws Exception {

		JMeterUtils.setJMeterHome(Constants.JMETER_HOME);
		JMeterUtils.loadJMeterProperties(JMeterUtils.getJMeterBinDir() + "/jmeter.properties");
		JMeterUtils.initLocale();

		LoopController loopController = new LoopController();
		loopController.setEnabled(true);
		loopController.setLoops(1);

		RecordingController rc = new RecordingController();
		rc.setName("Recording Controller");

		TestPlan testPlan = new TestPlan();

		ThreadGroup threadGroup1 = new ThreadGroup();
		threadGroup1.setSamplerController(loopController);
		threadGroup1.addTestElement(rc); // Target

		ListedHashTree testPlanTree = new ListedHashTree();
		testPlanTree.add(testPlan);
		testPlanTree.add(threadGroup1, testPlan);

		this.treeModel = new JMeterTreeModel(new Object());

		JMeterTreeNode root = (JMeterTreeNode) treeModel.getRoot();
		treeModel.addSubTree(testPlanTree, root);

		proxy = new ProxyControl();
		proxy.setNonGuiTreeModel(treeModel);
		proxy.setTarget(treeModel.getNodeOf(threadGroup1));
		proxy.setGroupingMode(2); // GROUPING_IN_SIMPLE_CONTROLLERS = 2
//		proxy.setGroupingMode(3); // GROUPING_STORE_FIRST_ONLY = 3
		proxy.setSamplerTypeName("2"); // SAMPLER_TYPE_HTTP_SAMPLER_HC4 = "2"
		proxy.setRegexMatch(Boolean.TRUE);
		proxy.setPort(port); // Global Settings -> Port
		proxy.setCaptureHttpHeaders(Boolean.TRUE);
		proxy.setSamplerFollowRedirects(Boolean.TRUE);
//		proxy.setSamplerRedirectAutomatically(Boolean.TRUE);
		proxy.setSamplerDownloadImages(Boolean.TRUE);

		treeModel.addComponent(proxy, (JMeterTreeNode) root.getChildAt(1));
	}

	/**
	 * The method start() is use to start JMeter recording controller proxy
	 * 
	 * @return true is started successfully else false
	 */
	public Integer start(Integer port) {
		try {
			this.proxy.startProxy();
		} catch (Exception e) {
			try {
				Integer freePort = getFreePort();
				
				port = freePort != null ? freePort : port;

				this.proxy.setPort(port);
			} catch (Exception e1) {

			}
		}

		return port;
	}

	/**
	 * The method stop() is use to stop JMeter recording controller proxy.
	 * 
	 * @return The tree that is recorded till now
	 */
	public String stop() {

		OutputStream out = null;

		try {
			this.proxy.stopProxy();

			if (this.treeModel == null) {
				return null;
			}

			out = new ByteArrayOutputStream();

			HashTree tree = treeModel.getTestPlan();

			this.convertSubTree(tree);

			SaveService.saveTree(tree, out);
		} catch (Exception e) {
			return null;
		}

		return out.toString();
	}

	/**
	 * The method convertSubTree() is use to beatify the tree
	 * 
	 * @param tree
	 *            beautified tree
	 */
	private void convertSubTree(HashTree tree) {
		for (Object o : new LinkedList<>(tree.list())) {
			JMeterTreeNode item = (JMeterTreeNode) o;
			convertSubTree(tree.getTree(item));
			TestElement testElement = item.getTestElement(); // requires JMeterTreeNode
			tree.replaceKey(item, testElement);
		}
	}

	/**
	 * The method isAvaiable() is use to check if port is available to listen
	 * 
	 * @param port
	 *            to be checked
	 * @return true is port is listening else false
	 */
	private static Boolean isAvaiable(Integer port) {
		boolean portTaken = false;
		ServerSocket socket = null;
		try {
			socket = new ServerSocket(port);
		} catch (IOException e) {
			portTaken = true;
		} finally {
			if (socket != null) {
				try {
					socket.close();
					socket = null;
				} catch (IOException e) {

				}
			}
		}

		return portTaken;
	}

	/**
	 * The method getFreePort() is use to get random free port
	 * 
	 * @return free port
	 */
	public static Integer getFreePort() {
		ServerSocket socket = null;
		
		try {
			// Get new port
			socket = new ServerSocket(0);
			socket.close();
		} catch (Exception e) {
			return null;
		}

		return socket.getLocalPort();
	}

}
